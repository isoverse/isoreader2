# binary file navigation =====

# reads in binary file
load_binary_file <- function(filepath) {
  # safety checks
  check_arg(
    filepath,
    !missing(filepath) &&
      is_scalar_character(filepath) &&
      file.exists(filepath) &&
      !file.info(filepath)$isdir,
    "must be the path to an existing binary file",
    include_type = FALSE
  )

  # read into an environment so it's mutable
  bfile <- env(
    prev_pos = 1L,
    pos = 1L,
    cnds = try_catch_cnds(TRUE)$conditions,
    has_blocking_cnds = FALSE
  )

  # read
  size <- file.info(filepath)$size
  con <- file(filepath, "rb")
  bfile$bin <- readBin(con, raw(), n = size)
  bfile$end <- length(bfile$bin)
  close(con)

  # analyze macrostructure
  bfile$objects <- NULL
  bfile$objects <- bfile |> read_all_CRuntimeClasses()
  bfile$prev_pos <- 1L
  bfile$current_object <- NULL
  bfile$object_version <- NULL

  return(bfile)
}

# skip position
skip_bytes <- function(bfile, n) {
  bfile$pos <- bfile$pos + as.integer(n)
  return(invisible(bfile))
}

# reset the bfile position
reset_pos <- function(bfile) {
  bfile$pos <- 1L
  return(invisible(bfile))
}

# check stream buffer
check_bfile_buffer <- function(bfile, n, size, type) {
  if (bfile$pos > bfile$end) {
    bfile |>
      register_cnd(
        cli_abort(
          "reached the end of the stream ({bfile$end})"
        )
      )
  }
  if (bfile$pos + n * size > bfile$end) {
    bfile |>
      register_cnd(
        cli_abort(
          "cannot read {n} {.field {type}}{?s} ({n * size} bytes) because stream ends at {bfile$end}"
        )
      )
  }
}

# move to a particular object class
# use reset_pos first if searching from start
# @param bfile the binary file environment
# @param which object class to look for
# @param after_current_pos look for object after current position (by default looks from start)
# @param reset_blocking_cnds reset the blocking cnds flag when moving to this block (i.e. try to read again even if there were errors in other blocks before)
move_to_object <- function(
  bfile,
  class,
  reset_blocking_cnds = TRUE
) {
  # reset bfile object info
  bfile$current_object <- NULL
  bfile$object_version <- NULL

  # find class
  object <- bfile$objects |>
    dplyr::filter(.data$class == !!class, .data$start >= !!bfile$pos)

  # did we find any?
  if (nrow(object) > 0L) {
    # yes, move there
    bfile$current_object <- object$class[1]
    bfile$object_version <- object$version[1]
    bfile$pos <- object$start[1]
    if (reset_blocking_cnds) {
      # reset
      bfile$has_blocking_cnds <- FALSE
    }
  } else {
    # no --> error
    bfile |>
      register_cnd(
        cli_abort(
          "could not find any object with class {cli::col_green(class)} in the file"
        ),
        pos = bfile$pos
      )
  }
  # read the class
  return(invisible(bfile))
}

# read object
read_object <- function(bfile, class, func, ...) {
  object_info <- bfile |> read_CRuntimeClass(class)
  object_read <- bfile |> func(...)
  # return (everything from object_info that's not in object_read + all of object_read)
  # note: if version is re-read, should there be a check here that they are the same?
  object_info |>
    dplyr::select(-dplyr::any_of(names(object_read))) |>
    dplyr::bind_cols(object_read)
}

# uses move_to_object and then the func to read it
move_to_and_read_object <- function(bfile, class, func, ...) {
  bfile |> move_to_object(class) |> read_object(class, func, ...)
}

# register a cnd with the bfile
register_cnd <- function(bfile, exp, pos = bfile$prev_pos) {
  new_cnds <- try_catch_cnds(exp)$conditions
  # trigger block if not blocked yet and there are new errors
  # note that warnings do NOT lead to a block
  if (!bfile$has_blocking_cnds) {
    bfile$has_blocking_cnds <- any(new_cnds$type == "error")
  }
  # add object and position information
  new_cnds <- new_cnds |>
    dplyr::mutate(
      condition = .data$condition |>
        purrr::map(
          ~ {
            .x$message <-
              paste0(
                if (!is.null(bfile$current_object)) {
                  format_inline(
                    "for {cli::col_blue(bfile$current_object)} (v{bfile$object_version}): "
                  )
                } else {
                  ""
                },
                .x$message,
                format_inline(" (current {.emph pos}={pos})")
              )
            .x
          }
        ),
      message = .data$condition |> purrr::map_chr(conditionMessage)
    )
  bfile$cnds <- dplyr::bind_rows(bfile$cnds, new_cnds)
  return(invisible(bfile))
}

# binary data reading ========

# unit lengths of binary number types
.bin_number_types <- c(
  uint8 = 1L,
  uint16 = 2L,
  int = 4L,
  float = 4L,
  double = 8L
)
# unit lengths of all binary data types
.bin_data_types <- c(
  raw = 1L,
  ascii = 1L,
  unicode = 2L,
  string = 4L,
  bool8 = 1L,
  bool16 = 2L,
  bool = 4L, # this seems to be the default
  .bin_number_types
)

# read n data of type from current position
# @param bfile environment with the binary file info
# @param type one of the .bin_data_types -- to read an array of data, use read_binary_data_array
# @param n number of times to read type
# @param expected value of the read
# @param block_if_unexpected whether to trigger a block or not (i.e. whether to throw an error or just a warning), throws an error by default
# @param advance whether to advance the position or not
read_binary_data <- function(
  bfile,
  type,
  n = 1L,
  expected = NULL,
  block_if_unexpected = TRUE,
  advance = TRUE
) {
  # safety checks
  stopifnot(
    !missing(bfile),
    !missing(type) && is_scalar_character(type),
    is_scalar_integerish(n)
  )
  if (!type %in% names(.bin_data_types)) {
    cli_abort("{.var type} must be one of {.field {names(.bin_data_types)}}")
  }
  if (type %in% c("string") && n > 1L) {
    cli_abort("can only read one {.field {type}} at a time")
  }
  size <- .bin_data_types[[type]]

  # check for blocking cnds
  if (bfile$has_blocking_cnds) {
    # return NA (NULL trips up new version of as_tibble), previous cnds have messed up the read of the current object
    return(invisible(NA))
  }

  # buffer check
  bfile |> check_bfile_buffer(n = n, size = size, type = type)

  # read
  value <- NA # NULL trips up new version of as_tibble
  start_pos <- bfile$pos
  if (type == "string") {
    # CString start sequence
    bfile |> read_binary_data("raw", n = 2, expected = as.raw(c(0xff, 0xfe)))
    # third is usually 0xff but what if strings are more than 255 chars long, can this be different then?
    bfile |>
      read_binary_data(
        "raw",
        n = 1,
        expected = as.raw(0xff),
        block_if_unexpected = FALSE
      )
    # string length
    string_length <- bfile |> read_binary_data("uint8", n = 1)
    value <- ""
    if (!is.null(string_length) && string_length > 0) {
      value <- bfile |> read_binary_data("unicode", n = string_length)
    }
  } else if (type == "unicode") {
    # read unicode
    value <- read_binary_data(bfile, "uint16", n = n) |> intToUtf8()
  } else if (type == "ascii") {
    # read ascii
    value <- read_binary_data(bfile, "uint8", n = n) |> intToUtf8()
  } else {
    # actual data read
    signed <- TRUE
    if (type %in% c("uint8", "uint16", "bool8", "bool16")) {
      read_type <- "int"
      signed <- FALSE
    } else if (type == "bool") {
      read_type <- "int"
    } else if (type == "float") {
      read_type <- "numeric"
    } else {
      read_type <- type
    }
    # read data
    bin <- bfile$bin[bfile$pos:(bfile$pos + size * n - 1L)]
    value <- readBin(bin, what = read_type, size = size, n = n, signed = signed)
    # convert booleans
    if (type %in% c("bool8", "bool16", "bool")) {
      if (!value %in% c(0L, 1L)) {
        bfile |>
          register_cnd(
            cli_warn(
              "expected boolean value (0 = FALSE or 1 = TRUE) but found {cli::col_red(value)}"
            )
          )
      }
      value <- as.logical(value)
    }
    # update position
    if (advance) {
      bfile$prev_pos <- bfile$pos
      bfile$pos <- bfile$pos + size * as.integer(n)
    }
  }
  # expected value check
  if (!is.null(expected) && !identical(value, expected)) {
    msg <- format_inline(
      "expected {cli::col_yellow(type)} {qty(expected)} value{?s} {cli::col_green(expected)} but found {cli::col_red(value)}"
    )
    if (block_if_unexpected) {
      bfile |> register_cnd(cli_abort("{msg}"))
    } else {
      bfile |> register_cnd(cli_warn("{msg}"))
    }
  }
  # reset position if we don't want the update
  if (!advance) {
    bfile$pos <- start_pos
  }
  return(value)
}

# read several binary data fields
# @param types if named, uses the names for the resulting data list, any unnamed elements or elements with "?" will be numbered with their data type .1, .2, .3, etc
# @param data previous data to add the collected data to, if any numbered data types are already in data, continues numbering from there
read_binary_data_list <- function(bfile, types, data = list()) {
  stopifnot(!missing(bfile), !missing(types))
  if (
    is.null(names(types)) ||
      !all(have_name(types)) ||
      any(names(types) %in% c("", "?"))
  ) {
    # number data types
    unique_types <- unique(types)
    unique_type_counts <-
      unique_types |>
      purrr::map_int(
        ~ sum(grepl(paste0("^", .x, "\\.[0-9]+$"), names(data)))
      ) |>
      set_names(unique_types)
    # create data names
    data_names <- if (is.null(names(types))) {
      rep("", length(types))
    } else {
      names(types)
    }
    missing_names <- !have_name(types) | data_names == "?" | data_names == ""
    data_names[missing_names] <- types[missing_names]
    data_names <- data_names |>
      purrr::map2_chr(seq_along(data_names), function(nm, idx) {
        if (nm %in% unique_types) {
          # needs numbering
          count <- unique_type_counts[[nm]] +
            sum(head(data_names, n = idx) == nm)
          return(paste0(nm, ".", count))
        } else {
          return(nm)
        }
      })
    types <- types |> set_names(data_names)
  }

  # read data
  new_data <- types |>
    purrr::map(
      ~ bfile |> read_binary_data(type = .x)
    )
  # return all
  return(c(data, new_data))
}

# read an array of binary data
# @param bfile environment with the binary file info
# @param types vector of .bin_number_types
# @param n how many times to read the array
read_binary_data_array <- function(bfile, types, n) {
  # safety checks
  if (!all(types %in% names(.bin_number_types))) {
    cli_abort("all {.var types} must be in {.field {names(.bin_number_types)}}")
  }
  sizes <- .bin_number_types[types] |> unname()

  # buffer check
  bfile |> check_bfile_buffer(n, sum(sizes), paste(types, collapse = "+"))

  # prep
  col_names <- types |>
    paste0(stats::ave(seq_along(types), types, FUN = seq_along))
  signed <- rep(TRUE, length(types))
  signed[types %in% c("uint8", "uint16")] <- FALSE
  types[types %in% c("uint8", "uint16")] <- "int"
  types[types %in% c("float")] <- "numeric"
  type_bytes <- seq_along(sizes) |> rep(times = sizes) |> rep(times = n)
  data <-
    types |>
    lapply(function(t) if (t == "int") integer(n) else numeric(n)) |>
    set_names(col_names) |>
    tibble::as_tibble()

  # read
  bin <- bfile$bin[bfile$pos:(bfile$pos + sum(sizes) * n - 1L)]
  for (i in seq_along(types)) {
    data[[i]] <- readBin(
      bin[type_bytes == i],
      what = types[i],
      size = sizes[i],
      n = n,
      signed = signed[i]
    )
  }
  bfile$pos <- bfile$pos + sum(sizes) * n
  return(data)
}

# # read object from current position
# # FIXME: maybe this is obsolete? (i.e. class object readers should maybe be called directly instead?)
# read_binary_object <- function(bfile, object) {
#   stopifnot(!missing(object) && is_scalar_character(object))
#   read_object <- paste0("read_", object)
#   if (!read_object %in% names(.bin_readers)) {
#     known_objects <- gsub("read_", "", names(.bin_readers))
#     cli_abort(
#       c(
#         "unknown object {.field {object}}",
#         "i" = "known objects: {.field {known_objects}}"
#       )
#     )
#   }
#   return(.bin_readers[[read_object]](bfile))
# }

# basic class readers ======

# find objects (instances of a Cxyz class serialized with the MFC library's CArchive)
read_all_CRuntimeClasses <- function(bfile) {
  # potential structure: ff ff . {4} 43 \x20-\x7e
  # the . {4} are two uint16s (version and name length), the 43 is the C character, the \x20-\x7e is another ascii character
  regexp <- "\xff\xff.{4}\x43[\x20-\x7e]"
  re_positions <- grepRaw(regexp, bfile$bin, all = TRUE, value = FALSE)

  # read classes
  start_pos <- bfile$pos
  objects <-
    dplyr::tibble(
      start = re_positions,
      class = re_positions |>
        purrr::map(
          ~ {
            bfile$pos <- .x
            bfile$has_blocking_cnds <- FALSE
            # read the initial marker
            bfile |>
              read_binary_data("raw", n = 2, expected = as.raw(c(0xff, 0xff)))
            # version
            version <- bfile |> read_binary_data("uint16")
            # length of class name
            class_name_length <- bfile |> read_binary_data("uint16")
            # actual class name
            class_name <- bfile |>
              read_binary_data("ascii", n = class_name_length)
            dplyr::tibble(
              version = version,
              class = class_name
            )
          }
        )
    ) |>
    tidyr::unnest("class") |>
    dplyr::mutate(
      end = .data$start + nchar(.data$class) + 6L - 1L,
      .after = "start"
    ) |>
    dplyr::mutate(
      # not entirely clear how the objects are indexed during serialization
      # so this is more for orientation than as an actual reference value
      idx = dplyr::row_number(),
      .before = 1L
    )

  # reset position
  bfile$pos <- start_pos

  # return
  return(objects)
}

# read the runtime class name
read_CRuntimeClass <- function(bfile, class = NULL, advance = TRUE) {
  # safety check
  if (bfile$has_blocking_cnds) {
    return(dplyr::tibble())
  }

  # get the start bytes
  start_pos <- bfile$pos
  start <- bfile |> read_binary_data("raw", n = 2)
  if (identical(start, as.raw(c(0xff, 0xff)))) {
    # is an actual CRuntimeClass definition --> pull it from the objects
    data <- bfile$objects |> dplyr::filter(.data$start == !!start_pos)
    if (
      !is.null(class) && (nrow(data) == 0L || !identical(data$class, class))
    ) {
      found <- if (nrow(data) == 0L) "none" else cli::col_red(data$class[[1]])
      bfile |>
        register_cnd(cli_abort(
          "expected object of type {.field {class}} but found {if(nrow(data) == 0L) 'none' else cli::col_red(data$class[1])}"
        ))
      return(dplyr::tibble())
    }

    # jump to the end
    bfile$pos <- data$end + 1L
  } else {
    # must be pointer --> reread the start bytes
    ref_idx <- bfile |> skip_bytes(-2) |> read_CRuntimeClass_reference()
    data <- bfile$objects |>
      dplyr::filter(.data$class == !!class) |>
      dplyr::slice_head(n = 1)
    if (nrow(data) == 0L) {
      # missing
      bfile |>
        register_cnd(cli_abort(
          "found object reference {ref_idx} but {.field {class}} is not a known class in this file"
        ))
      return(dplyr::tibble())
    }
    # add ref_idx and update start
    data <- data |>
      dplyr::mutate(ref_idx = !!ref_idx, .before = 1L, start = !!start_pos)
  }
  # don't advance?
  if (!advance) {
    bfile$pos <- start_pos
  }
  # is already a tibble, just remove misleading end (it's not end of the object, just end of the runtimeclass)
  data$end <- NULL
  return(data)
}

# read the reference index for the runtime class
read_CRuntimeClass_reference <- function(bfile) {
  # try to read cache ID for object
  ref_idx <- NA_integer_
  start <- bfile |> read_binary_data("raw", n = 2)
  if (identical(start, as.raw(c(0x7f, 0xff)))) {
    # cached object that has a high ID (>32,767), doubt this will ever happen
    raw_id <- bfile |> read_binary_data("raw", n = 4)
    cache_id <- readBin(raw_id, "int", size = 4)
    if (bitwAnd(cache_id, 0x80000000) != 0) {
      # remove high bit to get cache id
      # does this work if int is signed? I don't think so!
      ref_idx <- bitwAnd(cache_id, bitwNot(0x80000000))
    } else {
      # don't have the highest bit set, this can't be a class ID!
      bfile |>
        register_cnd(cli_abort(
          "expected object ID with high bit flag set but found {cli::col_red(raw_id)}"
        ))
    }
  } else {
    # cached object with ID <= 32767
    raw_id <- start
    cache_id <- readBin(raw_id, "int", size = 2, signed = FALSE)
    if (bitwAnd(cache_id, 0x8000) != 0) {
      # remove high bit to get cache id
      ref_idx <- bitwAnd(cache_id, bitwNot(0x8000))
    } else {
      # don't have the highest bit set, this can't be a class ID!
      bfile |>
        register_cnd(cli_abort(
          "expected object ID with high bit flag set but found {cli::col_red(raw_id)}"
        ))
    }
  }
  # return the ref_idx
  return(ref_idx)
}

# @param bfile binary file environemt
# @param name description
read_schema_version <- function(bfile, class_name, max_supported = NULL) {
  v <- bfile |> read_binary_data("int")

  if (!is.null(max_supported) && !is.na(v) && v > max_supported) {
    bfile |>
      register_cnd(
        cli::cli_warn(
          "{cli::col_blue(class_name)} version ({v}) is newer than supported ({max_supported})",
          .envir = environment()
        ),
        pos = bfile$pos
      )
  }

  v
}
