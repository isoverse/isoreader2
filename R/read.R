#' Read isotope data files
#'
#' @param file_paths paths to the isodat file(s), single value or vector of paths. Use [ir_find_isofiles()] to get files in a folder.
#' @param show_progress whether to show a progress bar, by default always enabled when running interactively e.g. inside Positron or RStudio (and disabled in a notebook), turn off with `show_progress = FALSE`
#' @param show_problems whether to show problems encountered along the way (rather than just keeping track of them with [ir_get_problems()]). Set to `show_problems = FALSE` to turn off the live printout. Either way, all encountered problems can be retrieved with running [ir_get_problems()] for the returned list
#' @param reextract whether to re-extract files (uses isoextract to read files
#'   from scratch); if `FALSE` (default) only files that have not been extracted
#'   yet (or whose previous extraction is out of date) are extracted. Use
#'   [ir_extract_isofiles()] directly for finer control over extraction (e.g.
#'   `pretty_json` or `dry_run`).
#' @return a tibble data frame (an `ir_isofiles` object) where each row holds the file path and nested tibbles of datasets extracted from the isodat files. Use [ir_aggregate_isofiles()] to aggregate data safely across files. Multiple such collections can be combined into one with a simple [c()] (see [c.ir_isofiles()]).
#' @seealso [c.ir_isofiles()] to combine collections of isofiles
#' @export
ir_read_isofiles <- function(
  file_paths,
  show_progress = is_interactive(),
  show_problems = TRUE,
  reextract = FALSE
) {
  # keep track of current env to anchor progress bars
  root_env <- current_env()

  # safety checks (file existence checks happen later for these)
  file_paths <- check_file_paths_parameter(file_paths)
  show_progress |>
    check_arg(is_scalar_logical(show_progress), "must be TRUE OR FALSE")
  show_problems |>
    check_arg(is_scalar_logical(show_problems), "must be TRUE OR FALSE")
  reextract |> check_arg(is_scalar_logical(reextract), "must be TRUE OR FALSE")

  # any paths?
  if (is_empty(file_paths)) {
    start <- start_info("is starting", show_progress = FALSE)
    finish_info("is finished, no isofiles paths provided", start = start)
    return(NULL)
  }

  # file paths info (strip the json for purposes of what the original files were)
  file_paths_info <- tibble(
    file_path = gsub("\\.json$", "", file_paths, ignore.case = TRUE)
  )

  # check if need to reextract
  if (reextract) {
    # reextract all
    file_paths_info <- file_paths_info |> dplyr::mutate(extract = TRUE)
  } else {
    # empty metadata
    empty_meta <- tibble(
      isoextract_version = NA_character_,
      file_type = NA_character_,
      previous_file_size = NA_integer_,
      complete = NA
    )
    # fetch metadata from json files to determine if any need reextraction
    file_paths_info <- file_paths_info |>
      dplyr::mutate(
        json_path = .data$file_path |> paste0(".json"),
        has_json = file.exists(.data$json_path),
        meta = purrr::map2(
          .data$json_path,
          .data$has_json,
          function(fp, has_json) {
            if (!has_json) {
              # no json file
              return(empty_meta)
            }
            out <- read_json_meta(fp) |> try_catch_cnds()
            if (is.null(out$result)) {
              # something went wrong reading the metadata --> re-extract
              return(empty_meta)
            }
            return(out$result)
          }
        )
      ) |>
      tidyr::unnest("meta") |>
      dplyr::left_join(.file_type_specs, by = "file_type") |>
      dplyr::mutate(
        version_ok = numeric_version(.data$isoextract_version) >=
          numeric_version(.data$min_isoextract_version),
        file_size = purrr::map_int(
          .data$file_path,
          function(fp) {
            if (grepl("\\.bch$", fp, ignore.case = TRUE)) {
              sum(file.size(list.files(
                fp,
                recursive = TRUE,
                full.names = TRUE
              )))
            } else if (
              grepl("\\.imexp$", fp, ignore.case = TRUE) && !file.exists(fp)
            ) {
              # FIXME: shouldn't need this anymore when we don't use the zip intermediate
              file.size(paste0(fp, ".zip"))
            } else {
              file.size(fp)
            }
          }
        ),
        size_identical = .data$file_size == .data$previous_file_size,
        # (re-) extract flag - conditions
        # has no json --> reextract
        extract = !.data$has_json |
          # version info missing --> reextract
          is.na(.data$version_ok) |
          # version insufficient --> reextract
          !.data$version_ok |
          # original file exists but size is not identical
          (!is.na(.data$file_size) & is.na(.data$size_identical)) |
          (!is.na(.data$file_size) & !.data$size_identical)
      )
  }

  # (re-)extract
  file_paths_info |>
    dplyr::filter(.data$extract) |>
    dplyr::pull("file_path") |>
    ir_extract_isofiles(
      show_progress = show_progress,
      show_problems = FALSE # show total errors later during file read
    )

  # read files safely
  read_safely <- function(file_path) {
    # progress
    if (!is.null(start$pb)) {
      cli_progress_update(
        id = start$pb,
        inc = 1,
        extra = list(file_path = file_path),
        status = "reading",
        .envir = root_env
      )
    }

    # start timer
    file_start <- start_info()

    # parse existing issues from isoextract
    issues_path <- file_path |> paste0(".issues.log")
    isoextract_problems <- empty_cnds_tibble()
    if (file.exists(issues_path)) {
      lines <- readLines(issues_path, warn = FALSE)
      lines <- lines[nzchar(trimws(lines))]
      isoextract_problems <- purrr::map(lines, function(line) {
        msg <- sub("^(error|warning): ", "", line, ignore.case = TRUE)
        out <- if (grepl("^warning", line, ignore.case = TRUE)) {
          try_catch_cnds(cli_warn(msg))
        } else {
          try_catch_cnds(cli_abort(msg))
        }
        out$conditions
      }) |>
        dplyr::bind_rows() |>
        dplyr::mutate(call = "ir_extract_isofiles")
    }

    # work on json path
    json_path <- file_path |> paste0(".json")

    if (!file.exists(json_path)) {
      # json file does not exist
      if (nrow(isoextract_problems) == 0) {
        # json does not exist but there are NO errors registered from isoextract --> something is off
        isoextract_problems <- try_catch_cnds(
          cli_abort(
            ".json output file from isoextract does not exist, try running with {.code reextract = TRUE}",
            .envir = root_env
          )
        )$conditions
      }
      out <- list(
        result = tibble(problems = list(empty_cnds_tibble())),
        conditions = empty_cnds_tibble()
      )
    } else {
      # file exsists!
      # function (so traceback is informative)
      func <- sprintf("read_%s_json", tolower(tools::file_ext(file_path)))
      func_quo <- expr((!!func)(json_path))

      # call with error handling
      out <-
        try_catch_cnds(
          eval_tidy(func_quo),
          error_value = tibble(problems = list(empty_cnds_tibble())),
          catch_errors = !ir_get_option("debug")
        )
    }

    # add a per-timepoint integer index (tp) to the traces of every reader
    if (!is.null(out$result) && "traces" %in% names(out$result)) {
      out$result$traces <- purrr::map(
        out$result$traces,
        add_trace_timepoint_index
      )
    }

    # merge problems from isoextract, this call, and any conditions in the result
    problems <- dplyr::bind_rows(isoextract_problems, out$conditions)
    if ("problems" %in% names(out$result)) {
      problems <- dplyr::bind_rows(problems, out$result$problems)
      out$result$problems <- list(problems)
    }

    # add file path to outer result
    out$result <- out$result |>
      dplyr::mutate(file_path = .env$file_path, .before = 1L)

    # add file_path and file_name as first columns of each inner metadata tibble
    if (
      "metadata" %in% names(out$result) && !is.null(out$result$metadata[[1]])
    ) {
      out$result$metadata[[1]] <- out$result$metadata[[1]] |>
        dplyr::mutate(
          file_path = .env$file_path,
          file_name = basename(.env$file_path),
          .before = 1L
        )
    } else {
      out$result$metadata <- tibble(
        file_path = .env$file_path,
        file_name = basename(.env$file_path)
      ) |>
        list()
    }

    # show problems?
    if (show_problems) {
      problems |>
        show_cnds(
          include_call = FALSE,
          summary_format = "{message}: {issues}",
          summary_indent = 1,
          message = format_inline("{.file {file_path}}"),
          collapse_single_line_cnd = TRUE
        )
    }

    # return result
    return(out$result)
  }

  # start progress bar
  start <- start_info(
    "is reading {pb_current}/{pb_total - 1} files {pb_bar} ",
    "| {pb_elapsed} | ETA {pb_eta} | {.file {basename(pb_extra$file_path)}} ",
    "| {.field {pb_status}}",
    pb_total = nrow(file_paths_info) + 1L,
    pb_extra = list(file_path = NA_character_),
    pb_status = "initializing",
    show_progress = show_progress,
    .env = root_env
  )

  # read files
  all_files <- file_paths_info$file_path |>
    purrr::map(read_safely) |>
    dplyr::bind_rows()

  # wrap up
  problems <- all_files$problems |> dplyr::bind_rows()
  finish_info(
    "finished reading {nrow(file_paths_info)} isotope data file{?s}/archive{?s}",
    start = start,
    conditions = problems,
    show_conditions = FALSE,
    summary_error_symbol = "!",
    .env = root_env
  )

  # return
  class(all_files) <- unique(c("ir_isofiles", class(all_files)))
  return(all_files)
}

#' Combine isofiles
#'
#' Combine multiple collections of isofiles (read by [ir_read_isofiles()]) into a single `ir_isofiles` object by row-binding them with [dplyr::bind_rows()]. This preserves the object structure and type.
#' @param ... `ir_isofiles` objects to combine
#' @return a single combined `ir_isofiles` object
#' @export
c.ir_isofiles <- function(...) {
  combined <- dplyr::bind_rows(...)
  class(combined) <- unique(c("ir_isofiles", class(combined)))
  return(combined)
}

# extract meta from a single JSON file
read_json_meta <- function(json_path) {
  meta <- query_json(json_path, "/meta")
  tibble(
    isoextract_version = meta$isoextract_version,
    file_type = meta$file_type,
    previous_file_size = meta$file_size_bytes,
    complete = meta$complete
  )
}

# add an integer per-timepoint index `tp` (1..n, ordered by `time.s`) to a traces
# tibble, computed within each trace series (i.e. each unique combination of the
# non-time/non-intensity columns such as species/channel/mass). Returns the input
# unchanged if it is not a traces tibble with a `time.s` column.
add_trace_timepoint_index <- function(traces) {
  if (
    is.null(traces) ||
      !is.data.frame(traces) ||
      !"time.s" %in% names(traces)
  ) {
    return(traces)
  }
  intensity_cols <- grep("^intensity\\.", names(traces), value = TRUE)
  series_cols <- setdiff(names(traces), c("time.s", "tp", intensity_cols))
  traces |>
    dplyr::mutate(
      .by = dplyr::all_of(series_cols),
      tp = as.integer(dplyr::row_number(.data[["time.s"]]))
    ) |>
    dplyr::relocate("tp", .before = "time.s")
}
