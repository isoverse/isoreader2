# isoextract installation ========

#' Check for the isoextract executable
#'
#' By default, this will install isoextract if it is missing or outdated.
#' This function runs automatically when needed and does not usually need to be called directly by the user.
#'
#' @param install_if_missing install isoextract if it's missing
#' @param reinstall_if_outdated install isoextract if it's outdated (i.e. not at least `min_version`)
#' @param reinstall_always whether to (re-)install no matter what
#' @param min_version the minimum version number required
#' @param source the URL (or local path) where to find isoextract, by default this is the latests release of the executables on github
#' @param ... passed on to `download.file` if (re-) installing isoextract
#' @export
ir_check_isoextract <- function(
  install_if_missing = !on_cran(),
  reinstall_if_outdated = !on_cran(),
  reinstall_always = FALSE,
  min_version = "0.2.0",
  show_version = TRUE,
  source = paste0(
    "https://github.com/isoverse/IsofileExtractor/releases/download/isoextract-v",
    min_version
  ),
  ...
) {
  # start
  start <- start_info()

  # check existence
  isoextract_exists <- file.exists(get_isoextract_path())
  outdated <- FALSE

  # version
  if (isoextract_exists) {
    isoextract_version <- get_isoextract_version()
    if (
      is.null(isoextract_version) ||
        isoextract_version < numeric_version(min_version)
    ) {
      outdated <- TRUE
      if (!is.null(isoextract_version)) {
        cli_bullets(
          c(
            "!" = "isoextract is outdated",
            "i" = "found version {isoextract_version} but need at least version {numeric_version(min_version)}"
          )
        )
      }
    }
  }

  # do we need to install?
  if (
    reinstall_always ||
      (!isoextract_exists && install_if_missing) ||
      (outdated && reinstall_if_outdated)
  ) {
    # yes we do
    cli_inform(c(
      ">" = "Trying to {if (isoextract_exists) 're'}install isoextract for your operating system {.pkg {basename(get_isoextract_path())}} (this requires an internet connection and may take a moment)..."
    ))

    # create folder
    dir.create(
      dirname(get_isoextract_path()),
      recursive = TRUE,
      showWarnings = FALSE
    )

    # download
    tryCatch(
      {
        tmpfile <- tempfile()
        if (dir.exists(source)) {
          # local folder (usually only used by developers)
          file.copy(file.path(source, basename(get_isoextract_path())), tmpfile)
        } else {
          # download to temp file
          utils::download.file(
            file.path(source, basename(get_isoextract_path())),
            destfile = tmpfile,
            mode = "wb",
            ...
          )
        }
        # remove the existing one
        if (isoextract_exists) {
          unlink(get_isoextract_path())
        }
        # move the downloaded one
        file.rename(tmpfile, get_isoextract_path())
      },
      error = function(cnd) {
        cli_abort(
          "could not download isoextract",
          parent = cnd
        )
      }
    )

    # make executable
    Sys.chmod(get_isoextract_path(), mode = "0777", use_umask = TRUE)

    # get new version
    isoextract_version <- get_isoextract_version()
    if (!is.null(isoextract_version)) {
      finish_info(
        "successfully installed isoextract version {isoextract_version}",
        start = start
      )
      return(invisible(NULL))
    }
  }

  # final check
  isoextract_version <- get_isoextract_version()
  if (
    is.null(isoextract_version) ||
      isoextract_version < numeric_version(min_version)
  ) {
    cli_abort(
      "cannot proceed, the required isoextract version {numeric_version(min_version)} is missing or does not work"
    )
  } else if (show_version) {
    finish_info(
      "found isoextract version {isoextract_version} ready for use",
      start = start
    )
  }
}

# check if we're on cran
on_cran <- function() {
  !interactive() && !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
}

# interactions with isoextract =======

# get the path to the executable
get_isoextract_path <- function() {
  libdir <- tools::R_user_dir("isoreader2", which = "cache")
  d <- file.path(libdir, "assembly")
  if (Sys.info()["sysname"] == "Darwin") {
    f <- file.path(d, "isoextract-osx-x64")
  } else if (Sys.info()["sysname"] == "Linux") {
    f <- file.path(d, "isoextract-linux-x64")
  } else {
    f <- file.path(d, "isoextract-win-x64.exe")
  }
  return(f)
}

# get version of the installed isoextract
get_isoextract_version <- function() {
  if (!file.exists(get_isoextract_path())) {
    return(NULL)
  }
  version <- system2(
    get_isoextract_path(),
    args = c("--version"),
    stdout = TRUE,
    stderr = TRUE
  )

  if (
    !is_scalar_character(version) ||
      !grepl("isoextract version", version, fixed = TRUE)
  ) {
    cli_bullets(
      c(
        "!" = "Could not determine isoextract version, executable returned:",
        version |>
          purrr::map_chr(~ format_inline("{.emph {col_red(.x)}}")) |>
          format_bullets_raw() |>
          set_names(" ")
      )
    )
    return(NULL)
  }
  # return
  regmatches(version, regexpr("\\d+(\\.\\d+)*", version)) |> numeric_version()
}

# check isofile paths before reading
check_file_paths_parameter <- function(file_paths) {
  # safety checks
  check_arg(
    file_paths,
    !missing(file_paths) &&
      is_character(file_paths),
    "must be file paths"
  )
  if (is_empty(file_paths)) {
    return(character())
  }

  # .bch paths are directories by design; all others must be files
  is_bch <- grepl("\\.bch$", file_paths, ignore.case = TRUE)

  # all non-bch paths are directories?
  if (any(!is_bch) && all(dir.exists(file_paths[!is_bch]))) {
    cli_abort(
      c(
        "{?this/these} path{?s} ({.file {file_paths[!is_bch]}}) {?is a/are} director{?y/ies}, not {?an /}isofile{?s}/archive{?s}",
        "i" = "did you mean to run {.strong ir_find_continuous_flow()} instead?"
      )
    )
  }
  return(file_paths)
}

#' run the isoextract executable on a vector of file paths
#' this is usually not called directly
#' @inheritParams ir_read_isofiles
#' @export
ir_extract_isofiles <- function(
  file_paths,
  show_progress = is_interactive(),
  show_problems = TRUE
) {
  # safety checks
  file_paths <- check_file_paths_parameter(file_paths)
  show_progress |>
    check_arg(is_scalar_logical(show_progress), "must be TRUE OR FALSE")
  show_problems |>
    check_arg(is_scalar_logical(show_problems), "must be TRUE OR FALSE")

  # check for isoextract
  ir_check_isoextract(show_version = FALSE)

  # any paths?
  if (is_empty(file_paths)) {
    start <- start_info("is starting", show_progress = FALSE)
    finish_info(
      "is finished, 0 files/archives required (re-)extraction",
      start = start
    )
    return(invisible(NULL))
  }

  # info / progress
  start <- start_info(
    "extracted {pb_extra$idx}/{pb_extra$total} files {pb_bar} ",
    "| {pb_elapsed} | ETA {pb_eta} | {.file {basename(pb_extra$file_path)}} ",
    "({bytes_to_text(file.size(pb_extra$file_path))}) ",
    "| {.field {pb_status}}",
    pb_total = sum(file.size(file_paths), na.rm = TRUE),
    pb_extra = list(
      idx = 0,
      file_path = NA_character_,
      total = length(file_paths)
    ),
    pb_status = "initializing",
    show_progress = show_progress
  )

  # pass files via a temp list file to avoid command-line length limits
  file_list <- tempfile("isoextract_file_list")
  writeLines(file_paths, file_list)

  # dispatch command line process
  on.exit({
    try(p$kill(), silent = TRUE)
  })
  p <- processx::process$new(
    get_isoextract_path(),
    c("--file-list", file_list),
    stdout = "|",
    stderr = "|"
  )

  # catch extra events
  idx <- 0L
  stderr_lines <- character(0)
  while (p$is_alive()) {
    p$poll_io(0)
    stderr_lines <- c(stderr_lines, p$read_error_lines())
    out_lines <- p$read_output_lines()
    # should the progress of the read files be shown?
    if (!is.null(start$pb)) {
      for (line in out_lines) {
        # check for "Written: /path/to/file.ext.json" -> input path = without .json
        if (startsWith(line, "Written:")) {
          idx <- idx + 1L
          input_path <- sub("\\.json$", "", sub("^Written: ", "", line))
          bytes <- file.size(input_path)
          cli_progress_update(
            id = start$pb,
            inc = if (!is.na(bytes)) bytes else 0,
            extra = list(
              idx = idx,
              file_path = input_path,
              total = length(file_paths)
            ),
            force = TRUE,
            status = "extracting"
          )
        }
      }
    }
  }

  # cleanup
  p$wait()
  p$poll_io(0)
  stderr_lines <- c(stderr_lines, p$read_error_lines())
  unlink(file_list)

  # any problems?
  problems <-
    stderr_lines[nzchar(stderr_lines)] |>
    purrr::map(~ try_catch_cnds(rlang::abort(.x))$conditions) |>
    purrr::list_rbind()
  if (nrow(problems) > 0) {
    problems$call <- NA_character_
  }

  # finish
  finish_info(
    "finished extracting {length(file_paths)} file{?s}/archive{?s} ",
    conditions = problems,
    show_conditions = show_problems,
    summary_error_symbol = "!",
    start = start
  )
}
