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
#' @param show_version whether to print the installed isoextract version after
#'   a successful check (default: `TRUE`)
#' @param source the URL (or local path) where to find isoextract, by default this is the latests release of the executables on github
#' @param check_isosolfs whether to also ensure the `isosolfs` helper executable
#'   is installed. `isosolfs` is required to read Qtegra notebooks (`.imexp`
#'   files) and is released alongside isoextract; the same `install_if_missing` /
#'   `reinstall_if_outdated` / `reinstall_always` / `show_version` settings are
#'   applied to it. Default is currently `FALSE` because isosolfs has not been
#'   released yet (enabling it would abort all reads); set to `TRUE` (or this
#'   will become the default) once isosolfs is available.
#' @param ... passed on to `download.file` if (re-) installing isoextract (and isosolfs)
#' @return called for its side effect of ensuring a working isoextract
#'   executable (at least `min_version`) is installed — and, when
#'   `check_isosolfs = TRUE`, isosolfs as well; returns `NULL` invisibly and
#'   aborts if a required executable cannot be made available
#' @export
ir_check_isoextract <- function(
  install_if_missing = !on_cran(),
  reinstall_if_outdated = !on_cran(),
  reinstall_always = FALSE,
  min_version = "0.2.1",
  show_version = TRUE,
  source = paste0(
    "https://github.com/isoverse/IsofileExtractor/releases/download/isoextract-v",
    min_version
  ),
  check_isosolfs = FALSE,
  ...
) {
  check_assembly(
    tool = "isoextract",
    exe_path = get_isoextract_path(),
    get_version = get_isoextract_version,
    min_version = min_version,
    source = source,
    install_if_missing = install_if_missing,
    reinstall_if_outdated = reinstall_if_outdated,
    reinstall_always = reinstall_always,
    show_version = show_version,
    ...
  )

  # isosolfs helper (needed to read Qtegra .imexp notebooks)
  if (check_isosolfs) {
    check_isosolfs(
      install_if_missing = install_if_missing,
      reinstall_if_outdated = reinstall_if_outdated,
      reinstall_always = reinstall_always,
      show_version = show_version,
      ...
    )
  }

  invisible(NULL)
}

# ensure the isosolfs helper executable (used to read Qtegra .imexp notebooks)
# is installed; mirrors ir_check_isoextract(). isosolfs is released alongside
# isoextract in the IsofileExtractor repository.
check_isosolfs <- function(
  install_if_missing = !on_cran(),
  reinstall_if_outdated = !on_cran(),
  reinstall_always = FALSE,
  min_version = "0.1",
  show_version = TRUE,
  source = paste0(
    "https://github.com/isoverse/IsofileExtractor/releases/download/isosolfs-v",
    min_version
  ),
  ...
) {
  check_assembly(
    tool = "isosolfs",
    exe_path = get_isosolfs_path(),
    get_version = get_isosolfs_version,
    min_version = min_version,
    source = source,
    install_if_missing = install_if_missing,
    reinstall_if_outdated = reinstall_if_outdated,
    reinstall_always = reinstall_always,
    show_version = show_version,
    ...
  )
}

# generic install/version check for a downloadable IsofileExtractor assembly
# executable (isoextract or isosolfs). Checks the installed version, (re-)installs
# from `source` if missing/outdated/forced, and aborts if a working executable of
# at least `min_version` cannot be made available. `get_version` is a function
# returning the installed numeric_version (or NULL). `...` is passed on to
# `download.file`.
check_assembly <- function(
  tool,
  exe_path,
  get_version,
  min_version,
  source,
  install_if_missing,
  reinstall_if_outdated,
  reinstall_always,
  show_version,
  ...
) {
  start <- start_info()

  # check existence + version
  exists <- file.exists(exe_path)
  outdated <- FALSE
  if (exists) {
    version <- get_version()
    if (is.null(version) || version < numeric_version(min_version)) {
      outdated <- TRUE
      if (!is.null(version)) {
        cli_bullets(c(
          "!" = "{tool} is outdated",
          "i" = "found version {version} but need at least version {numeric_version(min_version)}"
        ))
      }
    }
  }

  # do we need to install?
  if (
    reinstall_always ||
      (!exists && install_if_missing) ||
      (outdated && reinstall_if_outdated)
  ) {
    cli_inform(c(
      ">" = "Trying to {if (exists) 're'}install {tool} for your operating system {.pkg {basename(exe_path)}} (this requires an internet connection and may take a moment)..."
    ))

    dir.create(dirname(exe_path), recursive = TRUE, showWarnings = FALSE)

    tryCatch(
      {
        tmpfile <- tempfile()
        if (dir.exists(source)) {
          # local folder (usually only used by developers)
          file.copy(file.path(source, basename(exe_path)), tmpfile)
        } else {
          utils::download.file(
            file.path(source, basename(exe_path)),
            destfile = tmpfile,
            mode = "wb",
            ...
          )
        }
        if (exists) {
          unlink(exe_path)
        }
        file.rename(tmpfile, exe_path)
      },
      error = function(cnd) {
        cli_abort("could not download {tool}", parent = cnd)
      }
    )

    Sys.chmod(exe_path, mode = "0777", use_umask = TRUE)

    version <- get_version()
    if (!is.null(version)) {
      finish_info(
        "successfully installed {tool} version {version}",
        start = start
      )
      return(invisible(NULL))
    }
  }

  # final check
  version <- get_version()
  if (is.null(version) || version < numeric_version(min_version)) {
    cli_abort(
      "cannot proceed, the required {tool} version {numeric_version(min_version)} is missing or does not work"
    )
  } else if (show_version) {
    finish_info(
      "found {tool} version {version} ready for use",
      start = start
    )
  }
  invisible(NULL)
}

# check if we're on cran
on_cran <- function() {
  !interactive() && !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
}

# interactions with isoextract / isosolfs =======

# determine the runtime identifier (.NET RID) for the current platform, combining
# operating system and CPU architecture, e.g. "osx-arm64", "linux-x64", "win-x64".
# This matches the naming of the released executables.
get_assembly_runtime <- function() {
  os <- switch(
    Sys.info()[["sysname"]],
    Darwin = "osx",
    Linux = "linux",
    Windows = "win",
    "win" # fall back to Windows naming for anything else
  )
  # architecture detection is disabled until arm64 executables are released;
  # all platforms use the x64 build for now (arm64 machines run it via emulation,
  # e.g. Rosetta on Apple Silicon). Re-enable the lines below once arm64 binaries
  # are available.
  # machine <- tolower(Sys.info()[["machine"]])
  # arch <- if (grepl("arm|aarch", machine)) "arm64" else "x64"
  arch <- "x64"
  paste0(os, "-", arch)
}

# path to a platform-specific assembly executable (`tool` = "isoextract" or
# "isosolfs") in the package cache
get_assembly_path <- function(tool) {
  d <- file.path(tools::R_user_dir("isoreader2", which = "cache"), "assembly")
  runtime <- get_assembly_runtime()
  exe <- paste0(tool, "-", runtime, if (startsWith(runtime, "win")) ".exe")
  file.path(d, exe)
}

# version reported by an installed assembly executable, or NULL if it is missing
# or does not report a recognizable "<tool> version ..." string
get_assembly_version <- function(exe_path, tool) {
  if (!file.exists(exe_path)) {
    return(NULL)
  }
  version <- system2(
    exe_path,
    args = c("--version"),
    stdout = TRUE,
    stderr = TRUE
  )
  if (
    !is_scalar_character(version) ||
      !grepl(paste(tool, "version"), version, fixed = TRUE)
  ) {
    cli_bullets(
      c(
        "!" = "Could not determine {tool} version, executable returned:",
        version |>
          purrr::map_chr(~ format_inline("{.emph {col_red(.x)}}")) |>
          format_bullets_raw() |>
          set_names(" ")
      )
    )
    return(NULL)
  }
  regmatches(version, regexpr("\\d+(\\.\\d+)*", version)) |> numeric_version()
}

get_isoextract_path <- function() get_assembly_path("isoextract")
get_isoextract_version <- function() {
  get_assembly_version(get_isoextract_path(), "isoextract")
}
get_isosolfs_path <- function() get_assembly_path("isosolfs")
get_isosolfs_version <- function() {
  get_assembly_version(get_isosolfs_path(), "isosolfs")
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
#' @param pretty_json whether to write the JSON output in human-readable
#'   pretty-printed format (default: `FALSE`). Useful for debugging; has no
#'   effect on the data read back by [ir_read_isofiles()]. Note that
#'   pretty-printed files are larger than compact ones.
#' @return called for its side effect of running isoextract to write a `.json`
#'   sidecar file next to each input file; returns `NULL` invisibly
#' @export
ir_extract_isofiles <- function(
  file_paths,
  pretty_json = FALSE,
  show_progress = is_interactive(),
  show_problems = TRUE
) {
  # safety checks
  file_paths <- check_file_paths_parameter(file_paths)
  show_progress |>
    check_arg(is_scalar_logical(show_progress), "must be TRUE OR FALSE")
  show_problems |>
    check_arg(is_scalar_logical(show_problems), "must be TRUE OR FALSE")

  # any paths?
  if (is_empty(file_paths)) {
    start <- start_info("is starting", show_progress = FALSE)
    finish_info(
      "is finished, 0 files/archives required (re-)extraction",
      start = start
    )
    return(invisible(NULL))
  }

  # check for isoextract
  ir_check_isoextract(show_version = FALSE)

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
    c("--file-list", file_list, if (pretty_json) "--prettyJSON"),
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
