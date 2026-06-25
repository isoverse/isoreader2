# isoextract installation ========

#' Check for the isoextract executables
#'
#' By default, these will install the executable if it is missing or outdated.
#' They run automatically when needed and do not usually need to be called
#' directly by the user. In particular, `ir_check_isoextract()` calls
#' `ir_check_isosolfs()` automatically (unless `check_isosolfs = FALSE`), so
#' `ir_check_isosolfs()` rarely needs to be called on its own.
#'
#' @param install_if_missing install the executable if it's missing
#' @param reinstall_if_outdated install the executable if it's outdated (i.e. not at least `min_version`)
#' @param reinstall_always whether to (re-)install no matter what
#' @param min_version the minimum version number required
#' @param show_version whether to print the installed version after
#'   a successful check (default: `TRUE`)
#' @param ask_permission whether to ask for the user's permission before
#'   downloading a missing or outdated executable (default: `TRUE`). The prompt
#'   only appears in interactive sessions and only when a download is actually
#'   needed; if it is declined - or the session is not interactive - no download
#'   is attempted and the function aborts with instructions. Set to `FALSE` to
#'   allow the download without prompting (e.g. in scripts). When
#'   `ir_check_isoextract()` downloads isoextract with the user's consent it
#'   passes `ask_permission = FALSE` on to `ir_check_isosolfs()` so the user is
#'   not asked a second time.
#' @param source the URL (or local path) where to find the executable, by default this is the latest release of the executables on github
#' @param check_isosolfs whether to also ensure the `isosolfs` helper executable
#'   is installed (default: `TRUE`), by calling [ir_check_isosolfs()]. `isosolfs`
#'   is required to read Qtegra notebooks (`.imexp` files) and is released
#'   alongside isoextract; the same `install_if_missing` / `reinstall_if_outdated`
#'   / `reinstall_always` / `show_version` settings are applied to it.
#' @param ... passed on to `download.file` if (re-) installing the executable(s)
#' @return called for its side effect of ensuring a working executable (at least
#'   `min_version`) is installed — and, for `ir_check_isoextract()` when
#'   `check_isosolfs = TRUE`, isosolfs as well; returns `NULL` invisibly and
#'   aborts if a required executable cannot be made available
#' @name ir_check_isoextract
#' @export
ir_check_isoextract <- function(
  install_if_missing = !on_cran(),
  reinstall_if_outdated = !on_cran(),
  reinstall_always = FALSE,
  min_version = "0.3.0",
  show_version = TRUE,
  ask_permission = TRUE,
  source = paste0(
    "https://github.com/isoverse/IsofileExtractor/releases/download/isoextract-v",
    min_version
  ),
  check_isosolfs = TRUE,
  ...
) {
  granted <- check_assembly(
    tool = "isoextract",
    exe_path = get_isoextract_path(),
    get_version = ir_get_isoextract_version,
    min_version = min_version,
    source = source,
    install_if_missing = install_if_missing,
    reinstall_if_outdated = reinstall_if_outdated,
    reinstall_always = reinstall_always,
    show_version = show_version,
    ask_permission = ask_permission,
    ...
  )

  # isosolfs helper (needed to read Qtegra .imexp notebooks). If the user already
  # consented to downloading isoextract, don't ask again for isosolfs.
  if (check_isosolfs) {
    ir_check_isosolfs(
      install_if_missing = install_if_missing,
      reinstall_if_outdated = reinstall_if_outdated,
      reinstall_always = reinstall_always,
      show_version = show_version,
      ask_permission = if (isTRUE(granted)) FALSE else ask_permission,
      ...
    )
  }

  invisible(NULL)
}

#' @describeIn ir_check_isoextract ensure the `isosolfs` helper executable (used
#'   to read Qtegra `.imexp` notebooks) is installed. Released alongside
#'   isoextract and called automatically by `ir_check_isoextract()`, so it
#'   rarely needs to be called directly.
#' @export
ir_check_isosolfs <- function(
  install_if_missing = !on_cran(),
  reinstall_if_outdated = !on_cran(),
  reinstall_always = FALSE,
  min_version = "1.0.0",
  show_version = TRUE,
  ask_permission = TRUE,
  source = paste0(
    "https://github.com/isoverse/IsofileExtractor/releases/download/isosolfs-v",
    min_version
  ),
  ...
) {
  check_assembly(
    tool = "isosolfs (helper to open Qtegra notebooks)",
    exe_path = get_isosolfs_path(),
    get_version = get_isosolfs_version,
    min_version = min_version,
    source = source,
    install_if_missing = install_if_missing,
    reinstall_if_outdated = reinstall_if_outdated,
    reinstall_always = reinstall_always,
    show_version = show_version,
    ask_permission = ask_permission,
    ...
  )
}

# ask the user for permission to download an executable. Returns TRUE if granted.
# In a non-interactive session there is nobody to ask, so it returns FALSE (the
# caller then aborts with instructions to set `ask_permission = FALSE`).
request_download_permission <- function(tool, dest_dir, source) {
  cli_bullets(c(
    "!" = "{tool} is not installed (or is outdated) and needs to be downloaded before it can be used.",
    "i" = "Source: {.url {source}}",
    "i" = "Install location: {.path {dest_dir}}"
  ))
  if (!interactive()) {
    return(FALSE)
  }
  utils::menu(c("Yes, download it now", "No")) == 1L
}

# generic install/version check for a downloadable IsofileExtractor assembly
# executable (isoextract or isosolfs). Checks the installed version, (re-)installs
# from `source` if missing/outdated/forced, and aborts if a working executable of
# at least `min_version` cannot be made available. `get_version` is a function
# returning the installed numeric_version (or NULL). `...` is passed on to
# `download.file`. When `ask_permission` is TRUE, the user is prompted before a
# download is started. Returns (invisibly) TRUE if a download happened with the
# user's explicit consent, FALSE otherwise (so the caller can avoid re-prompting).
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
  ask_permission = FALSE,
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
  permission_granted <- FALSE
  if (
    reinstall_always ||
      (!exists && install_if_missing) ||
      (outdated && reinstall_if_outdated)
  ) {
    # ask for permission before downloading anything
    if (ask_permission) {
      if (!request_download_permission(tool, dirname(exe_path), source)) {
        cli_abort(c(
          "cannot install {tool}: permission to download was not granted",
          "i" = "re-run with {.code ask_permission = FALSE} to allow the download without being asked",
          "i" = "or install {tool} manually into {.path {dirname(exe_path)}}"
        ))
      }
      permission_granted <- TRUE
    }

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
      return(invisible(permission_granted))
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
  invisible(permission_granted)
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
  machine <- tolower(Sys.info()[["machine"]])
  arch <- if (grepl("arm|aarch", machine)) "arm64" else "x64"
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
    is_empty(version) ||
      !is_character(version) ||
      !grepl("version", version[1], fixed = TRUE)
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
  regmatches(
    version[1],
    regexpr("\\d+(\\.\\d+)*$", version[1])
  ) |>
    numeric_version()
}

get_isoextract_path <- function() get_assembly_path("isoextract")
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
        "i" = "did you mean to run {.strong ir_find_continuous_flow()}, {.strong ir_find_dual_inlet()}, or {.strong ir_find_scans()} instead?"
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
#' @param dry_run whether to run isoextract in "dry run" mode (default: `FALSE`).
#'   In dry run mode the files are parsed to test whether they can be read
#'   (a file-compatibility check) but no `.json` sidecar output is written.
#'   Combine with `show_problems = TRUE` to see which files (if any) cannot be
#'   extracted. Note that with `dry_run = TRUE`, the progress bar does not work
#'   as it depends on the JSON output files.
#' @return called for its side effect of running isoextract to write a `.json`
#'   sidecar file next to each input file (unless `dry_run = TRUE`); returns
#'   `NULL` invisibly
#' @export
ir_extract_isofiles <- function(
  file_paths,
  pretty_json = FALSE,
  dry_run = FALSE,
  show_progress = is_interactive(),
  show_problems = TRUE
) {
  # safety checks
  file_paths <- check_file_paths_parameter(file_paths)
  pretty_json |>
    check_arg(is_scalar_logical(pretty_json), "must be TRUE OR FALSE")
  dry_run |>
    check_arg(is_scalar_logical(dry_run), "must be TRUE OR FALSE")
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
    c(
      "--file-list",
      file_list,
      if (pretty_json) "--prettyJSON",
      if (dry_run) "--dry-run"
    ),
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
    "finished {if (dry_run) 'checking' else 'extracting'} {length(file_paths)} file{?s}/archive{?s} ",
    conditions = problems,
    show_conditions = show_problems,
    summary_error_symbol = "!",
    start = start
  )
}

#' @describeIn ir_extract_isofiles return the version of the installed
#'   `isoextract` executable as a [numeric_version][base::numeric_version], or
#'   `NULL` if it is not installed (or does not report a recognizable version)
#' @export
ir_get_isoextract_version <- function() {
  get_assembly_version(get_isoextract_path(), "isoextract")
}
