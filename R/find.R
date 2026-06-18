#' Bundled example files
#' @description `ir_examples_folder()` returns the path to the folder with the
#'   example isodat files bundled with the package (a convenience wrapper around
#'   `system.file("extdata", package = "isoreader2")`). `ir_copy_examples()`
#'   copies those example files into a local `folder` so they can be read,
#'   re-extracted, or modified without touching the read-only package
#'   installation.
#' @return `ir_examples_folder()` returns the path to the example files folder as
#'   a single string.
#' @examples
#' ir_examples_folder() |> ir_find_scans()
#' @export
ir_examples_folder <- function() {
  system.file("extdata", package = "isoreader2")
}

#' @describeIn ir_examples_folder copy the bundled example files into a local
#'   `folder`, creating it if necessary and only copying files that do not
#'   already exist there (existing files are left untouched).
#' @param folder target directory to copy the example files into (default
#'   `"examples"`); created if it does not exist
#' @return `ir_copy_examples()` invisibly returns the path to the created examples folder
#' @examples
#' \dontrun{
#' ir_copy_examples() |> ir_find_continuous_flow()
#' }
#' @export
ir_copy_examples <- function(folder = "examples") {
  check_arg(
    folder,
    is_scalar_character(folder),
    "must be a single folder path"
  )
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }
  sources <- list.files(ir_examples_folder(), full.names = TRUE)
  targets <- file.path(folder, basename(sources))
  to_copy <- !file.exists(targets)
  if (any(to_copy)) {
    file.copy(sources[to_copy], folder, overwrite = FALSE)
  }
  cli_inform(c(
    "v" = "copied {sum(to_copy)} example file{?s} to {.path {folder}}",
    if (any(!to_copy)) {
      c("i" = "skipped {sum(!to_copy)} already-existing file{?s}")
    }
  ))
  invisible(folder)
}

#' Find isodat files
#' @description Finds isodat files with the specified extensions in one or more folders.
#' @param folder path to a folder with isodat files, or a character vector of folder paths
#' @param types file extensions to include (without leading dot), default is all supported types: `c("dxf", "cf", "iarc", "larc", "bch", "imexp", "caf", "did", "scn")`
#' @param pattern provide a name pattern to find only specific files
#' @param recursive whether to find files recursively
#' @return a sorted character vector of unique paths that correspond to the original data files (without `.json` suffixes if those are the versions of the files that are present)
#' @export
ir_find_isofiles <- function(
  folder,
  types = c("dxf", "cf", "iarc", "larc", "bch", "imexp", "caf", "did", "scn"),
  pattern = NULL,
  recursive = TRUE
) {
  # safety checks
  check_arg(
    folder,
    !missing(folder) &&
      is_character(folder) &&
      length(folder) > 0 &&
      all(dir.exists(folder)),
    format_inline(
      "must point to {qty(if(!missing(folder)) length(folder) else 1)}{?an/} existing director{?y/ies}"
    ),
    include_type = FALSE,
    include_value = TRUE
  )
  check_arg(
    types,
    is_character(types) && length(types) > 0,
    "must be a non-empty character vector of file extensions"
  )
  check_arg(
    pattern,
    is.null(pattern) || is_scalar_character(pattern),
    "must be a single string if provided"
  )
  check_arg(recursive, is_scalar_logical(recursive), "must be TRUE or FALSE")

  types <- tolower(types)

  # .bch entries are folders; all other extensions are files
  file_types <- setdiff(types, "bch")
  has_bch <- "bch" %in% types

  # file-based types
  if ("imexp" %in% file_types) {
    file_types <- file_types |> c("imexp.zip")
  }
  files <- character(0)
  json_files <- character(0)
  if (length(file_types) > 0L) {
    ext_pattern <- paste0("\\.(", paste(file_types, collapse = "|"), ")$")
    json_ext_pattern <- paste0(
      "\\.(",
      paste(file_types, collapse = "|"),
      ")\\.json$"
    )
    files <- list.files(
      folder,
      pattern = ext_pattern,
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = recursive
    )
    files <- sub("\\.zip$", "", files, ignore.case = TRUE)
    json_files <- list.files(
      folder,
      pattern = json_ext_pattern,
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = recursive
    )
    json_files <- sub("(\\.zip)?\\.json$", "", json_files, ignore.case = TRUE)
  }

  # .bch folders and their .json sidecars
  bch_dirs <- character(0)
  bch_json_files <- character(0)
  if (has_bch) {
    all_dirs <- list.dirs(folder, full.names = TRUE, recursive = recursive)
    bch_dirs <- all_dirs[grepl("\\.bch$", all_dirs, ignore.case = TRUE)]
    bch_json_files <- list.files(
      folder,
      pattern = "\\.bch\\.json$",
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = recursive
    )
    bch_json_files <- sub("\\.json$", "", bch_json_files, ignore.case = TRUE)
  }

  files <- unique(c(files, json_files, bch_dirs, bch_json_files))

  if (!is.null(pattern)) {
    files <- files[grepl(pattern, files)]
  }

  return(sort(files))
}

#' @describeIn ir_find_isofiles finds continuous flow files (`.dxf`, `.cf`)
#' @examples
#' ir_find_continuous_flow(system.file("extdata", package = "isoreader2"))
#' @export
ir_find_continuous_flow <- function(folder, pattern = NULL, recursive = TRUE) {
  ir_find_isofiles(
    folder,
    types = c("dxf", "cf", "iarc", "larc", "bch", "imexp"),
    pattern = pattern,
    recursive = recursive
  )
}

#' @describeIn ir_find_isofiles finds dual inlet files (`.did`, `.caf`)
#' @examples
#' ir_find_dual_inlet(system.file("extdata", package = "isoreader2"))
#' @export
ir_find_dual_inlet <- function(folder, pattern = NULL, recursive = TRUE) {
  ir_find_isofiles(
    folder,
    types = c("did", "caf"),
    pattern = pattern,
    recursive = recursive
  )
}

#' @describeIn ir_find_isofiles finds scan files (`.scn`)
#' @examples
#' ir_find_scans(system.file("extdata", package = "isoreader2"))
#' @export
ir_find_scans <- function(folder, pattern = NULL, recursive = TRUE) {
  ir_find_isofiles(
    folder,
    types = "scn",
    pattern = pattern,
    recursive = recursive
  )
}
