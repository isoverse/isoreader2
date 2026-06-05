#' Find isodat files
#' @description Finds isodat files with the specified extensions in one or more folders.
#' @param folder path to a folder with isodat files, or a character vector of folder paths
#' @param types file extensions to include (without leading dot), default is all supported types: `c("dxf", "cf", "iarc", "bch", "caf", "did", "scn")`
#' @param pattern provide a name pattern to find only specific files
#' @param recursive whether to find files recursively
#' @return a sorted character vector of unique paths (`.json` sidecar suffixes are stripped so each entry appears once regardless of whether a sidecar exists; `.bch` entries are folders, all others are files)
#' @export
ir_find_isofiles <- function(
  folder,
  types = c("dxf", "cf", "iarc", "bch", "caf", "did", "scn"),
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
    json_files <- list.files(
      folder,
      pattern = json_ext_pattern,
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = recursive
    )
    json_files <- sub("\\.json$", "", json_files, ignore.case = TRUE)
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
    types = c("dxf", "cf", "iarc", "bch"),
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
