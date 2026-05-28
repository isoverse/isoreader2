#' Find isodat files
#' @description Finds isodat files with the specified extensions in one or more folders.
#' @param folder path to a folder with isodat files, or a character vector of folder paths
#' @param types file extensions to include (without leading dot)
#' @param pattern provide a name pattern to find only specific files
#' @param recursive whether to find files recursively
#' @return a sorted character vector of unique file paths (`.json` sidecar suffixes are stripped so each file appears once regardless of whether a sidecar exists)
#' @export
ir_find_isofiles <- function(
  folder,
  types,
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
    !missing(types) && is_character(types) && length(types) > 0,
    "must be a non-empty character vector of file extensions"
  )
  check_arg(
    pattern,
    is.null(pattern) || is_scalar_character(pattern),
    "must be a single string if provided"
  )
  check_arg(recursive, is_scalar_logical(recursive), "must be TRUE or FALSE")

  types <- tolower(types)
  ext_pattern <- paste0("\\.(", paste(types, collapse = "|"), ")$")
  json_ext_pattern <- paste0("\\.(", paste(types, collapse = "|"), ")\\.json$")

  # direct isodat files
  files <- list.files(
    folder,
    pattern = ext_pattern,
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = recursive
  )

  # .json data files (e.g. .dxf.json) — strip the .json suffix so paths are canonical
  json_files <- list.files(
    folder,
    pattern = json_ext_pattern,
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = recursive
  )
  json_files <- sub("\\.json$", "", json_files, ignore.case = TRUE)

  files <- unique(c(files, json_files))

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
  ir_find_isofiles(folder, types = c("dxf", "cf"), pattern = pattern, recursive = recursive)
}

#' @describeIn ir_find_isofiles finds dual inlet files (`.did`, `.caf`)
#' @examples
#' ir_find_dual_inlet(system.file("extdata", package = "isoreader2"))
#' @export
ir_find_dual_inlet <- function(folder, pattern = NULL, recursive = TRUE) {
  ir_find_isofiles(folder, types = c("did", "caf"), pattern = pattern, recursive = recursive)
}

#' @describeIn ir_find_isofiles finds scan files (`.scn`)
#' @examples
#' ir_find_scans(system.file("extdata", package = "isoreader2"))
#' @export
ir_find_scans <- function(folder, pattern = NULL, recursive = TRUE) {
  ir_find_isofiles(folder, types = "scn", pattern = pattern, recursive = recursive)
}
