#' Find continuous flow files
#' @description Finds all continuous flow isodat files in a folder.
#' @param folder path to a folder with isodat files
#' @param types file extensions to include (without leading dot), default is
#'   `c("dxf", "cf")`
#' @param pattern provide a name pattern to find only specific files
#' @param recursive whether to find files recursively
#'
#' @examples
#'
#' # all continuous flow files provided with the isoreader2 package
#' ir_find_continuous_flow(system.file("extdata", package = "isoreader2"))
#'
#' @export
ir_find_continuous_flow <- function(
  folder,
  types = c("dxf", "cf"),
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
