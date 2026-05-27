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

#' Read continuous flow files
#'
#' @param file_paths paths to the continuous flow file(s), single value or vector of paths. Use [ir_find_continuous_flow()] to get all continuous flow files in a folder.
#' @param show_progress whether to show a progress bar, by default always enabled when running interactively e.g. inside Positron or RStudio (and disabled in a notebook), turn off with `show_progress = FALSE`
#' @param show_problems whether to show problems encountered along the way (rather than just keeping track of them with [ir_get_problems()]). Set to `show_problems = FALSE` to turn off the live printout. Either way, all encountered problems can be retrieved with running [ir_get_problems()] for the returned list
#' @param reextract whether to re-extract files (uses isoextract to read files from scratch), if FALSE (default) only extract files not previously extracted
#' @return a tibble data frame where each row holds the file path and nested tibbles of datasets extracted from the continuous flow files. Use [orbi_aggregate_raw()] to aggregate data safely across files.
#' @export
ir_read_continuous_flow <- function(
  file_paths,
  show_progress = rlang::is_interactive(),
  show_problems = TRUE,
  reextract = FALSE
) {
  # keep track of current env to anchor progress bars
  root_env <- current_env()

  # safety checks
  check_arg(
    file_paths,
    !missing(file_paths) &&
      is_character(file_paths) &&
      length(file_paths) > 0,
    "must be at least one file path"
  )

  # all directories?
  if (all(dir.exists(file_paths))) {
    cli_abort(
      c(
        "{?this/these} path{?s} ({.file {file_paths}}) {?is a/are} director{?y/ies}, not {?a /}raw file{?s}",
        "i" = "did you mean to run {.strong ir_find_continuous_flow()} instead?"
      )
    )
  }

  # check which files have .json extracts already
  file_paths_info <- tibble(
    file_path = gsub("\\.json$", "", file_paths),
    has_json = file.exists(file_path |> paste0(".json"))
  )

  # initialize progress bar
  start <- start_info(
    "preparing to read {length(file_paths)} continuous flow file{?s}",
    pb_total = nrow(file_paths_info),
    show_progress = show_progress,
    .env = root_env
  )

  # empty metadata
  empty_meta <- tibble::tibble(
    isoextract_version = NA_character_,
    file_type = NA_character_,
    file_size_bytes = NA_integer_,
    complete = NA
  )

  # fetch metadata from json files
  file_paths_info <- file_paths_info |>
    dplyr::mutate(
      meta = purrr::map2(
        .data$file_path,
        .data$has_json,
        function(fp, has_json) {
          if (!has_json) {
            # no json file
            return(empty_meta)
          }
          out <- read_json_meta(paste0(fp, ".json")) |> try_catch_cnds()
          if (is.null(out$result)) {
            # something went wrong reading the metadata --> reread
            return(empty_meta)
          }
          return(out$result)
        }
      )
    ) |>
    tidyr::unnest(.data$meta) |>
    dplyr::left_join(.file_type_specs, by = "file_type") |>
    dplyr::mutate(
      version_ok = numeric_version(.data$isoextract_version) >=
        numeric_version(.data$min_isoextract_version)
    )

  finish_info(
    "read {nrow(file_paths_info)} continuous flow file{?s}",
    start = start,
    #conditions = all_conditions,
    show_conditions = show_problems,
    .env = root_env
  )

  return(file_paths_info)
}

# extract meta from a single JSON file
read_json_meta <- function(json_path) {
  meta <- RcppSimdJson::fload(json_path, query = "/meta")
  tibble::tibble(
    isoextract_version = meta$isoextract_version,
    file_type = meta$file_type,
    file_size_bytes = meta$file_size_bytes,
    complete = meta$complete
  )
}
