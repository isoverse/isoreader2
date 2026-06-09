# storage for ir_aggregated_data ==========

# check arrow is available, try to install from CRAN once if not
check_arrow <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    message(
      "package 'arrow' is required for storage but is not installed, attempting to install from CRAN..."
    )
    utils::install.packages("arrow")
    if (!requireNamespace("arrow", quietly = TRUE)) {
      cli_abort(
        c(
          "package {.pkg arrow} is required for {.fn ir_save_aggregated_data}/{.fn ir_load_aggregated_data} but could not be installed",
          "i" = "install manually with {.code install.packages('arrow')}"
        )
      )
    }
  }
}

#' Save and load aggregated isofile data
#'
#' `ir_save_aggregated_data()` serializes an [ir_aggregate_isofiles()] result to a parquet file.
#' Empty datasets (no columns) are dropped. The `condition` column of `problems`
#' is set to `NULL` per row because R condition objects cannot be stored in parquet.
#' `ir_load_aggregated_data()` reads the file back and returns an `ir_aggregated_data` object.
#'
#' Requires the \pkg{arrow} package (suggested). If not installed, one
#' installation attempt from CRAN is made automatically.
#'
#' @param aggregated_data datasets aggregated from [ir_aggregate_isofiles()]
#' @param file path to the parquet file (`.parquet` extension added if absent)
#' @return `ir_save_aggregated_data()` returns `aggregated_data` invisibly;
#'   `ir_load_aggregated_data()` returns an `ir_aggregated_data` object.
#' @name ir_storage
NULL

#' @describeIn ir_storage save aggregated data to a parquet file
#' @export
ir_save_aggregated_data <- function(aggregated_data, file) {
  check_arrow()
  check_arg(
    aggregated_data,
    !missing(aggregated_data) && is(aggregated_data, "ir_aggregated_data"),
    "must be a set of aggregated isofiles"
  )
  check_arg(file, !missing(file) && is_scalar_character(file), "must be a path")

  if (!grepl("\\.parquet$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".parquet")
  }

  start <- start_info("is saving to {.file {file}}")

  # drop datasets with no columns
  to_save <- aggregated_data[purrr::map_lgl(aggregated_data, ~ ncol(.x) > 0)]

  # null out condition objects in problems — they can't be stored in parquet
  if (
    "problems" %in% names(to_save) && "condition" %in% names(to_save$problems)
  ) {
    to_save$problems$condition <- vector("list", nrow(to_save$problems))
  }

  to_save |>
    purrr::map(list) |>
    tibble::as_tibble() |>
    arrow::write_parquet(file)

  finish_info(
    "saved data from {length(unique(to_save$metadata$uidx))} isofiles with {numbers_to_text(nrow(to_save$metadata))} {qty(nrow(to_save$metadata))}analys{?is/es} to {.file {file}}",
    start = start
  )
  return(invisible(aggregated_data))
}

#' @describeIn ir_storage load aggregated data from a parquet file
#' @export
ir_load_aggregated_data <- function(file) {
  check_arrow()
  check_arg(
    file,
    !missing(file) && is_scalar_character(file),
    "must be a string"
  )

  if (!grepl("\\.parquet$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".parquet")
  }

  start <- start_info("is loading from {.file {file}}")

  df <- arrow::read_parquet(file)
  result <- purrr::map(as.list(df), ~ .x[[1]])
  class(result) <- unique(c("ir_aggregated_data", class(result)))

  finish_info(
    "loaded data for {length(unique(result$metadata$uidx))} isofiles with {numbers_to_text(nrow(result$metadata))} {qty(nrow(result$metadata))}analys{?is/es} from {.file {file}}",
    start = start
  )
  return(result)
}

# storage for ir_isofiles ==========

#' Save and load isofiles
#'
#' `ir_save_isofiles()` serializes a collection of isofiles read with
#' [ir_read_isofiles()] to an RDS file using [readr::write_rds()], storing the
#' whole `ir_isofiles` object as-is (including all nested datasets and condition
#' objects) without any changes. `ir_load_isofiles()` reads the file back with
#' [readr::read_rds()] and returns the `ir_isofiles` object exactly as it was
#' saved.
#'
#' This operates at the unaggregated `ir_isofiles` level. To store an aggregated
#' result instead, use [ir_save_aggregated_data()] / [ir_load_aggregated_data()].
#'
#' @param isofiles a collection of isofiles from [ir_read_isofiles()]
#' @param file path to the RDS file (`.rds` extension added if absent)
#' @return `ir_save_isofiles()` returns `isofiles` invisibly;
#'   `ir_load_isofiles()` returns an `ir_isofiles` object.
#' @name ir_isofiles_storage
NULL

#' @describeIn ir_isofiles_storage save isofiles to an RDS file
#' @export
ir_save_isofiles <- function(isofiles, file) {
  check_arg(
    isofiles,
    !missing(isofiles) && is(isofiles, "ir_isofiles"),
    "must be a collection of isofiles (use ir_read_isofiles())"
  )
  check_arg(file, !missing(file) && is_scalar_character(file), "must be a path")

  if (!grepl("\\.rds$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".rds")
  }

  start <- start_info("is saving to {.file {file}}")

  readr::write_rds(isofiles, file)

  finish_info(
    "saved {numbers_to_text(nrow(isofiles))} {qty(nrow(isofiles))}isofile{?s} to {.file {file}}",
    start = start
  )
  return(invisible(isofiles))
}

#' @describeIn ir_isofiles_storage load isofiles from an RDS file
#' @export
ir_load_isofiles <- function(file) {
  check_arg(
    file,
    !missing(file) && is_scalar_character(file),
    "must be a string"
  )

  if (!grepl("\\.rds$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".rds")
  }

  start <- start_info("is loading from {.file {file}}")

  result <- readr::read_rds(file)

  if (!is(result, "ir_isofiles")) {
    cli_abort(
      c(
        "the file {.file {file}} does not contain a collection of isofiles",
        "i" = "it holds {.obj_type_friendly {result}} instead of an {.cls ir_isofiles} object"
      )
    )
  }

  finish_info(
    "loaded {numbers_to_text(nrow(result))} {qty(nrow(result))}isofile{?s} from {.file {file}}",
    start = start
  )
  return(result)
}
