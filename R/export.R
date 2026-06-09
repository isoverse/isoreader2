# export functions ==========

# check openxlsx is available, try to install from CRAN once if not
check_openxlsx <- function() {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    message(
      "package 'openxlsx' is required for Excel export but is not installed, attempting to install from CRAN..."
    )
    utils::install.packages("openxlsx")
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      cli_abort(
        c(
          "package {.pkg openxlsx} is required for {.fn ir_export_to_excel} but could not be installed",
          "i" = "install manually with {.code install.packages('openxlsx')}"
        )
      )
    }
  }
}

#' Export data to Excel
#'
#' Exports a data frame or an [ir_aggregate_isofiles()] result to an Excel file.
#' Each dataset in the aggregated data becomes its own sheet. The `include`
#' parameter controls which datasets are exported (default: just the metadata).
#' Possibilities: `"metadata"`, `"traces"` (for continuous flow data), `"cycles"`
#' (for dual inlet data), "`scans`" (for scan data), `"resistors"` (all file
#' formats), and `"isodat_data_table"` (only available from isodat .dxf, .cf
#' .did, and .caf files).
#'
#' Requires the \pkg{openxlsx} package. If not installed, one
#' installation attempt from CRAN is made automatically.
#'
#' @param data a data frame or an `ir_aggregated_data` object from [ir_aggregate_isofiles()]
#' @param file path to the `.xlsx` file (`.xlsx` extension added if absent)
#' @param include for `ir_aggregated_data` only: character vector of dataset
#'   names to include as sheets. Default `"metadata"` exports only the metadata.
#' @param dbl_digits number of decimal places shown for double columns (all
#'   digits are stored; this only affects display formatting in Excel)
#' @param int_format Excel number format string for integer columns
#' @param dbl_format Excel number format string for double columns (derived
#'   automatically from `dbl_digits` if not set)
#' @param show_progress whether to show a progress indicator
#' @return `data` invisibly, for use in pipes
#' @export
ir_export_to_excel <- function(
  data,
  file,
  include = "metadata",
  dbl_digits = 2,
  int_format = "0",
  dbl_format = sprintf(sprintf("%%.%sf", dbl_digits), 0),
  show_progress = is_interactive()
) {
  check_openxlsx()
  check_arg(
    data,
    !missing(data) && (is.data.frame(data) || is(data, "ir_aggregated_data")),
    "must be a data frame or a set of aggregated isofiles"
  )
  check_arg(file, !missing(file) && is_scalar_character(file), "must be a path")
  check_arg(include, is_character(include), "must be a character vector")

  if (!grepl("\\.xlsx$", file, ignore.case = TRUE)) {
    file <- paste0(file, ".xlsx")
  }

  # create output directory if needed
  output_dir <- dirname(file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  start <- start_info("is writing {.field {pb_status}} | {pb_elapsed}")

  wb <- openxlsx::createWorkbook()

  if (is(data, "ir_aggregated_data")) {
    # select sheets to export
    available <- names(data)[purrr::map_lgl(data, ~ ncol(.x) > 0)]
    sheets <- intersect(include, available)
    if (length(sheets) == 0) {
      cli_abort(
        c(
          "no datasets available to export",
          "i" = "none of {.field {include}} matched a non-empty dataset in {.arg data}"
        )
      )
    }
    info <- character(length(sheets))
    for (i in seq_along(sheets)) {
      if (show_progress) {
        cli_progress_update(
          id = start$pb,
          inc = 0,
          status = sheets[i],
          force = TRUE
        )
      }
      info[i] <- format_inline(
        "{numbers_to_text(nrow(data[[sheets[i]]]))} {qty(nrow(data[[sheets[i]]]))}row{?s} of {.field {sheets[i]}}"
      )
      add_excel_sheet(
        wb,
        sheet_name = sheets[i],
        dataset = data[[sheets[i]]],
        dbl_digits = dbl_digits,
        int_format = int_format,
        dbl_format = dbl_format
      )
    }
  } else {
    if (show_progress) {
      cli_progress_update(id = start$pb, inc = 0, status = "data", force = TRUE)
    }
    info <- format_inline(
      "{numbers_to_text(nrow(data))} {qty(nrow(data))}row{?s}, {ncol(data)} column{?s}"
    )
    add_excel_sheet(
      wb,
      sheet_name = "data",
      dataset = data,
      dbl_digits = dbl_digits,
      int_format = int_format,
      dbl_format = dbl_format
    )
  }

  if (show_progress) {
    cli_progress_update(id = start$pb, inc = 0, status = "file", force = TRUE)
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  finish_info(
    "exported {info} to {.file {file}}",
    start = start
  )
  return(invisible(data))
}

# internal: add a formatted sheet to an openxlsx workbook
add_excel_sheet <- function(
  wb,
  sheet_name,
  dataset,
  dbl_digits = 2,
  col_max_width = 75,
  int_format = "0",
  dbl_format = sprintf(sprintf("%%.%sf", dbl_digits), 0)
) {
  openxlsx::addWorksheet(wb, sheet_name)
  hs <- openxlsx::createStyle(textDecoration = "bold")

  start_row <- 1L
  sheet_data <- dplyr::ungroup(dataset)
  if (ncol(sheet_data) > 0) {
    openxlsx::writeData(
      wb,
      sheet_name,
      sheet_data,
      startRow = start_row,
      headerStyle = hs
    )

    int_cols <- which(purrr::map_lgl(sheet_data, is.integer))
    dbl_cols <- setdiff(which(purrr::map_lgl(sheet_data, is.numeric)), int_cols)
    if (dbl_digits < 1) {
      int_cols <- c(int_cols, dbl_cols)
      dbl_cols <- integer()
    }

    if (length(int_cols) > 0) {
      openxlsx::addStyle(
        wb,
        sheet_name,
        style = openxlsx::createStyle(numFmt = int_format),
        rows = (start_row + 1L):(start_row + 1L + nrow(sheet_data)),
        cols = int_cols,
        gridExpand = TRUE
      )
    }

    if (length(dbl_cols) > 0) {
      openxlsx::addStyle(
        wb,
        sheet_name,
        style = openxlsx::createStyle(numFmt = dbl_format),
        rows = (start_row + 1L):(start_row + 1L + nrow(sheet_data)),
        cols = dbl_cols,
        gridExpand = TRUE
      )
    }

    header_widths <- purrr::map_int(names(sheet_data), nchar)
    calculate_data_width <- function(x) {
      if (is.integer(x)) {
        x <- sprintf("%d", x)
      } else if (is.numeric(x)) {
        x <- sprintf(paste0("%.", dbl_digits, "f"), x)
      } else {
        x <- as.character(x)
      }
      max(c(0L, nchar(x)), na.rm = TRUE)
    }
    data_widths <- dplyr::summarize(
      sheet_data,
      dplyr::across(dplyr::everything(), calculate_data_width)
    ) |>
      unlist(use.names = FALSE)
    col_widths <- pmin(pmax(header_widths, data_widths), col_max_width)
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = seq_along(col_widths),
      widths = col_widths
    )
  }
}
