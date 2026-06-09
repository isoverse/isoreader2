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
#' Exports one or more data frames / tibbles (typically retrieved with the
#' `ir_get_*()` functions, e.g. [ir_get_metadata()], [ir_get_traces()]) to an
#' Excel file, one sheet per data frame. Pass the data frames as `...`: **named**
#' arguments use the name as the sheet name, **unnamed** arguments are placed in
#' a sheet named after their position (e.g. the 3rd unnamed data frame goes into
#' `"Sheet3"`).
#'
#' This function only accepts data frames. To store a complete
#' [ir_aggregate_isofiles()] result use [ir_save_aggregated_data()] instead.
#'
#' Requires the \pkg{openxlsx} package. If not installed, one
#' installation attempt from CRAN is made automatically.
#'
#' @param ... one or more data frames / tibbles to export, one per sheet. Named
#'   arguments set the sheet name; unnamed arguments use `"Sheet{position}"`.
#' @param file path to the `.xlsx` file (`.xlsx` extension added if absent)
#' @param dbl_digits number of decimal places shown for double columns (all
#'   digits are stored; this only affects display formatting in Excel)
#' @param int_format Excel number format string for integer columns
#' @param dbl_format Excel number format string for double columns (derived
#'   automatically from `dbl_digits` if not set)
#' @param show_progress whether to show a progress indicator
#' @return the exported data invisibly (the single data frame if one was
#'   provided, otherwise the list of data frames), for use in pipes
#' @examples
#' \dontrun{
#' agg <- ir_examples_folder() |>
#'   ir_find_continuous_flow() |>
#'   ir_read_isofiles() |>
#'   ir_aggregate_isofiles()
#' ir_export_to_excel(
#'   metadata = ir_get_metadata(agg),
#'   traces = ir_get_traces(agg),
#'   file = "my_export.xlsx"
#' )
#' }
#' @export
ir_export_to_excel <- function(
  ...,
  file,
  dbl_digits = 2,
  int_format = "0",
  dbl_format = sprintf(sprintf("%%.%sf", dbl_digits), 0),
  show_progress = is_interactive()
) {
  check_openxlsx()
  check_arg(file, !missing(file) && is_scalar_character(file), "must be a path")

  datasets <- rlang::list2(...)
  if (length(datasets) == 0) {
    cli_abort(
      c(
        "no data provided to export",
        "i" = "pass one or more data frames/tibbles, e.g. {.code ir_export_to_excel(metadata = ir_get_metadata(agg), file = \"out.xlsx\")}"
      )
    )
  }

  # all `...` arguments must be data frames
  is_df <- purrr::map_lgl(datasets, is.data.frame)
  if (!all(is_df)) {
    bad <- which(!is_df)
    positions <- paste(bad, collapse = ", ")
    cli_abort(
      c(
        "every {.arg ...} argument must be a data frame or tibble",
        "i" = "{qty(length(bad))}argument{?s} at position{?s} {positions} {?is/are} not a data frame",
        "i" = "to store a full aggregated dataset use {.fn ir_save_aggregated_data} instead"
      )
    )
  }

  # sheet names: named args use the name, unnamed use "Sheet{position}"
  nms <- names(datasets) %||% rep("", length(datasets))
  nms[is.na(nms)] <- ""
  sheet_names <- ifelse(
    nzchar(nms),
    nms,
    paste0("Sheet", seq_along(datasets))
  )
  # Excel sheet names are limited to 31 characters and must be unique
  sheet_names <- substr(sheet_names, 1L, 31L)
  if (anyDuplicated(sheet_names)) {
    cli_abort(
      c(
        "sheet names must be unique",
        "i" = "duplicated (after truncation to 31 characters): {.field {unique(sheet_names[duplicated(sheet_names)])}}"
      )
    )
  }

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
  info <- character(length(datasets))
  for (i in seq_along(datasets)) {
    if (show_progress) {
      cli_progress_update(
        id = start$pb,
        inc = 0,
        status = sheet_names[i],
        force = TRUE
      )
    }
    info[i] <- format_inline(
      "{numbers_to_text(nrow(datasets[[i]]))} {qty(nrow(datasets[[i]]))}row{?s} of {.field {sheet_names[i]}}"
    )
    add_excel_sheet(
      wb,
      sheet_name = sheet_names[i],
      dataset = datasets[[i]],
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
  return(invisible(if (length(datasets) == 1L) datasets[[1L]] else datasets))
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
