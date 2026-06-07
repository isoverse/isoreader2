# metadata operations on ir_aggregated_data ==========

#' Filter, mutate, or join the metadata of aggregated isofiles
#'
#' These functions operate on the `$metadata` component of an [ir_aggregate_isofiles()] result.
#' `ir_filter_metadata()` additionally cascades the filter to all other datasets:
#' `traces`, `cycles`, and `scans` are filtered by the remaining `uidx` + `analysis` combinations;
#' `resistors` and `problems` are filtered by the remaining `uidx` values.
#' After filtering, columns that are entirely `NA` across all remaining rows are
#' dropped from every dataset.
#' All three functions also clear the *not-aggregated* column information (columns
#' present in the source files but not included in the aggregator) from every
#' dataset, since that information is no longer meaningful after the metadata has
#' been modified.
#'
#' @param aggregated_data datasets aggregated from [ir_aggregate_isofiles()]
#' @param ... passed to [dplyr::filter()], [dplyr::mutate()], or [dplyr::left_join()] respectively
#' @return the `aggregated_data` object with an updated `$metadata`
#' @name ir_metadata
NULL

# internal: remove unused_columns attributes from all datasets
drop_not_aggregated_info <- function(aggregated_data) {
  for (ds in names(aggregated_data)) {
    if (is.data.frame(aggregated_data[[ds]])) {
      attr(aggregated_data[[ds]], "unused_columns") <- NULL
    }
  }
  return(aggregated_data)
}

#' @describeIn ir_metadata filter rows of the metadata (and cascade to other datasets)
#' @export
ir_filter_metadata <- function(aggregated_data, ...) {
  check_arg(
    aggregated_data,
    !missing(aggregated_data) && is(aggregated_data, "ir_aggregated_data"),
    "must be a set of aggregated isofiles"
  )

  aggregated_data$metadata <- dplyr::filter(aggregated_data$metadata, ...)

  # cascade: unique uidx + analysis remaining after filter
  remaining <- dplyr::distinct(
    dplyr::select(
      aggregated_data$metadata,
      dplyr::any_of(c("uidx", "analysis"))
    )
  )
  remaining_uidx <- unique(aggregated_data$metadata$uidx)

  # traces/cycles/scans: filter by uidx + analysis (whichever are present)
  for (ds in intersect(
    c("traces", "cycles", "scans"),
    names(aggregated_data)
  )) {
    by <- intersect(c("uidx", "analysis"), names(aggregated_data[[ds]]))
    if (length(by) > 0) {
      aggregated_data[[ds]] <- dplyr::semi_join(
        aggregated_data[[ds]],
        remaining,
        by = by
      )
    }
  }

  # resistors + problems: filter by uidx only
  for (ds in intersect(c("resistors", "problems"), names(aggregated_data))) {
    if ("uidx" %in% names(aggregated_data[[ds]])) {
      aggregated_data[[ds]] <- dplyr::filter(
        aggregated_data[[ds]],
        .data$uidx %in% remaining_uidx
      )
    }
  }

  # drop all-NA columns from every dataset
  for (ds in names(aggregated_data)) {
    if (is.data.frame(aggregated_data[[ds]]) && ncol(aggregated_data[[ds]]) > 0) {
      aggregated_data[[ds]] <- dplyr::select(
        aggregated_data[[ds]],
        dplyr::where(~!all(is.na(.)))
      )
    }
  }

  aggregated_data <- drop_not_aggregated_info(aggregated_data)
  return(aggregated_data)
}

#' @describeIn ir_metadata add or modify columns in the metadata
#' @export
ir_mutate_metadata <- function(aggregated_data, ...) {
  check_arg(
    aggregated_data,
    !missing(aggregated_data) && is(aggregated_data, "ir_aggregated_data"),
    "must be a set of aggregated isofiles"
  )
  aggregated_data$metadata <- dplyr::mutate(aggregated_data$metadata, ...)
  aggregated_data <- drop_not_aggregated_info(aggregated_data)
  return(aggregated_data)
}

#' @describeIn ir_metadata left-join additional columns into the metadata
#' @param y data frame to join to the metadata
#' @param by character vector of columns to join by (passed to [dplyr::left_join()])
#' @export
ir_join_metadata <- function(aggregated_data, y, by) {
  check_arg(
    aggregated_data,
    !missing(aggregated_data) && is(aggregated_data, "ir_aggregated_data"),
    "must be a set of aggregated isofiles"
  )
  n_before <- nrow(aggregated_data$metadata)
  aggregated_data$metadata <- dplyr::left_join(
    aggregated_data$metadata,
    y,
    by = by
  )
  if (nrow(aggregated_data$metadata) > n_before) {
    cli_abort(
      c(
        "joining {.arg y} to metadata by {.field {by}} duplicated rows ({n_before} \u2192 {nrow(aggregated_data$metadata)})",
        "i" = "make sure {.arg y} has at most one row per unique combination of {.field {by}}"
      )
    )
  }
  aggregated_data <- drop_not_aggregated_info(aggregated_data)
  return(aggregated_data)
}
