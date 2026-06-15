# metadata operations on ir_aggregated_data and ir_isofiles ==========

#' Filter, mutate, or join the metadata of isofiles
#'
#' These functions modify the metadata of either an [ir_aggregate_isofiles()]
#' result (`ir_aggregated_data`) or a collection of isofiles read with
#' [ir_read_isofiles()] (`ir_isofiles`).
#'
#' For `ir_aggregated_data`, the operation is applied once to the combined
#' `$metadata` data frame. For `ir_filter_metadata()`, the filter then cascades
#' to all other datasets: `traces`, `cycles`, and `scans` are filtered by the
#' remaining `uidx` + `analysis` combinations; `resistors` and `problems` are
#' filtered by the remaining `uidx` values.
#'
#' For `ir_isofiles`, the same operation is instead applied **individually to
#' each row** (i.e. to each file's own nested datasets), since an `ir_isofiles`
#' object has no combined metadata to operate on. Within each row, the filter
#' cascade uses whichever linking columns are present (typically `analysis`).
#' For `ir_filter_metadata()`, any file whose metadata ends up with 0 rows after
#' the filter is removed from the `ir_isofiles` collection entirely.
#'
#' Operating on an unaggregated `ir_isofiles` object is supported for convenience,
#' but is **significantly slower** than operating on an `ir_aggregated_data`
#' result, because the operation has to be carried out separately on every file
#' rather than once on the combined metadata. For anything beyond small
#' collections, prefer aggregating first with [ir_aggregate_isofiles()] and then
#' applying these functions to the result.
#'
#' After filtering, columns that are entirely `NA` across all remaining rows are
#' dropped from every (non-empty) dataset.
#' All three functions also clear the *not-aggregated* column information (columns
#' present in the source files but not included in the aggregator) from every
#' dataset, since that information is no longer meaningful after the metadata has
#' been modified.
#'
#' @param isofiles datasets aggregated from [ir_aggregate_isofiles()]
#'   (`ir_aggregated_data`) or a collection of isofiles from [ir_read_isofiles()]
#'   (`ir_isofiles`)
#' @param ... passed to [dplyr::filter()], [dplyr::mutate()], or [dplyr::left_join()] respectively
#' @return the `isofiles` object (of the same type as the input) with updated metadata
#' @name ir_metadata
NULL

# internal: check the isofiles argument is a supported type
check_isofiles_arg <- function(
  isofiles,
  .arg = caller_arg(isofiles),
  .env = caller_env()
) {
  check_arg(
    isofiles,
    !missing(isofiles) &&
      (is(isofiles, "ir_aggregated_data") || is(isofiles, "ir_isofiles")),
    paste(
      "must be a set of aggregated isofiles (use ir_aggregate_isofiles())",
      "or a collection of isofiles (use ir_read_isofiles())"
    ),
    .arg = .arg,
    .env = .env
  )
}

# internal: remove unused_columns attributes from all datasets in a list
drop_not_aggregated_info <- function(datasets) {
  for (ds in names(datasets)) {
    if (is.data.frame(datasets[[ds]])) {
      attr(datasets[[ds]], "unused_columns") <- NULL
    }
  }
  return(datasets)
}

# internal: apply a function `f` (operating on a named list of datasets) either
# once to an aggregated dataset, or individually to each row of an ir_isofiles
# object (gathering its nested dataset list-columns into a list, applying `f`,
# and writing the results back into the same row). When `drop_empty_metadata` is
# TRUE, ir_isofiles rows whose metadata ends up with 0 rows are removed entirely.
apply_metadata_op <- function(isofiles, f, drop_empty_metadata = FALSE) {
  if (is(isofiles, "ir_isofiles")) {
    # nested dataset columns are the list-columns holding data frames
    ds_cols <- names(isofiles)[purrr::map_lgl(
      isofiles,
      \(col) is.list(col) && all(purrr::map_lgl(col, is.data.frame))
    )]
    keep <- rep(TRUE, nrow(isofiles))
    for (i in seq_len(nrow(isofiles))) {
      datasets <- purrr::map(isofiles[ds_cols], \(col) col[[i]])
      result <- f(datasets)
      for (ds in names(result)) {
        isofiles[[ds]][[i]] <- result[[ds]]
      }
      if (drop_empty_metadata && nrow(result$metadata) == 0L) {
        keep[i] <- FALSE
      }
    }
    if (!all(keep)) {
      cls <- class(isofiles)
      isofiles <- isofiles[keep, ]
      class(isofiles) <- cls
    }
    return(isofiles)
  }
  # aggregated data: apply once to the whole object (a list of datasets)
  return(f(isofiles))
}

# core operations on a named list of datasets ====

# filter the metadata and cascade to the other datasets
filter_metadata_datasets <- function(datasets, ...) {
  datasets$metadata <- dplyr::filter(datasets$metadata, ...)

  # cascade: unique uidx + analysis remaining after filter
  remaining <- dplyr::distinct(
    dplyr::select(datasets$metadata, dplyr::any_of(c("uidx", "analysis")))
  )
  remaining_uidx <- if ("uidx" %in% names(datasets$metadata)) {
    unique(datasets$metadata$uidx)
  } else {
    NULL
  }

  # traces/cycles/scans: filter by uidx + analysis (whichever are present)
  for (ds in intersect(c("traces", "cycles", "scans"), names(datasets))) {
    by <- intersect(c("uidx", "analysis"), names(datasets[[ds]]))
    if (length(by) > 0) {
      datasets[[ds]] <- dplyr::semi_join(datasets[[ds]], remaining, by = by)
    }
  }

  # resistors + problems: filter by uidx only (if present)
  for (ds in intersect(c("resistors", "problems"), names(datasets))) {
    if ("uidx" %in% names(datasets[[ds]])) {
      datasets[[ds]] <- dplyr::filter(
        datasets[[ds]],
        .data$uidx %in% remaining_uidx
      )
    }
  }

  # drop all-NA columns from every non-empty dataset
  for (ds in names(datasets)) {
    if (
      is.data.frame(datasets[[ds]]) &&
        ncol(datasets[[ds]]) > 0 &&
        nrow(datasets[[ds]]) > 0
    ) {
      datasets[[ds]] <- dplyr::select(
        datasets[[ds]],
        dplyr::where(~ !all(is.na(.)))
      )
    }
  }

  return(drop_not_aggregated_info(datasets))
}

# mutate the metadata
mutate_metadata_datasets <- function(datasets, ...) {
  datasets$metadata <- dplyr::mutate(datasets$metadata, ...)
  return(drop_not_aggregated_info(datasets))
}

# left-join into the metadata (erroring if it would duplicate rows)
join_metadata_datasets <- function(datasets, y, by, .env = caller_env()) {
  n_before <- nrow(datasets$metadata)
  datasets$metadata <- dplyr::left_join(datasets$metadata, y, by = by)
  if (nrow(datasets$metadata) > n_before) {
    cli_abort(
      c(
        "joining {.arg y} to metadata by {.field {by}} duplicated rows ({n_before} \u2192 {nrow(datasets$metadata)})",
        "i" = "make sure {.arg y} has at most one row per unique combination of {.field {by}}"
      ),
      call = .env
    )
  }
  return(drop_not_aggregated_info(datasets))
}

# public functions ====

#' @describeIn ir_metadata filter rows of the metadata (and cascade to the other datasets)
#' @export
ir_filter_metadata <- function(isofiles, ...) {
  check_isofiles_arg(isofiles)
  dots <- rlang::enquos(...)
  apply_metadata_op(
    isofiles,
    \(datasets) rlang::inject(filter_metadata_datasets(datasets, !!!dots)),
    drop_empty_metadata = TRUE
  )
}

#' @describeIn ir_metadata add or modify columns in the metadata
#' @export
ir_mutate_metadata <- function(isofiles, ...) {
  check_isofiles_arg(isofiles)
  dots <- rlang::enquos(...)
  apply_metadata_op(
    isofiles,
    \(datasets) rlang::inject(mutate_metadata_datasets(datasets, !!!dots))
  )
}

#' @describeIn ir_metadata left-join additional columns into the metadata
#' @param y data frame to join to the metadata
#' @param by character vector of columns to join by (passed to [dplyr::left_join()])
#' @export
ir_join_metadata <- function(isofiles, y, by) {
  check_isofiles_arg(isofiles)
  env <- caller_env()
  apply_metadata_op(
    isofiles,
    \(datasets) join_metadata_datasets(datasets, y, by, .env = env)
  )
}

#' Filter isofiles by measurement type
#'
#' Convenience wrappers around [ir_filter_metadata()] that keep only the files of
#' a single measurement type (using the metadata `type` column): continuous flow
#' (`"cf"`), dual inlet (`"di"`), or scan (`"scan"`). Like [ir_filter_metadata()]
#' they work on both `ir_isofiles` (from [ir_read_isofiles()]) and
#' `ir_aggregated_data` (from [ir_aggregate_isofiles()]) objects, cascade to the
#' other datasets, and drop any file whose metadata ends up empty.
#'
#' Files whose metadata has no `type` column (e.g. a file that errored during
#' reading) never match and are dropped.
#'
#' @param isofiles a collection of isofiles from [ir_read_isofiles()]
#'   (`ir_isofiles`) or datasets aggregated from [ir_aggregate_isofiles()]
#'   (`ir_aggregated_data`)
#' @return the `isofiles` object filtered to the requested measurement type
#' @name ir_filter_for
NULL

# safe row predicate for filtering metadata by `type`: returns TRUE for rows
# whose `type` equals `value`, and all FALSE (dropping the file) when there is no
# `type` column at all (e.g. a file that errored on read so it never got a type)
metadata_type_is <- function(metadata, value) {
  if ("type" %in% names(metadata)) {
    !is.na(metadata$type) & metadata$type == value
  } else {
    rep(FALSE, nrow(metadata))
  }
}

#' @describeIn ir_filter_for keep only continuous flow files (`type == "cf"`)
#' @export
ir_filter_for_continuous_flow <- function(isofiles) {
  ir_filter_metadata(
    isofiles,
    metadata_type_is(dplyr::pick(dplyr::everything()), "cf")
  )
}

#' @describeIn ir_filter_for keep only dual inlet files (`type == "di"`)
#' @export
ir_filter_for_dual_inlet <- function(isofiles) {
  ir_filter_metadata(
    isofiles,
    metadata_type_is(dplyr::pick(dplyr::everything()), "di")
  )
}

#' @describeIn ir_filter_for keep only scan files (`type == "scan"`)
#' @export
ir_filter_for_scans <- function(isofiles) {
  ir_filter_metadata(
    isofiles,
    metadata_type_is(dplyr::pick(dplyr::everything()), "scan")
  )
}
