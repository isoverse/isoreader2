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

# internal: the nested dataset columns of an ir_isofiles object, i.e. the
# list-columns holding data frames (metadata, traces, cycles, scans, ...).
# In a mixed collection a dataset only some of the file types have is NULL in the
# other rows (e.g. `traces` for a dual inlet file), so entries are allowed to be
# either a data frame or NULL - requiring all of them to be data frames would
# silently exclude `traces`/`cycles`/`scans` from every operation below as soon
# as file types are mixed.
isofiles_dataset_cols <- function(isofiles) {
  names(isofiles)[purrr::map_lgl(
    isofiles,
    \(col) {
      is.list(col) &&
        any(purrr::map_lgl(col, is.data.frame)) &&
        all(purrr::map_lgl(col, \(x) is.data.frame(x) || is.null(x)))
    }
  )]
}

# internal: gather the nested datasets of ir_isofiles row `i` into the same named
# list of data frames that the dataset operations below expect
isofiles_row_datasets <- function(isofiles, i, ds_cols) {
  purrr::map(isofiles[ds_cols], \(col) col[[i]])
}

# internal: apply a function `f` (operating on a named list of datasets) either
# once to an aggregated dataset, or individually to each row of an ir_isofiles
# object (gathering its nested dataset list-columns into a list, applying `f`,
# and writing the results back into the same row). When `drop_empty_metadata` is
# TRUE, ir_isofiles rows whose metadata ends up with 0 rows are removed entirely.
apply_metadata_op <- function(isofiles, f, drop_empty_metadata = FALSE) {
  if (is(isofiles, "ir_isofiles")) {
    ds_cols <- isofiles_dataset_cols(isofiles)
    keep <- rep(TRUE, nrow(isofiles))
    for (i in seq_len(nrow(isofiles))) {
      result <- f(isofiles_row_datasets(isofiles, i, ds_cols))
      for (ds in names(result)) {
        # single-bracket assignment of a 1-element list so that a NULL result
        # (a dataset this file type does not have) stays a NULL entry instead of
        # deleting the element and shortening the whole list-column
        isofiles[[ds]][i] <- list(result[[ds]])
      }
      if (
        drop_empty_metadata &&
          is.data.frame(result$metadata) &&
          nrow(result$metadata) == 0L
      ) {
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

# internal: cascade an already-filtered `metadata` to all the other datasets:
# traces/cycles/scans are restricted to the remaining uidx + analysis
# combinations, resistors/problems to the remaining uidx values. Finally all-NA
# columns and the "not aggregated" column info are dropped everywhere.
cascade_metadata_filter <- function(datasets) {
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

# filter the metadata and cascade to the other datasets
filter_metadata_datasets <- function(datasets, ...) {
  datasets$metadata <- dplyr::filter(datasets$metadata, ...)
  return(cascade_metadata_filter(datasets))
}

# internal: which of the datasets can be filtered by mass, i.e. are traces/
# cycles/scans that actually carry a `mass` column
mass_dataset_names <- function(datasets) {
  nms <- intersect(c("traces", "cycles", "scans"), names(datasets))
  nms[purrr::map_lgl(
    datasets[nms],
    \(ds) is.data.frame(ds) && "mass" %in% names(ds)
  )]
}

# internal: collect the mass values available across an entire ir_aggregated_data
# or ir_isofiles object (for the latter across ALL files, so that the `mass`
# selection is resolved once against the whole collection rather than per file -
# a mass present in only some of the files must not error on the others).
# Returns `any`, whether there is any mass-carrying dataset at all, and `values`,
# the unique mass values as character (NAs dropped).
collect_mass_info <- function(isofiles) {
  per_object <- function(datasets) {
    nms <- mass_dataset_names(datasets)
    list(
      any = length(nms) > 0L,
      values = unlist(
        purrr::map(nms, \(nm) as.character(datasets[[nm]]$mass)),
        use.names = FALSE
      )
    )
  }
  infos <- if (is(isofiles, "ir_isofiles")) {
    ds_cols <- isofiles_dataset_cols(isofiles)
    purrr::map(
      seq_len(nrow(isofiles)),
      \(i) per_object(isofiles_row_datasets(isofiles, i, ds_cols))
    )
  } else {
    list(per_object(isofiles))
  }
  values <- unique(unlist(purrr::map(infos, "values"), use.names = FALSE))
  list(
    any = any(purrr::map_lgl(infos, "any")),
    values = values[!is.na(values)]
  )
}

# filter traces/cycles/scans to the already resolved `selected` mass values, then
# restrict the metadata to the records (uidx + analysis) that still have data in
# at least one of them and cascade that to the remaining datasets
filter_masses_datasets <- function(datasets, selected) {
  nms <- mass_dataset_names(datasets)
  for (ds in nms) {
    keep <- as.character(datasets[[ds]]$mass) %in% selected
    datasets[[ds]] <- datasets[[ds]][keep, , drop = FALSE]
  }

  # the records that still have data: the uidx/analysis combinations left in any
  # of the mass datasets (only those that link to the metadata at all)
  by <- intersect(c("uidx", "analysis"), names(datasets$metadata))
  if (length(by) > 0L) {
    remaining <- purrr::compact(purrr::map(nms, \(nm) {
      if (all(by %in% names(datasets[[nm]]))) {
        dplyr::distinct(dplyr::select(datasets[[nm]], dplyr::all_of(by)))
      }
    }))
    # without any linked dataset there is no way to tell which records lost their
    # data, so the metadata is left untouched
    if (length(remaining) > 0L) {
      datasets$metadata <- dplyr::semi_join(
        datasets$metadata,
        dplyr::distinct(dplyr::bind_rows(remaining)),
        by = by
      )
    }
  }

  return(cascade_metadata_filter(datasets))
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

# internal: total number of mass data rows and metadata records in an
# ir_aggregated_data / ir_isofiles object (for the info message)
count_mass_data <- function(isofiles) {
  per_object <- function(datasets) {
    c(
      rows = sum(purrr::map_int(
        mass_dataset_names(datasets),
        \(nm) nrow(datasets[[nm]])
      )),
      records = if (is.data.frame(datasets$metadata)) {
        nrow(datasets$metadata)
      } else {
        0L
      }
    )
  }
  counts <- if (is(isofiles, "ir_isofiles")) {
    ds_cols <- isofiles_dataset_cols(isofiles)
    purrr::map(
      seq_len(nrow(isofiles)),
      \(i) per_object(isofiles_row_datasets(isofiles, i, ds_cols))
    )
  } else {
    list(per_object(isofiles))
  }
  list(
    rows = sum(purrr::map_dbl(counts, "rows")),
    records = sum(purrr::map_dbl(counts, "records"))
  )
}

#' Filter isofiles by mass
#'
#' Keeps only the requested masses in the `traces`, `cycles`, and `scans` data of
#' either an [ir_aggregate_isofiles()] result (`ir_aggregated_data`) or a
#' collection of isofiles read with [ir_read_isofiles()] (`ir_isofiles`). Like
#' [ir_filter_metadata()] it works on both object types and returns the same type
#' it was given.
#'
#' Any metadata record (`uidx` + `analysis`) that has no data left in *any* of
#' `traces`/`cycles`/`scans` afterwards is removed from the metadata, and the
#' removal cascades to the other datasets exactly as in [ir_filter_metadata()]
#' (`resistors` and `problems` are restricted to the remaining `uidx`). For an
#' `ir_isofiles` object, a file whose metadata ends up empty is dropped from the
#' collection entirely. Note that this also drops records that never had any
#' `traces`/`cycles`/`scans` data to begin with (e.g. a file that failed to
#' read) - check [ir_get_problems()] before filtering if you need to keep track
#' of those.
#'
#' The `mass` selection is always resolved **once against the whole object** (all
#' files of an `ir_isofiles` collection, and all of `traces`/`cycles`/`scans` of
#' an aggregated dataset combined), so selecting a mass that only some of the
#' files or datasets contain works and simply leaves the others without data.
#'
#' Ratios calculated with [ir_calculate_ratios()] live in the `ratio_name` /
#' `ratio` columns of the rows of their **numerator** mass, so they are kept or
#' removed with that mass: filtering to `mass = 44` also removes the `"45/44"`
#' ratio (which sits on the mass 45 rows), while `mass = c(44, 45)` keeps it.
#' `resistors` are never filtered by mass since they describe the instrument
#' configuration rather than measured data.
#'
#' As for the metadata operations, columns that are entirely `NA` across all
#' remaining rows are dropped from every (non-empty) dataset and the
#' *not-aggregated* column information is cleared.
#'
#' @inheritParams ir_metadata
#' @param mass which masses to keep, as a [tidyselect][tidyselect::language]
#'   expression evaluated as if the masses present in the data were column names
#'   - the same syntax as the `mass` argument of [ir_plot_traces()]. E.g.
#'   `c("44", "45")` or `44:48` for specific masses, `-"45"`/`!"45"` to exclude
#'   one, `everything()` for all of them, and helpers such as `starts_with("4")`,
#'   `matches()`, `all_of()`, or `any_of()`. Unlike plain tidyselect, numbers
#'   select by name rather than by position (`44:48` means the masses 44 to 48,
#'   not the 44th to 48th mass). Selecting a mass that is not in the data is an
#'   error that lists the available masses; use `any_of()` to ignore missing ones.
#' @return the `isofiles` object (of the same type as the input) with only the
#'   selected masses
#' @examples
#' \dontrun{
#' # keep only the CO2 masses
#' dataset |> ir_filter_masses(44:46)
#'
#' # keep everything except mass 45
#' dataset |> ir_filter_masses(-"45")
#' }
#' @export
ir_filter_masses <- function(isofiles, mass) {
  check_isofiles_arg(isofiles)
  if (missing(mass)) {
    cli_abort(c(
      "{.arg mass} must be provided",
      "i" = "e.g. {.code mass = 44:46}, {.code mass = c(\"44\", \"45\")}, or {.code mass = everything()}"
    ))
  }
  mass_quo <- rlang::enquo(mass)
  env <- caller_env()

  start <- start_info("is running")

  # resolve the selection once against the masses of the whole object
  info <- collect_mass_info(isofiles)
  if (!info$any) {
    cli_abort(c(
      "no {.field traces}, {.field cycles}, or {.field scans} data with a {.field mass} column found to filter",
      "i" = "aggregate with a suitable aggregator first (e.g. {.code ir_aggregate_isofiles(aggregator = \"standard\")})"
    ))
  }
  selected <- eval_trace_selection(
    mass_quo,
    info$values,
    "mass",
    "masses",
    .env = env
  )
  if (length(selected) == 0L) {
    cli_abort(c(
      "{.arg mass} selects no masses, which would remove all data",
      "i" = "available masses: {.val {info$values}}"
    ))
  }

  before <- count_mass_data(isofiles)
  isofiles <- apply_metadata_op(
    isofiles,
    \(datasets) filter_masses_datasets(datasets, selected),
    drop_empty_metadata = TRUE
  )
  after <- count_mass_data(isofiles)

  n_rows <- before$rows - after$rows
  n_records <- before$records - after$records
  finish_info(
    paste0(
      "kept {qty(length(selected))}mass{?/es} {.field {selected}} ",
      "and removed {numbers_to_text(n_rows)} of {numbers_to_text(before$rows)}",
      " data {qty(before$rows)}row{?s}",
      if (n_records > 0) {
        " together with {numbers_to_text(n_records)}{qty(n_records)} record{?s} left without data"
      }
    ),
    start = start
  )

  return(isofiles)
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
