# isoreader2 specific implementation ==========

#' Get data frame from aggregated data
#'
#' Retrieve a specific subset of the aggregated data into a single data frame by specifying which columns to take from each dataset (metadata, traces, cycles, scans, resistors) using [dplyr::select()] syntax.
#' If data from more than one dataset is selected (e.g. some columns from `traces` AND some from `resistors`), the datasets are combined with an [dplyr::inner_join()] using the columns listed in `by` (only the ones actually in the datasets). Joins that would lead to duplicated data entries (i.e. many-to-many joins) are not allowed and will throw an error to avoid unexpected replications of individual datapoints. If you really want to do such a join, you'll have to do it manually.
#' @param aggregated_data datasets aggregated from [ir_aggregate_isofiles()]
#' @param metadata columns to get from the aggregated `metadata`, all [dplyr::select()] syntax is supported
#' @param traces columns to get from the aggregated `traces`, all [dplyr::select()] syntax is supported
#' @param cycles columns to get from the aggregated `cycles`, all [dplyr::select()] syntax is supported
#' @param scans columns to get from the aggregated `scans`, all [dplyr::select()] syntax is supported
#' @param resistors columns to get from the aggregated `resistors`, all [dplyr::select()] syntax is supported
#' @return a tibble
#' @export
ir_get_data <- function(
  aggregated_data,
  metadata = c("file_name"),
  traces = NULL,
  cycles = NULL,
  scans = NULL,
  resistors = NULL,
  by = c("uidx", "analysis", "config", "species", "channel", "mass")
) {
  aggregated_data |>
    check_arg(
      !missing(aggregated_data) && is(aggregated_data, "ir_aggregated_data"),
      "must be a set of aggregated isofiles (use ir_aggregate_isofiles())"
    )
  # what is requested?
  req_traces <- quo_is_null(enquo(traces))
  req_cycles <- quo_is_null(enquo(cycles))
  req_scans <- quo_is_null(enquo(scans))
  req_resistors <- quo_is_null(enquo(resistors))

  # only one of traces/cycles/scans allowed
  data_series <- c("traces", "cycles", "scans")
  provided <- data_series[
    !c(req_traces, req_cycles, req_scans)
  ]
  if (length(provided) > 1) {
    cli_abort(
      c(
        "only one of {.field traces}, {.field cycles}, or {.field scans} can be requested at a time",
        "i" = "you specified: {.field {provided}}"
      )
    )
  }

  # get data
  out <- get_data(
    .ds = aggregated_data,
    metadata = {{ metadata }},
    traces = {{ traces }},
    cycles = {{ cycles }},
    scans = {{ scans }},
    by = by
  )
  return(out)
}

# generic get data from aggregated ===========

# get data from an aggregated list of datasets via inner joins
# @param .ds the list of datasets (uses .ds to avoid accidental partial mapping by ...)
# @param ... named arguments for each dataset that should be included with a tidyselect expression for the columns to include
# @param by which columns to use for join by operations (if there are any)
# @param always_include_by - include the by columns in any dataset where they exist
# @param relationship passed to inner_join, default expectation is one to many
get_data <- function(
  .ds,
  ...,
  by = c(),
  always_include_by = TRUE,
  relationship = "one-to-many",
  .env = caller_env(),
  .call = caller_call()
) {
  # start
  start <- start_info("is running", .call = .call)

  # basic safety checks
  stopifnot(
    "`.ds` must be a list" = !missing(.ds) && is_list(.ds),
    "`.ds` must contain at least one data frame" = length(.ds) > 0 &&
      sum(sapply(.ds, is.data.frame) > 0),
    "`by` must be a character vector" = is_empty(by) || is_character(by)
  )

  # any selectors?
  selectors <- rlang::enquos(...)
  selectors <- selectors[!purrr::map_lgl(selectors, quo_is_null)]
  if (length(selectors) == 0) {
    cli_abort(
      c(
        "no dataset column selections provided",
        "i" = "available dataset{?s}: {cli::col_blue(names(.ds))}"
      ),
      call = .env
    )
  }

  # valid selectors?
  if (any(missing <- !names(selectors) %in% names(.ds))) {
    cli_abort(
      c(
        "dataset{?s} not in the data: {cli::col_blue(names(selectors)[missing])}",
        "i" = "available dataset{?s}: {cli::col_blue(names(.ds))}"
      ),
      call = .env
    )
  }

  # quick loop (better errors than map for the small number of .ds)
  join_bys <- list()
  out <- tibble()
  for (i in seq_along(selectors)) {
    # fetch
    new_data <- tryCatch(
      .ds[[names(selectors)[i]]] |>
        dplyr::select(dplyr::any_of(!!by), !!selectors[[i]]),
      error = function(cnd) {
        cli_abort(
          c(
            "encountered an error selecting columns for {cli::col_blue(names(selectors)[i])}",
            "i" = "columns in {cli::col_blue(names(selectors)[i])}: {.field {names(.ds[[names(selectors)[i]]])}}"
          ),
          parent = cnd,
          call = .env
        )
      }
    )
    # join
    if (i > 1) {
      join_by <- intersect(by, names(out)) |> intersect(names(new_data))
      if (length(join_by) == 0) {
        cli_abort(
          c(
            paste(
              "unclear how to join {cli::col_blue(names(selectors)[i])}",
              "with {cli::col_blue(names(selectors)[1:(i-1)])}"
            ),
            "i" = if (length(by) == 0) {
              "there are no join columns defined in {.var by}"
            } else {
              "allowed join columns: {.field {by}}"
            },
            "i" = "selected columns in {cli::col_blue(names(selectors)[i])}: {.field {names(new_data)}}",
            "i" = "selected columns in {cli::col_blue(names(selectors)[1:(i-1)])}: {.field {names(out)}}"
          ),
          call = .env
        )
      }

      # catch join error/warnings
      catch_error_and_warning <- function(cnd) {
        cli_abort(
          c(
            paste(
              "encountered issue when joining {cli::col_blue(names(selectors)[i])}",
              "with {cli::col_blue(names(selectors)[1:(i-1)])} by {.field {join_by}}",
              "with relationship \"{relationship}\""
            ),
            "i" = "are you sure the by column{?s} ({.field {join_by}}) {?is/are} sufficient and this operation is really what is intended?"
          ),
          parent = cnd,
          call = .env
        )
      }

      out <-
        tryCatch(
          dplyr::inner_join(
            out,
            new_data,
            by = join_by,
            relationship = relationship
          ),
          warning = catch_error_and_warning,
          error = catch_error_and_warning
        )
      join_bys <- c(join_bys, join_by)
    } else {
      # just one dataset
      out <- new_data
    }
  }

  # info
  n_rows <- purrr::map_int(.ds[names(selectors)], nrow) |> numbers_to_text()

  details <-
    if (length(selectors) == 1) {
      sprintf("{cli::col_blue('%s')}", names(selectors))
    } else {
      sprintf("{cli::col_blue('%s')} (%s)", names(selectors), n_rows)
    }
  details <- details |> purrr::map_chr(format_inline)

  # info
  finish_info(
    "retrieved {numbers_to_text(nrow(out))} records from ",
    if (length(selectors) > 1) "the combination of ",
    "{details}",
    if (length(selectors) > 1) " via {.field {unique(unlist(join_bys))}}",
    start = start,
    .call = .call
    # don't pass parent .env since we want these glues to execute in THIS env
  )

  # return
  return(out)
}
