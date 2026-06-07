# intensity calculations =====

# Returns list(base, scale) for a unit string, where base is "V", "A", or "cps"
# and scale converts base units to this unit (e.g., "mV": base="V", scale=1e3).
intensity_unit_info <- function(unit) {
  if (unit == "cps") {
    return(list(base = "cps", scale = 1))
  }
  list(
    base = substr(unit, nchar(unit), nchar(unit)),
    scale = switch(
      unit,
      "mV" = 1e3,
      "V" = 1,
      "fA" = 1e15,
      "pA" = 1e12,
      "nA" = 1e9,
      "µA" = 1e6,
      "mA" = 1e3,
      "A" = 1
    )
  )
}

#' Convert intensity between units
#'
#' Note: this function is rarely called directly, it's run as part of ir_aggregate_isofiles to standardize the trace/cycle/scan datasets before aggregation.
#'
#' Converts the intensity column of a dataset between voltage, current, and
#' count-per-second units using `A = CPS * e` and `V = A * R`. Automatically
#' detects the source unit from any `intensity.<unit>` column present in
#' `dataset`. Joins `resistors` by `species` and `channel` (plus `config` when
#' present in both) only when the conversion path crosses the A/V boundary.
#' Returns `dataset` with the source intensity column replaced by
#' `intensity.<units>`.
#'
#' @param dataset a data frame with columns `species`, `channel`, and an
#'   `intensity.<unit>` column (e.g. `intensity.mV`, `intensity.nA`).
#' @param resistors a data frame with columns `species`, `channel`, and
#'   `resistance.Ohm`. Required when converting between voltage and
#'   current/CPS; ignored otherwise. If a `config` column is present in both
#'   `dataset` and `resistors` it is included in the join key.
#' @param units target unit, one of `"mV"`, `"V"`, `"fA"`, `"pA"`, `"nA"`,
#'   `"µA"`, `"mA"`, `"A"`, `"cps"`.
#' @export
ir_convert_intensity <- function(
  dataset,
  resistors = NULL,
  units = c("mV", "V", "fA", "pA", "nA", "µA", "mA", "A", "cps")
) {
  e <- 1.602176634e-19 # elementary charge (C)

  units <- units |> arg_match()
  tgt <- intensity_unit_info(units)

  # detect source column
  dataset |> check_tibble(req_cols = c("species", "channel"))
  src_col <- grep("^intensity\\.", names(dataset), value = TRUE)
  if (length(src_col) == 0L) {
    cli_abort("dataset has no {.field intensity.*} column")
  }
  src_col <- src_col[1L]
  src <- intensity_unit_info(sub("^intensity\\.", "", src_col))

  # join resistors when conversion path crosses the A<->V boundary
  needs_R <- (src$base == "V") != (tgt$base == "V")
  if (needs_R) {
    if (is.null(resistors)) {
      cli_abort(
        "converting {.field intensity} from {col_magenta(src$base)} to {col_magenta(units)} requires resistor values but none are available"
      )
    }
    resistors |>
      check_tibble(req_cols = c("species", "channel", "resistance.Ohm"))

    join_keys <- c("species", "channel")
    if ("config" %in% names(dataset) && "config" %in% names(resistors)) {
      join_keys <- c(join_keys, "config")
    }
    dataset <- dataset |>
      dplyr::left_join(
        resistors |>
          dplyr::select(dplyr::all_of(c(join_keys, "resistance.Ohm"))),
        by = join_keys
      )
  }

  # convert: src unit -> src base -> tgt base -> tgt unit
  # src$scale: base->src, so src->base = divide by src$scale
  # tgt$scale: base->tgt
  intensity_src_base <- dataset[[src_col]] / src$scale

  intensity_tgt_base <- switch(
    paste0(src$base, "_", tgt$base),
    "V_V" = intensity_src_base,
    "A_A" = intensity_src_base,
    "cps_cps" = intensity_src_base,
    "A_cps" = intensity_src_base / e,
    "cps_A" = intensity_src_base * e,
    "V_A" = intensity_src_base / dataset$resistance.Ohm,
    "A_V" = intensity_src_base * dataset$resistance.Ohm,
    "cps_V" = intensity_src_base * e * dataset$resistance.Ohm,
    "V_cps" = intensity_src_base / dataset$resistance.Ohm / e
  )

  tgt_col <- paste0("intensity.", units)
  drop_cols <- c(if (src_col != tgt_col) src_col, "resistance.Ohm")
  dataset |>
    dplyr::mutate(!!tgt_col := intensity_tgt_base * tgt$scale) |>
    dplyr::select(-dplyr::any_of(drop_cols))
}
