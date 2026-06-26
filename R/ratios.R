# ratio calculations ==========

#' Calculate isotope ratios
#'
#' Calculate intensity ratios of each mass relative to a base mass for every
#' measurement in an [ir_aggregate_isofiles()] result. Ratios are added directly
#' to the `traces` (continuous flow), `cycles` (dual inlet), and/or `scans` data
#' present in the aggregated data as two extra columns: `ratio_name` (e.g.
#' `"29/28"`) and `ratio`. The ratio at each `time.s`/`cycle`/`x` position
#' (within every `uidx` and `analysis`) is
#'
#' \deqn{ratio = (I_{mass} + num\_add) / (I_{base} + denom\_add)}
#'
#' i.e. the intensity of the mass divided by the intensity of the base mass of
#' the same species, after adding an additive offset to numerator and denominator
#' (see below). Base mass rows are kept and have `NA` in both columns. Calling
#' this function again recomputes (overwrites) the `ratio_name`/`ratio` columns.
#' The resulting ratios are not constrained in any way (they can be any value).
#'
#' The base mass for a species is, by default, the numerically lowest mass
#' measured for that species. Override it for individual species via `...` (e.g.
#' `SO2 = 64`, `N2 = 28`).
#'
#' # Additive offsets
#'
#' The additive offsets apply to continuous-flow (`traces`) and `scans` data
#' **only**. Dual inlet (`cycles`) data is **not** offset and always uses the
#' plain ratio `I_mass / I_base` regardless of the `num_add.*`/`denom_add.*`
#' settings.
#'
#' For traces and scans, which pair of additive offsets is used depends on the
#' intensity unit family of the data: voltage (`V`, `mV`) uses
#' `num_add.V`/`denom_add.V`, current (`A`, `mA`, `µA`, `nA`, `pA`, `fA`) uses
#' `num_add.nA`/`denom_add.nA`, and counts (`cps`) uses
#' `num_add.cps`/`denom_add.cps`. The offsets are specified in their family's
#' reference unit (volts, nanoamperes, cps) and are automatically scaled to the
#' data's actual intensity unit before being added. For example, with
#' `intensity.mV` data the default `num_add.V = 100` (volts) is multiplied by
#' 1000 and 100000 mV are added; with `intensity.pA` data the default
#' `num_add.nA = 0` would be multiplied by 1000 (1 nA = 1000 pA).
#'
#' # Normalization
#'
#' `normalize_ratios` is `NULL` by default (no normalization). Pass a function to
#' divide every ratio by the value that function returns for its
#' `uidx`/`analysis`/`ratio_name` group (the function receives the group's
#' non-`NA` ratios). For example `normalize_ratios = mean` centers each ratio
#' around 1, while `median`, `min`, or `max` normalize to the group median,
#' minimum, or maximum, respectively.
#'
#' @param aggregated_data datasets aggregated from [ir_aggregate_isofiles()]
#'   (must include at least one of `traces`, `cycles`, or `scans`)
#' @param ... named base masses for individual species, e.g. `SO2 = 64, N2 = 28`.
#'   Species not listed here use their numerically lowest measured mass as the
#'   base mass.
#' @param num_add.V,denom_add.V additive offset (in volts) for the numerator and
#'   denominator intensities of voltage-unit data (`V`/`mV`). Applies to `traces`
#'   and `scans` only, not to `cycles` (dual inlet). Default `num_add.V = 100`,
#'   `denom_add.V = num_add.V`.
#' @param num_add.nA,denom_add.nA additive offset (in nanoamperes) for the
#'   numerator and denominator intensities of current-unit data
#'   (`A`/`mA`/`µA`/`nA`/`pA`/`fA`). Applies to `traces` and `scans` only, not to
#'   `cycles` (dual inlet). Default `num_add.nA = 0`, `denom_add.nA = num_add.nA`.
#' @param num_add.cps,denom_add.cps additive offset (in cps) for the numerator and
#'   denominator intensities of count-unit data (`cps`). Applies to `traces` and
#'   `scans` only, not to `cycles` (dual inlet). Default `num_add.cps = 0`,
#'   `denom_add.cps = num_add.cps`.
#' @param normalize_ratios `NULL` (default) for no normalization, or a function
#'   (e.g. `mean`, `median`, `min`, `max`) applied per
#'   `uidx`/`analysis`/`ratio_name` group; each ratio is divided by the value the
#'   function returns for its group's non-`NA` ratios.
#' @return the `aggregated_data` with `ratio_name` and `ratio` columns added to
#'   each of the `traces`, `cycles`, and/or `scans` datasets that is present.
#'   Both columns are `NA` for base mass rows (and for any species whose
#'   requested base mass could not be found).
#' @export
ir_calculate_ratios <- function(
  aggregated_data,
  ...,
  num_add.V = 100,
  denom_add.V = num_add.V,
  num_add.nA = 0,
  denom_add.nA = num_add.nA,
  num_add.cps = 0,
  denom_add.cps = num_add.cps,
  normalize_ratios = NULL
) {
  # safety checks
  aggregated_data |>
    check_arg(
      !missing(aggregated_data) && is(aggregated_data, "ir_aggregated_data"),
      "must be a set of aggregated isofiles (use ir_aggregate_isofiles())"
    )
  check_arg(
    normalize_ratios,
    is.null(normalize_ratios) || is.function(normalize_ratios),
    "must be NULL or a function (e.g. mean, median, min, max)"
  )

  # validate the additive offset factors (each a single finite number)
  add_factors <- list(
    num_add.V = num_add.V,
    denom_add.V = denom_add.V,
    num_add.nA = num_add.nA,
    denom_add.nA = denom_add.nA,
    num_add.cps = num_add.cps,
    denom_add.cps = denom_add.cps
  )
  bad <- names(add_factors)[
    !purrr::map_lgl(
      add_factors,
      \(x) is.numeric(x) && length(x) == 1L && is.finite(x)
    )
  ]
  if (length(bad) > 0L) {
    cli_abort(
      "additive offset{?s} {.arg {bad}} must each be a single finite number"
    )
  }
  # offsets keyed by intensity family reference unit (V / nA / cps)
  num_add <- c(V = num_add.V, nA = num_add.nA, cps = num_add.cps)
  denom_add <- c(V = denom_add.V, nA = denom_add.nA, cps = denom_add.cps)

  # parse base mass overrides from ...
  base_masses <- rlang::list2(...)
  if (length(base_masses) > 0L) {
    if (!rlang::is_named(base_masses) || any(!nzchar(names(base_masses)))) {
      cli_abort(c(
        "base masses provided in {.arg ...} must be named by species",
        "i" = "e.g. {.code ir_calculate_ratios(data, SO2 = 64, N2 = 28)}"
      ))
    }
    ok <- purrr::map_lgl(base_masses, \(x) is.numeric(x) && length(x) == 1L)
    if (any(!ok)) {
      cli_abort(
        "each base mass in {.arg ...} must be a single number (problem with {.field {names(base_masses)[!ok]}})"
      )
    }
    base_masses <- purrr::map_dbl(base_masses, as.numeric)
  }

  start <- start_info("is running")

  # which data series can ratios be calculated from (present, non-empty, with an
  # intensity column)? each maps to its position column.
  series <- c(traces = "time.s", cycles = "cycle", scans = "x")
  present <- names(series)[purrr::map_lgl(names(series), function(nm) {
    is.data.frame(aggregated_data[[nm]]) &&
      nrow(aggregated_data[[nm]]) > 0L &&
      any(grepl("^intensity\\.", names(aggregated_data[[nm]])))
  })]
  if (length(present) == 0L) {
    cli_abort(c(
      "no {.field traces}, {.field cycles}, or {.field scans} data with intensities found to calculate ratios from",
      "i" = "aggregate with a suitable aggregator first (e.g. {.code ir_aggregate_isofiles(aggregator = \"standard\")})"
    ))
  }

  # add ratio columns to each present series. The additive offsets apply only to
  # continuous-flow (traces) and scan data; dual inlet (cycles) always uses plain
  # ratios, so the offsets are forced to 0 for cycles.
  no_add <- c(V = 0, nA = 0, cps = 0)
  all_species <- character(0)
  n_ratios <- 0L
  for (nm in present) {
    is_cycles <- nm == "cycles"
    res <- add_series_ratios(
      aggregated_data[[nm]],
      x_col = series[[nm]],
      base_masses = base_masses,
      group_extra = if (is_cycles) "type" else character(0),
      num_add = if (is_cycles) no_add else num_add,
      denom_add = if (is_cycles) no_add else denom_add,
      normalize = normalize_ratios
    )
    aggregated_data[[nm]] <- res$data
    all_species <- union(all_species, names(res$base))
    n_ratios <- n_ratios + sum(!is.na(res$data$ratio))
  }

  # warn about base masses specified for species not present in any series
  unused <- setdiff(names(base_masses), all_species)
  if (length(unused) > 0L) {
    cli_warn(
      "base mass{?es} {?was/were} specified for species not present in the data: {.field {unused}}"
    )
  }

  # modifying the datasets invalidates the recorded "not aggregated" column info,
  # so drop it everywhere (consistent with the metadata operations)
  aggregated_data <- drop_not_aggregated_info(aggregated_data)

  finish_info(
    "calculated {numbers_to_text(n_ratios)} ratio{?s} and added {.field ratio_name}/{.field ratio} columns to {.field {present}}",
    start = start
  )

  return(aggregated_data)
}

# normalize a group's ratios with `fn`: applies `fn` to the non-NA values and
# returns NA for an all-NA group (so the divide leaves it NA and avoids warnings
# from functions like min/max on empty input).
ratio_norm_value <- function(x, fn) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  fn(x)
}

# add `ratio_name` and `ratio` columns to a single data series (traces/cycles/
# scans). `x_col` is the series' position column (time.s/cycle/x); `group_extra`
# adds extra grouping columns (e.g. "type" for cycles, where standard/sample
# share cycle numbers). `num_add`/`denom_add` are the additive offsets keyed by
# intensity family reference unit (`V`/`nA`/`cps`); the matching one is scaled to
# the data's actual intensity unit and added to numerator/denominator. `normalize`
# is NULL or a function applied per uidx/analysis/ratio_name group. Base mass rows
# (and species with an unusable requested base mass) get NA in both columns; all
# rows are retained. Any pre-existing `ratio_name`/`ratio` columns are dropped
# first so the call is idempotent. Returns a list with `data` (the series with the
# two columns) and `base` (the named character vector of base masses per species).
add_series_ratios <- function(
  data,
  x_col,
  base_masses = double(),
  group_extra = character(0),
  num_add = c(V = 0, nA = 0, cps = 0),
  denom_add = c(V = 0, nA = 0, cps = 0),
  normalize = NULL
) {
  intensity_col <- grep("^intensity\\.", names(data), value = TRUE)[1]
  group_cols <- intersect(
    c("uidx", "analysis", group_extra, "species", x_col),
    names(data)
  )

  # scale the additive offsets from their family reference unit (V / nA / cps) to
  # the data's actual intensity unit. `intensity_unit_info()` gives base (V/A/cps)
  # and scale (base -> unit); the offset's value in the data unit is
  # offset_ref * scale(unit) / scale(reference unit).
  info <- intensity_unit_info(sub("^intensity\\.", "", intensity_col))
  ref_key <- switch(info$base, V = "V", A = "nA", cps = "cps")
  ref_scale <- intensity_unit_info(ref_key)$scale
  num_add_unit <- unname(num_add[ref_key]) * info$scale / ref_scale
  denom_add_unit <- unname(denom_add[ref_key]) * info$scale / ref_scale

  # drop any pre-existing ratio columns so re-running overwrites cleanly
  data <- dplyr::select(data, -dplyr::any_of(c("ratio_name", "ratio")))

  # determine the base mass (as the character value used in the `mass` column)
  # for each species: an override from `base_masses` if given, else the
  # numerically lowest measured mass
  species_list <- unique(data$species)
  base <- purrr::map_chr(species_list, function(sp) {
    sp_masses <- unique(data$mass[data$species == sp])
    if (sp %in% names(base_masses)) {
      hit <- sp_masses[as.numeric(sp_masses) == base_masses[[sp]]]
      if (length(hit) == 0L) {
        cli_warn(c(
          "the requested base mass {base_masses[[sp]]} for species {.field {sp}} was not found in the data - no ratios calculated for it",
          "i" = "available {qty(length(sp_masses))}mass{?es} for {.field {sp}}: {.field {sort(as.numeric(sp_masses))}}"
        ))
        return(NA_character_)
      }
      hit[1]
    } else {
      sp_masses[which.min(as.numeric(sp_masses))]
    }
  })
  names(base) <- species_list

  # base intensity for each group (uidx/analysis/[type]/species/position)
  data <- dplyr::mutate(data, .base_mass = base[.data$species])
  base_int <- data |>
    dplyr::filter(.data$mass == .data[[".base_mass"]]) |>
    dplyr::select(dplyr::all_of(c(group_cols, intensity_col)))
  names(base_int)[names(base_int) == intensity_col] <- ".base_intensity"
  base_int <- dplyr::distinct(base_int)

  # ratio = (intensity(mass) + num_add) / (intensity(base mass) + denom_add); NA
  # for the base mass itself (and where no base mass could be determined). All
  # rows are kept and the ratio is not constrained in any way.
  data <- data |>
    dplyr::left_join(base_int, by = group_cols) |>
    dplyr::mutate(
      # no ratio for the base mass row, or where no base mass was found
      .no_ratio = is.na(.data[[".base_mass"]]) |
        .data$mass == .data[[".base_mass"]],
      ratio_name = dplyr::if_else(
        .data$.no_ratio,
        NA_character_,
        sprintf("%s/%s", .data$mass, .data[[".base_mass"]])
      ),
      ratio = dplyr::if_else(
        .data$.no_ratio,
        NA_real_,
        (.data[[intensity_col]] + num_add_unit) /
          (.data$.base_intensity + denom_add_unit)
      )
    ) |>
    dplyr::select(-".base_mass", -".base_intensity", -".no_ratio")

  # optionally normalize each ratio by `normalize(group ratios)` within its
  # file/analysis/ratio_name group (base mass rows stay NA)
  if (!is.null(normalize)) {
    norm_cols <- intersect(c("uidx", "analysis", "ratio_name"), names(data))
    data <- dplyr::mutate(
      data,
      .by = dplyr::all_of(norm_cols),
      ratio = .data$ratio / ratio_norm_value(.data$ratio, normalize)
    )
  }

  data <- dplyr::relocate(
    data,
    "ratio_name",
    "ratio",
    .after = dplyr::all_of(intensity_col)
  )

  list(data = data, base = base)
}
