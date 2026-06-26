# visualization functions ==========

# internal: label formatter for scientific notation on linear intensity axes.
# Returns expression labels of the form 1.5 %.% 10^3. Zero is always shown as 0.
#' @importFrom stats na.omit
label_scientific <- function() {
  parser1 <- scales::label_scientific()
  parser2 <- scales::label_parse()
  function(x) {
    parsed_x <- parser1(x)
    out <- sub("e\\+?", " %.% 10^", parsed_x)
    out <- parser2(out)
    out[x == 0.0] <- 0
    return(out)
  }
}

#' Default isoreader2 plotting theme
#'
#' This theme is always applied by the plotting functions
#' ([ir_plot_continuous_flow()], [ir_plot_dual_inlet()], [ir_plot_scans()]).
#' To customize a plot, add a [ggplot2::theme()] on top of the returned plot,
#' e.g. `ir_plot_continuous_flow(...) + ggplot2::theme(text = element_text(size = 20))`.
#'
#' @param text_size base font size in points (default: `16`)
#' @return a `ggplot2` theme object
#' @export
ir_default_theme <- function(text_size = 16) {
  check_arg(
    text_size,
    is.numeric(text_size) && length(text_size) == 1L,
    "must be a single number"
  )
  ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank()
    )
}

# internal: add faceting to a plot based on the captured `facet` expression.
# A two-sided formula (e.g. `species ~ mass`) is faceted with
# [ggplot2::facet_grid()]; any other column/expression is faceted with
# [ggplot2::facet_wrap()]. `scales`, `nrow`, `ncol`, and `...` are forwarded to
# the facet function (`nrow`/`ncol` only apply to facet_wrap). A `NULL` facet
# adds no faceting. The referenced columns/expression are validated against
# `data` first.
add_facets <- function(
  p,
  facet_quo,
  data,
  scales,
  nrow = NULL,
  ncol = NULL,
  ...,
  geometry_set = FALSE,
  .env = caller_env()
) {
  if (rlang::quo_is_null(facet_quo)) {
    return(p)
  }
  facet_expr <- rlang::quo_get_expr(facet_quo)
  if (rlang::is_formula(facet_expr)) {
    # validate that the formula's variables exist in the data
    vars <- setdiff(all.vars(facet_expr), ".")
    missing <- setdiff(vars, names(data))
    if (length(missing) > 0) {
      cli_abort(
        c(
          "{.field facet} formula {.emph {rlang::as_label(facet_quo)}} references unknown column{?s}: {.field {missing}}",
          "i" = "available columns: {.field {names(data)}}"
        ),
        call = .env
      )
    }
    # only warn if the user explicitly set nrow/ncol (not the defaults)
    if (geometry_set && (!is.null(nrow) || !is.null(ncol))) {
      cli_warn(
        c(
          "!" = "{.arg nrow}/{.arg ncol} only apply when faceting a single variable or expression ({.fn facet_wrap}) and are ignored for the formula facet {.emph {rlang::as_label(facet_quo)}} ({.fn facet_grid})"
        ),
        call = .env
      )
    }
    return(
      p +
        ggplot2::facet_grid(rlang::eval_tidy(facet_quo), scales = scales, ...)
    )
  }
  # non-formula: validate as a column expression and facet_wrap
  check_aes_expr(facet_quo, "facet", data, .env = .env)
  p +
    ggplot2::facet_wrap(
      ggplot2::vars(!!facet_quo),
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      ...
    )
}

#' Automatic / default behavior
#'
#' A sentinel that requests automatic behavior for an argument (currently the
#' `data_type_as_facet` argument of the plotting functions
#' [ir_plot_continuous_flow()], [ir_plot_dual_inlet()], [ir_plot_scans()]). It is
#' the default for those arguments; pass `TRUE`/`FALSE` to override the automatic
#' choice.
#'
#' @return an opaque sentinel of class `ir_auto`
#' @export
auto <- function() {
  structure(list(), class = "ir_auto")
}

# internal: TRUE when x is the auto() sentinel
is_auto <- function(x) inherits(x, "ir_auto")

# internal: decide whether the `data_type` column should become the row variable
# of a facet_grid. `data_type_as_facet` is auto() (decide from the data: TRUE when
# more than one data_type is present, e.g. intensities and ratios), TRUE (force),
# or FALSE (never). When `facet` is a two-sided formula it fully specifies the
# facet, so data_type is never used; if the user explicitly set
# data_type_as_facet = TRUE alongside a formula facet, a warning is emitted
# (the two are mutually exclusive and the formula wins).
use_data_type_facet <- function(
  facet_quo,
  data_type_as_facet,
  data,
  .env = caller_env()
) {
  if (rlang::is_formula(rlang::quo_get_expr(facet_quo))) {
    if (isTRUE(data_type_as_facet)) {
      cli_warn(
        c(
          "{.arg data_type_as_facet = TRUE} is ignored because {.arg facet} is a formula",
          "i" = "specifying both is mutually exclusive; the formula facet takes precedence"
        ),
        call = .env
      )
    }
    return(FALSE)
  }
  if (is_auto(data_type_as_facet)) {
    return(
      "data_type" %in% names(data) && dplyr::n_distinct(data$data_type) > 1L
    )
  }
  isTRUE(data_type_as_facet)
}

# internal: add faceting, using `data_type` as the facet_grid row variable when
# `use_data_type` is TRUE - facet_grid(data_type ~ .) for a NULL facet, or
# facet_grid(data_type ~ <facet>) for a single-variable facet. Otherwise delegate
# to add_facets() (formula -> facet_grid, single var -> facet_wrap, NULL -> none).
add_data_type_or_facets <- function(
  p,
  facet_quo,
  use_data_type,
  data,
  scales,
  nrow = NULL,
  ncol = NULL,
  ...,
  geometry_set = FALSE,
  .env = caller_env()
) {
  if (!use_data_type) {
    return(add_facets(
      p,
      facet_quo,
      data,
      scales,
      nrow,
      ncol,
      ...,
      geometry_set = geometry_set,
      .env = .env
    ))
  }
  if (geometry_set && (!is.null(nrow) || !is.null(ncol))) {
    cli_warn(
      c(
        "!" = "{.arg nrow}/{.arg ncol} only apply to {.fn facet_wrap} and are ignored when {.field data_type} is used as a {.fn facet_grid} row"
      ),
      call = .env
    )
  }
  # `switch = "y"` moves the data_type row strips to the left of the plot; the
  # matching `strip.placement = "outside"` (added after ir_default_theme() in the
  # plotting functions, since that complete theme would otherwise reset it) puts
  # them outside the y axis, right next to the per-row y-axis values, so the strip
  # reads as that row's y-axis label.
  if (rlang::quo_is_null(facet_quo)) {
    return(
      p +
        ggplot2::facet_grid(
          rows = ggplot2::vars(data_type),
          scales = scales,
          switch = "y",
          ...
        )
    )
  }
  check_aes_expr(facet_quo, "facet", data, .env = .env)
  p +
    ggplot2::facet_grid(
      rows = ggplot2::vars(data_type),
      cols = ggplot2::vars(!!facet_quo),
      scales = scales,
      switch = "y",
      ...
    )
}

# internal: if a `trace` column is present and not already a factor, convert it
# to a factor with levels sorted by species and then by the numerical (numerator)
# mass number of the trace label. A trace label is either "<species>: <mass>"
# (e.g. "CO2: 44") or, for a ratio, "<species>: <numerator>/<denominator>" (e.g.
# "N2: 29/28"); both are keyed by their leading mass number (44, 29) so an
# intensity trace and its ratios sort next to each other, with the intensity
# trace before its ratios. Traces without a parseable number sort last.
sort_trace_factor <- function(plot_data) {
  if ("trace" %in% names(plot_data) && !is.factor(plot_data$trace)) {
    trace_levels <- unique(as.character(plot_data$trace))
    # split each label into species, leading mass number, and ratio flag
    species_part <- sub(":.*$", "", trace_levels)
    mass_part <- sub("^[^:]*:\\s*", "", trace_levels)
    # coercion NAs (labels without a number) are expected, so silence them
    mass_number <- suppressWarnings(as.numeric(sub("/.*$", "", mass_part)))
    is_ratio <- grepl("/", trace_levels)
    trace_levels <- trace_levels[order(
      species_part,
      mass_number,
      is_ratio,
      trace_levels,
      na.last = TRUE
    )]
    plot_data <- dplyr::mutate(
      plot_data,
      trace = factor(.data$trace, levels = trace_levels)
    )
  }
  return(plot_data)
}

# internal: build a named colour vector (trace level -> colour) so that traces
# sharing the same species + (numerator) mass get the same colour - e.g. the
# intensity trace "N2: 29" and the ratio trace "N2: 29/28" (whose numerator mass
# is 29, carried in the `mass` column). Each distinct species/mass key is given
# one colour, drawn from the supplied `color_values` palette when it has enough
# entries, otherwise from an evenly spaced hue palette. Returns `color_values`
# unchanged if the data lacks the columns needed to key by (it can then be used
# as a plain ordered palette).
build_trace_color_values <- function(plot_data, color_values) {
  if (!all(c("trace", "species", "mass") %in% names(plot_data))) {
    return(color_values)
  }
  trace_levels <- if (is.factor(plot_data$trace)) {
    levels(plot_data$trace)
  } else {
    unique(as.character(plot_data$trace))
  }
  # distinct trace -> (species, mass) colour key, as present in the data
  key_map <- plot_data |>
    dplyr::distinct(.data$trace, .data$species, .data$mass) |>
    dplyr::mutate(
      .trace = as.character(.data$trace),
      .key = paste(.data$species, .data$mass)
    )
  trace_to_key <- stats::setNames(key_map$.key, key_map$.trace)
  # restrict to (and order by) the levels actually present in the data
  trace_levels <- trace_levels[trace_levels %in% names(trace_to_key)]
  keys_in_order <- unique(trace_to_key[trace_levels])
  n_keys <- length(keys_in_order)
  if (n_keys == 0) {
    return(color_values)
  }
  # one colour per key: use the supplied palette if it is large enough, else hues
  key_colors <- if (!is.null(color_values) && length(color_values) >= n_keys) {
    unname(color_values)[seq_len(n_keys)]
  } else {
    scales::hue_pal()(n_keys)
  }
  key_colors <- stats::setNames(key_colors, keys_in_order)
  # named vector: trace level -> its key's colour
  stats::setNames(unname(key_colors[trace_to_key[trace_levels]]), trace_levels)
}

# internal: add the colour aesthetic and a matching colour scale. When the
# colour column is a factor (e.g. `mass` or `trace`), unused levels are kept by
# default (`drop_unused_levels = FALSE`) so that the colour mapping stays stable
# when the plotted data is a subset of the full dataset (e.g. zoomed to a window)
# instead of re-coloring the remaining groups; pass `drop_unused_levels = TRUE`
# to show only the levels actually present. The manual palette is only used when
# it supplies enough colours for the levels being shown; otherwise the default
# discrete scale (which generates as many hues as needed) is used.
add_color_aes <- function(
  p,
  color_quo,
  color_values,
  plot_data,
  drop_unused_levels = FALSE
) {
  if (rlang::quo_is_null(color_quo)) {
    return(p)
  }
  p <- p + ggplot2::aes(color = !!color_quo)
  color_vals <- rlang::eval_tidy(color_quo, plot_data)
  is_factor <- is.factor(color_vals)
  # the palette must cover either every factor level (drop_unused_levels = FALSE,
  # the default, which keeps the colour mapping stable across subsets of the same
  # dataset) or only the levels actually present (drop_unused_levels = TRUE)
  n_colors <- if (is_factor && !drop_unused_levels) {
    nlevels(color_vals)
  } else {
    dplyr::n_distinct(color_vals)
  }
  if (!is.null(color_values) && length(color_values) >= n_colors) {
    p <- p +
      scale_color_manual(
        values = color_values,
        drop = if (is_factor) drop_unused_levels else TRUE
      )
  } else if (is_factor) {
    p <- p + ggplot2::scale_color_discrete(drop = drop_unused_levels)
  }
  return(p)
}

# internal: subset `plot_data` to a display `window` (length-2 c(min, max)) along
# column `col`, additionally keeping, per line (grouped by `group_cols`), the
# single data point just below and just above the window. Those bracketing points
# let the clipped lines interpolate correctly across the window edges. Because
# they are found relative to the window bounds (not to the in-window points), a
# window that contains no data points of its own is fully supported - the line is
# simply drawn between the bracketing points on either side. Assumes `window`
# is already validated as min < max.
apply_plot_window <- function(plot_data, col, window, group_cols) {
  group_cols <- intersect(group_cols, names(plot_data))
  plot_data |>
    dplyr::arrange(!!!rlang::syms(c(group_cols, col))) |>
    dplyr::mutate(
      .by = dplyr::all_of(group_cols),
      # inside the window
      .window_keep = (.data[[col]] >= window[1] & .data[[col]] <= window[2]) |
        # nearest point just below the window (left edge interpolation)
        (.data[[col]] < window[1] &
          .data[[col]] ==
            suppressWarnings(max(
              .data[[col]][.data[[col]] < window[1]],
              na.rm = TRUE
            ))) |
        # nearest point just above the window (right edge interpolation)
        (.data[[col]] > window[2] &
          .data[[col]] ==
            suppressWarnings(min(
              .data[[col]][.data[[col]] > window[2]],
              na.rm = TRUE
            )))
    ) |>
    dplyr::filter(.data[[".window_keep"]]) |>
    dplyr::select(-".window_keep")
}

# internal: when `drop_unused_levels` is TRUE, restrict the data to the factor
# levels that are actually visible so they disappear from scales/legends. With a
# `window` set this also drops the line groups (`group_cols`, i.e. mass/trace
# combinations) that fall entirely outside it: apply_plot_window() keeps a single
# bracketing point just outside the window for line continuity, which otherwise
# keeps an off-window trace "present" (one row) and thus in the legend. A group
# counts as visible if it has a data point inside the window or points bracketing
# it on both sides (a line that crosses the window). Then droplevels() removes the
# now-unused mass/trace levels. With drop_unused_levels = FALSE the data is
# returned unchanged (all levels kept for a stable colour mapping).
apply_drop_unused_levels <- function(
  plot_data,
  drop_unused_levels,
  x_col,
  window,
  group_cols
) {
  if (!drop_unused_levels) {
    return(plot_data)
  }
  if (!is.null(window)) {
    group_cols <- intersect(group_cols, names(plot_data))
    plot_data <- dplyr::filter(
      plot_data,
      .by = dplyr::all_of(group_cols),
      any(
        .data[[x_col]] >= window[1] & .data[[x_col]] <= window[2],
        na.rm = TRUE
      ) |
        (any(.data[[x_col]] < window[1], na.rm = TRUE) &
          any(.data[[x_col]] > window[2], na.rm = TRUE))
    )
  }
  droplevels(plot_data)
}

# internal: keep rows of `plot_data` whose `col` matches `values` (compared as
# character, so numeric/character work interchangeably). Errors informatively
# (listing what is available) if nothing matches.
filter_to_values <- function(plot_data, col, values, .env = caller_env()) {
  if (!col %in% names(plot_data)) {
    cli_abort(
      "cannot filter by {.field {col}}: there is no {.field {col}} column in the data",
      call = .env
    )
  }
  keep <- as.character(plot_data[[col]]) %in% as.character(values)
  if (!any(keep)) {
    cli_abort(
      c(
        "no data left after filtering {.field {col}} to {.val {values}}",
        "i" = "available {col}: {.val {unique(as.character(plot_data[[col]]))}}"
      ),
      call = .env
    )
  }
  plot_data[keep, ]
}

# internal: filter plot data to the requested `species` and/or `mass` values.
# A `NULL` selection keeps everything; a vector filters (erroring if it matches
# nothing).
filter_plot_data <- function(plot_data, species, mass, .env = caller_env()) {
  if (!is.null(species)) {
    plot_data <- filter_to_values(plot_data, "species", species, .env = .env)
  }
  if (!is.null(mass)) {
    plot_data <- filter_to_values(plot_data, "mass", mass, .env = .env)
  }
  plot_data
}

# internal: interpret a NULL/empty/values selection argument: `NULL` -> "all"
# (no filtering), a zero-length but non-NULL vector (e.g. `character(0)`) ->
# "none", anything else -> "some" (those specific values). Note that `c()` is
# `NULL` in R and therefore maps to "all"; use `character(0)` / `numeric(0)` to
# select none.
selection_mode <- function(x) {
  if (is.null(x)) {
    "all"
  } else if (length(x) == 0L) {
    "none"
  } else {
    "some"
  }
}

# internal: turn the `dataset` argument (an `ir_aggregated_data` object or a
# plain data frame) into the flat data frame the tibble/plotting functions work
# on. For an `ir_aggregated_data` the named `dataset_name` ("traces"/"cycles"/
# "scans") must be present and non-empty; it is inner-joined with `$metadata`
# (bringing in every metadata column not already present) by uidx/analysis. A
# plain data frame is returned unchanged. Errors if the dataset is a raw
# isofiles object, is not a data frame/aggregated data, or is unavailable/empty.
prepare_plot_data <- function(dataset, dataset_name, .env = caller_env()) {
  if (!missing(dataset) && is(dataset, "ir_isofiles")) {
    cli_abort(
      c(
        "{.arg dataset} is a raw isofiles object and cannot be plotted directly",
        "i" = "aggregate it first with {.code ir_aggregate_isofiles(dataset)}"
      ),
      call = .env
    )
  }
  check_arg(
    dataset,
    !missing(dataset) &&
      (is.data.frame(dataset) || is(dataset, "ir_aggregated_data")),
    "must be a data frame or a set of aggregated isofiles",
    .arg = "dataset",
    .env = .env
  )

  if (is(dataset, "ir_aggregated_data")) {
    if (
      !dataset_name %in% names(dataset) ||
        ncol(dataset[[dataset_name]]) == 0 ||
        nrow(dataset[[dataset_name]]) == 0
    ) {
      cli_abort(
        c(
          "no {.field {dataset_name}} available in the provided {.field dataset}",
          "i" = "make sure you are reading the matching isofiles and the aggregator includes columns from {.field {dataset_name}}"
        ),
        call = .env
      )
    }
    meta_extra_cols <- setdiff(
      names(dataset$metadata),
      names(dataset[[dataset_name]])
    )
    plot_data <- dplyr::inner_join(
      dataset[[dataset_name]],
      dplyr::select(
        dataset$metadata,
        dplyr::any_of(c("uidx", "analysis", meta_extra_cols))
      ),
      by = c("uidx", "analysis")
    )
  } else {
    plot_data <- dataset
  }

  if (nrow(plot_data) == 0) {
    cli_abort("no data to plot (0 rows)", call = .env)
  }
  plot_data
}

# internal: shared core for ir_generate_traces/cycles/scans_tibble(). Prepares
# the flat data (join + validation), filters by species, detects the intensity
# column, then builds the plotting tibble of intensity rows and (optionally)
# ratio rows:
#  - intensity rows carry `trace` = "<species>: <mass>" (always regenerated),
#    `data_type` = "intensity [UNITS]", and `value` = the intensity. They are
#    selected by `mass`: NULL = all masses, character(0)/numeric(0) = none, a
#    vector = those masses.
#  - ratio rows carry `trace` = "<species>: <ratio_name>", `data_type` =
#    "ratios", and `value` = the ratio. They are selected by `ratio`: NULL = all
#    available ratios, character(0) = none, a vector = those ratio names.
#    Requesting specific ratios when the `ratio_name`/`ratio` columns are absent
#    errors (pointing to ir_calculate_ratios()); with `ratio = NULL` and no ratio
#    columns, no ratios are simply added.
# Finally `trace` is turned into a sorted factor.
generate_plot_tibble <- function(
  dataset,
  dataset_name,
  required_cols,
  species = NULL,
  mass = NULL,
  ratio = NULL,
  .env = caller_env()
) {
  # validate filters
  check_arg(
    species,
    is.null(species) || is.character(species) || is.numeric(species),
    "must be NULL or a character/numeric vector",
    .env = .env
  )
  check_arg(
    mass,
    is.null(mass) || is.character(mass) || is.numeric(mass),
    "must be NULL or a character/numeric vector",
    .env = .env
  )
  check_arg(
    ratio,
    is.null(ratio) || is.character(ratio),
    "must be NULL or a character vector of ratio names (e.g. c(\"45/44\"))",
    .env = .env
  )

  # join metadata / validate the dataset
  plot_data <- prepare_plot_data(dataset, dataset_name, .env = .env)

  # required columns
  missing_cols <- setdiff(required_cols, names(plot_data))
  if (length(missing_cols) > 0) {
    cli_abort(
      c(
        "{dataset_name} data is missing required {qty(length(missing_cols))}column{?s}: {.field {missing_cols}}",
        "i" = "make sure the aggregator includes {.field {required_cols}}"
      ),
      call = .env
    )
  }

  # species filter applies to both intensities and ratios
  plot_data <- filter_plot_data(plot_data, species, NULL, .env = .env)

  # detect the intensity column -> units
  intensity_cols <- grep("^intensity\\.", names(plot_data), value = TRUE)
  if (length(intensity_cols) == 0) {
    cli_abort(
      c(
        "no intensity column found in {dataset_name} data",
        "i" = "expected a column whose name matches {.code intensity.*}"
      ),
      call = .env
    )
  }
  intensity_col <- intensity_cols[1]
  intensity_units <- sub("^intensity\\.", "", intensity_col)

  # Prepare the intensity (mass) data and the ratio data SEPARATELY and combine
  # them with bind_rows BEFORE applying the mass/ratio selections, so that a ratio
  # (e.g. "45/44") still plots when its numerator mass (45) is not in the `mass`
  # selection - the mass filter only ever drops intensity rows, never ratio rows.
  mass_mode <- selection_mode(mass)
  ratio_mode <- selection_mode(ratio)
  has_ratio_cols <- all(c("ratio_name", "ratio") %in% names(plot_data))
  # an explicit ratio request needs the ratio columns; `ratio = NULL` just adds
  # no ratios when they are absent
  if (ratio_mode == "some" && !has_ratio_cols) {
    cli_abort(
      c(
        "cannot include ratios: the {.field ratio_name}/{.field ratio} columns are not present in the {dataset_name} data",
        "i" = "calculate ratios first with {.code ir_calculate_ratios()}"
      ),
      call = .env
    )
  }

  # intensity rows for every mass: (re)generate trace/data_type/value from scratch
  # so `trace` is always "<species>: <mass>"
  intensity_rows <- dplyr::mutate(
    plot_data,
    trace = sprintf("%s: %s", .data$species, .data$mass),
    data_type = paste0("intensity [", intensity_units, "]"),
    value = .data[[intensity_col]]
  )
  # ratio rows for every available ratio (empty when no ratios were calculated)
  ratio_rows <- if (has_ratio_cols) {
    plot_data |>
      dplyr::filter(!is.na(.data$ratio_name)) |>
      dplyr::mutate(
        trace = sprintf("%s: %s", .data$species, .data$ratio_name),
        data_type = "ratios",
        value = .data$ratio
      )
  } else {
    intensity_rows[0, , drop = FALSE]
  }

  # combine BEFORE filtering, then select intensity rows by `mass` and ratio rows
  # by `ratio` (NULL = all of that type, character(0) = none, a vector = those)
  plot_data <- dplyr::bind_rows(intensity_rows, ratio_rows)
  is_ratio <- plot_data$data_type == "ratios"

  keep_mass <- switch(
    mass_mode,
    all = !is_ratio,
    none = rep(FALSE, nrow(plot_data)),
    some = !is_ratio & as.character(plot_data$mass) %in% as.character(mass)
  )
  if (mass_mode == "some" && !any(keep_mass)) {
    available <- unique(as.character(plot_data$mass[!is_ratio]))
    cli_abort(
      c(
        "no data left after filtering {.field mass} to {.val {mass}}",
        "i" = "available mass: {.val {available}}"
      ),
      call = .env
    )
  }

  keep_ratio <- switch(
    ratio_mode,
    all = is_ratio,
    none = rep(FALSE, nrow(plot_data)),
    some = is_ratio & plot_data$ratio_name %in% ratio
  )
  if (ratio_mode == "some" && !any(keep_ratio)) {
    available <- sort(unique(plot_data$ratio_name[is_ratio]))
    cli_abort(
      c(
        "no data left after filtering ratios to {.val {ratio}}",
        "i" = "available ratio{?s}: {.val {available}}"
      ),
      call = .env
    )
  }

  plot_data <- plot_data[keep_mass | keep_ratio, ]
  if (nrow(plot_data) == 0) {
    cli_abort(
      c(
        "no data to plot: the {.arg mass}/{.arg ratio} selection left no rows",
        "i" = "use {.code mass = NULL} for all masses and/or {.code ratio = NULL} for all ratios"
      ),
      call = .env
    )
  }

  # turn the trace identifier into a sorted factor (species + numerator mass)
  sort_trace_factor(plot_data)
}

#' Generate the tibble used by the plotting functions
#'
#' These helpers build the exact flat tibble that [ir_plot_continuous_flow()]
#' (`ir_generate_traces_tibble()`), [ir_plot_dual_inlet()]
#' (`ir_generate_cycles_tibble()`), and [ir_plot_scans()]
#' (`ir_generate_scans_tibble()`) plot, so it can be inspected or used
#' independently of producing a plot. The `dataset` is prepared exactly as for
#' the plotting functions (an `ir_aggregated_data` object has its `traces` /
#' `cycles` / `scans` dataset inner-joined with `$metadata`; a plain data frame
#' is used as is), filtered by `species`, and then split into intensity rows and
#' (optionally) ratio rows, each augmented with three columns:
#' \itemize{
#'   \item `trace` - the identifier `"<species>: <mass>"` for intensity rows
#'     (e.g. `"CO2: 44"`) or `"<species>: <ratio_name>"` for ratio rows (e.g.
#'     `"CO2: 45/44"`), always (re)generated and returned as a factor sorted by
#'     species and numerical (numerator) mass.
#'   \item `data_type` - `"intensity [UNITS]"` (e.g. `"intensity [mV]"`) for the
#'     intensity rows, or `"ratios"` for ratio rows.
#'   \item `value` - the value to plot: the intensity for intensity rows, or the
#'     (optionally fold-clamped) ratio for ratio rows.
#' }
#'
#' @param dataset an `ir_aggregated_data` object from [ir_aggregate_isofiles()]
#'   or a plain data frame with the required columns (see the matching plotting
#'   function)
#' @param species optional vector to filter to specific species (e.g. `"CO2"` or
#'   `c("N2", "CO2")`); default `NULL` keeps all species.
#' @param mass which masses to include as intensity traces: `NULL` (default) for
#'   all masses, a vector (e.g. `44` or `c(44, 45)`) for specific masses, or a
#'   zero-length vector (`numeric(0)`/`character(0)`) for none. Note that `c()`
#'   is `NULL` in R, i.e. all masses.
#' @param ratio which ratios to include (computed with [ir_calculate_ratios()]):
#'   `NULL` (default) for all available ratios, a character vector of ratio names
#'   (e.g. `c("45/44", "46/44")`) for specific ones, or `character(0)` for none.
#'   Requesting specific ratio names when ratios have not been calculated is an
#'   error pointing to [ir_calculate_ratios()]; with `ratio = NULL` and no ratios
#'   present none are simply added.
#' @return a tibble with the prepared data plus the `trace`, `data_type`, and
#'   `value` columns described above.
#' @name ir_generate_tibble
#' @export
ir_generate_traces_tibble <- function(
  dataset,
  species = NULL,
  mass = NULL,
  ratio = NULL
) {
  generate_plot_tibble(
    dataset,
    "traces",
    required_cols = c("species", "time.s", "mass"),
    species = species,
    mass = mass,
    ratio = ratio
  )
}

#' @rdname ir_generate_tibble
#' @export
ir_generate_cycles_tibble <- function(
  dataset,
  species = NULL,
  mass = NULL,
  ratio = NULL
) {
  generate_plot_tibble(
    dataset,
    "cycles",
    required_cols = c("species", "cycle", "type", "mass"),
    species = species,
    mass = mass,
    ratio = ratio
  )
}

#' @rdname ir_generate_tibble
#' @export
ir_generate_scans_tibble <- function(
  dataset,
  species = NULL,
  mass = NULL,
  ratio = NULL
) {
  generate_plot_tibble(
    dataset,
    "scans",
    required_cols = c("species", "scan_type", "x_units", "x", "mass"),
    species = species,
    mass = mass,
    ratio = ratio
  )
}

#' Plot scan data
#'
#' Plots scan data from an [ir_aggregate_isofiles()] result or a plain data
#' frame. The data is prepared with [ir_generate_scans_tibble()] (which, for an
#' `ir_aggregated_data` object, inner-joins the `$scans` dataset with
#' `$metadata`). The plot data must contain `species`, `x`, `scan_type`,
#' `x_units`, `mass`, and an `intensity.*` column — an error is thrown if any are
#' missing. A `trace` identifier (`"<species>: <mass>"`) is always regenerated
#' and the plotted `value` together with a `data_type` label (`"intensity
#' [UNITS]"`, or `"ratios"` for ratio rows) are added. `scan_type` and `x_units`
#' are combined for the x axis label.
#'
#' @param dataset an `ir_aggregated_data` object from [ir_aggregate_isofiles()]
#'   or a plain data frame with `species`, `x`, `scan_type`, `x_units`, `mass`,
#'   and an `intensity.*` column
#' @param scan_type which scan type to plot (e.g. `"high voltage"`). Required
#'   when the data contains more than one scan type; an error lists the
#'   available types. If the data contains only one scan type, the parameter
#'   must either be `NULL` or match that type exactly.
#' @param species optional vector to filter the displayed data to specific
#'   species (e.g. `"CO2"` or `c("N2", "CO2")`); default `NULL` shows all species.
#' @param mass which masses to include as intensity traces: `NULL` (default)
#'   shows all masses, a vector (e.g. `44` or `c(44, 45)`) shows specific masses,
#'   and a zero-length vector (`numeric(0)`/`character(0)`) shows none. Note that
#'   `c()` is `NULL` in R (i.e. all masses).
#' @param ratio which ratios to additionally include (computed with
#'   [ir_calculate_ratios()]): `NULL` (default) shows all available ratios, a
#'   character vector of ratio names (e.g. `c("45/44", "46/44")`) shows specific
#'   ones, and `character(0)` shows none. Requesting specific ratio names when
#'   ratios have not been calculated is an error pointing to
#'   [ir_calculate_ratios()] (with `ratio = NULL` and no ratios present, none are
#'   simply added). Ratio rows are plotted on the same `value` axis with
#'   `data_type = "ratios"`; the default `facet = data_type` (with free scales)
#'   separates them from the intensities.
#' @param facet column or expression to facet by (default: `NULL`, no extra
#'   faceting). When
#'   `data_type` is used as a facet row (see `data_type_as_facet`), a single
#'   `facet` variable becomes the facet_grid column (`data_type ~ facet`) and a
#'   `NULL` facet gives `data_type ~ .`. Otherwise a plain column or expression
#'   (e.g. `file_name` or `paste(species, mass)`) is faceted with
#'   [ggplot2::facet_wrap()], and a two-sided formula (e.g. `species ~ mass`) is
#'   faceted with [ggplot2::facet_grid()]. Set to `NULL` to suppress faceting.
#' @param data_type_as_facet whether the `data_type` column (intensities vs
#'   ratios) is used as the [ggplot2::facet_grid()] row variable: `auto()`
#'   (default) uses it only when more than one data type is present; `TRUE`
#'   always uses it; `FALSE` never does. When used, the y axis label is dropped
#'   (the facet strip provides it) and the facet becomes `data_type ~ .` (a `NULL`
#'   `facet`) or `data_type ~ facet` (a single-variable `facet`). It is ignored
#'   when `facet` is a two-sided formula (a warning is issued if
#'   `data_type_as_facet = TRUE` is combined with a formula `facet`, since the two
#'   are mutually exclusive).
#' @param scales whether facet scales should be `"free"` (default), `"fixed"`,
#'   `"free_x"`, or `"free_y"`; passed on to [ggplot2::facet_wrap()] /
#'   [ggplot2::facet_grid()].
#' @param nrow,ncol number of rows and columns of facet panels (`nrow` default
#'   `NULL` lets ggplot2 choose; `ncol` default `1` stacks the panels in a
#'   single column). Only applies when `facet` is a single variable or
#'   expression (faceted with [ggplot2::facet_wrap()]); ignored when `facet` is a
#'   formula (faceted with [ggplot2::facet_grid()]), with a warning if you set
#'   them explicitly.
#' @param color column or expression for the colour aesthetic (default:
#'   `trace`, the per-species/mass trace identifier, e.g. `"CO2: 44"`). When
#'   colouring by `trace`, traces that share the same species and (numerator)
#'   mass are given the same colour, so an intensity trace (`"N2: 29"`) and its
#'   ratio traces (`"N2: 29/28"`) match.
#' @param linetype column or expression for the linetype aesthetic (default:
#'   `NULL`, i.e. no linetype aesthetic)
#' @param color_values named or unnamed character vector of colours passed to
#'   [ggplot2::scale_color_manual()], or `NULL` to use the ggplot2 default
#'   colour palette (default: [palette.colors()])
#' @param drop_unused_levels whether to drop unused `trace` factor levels (e.g.
#'   traces that are absent after zooming to a window) from the colour scale and
#'   legend. Default `FALSE` keeps every level so the colour mapping stays stable
#'   across subsets of the same dataset; set to `TRUE` to show only the levels
#'   actually present in the plotted data.
#' @param scientific whether to format y axis labels in scientific notation
#'   (default: `FALSE`)
#' @param ... additional arguments passed on to [ggplot2::facet_wrap()] or
#'   [ggplot2::facet_grid()] (e.g. `labeller`)
#' @param x_window optional numeric vector of length 2 giving the x axis
#'   display window `c(min, max)` (must have `min < max`). The data point just
#'   outside each edge of the window is retained so the clipped lines interpolate
#'   correctly across the window boundaries and y autoscales correctly at the
#'   edges; [ggplot2::coord_cartesian()] clips the display. A window that contains
#'   no data points of its own is allowed (the line is drawn between the
#'   bracketing points). Default `NULL` shows the full x range.
#' @param n_x_breaks desired number of x axis tick marks (default: `5`)
#' @param n_y_breaks desired number of y axis tick marks (default: `5`)
#' @return a `ggplot` object with [ir_default_theme()] applied. To customize the
#'   plot, add ggplot2 layers on top (e.g. `+ ggplot2::theme(...)` or
#'   `+ ggplot2::labs(...)`); attach ggplot2 with `library(ggplot2)` first.
#' @export
ir_plot_scans <- function(
  dataset,
  scan_type = NULL,
  species = NULL,
  mass = NULL,
  ratio = NULL,
  facet = NULL,
  data_type_as_facet = auto(),
  scales = "free",
  nrow = NULL,
  ncol = 1,
  color = trace,
  linetype = NULL,
  color_values = palette.colors(),
  drop_unused_levels = FALSE,
  scientific = FALSE,
  x_window = NULL,
  n_x_breaks = 5,
  n_y_breaks = 5,
  ...
) {
  # plot-specific argument checks (dataset / species / mass / ratio are checked
  # by ir_generate_scans_tibble() below)
  check_arg(
    scan_type,
    is.null(scan_type) || rlang::is_scalar_character(scan_type),
    "must be NULL or a single string"
  )
  check_arg(
    color_values,
    is.null(color_values) || is.character(color_values),
    "must be NULL or a character vector of colours"
  )
  check_arg(scientific, rlang::is_bool(scientific), "must be TRUE or FALSE")
  check_arg(
    drop_unused_levels,
    rlang::is_bool(drop_unused_levels),
    "must be TRUE or FALSE"
  )
  check_arg(
    x_window,
    is.null(x_window) ||
      (is.numeric(x_window) &&
        length(x_window) == 2 &&
        x_window[1] < x_window[2]),
    "must be NULL or a numeric vector of length 2 with min < max"
  )
  check_arg(
    n_x_breaks,
    rlang::is_scalar_integerish(n_x_breaks) && n_x_breaks > 0,
    "must be a positive whole number"
  )
  check_arg(
    n_y_breaks,
    rlang::is_scalar_integerish(n_y_breaks) && n_y_breaks > 0,
    "must be a positive whole number"
  )
  check_arg(scales, rlang::is_scalar_character(scales), "must be a string")
  check_arg(
    data_type_as_facet,
    is_auto(data_type_as_facet) || rlang::is_bool(data_type_as_facet),
    "must be auto(), TRUE, or FALSE"
  )

  # capture aesthetics before any data manipulation
  facet_quo <- rlang::enquo(facet)
  color_quo <- rlang::enquo(color)
  linetype_quo <- rlang::enquo(linetype)

  # build the plotting tibble (join, filter, trace/data_type/value, ratios)
  plot_data <- ir_generate_scans_tibble(
    dataset,
    species = species,
    mass = mass,
    ratio = ratio
  )

  # enforce a single scan type
  available_scan_types <- unique(plot_data$scan_type)
  if (is.null(scan_type)) {
    if (length(available_scan_types) > 1) {
      cli_abort(
        c(
          "data contains {length(available_scan_types)} scan types: {.field {available_scan_types}}",
          "i" = "use {.arg scan_type} to select one, e.g. {.code scan_type = \"{available_scan_types[1]}\"}"
        )
      )
    }
  } else {
    if (!scan_type %in% available_scan_types) {
      cli_abort(
        c(
          "{.arg scan_type} {.val {scan_type}} is not in the data",
          "i" = "available {qty(length(available_scan_types))}type{?s}: {.field {available_scan_types}}"
        )
      )
    }
    plot_data <- dplyr::filter(plot_data, .data$scan_type == !!scan_type)
  }

  # derive axis labels: y from the data_type when unique (e.g. "intensity [mV]"),
  # otherwise the generic "value" since intensities and ratios are mixed
  x_lab <- paste0(
    unique(plot_data$scan_type)[1],
    " [",
    unique(plot_data$x_units)[1],
    "]"
  )
  use_dt_facet <- use_data_type_facet(facet_quo, data_type_as_facet, plot_data)
  # y axis label: NULL when data_type is shown via the facet strip, otherwise the
  # data type(s) being plotted
  y_lab <- if (use_dt_facet) {
    NULL
  } else {
    paste(unique(plot_data$data_type), collapse = " / ")
  }

  # one line per trace (incl. ratio traces); group on the trace identifier
  group_cols <- intersect(
    c("uidx", "analysis", "channel", "trace"),
    names(plot_data)
  )

  # x window: applied after the trace factor is built (in the tibble) so it keeps
  # its full levels and the colour mapping stays stable when zoomed. An empty
  # window (no data points inside it) is fine - the line still interpolates from
  # the bracketing points just outside the window (see apply_plot_window()).
  if (!is.null(x_window)) {
    plot_data <- apply_plot_window(plot_data, "x", x_window, group_cols)
  }

  # optionally drop trace factor levels (e.g. outside the window) not visible
  plot_data <- apply_drop_unused_levels(
    plot_data,
    drop_unused_levels,
    "x",
    x_window,
    group_cols
  )

  # validate aesthetic expressions against the actual plot data
  check_aes_expr(color_quo, "color", plot_data)
  check_aes_expr(linetype_quo, "linetype", plot_data)

  # base plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::aes(
      x = !!sym("x"),
      y = !!sym("value"),
      group = paste(!!!rlang::syms(group_cols))
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n_x_breaks),
      expand = if (!is.null(x_window)) FALSE else ggplot2::waiver()
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n_y_breaks),
      labels = if (scientific) label_scientific() else ggplot2::waiver()
    )

  # additional aesthetics; when colouring by trace, share a colour across traces
  # of the same species/mass (e.g. "N2: 29" and "N2: 29/28")
  if (rlang::quo_is_symbol(color_quo, "trace")) {
    color_values <- build_trace_color_values(plot_data, color_values)
  }
  p <- add_color_aes(p, color_quo, color_values, plot_data, drop_unused_levels)
  if (!rlang::quo_is_null(linetype_quo)) {
    p <- p + ggplot2::aes(linetype = !!linetype_quo)
  }

  # facets
  p <- add_data_type_or_facets(
    p,
    facet_quo,
    use_dt_facet,
    plot_data,
    scales,
    nrow,
    ncol,
    ...,
    geometry_set = !missing(nrow) || !missing(ncol)
  )

  # x window: clip display to the requested range
  if (!is.null(x_window)) {
    p <- p + ggplot2::coord_cartesian(xlim = x_window)
  }

  p <- p + ir_default_theme()
  # place the data_type facet strip outside the y axis (on the left, next to the
  # per-row y-axis values) when it is used as a facet row
  if (use_dt_facet) {
    p <- p + ggplot2::theme(strip.placement = "outside")
  }

  return(p)
}

#' Plot continuous flow data
#'
#' Plots chromatographic trace data from an [ir_aggregate_isofiles()] result or
#' a plain data frame. The data is prepared with [ir_generate_traces_tibble()]
#' (which, for an `ir_aggregated_data` object, inner-joins the `$traces` dataset
#' with `$metadata`). The plot data must contain `species`, `time.s`, `mass`, and
#' an `intensity.*` column — an error is thrown if any are missing. A `trace`
#' identifier (`"<species>: <mass>"`) is always regenerated and the plotted
#' `value` together with a `data_type` label (`"intensity [UNITS]"`, or
#' `"ratios"` for ratio rows) are added.
#'
#' @param dataset an `ir_aggregated_data` object from [ir_aggregate_isofiles()]
#'   or a plain data frame with `species`, `time.s`, `mass`, and an
#'   `intensity.*` column
#' @param species optional vector to filter the displayed data to specific
#'   species (e.g. `"CO2"` or `c("N2", "CO2")`); default `NULL` shows all species.
#' @param mass which masses to include as intensity traces: `NULL` (default)
#'   shows all masses, a vector (e.g. `44` or `c(44, 45)`) shows specific masses,
#'   and a zero-length vector (`numeric(0)`/`character(0)`) shows none. Note that
#'   `c()` is `NULL` in R (i.e. all masses).
#' @param ratio which ratios to additionally include (computed with
#'   [ir_calculate_ratios()]): `NULL` (default) shows all available ratios, a
#'   character vector of ratio names (e.g. `c("45/44", "46/44")`) shows specific
#'   ones, and `character(0)` shows none. Requesting specific ratio names when
#'   ratios have not been calculated is an error pointing to
#'   [ir_calculate_ratios()] (with `ratio = NULL` and no ratios present, none are
#'   simply added). Ratio rows are plotted on the same `value` axis with
#'   `data_type = "ratios"`; the default `facet = data_type` (with free scales)
#'   separates them from the intensities.
#' @param facet column or expression to facet by (default: `NULL`, no extra
#'   faceting). When
#'   `data_type` is used as a facet row (see `data_type_as_facet`), a single
#'   `facet` variable becomes the facet_grid column (`data_type ~ facet`) and a
#'   `NULL` facet gives `data_type ~ .`. Otherwise a plain column or expression
#'   (e.g. `file_name` or `paste(species, mass)`) is faceted with
#'   [ggplot2::facet_wrap()], and a two-sided formula (e.g. `species ~ mass`) is
#'   faceted with [ggplot2::facet_grid()]. Set to `NULL` to suppress faceting.
#' @param data_type_as_facet whether the `data_type` column (intensities vs
#'   ratios) is used as the [ggplot2::facet_grid()] row variable: `auto()`
#'   (default) uses it only when more than one data type is present; `TRUE`
#'   always uses it; `FALSE` never does. When used, the y axis label is dropped
#'   (the facet strip provides it) and the facet becomes `data_type ~ .` (a `NULL`
#'   `facet`) or `data_type ~ facet` (a single-variable `facet`). It is ignored
#'   when `facet` is a two-sided formula (a warning is issued if
#'   `data_type_as_facet = TRUE` is combined with a formula `facet`, since the two
#'   are mutually exclusive).
#' @param scales whether facet scales should be `"free"` (default), `"fixed"`,
#'   `"free_x"`, or `"free_y"`; passed on to [ggplot2::facet_wrap()] /
#'   [ggplot2::facet_grid()].
#' @param nrow,ncol number of rows and columns of facet panels (`nrow` default
#'   `NULL` lets ggplot2 choose; `ncol` default `1` stacks the panels in a
#'   single column). Only applies when `facet` is a single variable or
#'   expression (faceted with [ggplot2::facet_wrap()]); ignored when `facet` is a
#'   formula (faceted with [ggplot2::facet_grid()]), with a warning if you set
#'   them explicitly.
#' @param color column or expression for the colour aesthetic (default:
#'   `trace`, the per-species/mass trace identifier, e.g. `"CO2: 44"`). When
#'   colouring by `trace`, traces that share the same species and (numerator)
#'   mass are given the same colour, so an intensity trace (`"N2: 29"`) and its
#'   ratio traces (`"N2: 29/28"`) match.
#' @param linetype column or expression for the linetype aesthetic (default:
#'   `NULL`, i.e. no linetype aesthetic)
#' @param color_values named or unnamed character vector of colours passed to
#'   [ggplot2::scale_color_manual()], or `NULL` to use the ggplot2 default
#'   colour palette (default: [palette.colors()])
#' @param drop_unused_levels whether to drop unused `trace` factor levels (e.g.
#'   traces that are absent after zooming to a window) from the colour scale and
#'   legend. Default `FALSE` keeps every level so the colour mapping stays stable
#'   across subsets of the same dataset; set to `TRUE` to show only the levels
#'   actually present in the plotted data.
#' @param scientific whether to format y axis labels in scientific notation
#'   (default: `FALSE`)
#' @param ... additional arguments passed on to [ggplot2::facet_wrap()] or
#'   [ggplot2::facet_grid()] (e.g. `labeller`)
#' @param time_window.s,time_window.min optional numeric vector of length 2
#'   giving the time axis display window `c(min, max)`, either in seconds
#'   (`time_window.s`) or in minutes (`time_window.min`, converted to seconds
#'   internally — the function always works in seconds). Provide at most one; if
#'   both are given, `time_window.s` is used. Must have `min < max`. The data
#'   point just outside each edge of the window is retained so the clipped lines
#'   interpolate correctly across the window boundaries and y autoscales correctly
#'   at the edges; [ggplot2::coord_cartesian()] clips the display. A window that
#'   contains no data points of its own is allowed (the line is drawn between the
#'   bracketing points). Default `NULL` (both) shows the full time range.
#' @param short_time_labels whether to use compact time axis labels with no
#'   space between value and unit and abbreviated units (`hr`, `m`, `s`)
#'   (default: `FALSE`)
#' @param n_time_breaks desired number of time axis tick marks (default: `5`)
#' @param n_y_breaks desired number of y axis tick marks (default: `5`)
#' @return a `ggplot` object with [ir_default_theme()] applied. To customize the
#'   plot, add ggplot2 layers on top (e.g. `+ ggplot2::theme(...)` or
#'   `+ ggplot2::labs(...)`); attach ggplot2 with `library(ggplot2)` first.
#' @export
ir_plot_continuous_flow <- function(
  dataset,
  species = NULL,
  mass = NULL,
  ratio = NULL,
  facet = NULL,
  data_type_as_facet = auto(),
  scales = "free",
  nrow = NULL,
  ncol = 1,
  color = trace,
  linetype = NULL,
  color_values = palette.colors(),
  drop_unused_levels = FALSE,
  scientific = FALSE,
  time_window.s = if (is.null(time_window.min)) NULL else 60 * time_window.min,
  time_window.min = NULL,
  short_time_labels = FALSE,
  n_time_breaks = 5,
  n_y_breaks = 5,
  ...
) {
  # plot-specific argument checks (dataset / species / mass / ratio are checked
  # by ir_generate_traces_tibble() below)
  check_arg(
    color_values,
    is.null(color_values) || is.character(color_values),
    "must be NULL or a character vector of colours"
  )
  check_arg(scientific, rlang::is_bool(scientific), "must be TRUE or FALSE")
  check_arg(
    drop_unused_levels,
    rlang::is_bool(drop_unused_levels),
    "must be TRUE or FALSE"
  )
  check_arg(
    short_time_labels,
    rlang::is_bool(short_time_labels),
    "must be TRUE or FALSE"
  )
  check_arg(
    time_window.min,
    is.null(time_window.min) ||
      (is.numeric(time_window.min) &&
        length(time_window.min) == 2 &&
        time_window.min[1] < time_window.min[2]),
    "must be NULL or a numeric vector of length 2 with min < max"
  )
  check_arg(
    time_window.s,
    is.null(time_window.s) ||
      (is.numeric(time_window.s) &&
        length(time_window.s) == 2 &&
        time_window.s[1] < time_window.s[2]),
    "must be NULL or a numeric vector of length 2 with min < max"
  )
  # the rest of the function works in seconds
  time_window <- time_window.s
  check_arg(
    n_time_breaks,
    rlang::is_scalar_integerish(n_time_breaks) && n_time_breaks > 0,
    "must be a positive whole number"
  )
  check_arg(
    n_y_breaks,
    rlang::is_scalar_integerish(n_y_breaks) && n_y_breaks > 0,
    "must be a positive whole number"
  )
  check_arg(scales, rlang::is_scalar_character(scales), "must be a string")
  check_arg(
    data_type_as_facet,
    is_auto(data_type_as_facet) || rlang::is_bool(data_type_as_facet),
    "must be auto(), TRUE, or FALSE"
  )

  # capture aesthetics before any data manipulation
  facet_quo <- rlang::enquo(facet)
  color_quo <- rlang::enquo(color)
  linetype_quo <- rlang::enquo(linetype)

  # build the plotting tibble (join, filter, trace/data_type/value, ratios)
  plot_data <- ir_generate_traces_tibble(
    dataset,
    species = species,
    mass = mass,
    ratio = ratio
  )
  time_col <- "time.s"

  # one line per trace (incl. ratio traces); group on the trace identifier
  group_cols <- intersect(
    c("uidx", "analysis", "channel", "trace"),
    names(plot_data)
  )

  # time window: applied after the trace factor is built (in the tibble) so it
  # keeps its full levels and the colour mapping stays stable when zoomed. An
  # empty window (no data points inside it) is fine - the line still interpolates
  # from the bracketing points just outside the window (see apply_plot_window()).
  if (!is.null(time_window)) {
    plot_data <- apply_plot_window(plot_data, time_col, time_window, group_cols)
  }

  # optionally drop trace factor levels (e.g. outside the window) not visible
  plot_data <- apply_drop_unused_levels(
    plot_data,
    drop_unused_levels,
    time_col,
    time_window,
    group_cols
  )

  # validate aesthetic expressions against the actual plot data
  check_aes_expr(color_quo, "color", plot_data)
  check_aes_expr(linetype_quo, "linetype", plot_data)

  # axis labels: y from the data_type when unique (e.g. "intensity [mV]"),
  # otherwise the generic "value" since intensities and ratios are mixed
  x_lab <- NULL # the time is obvious from the units
  use_dt_facet <- use_data_type_facet(facet_quo, data_type_as_facet, plot_data)
  # y axis label: NULL when data_type is shown via the facet strip, otherwise the
  # data type(s) being plotted
  y_lab <- if (use_dt_facet) {
    NULL
  } else {
    paste(unique(plot_data$data_type), collapse = " / ")
  }

  # base plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::aes(
      x = !!sym(time_col),
      y = !!sym("value"),
      group = paste(!!!rlang::syms(group_cols))
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::scale_x_continuous(
      breaks = breaks_pretty_duration(n = n_time_breaks),
      labels = labels_duration(short_format = short_time_labels),
      expand = if (!is.null(time_window)) FALSE else ggplot2::waiver()
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n_y_breaks),
      labels = if (scientific) label_scientific() else ggplot2::waiver()
    )

  # additional aesthetics; when colouring by trace, share a colour across traces
  # of the same species/mass (e.g. "N2: 29" and "N2: 29/28")
  if (rlang::quo_is_symbol(color_quo, "trace")) {
    color_values <- build_trace_color_values(plot_data, color_values)
  }
  p <- add_color_aes(p, color_quo, color_values, plot_data, drop_unused_levels)
  if (!rlang::quo_is_null(linetype_quo)) {
    p <- p + ggplot2::aes(linetype = !!linetype_quo)
  }

  # facets
  p <- add_data_type_or_facets(
    p,
    facet_quo,
    use_dt_facet,
    plot_data,
    scales,
    nrow,
    ncol,
    ...,
    geometry_set = !missing(nrow) || !missing(ncol)
  )

  # time window: clip display to the requested range
  if (!is.null(time_window)) {
    p <- p + ggplot2::coord_cartesian(xlim = time_window)
  }

  p <- p + ir_default_theme()
  # place the data_type facet strip outside the y axis (on the left, next to the
  # per-row y-axis values) when it is used as a facet row
  if (use_dt_facet) {
    p <- p + ggplot2::theme(strip.placement = "outside")
  }

  return(p)
}

#' Plot dual inlet cycle data
#'
#' Plots cycle data from an [ir_aggregate_isofiles()] result or a plain data
#' frame. The data is prepared with [ir_generate_cycles_tibble()] (which, for an
#' `ir_aggregated_data` object, inner-joins the `$cycles` dataset with
#' `$metadata`). The plot data must contain `species`, `cycle`, `type`, `mass`,
#' and an `intensity.*` column — an error is thrown if any are missing. A `trace`
#' identifier (`"<species>: <mass>"`) is always regenerated and the plotted
#' `value` together with a `data_type` label (`"intensity [UNITS]"`, or
#' `"ratios"` for ratio rows) are added.
#'
#' @param dataset an `ir_aggregated_data` object from [ir_aggregate_isofiles()]
#'   or a plain data frame with `species`, `cycle`, `type`, `mass`, and an
#'   `intensity.*` column
#' @param species optional vector to filter the displayed data to specific
#'   species (e.g. `"CO2"` or `c("N2", "CO2")`); default `NULL` shows all species.
#' @param mass which masses to include as intensity traces: `NULL` (default)
#'   shows all masses, a vector (e.g. `44` or `c(44, 45)`) shows specific masses,
#'   and a zero-length vector (`numeric(0)`/`character(0)`) shows none. Note that
#'   `c()` is `NULL` in R (i.e. all masses).
#' @param ratio which ratios to additionally include (computed with
#'   [ir_calculate_ratios()]): `NULL` (default) shows all available ratios, a
#'   character vector of ratio names (e.g. `c("45/44", "46/44")`) shows specific
#'   ones, and `character(0)` shows none. Requesting specific ratio names when
#'   ratios have not been calculated is an error pointing to
#'   [ir_calculate_ratios()] (with `ratio = NULL` and no ratios present, none are
#'   simply added). Ratio rows are plotted on the same `value` axis with
#'   `data_type = "ratios"`; the default `facet = data_type` (with free scales)
#'   separates them from the intensities.
#' @param facet column or expression to facet by (default: `NULL`, no extra
#'   faceting). When
#'   `data_type` is used as a facet row (see `data_type_as_facet`), a single
#'   `facet` variable becomes the facet_grid column (`data_type ~ facet`) and a
#'   `NULL` facet gives `data_type ~ .`. Otherwise a plain column or expression
#'   (e.g. `file_name` or `paste(species, mass)`) is faceted with
#'   [ggplot2::facet_wrap()], and a two-sided formula (e.g. `species ~ mass`) is
#'   faceted with [ggplot2::facet_grid()]. Set to `NULL` to suppress faceting.
#' @param data_type_as_facet whether the `data_type` column (intensities vs
#'   ratios) is used as the [ggplot2::facet_grid()] row variable: `auto()`
#'   (default) uses it only when more than one data type is present; `TRUE`
#'   always uses it; `FALSE` never does. When used, the y axis label is dropped
#'   (the facet strip provides it) and the facet becomes `data_type ~ .` (a `NULL`
#'   `facet`) or `data_type ~ facet` (a single-variable `facet`). It is ignored
#'   when `facet` is a two-sided formula (a warning is issued if
#'   `data_type_as_facet = TRUE` is combined with a formula `facet`, since the two
#'   are mutually exclusive).
#' @param scales whether facet scales should be `"free"` (default), `"fixed"`,
#'   `"free_x"`, or `"free_y"`; passed on to [ggplot2::facet_wrap()] /
#'   [ggplot2::facet_grid()].
#' @param nrow,ncol number of rows and columns of facet panels (`nrow` default
#'   `NULL` lets ggplot2 choose; `ncol` default `1` stacks the panels in a
#'   single column). Only applies when `facet` is a single variable or
#'   expression (faceted with [ggplot2::facet_wrap()]); ignored when `facet` is a
#'   formula (faceted with [ggplot2::facet_grid()]), with a warning if you set
#'   them explicitly.
#' @param color column or expression for the colour aesthetic (default:
#'   `trace`, the per-species/mass trace identifier, e.g. `"CO2: 44"`). When
#'   colouring by `trace`, traces that share the same species and (numerator)
#'   mass are given the same colour, so an intensity trace (`"CO2: 45"`) and its
#'   ratio traces (`"CO2: 45/44"`) match.
#' @param shape column or expression for the point shape aesthetic (default:
#'   `type`, distinguishing `"standard"` from `"sample"` cycles)
#' @param linetype column or expression for the linetype aesthetic (default:
#'   `NULL`, i.e. no linetype aesthetic)
#' @param color_values named or unnamed character vector of colours passed to
#'   [ggplot2::scale_color_manual()], or `NULL` to use the ggplot2 default
#'   colour palette (default: [palette.colors()])
#' @param drop_unused_levels whether to drop unused `trace` factor levels (e.g.
#'   traces that are absent after zooming to a window) from the colour scale and
#'   legend. Default `FALSE` keeps every level so the colour mapping stays stable
#'   across subsets of the same dataset; set to `TRUE` to show only the levels
#'   actually present in the plotted data.
#' @param scientific whether to format y axis labels in scientific notation
#'   (default: `FALSE`)
#' @param cycle_window optional numeric vector of length 2 giving the cycle axis
#'   display window `c(min, max)` (must have `min < max`). The data point just
#'   outside each edge of the window is retained so the clipped lines interpolate
#'   correctly across the window boundaries and y autoscales correctly at the
#'   edges; [ggplot2::coord_cartesian()] clips the display. A window that contains
#'   no data points of its own is allowed (the line is drawn between the
#'   bracketing points). Default `NULL` shows all cycles.
#' @param n_y_breaks desired number of y axis tick marks (default: `5`)
#' @param ... additional arguments passed on to [ggplot2::facet_wrap()] or
#'   [ggplot2::facet_grid()] (e.g. `labeller`)
#' @return a `ggplot` object with [ir_default_theme()] applied. To customize the
#'   plot, add ggplot2 layers on top (e.g. `+ ggplot2::theme(...)` or
#'   `+ ggplot2::labs(...)`); attach ggplot2 with `library(ggplot2)` first.
#' @export
ir_plot_dual_inlet <- function(
  dataset,
  species = NULL,
  mass = NULL,
  ratio = NULL,
  facet = NULL,
  data_type_as_facet = auto(),
  scales = "free",
  nrow = NULL,
  ncol = 1,
  color = trace,
  shape = type,
  linetype = NULL,
  color_values = palette.colors(),
  drop_unused_levels = FALSE,
  scientific = FALSE,
  cycle_window = NULL,
  n_y_breaks = 5,
  ...
) {
  # plot-specific argument checks (dataset / species / mass / ratio are checked
  # by ir_generate_cycles_tibble() below)
  check_arg(
    color_values,
    is.null(color_values) || is.character(color_values),
    "must be NULL or a character vector of colours"
  )
  check_arg(scientific, rlang::is_bool(scientific), "must be TRUE or FALSE")
  check_arg(
    drop_unused_levels,
    rlang::is_bool(drop_unused_levels),
    "must be TRUE or FALSE"
  )
  check_arg(
    n_y_breaks,
    rlang::is_scalar_integerish(n_y_breaks) && n_y_breaks > 0,
    "must be a positive whole number"
  )
  check_arg(scales, rlang::is_scalar_character(scales), "must be a string")
  check_arg(
    data_type_as_facet,
    is_auto(data_type_as_facet) || rlang::is_bool(data_type_as_facet),
    "must be auto(), TRUE, or FALSE"
  )
  check_arg(
    cycle_window,
    is.null(cycle_window) ||
      (is.numeric(cycle_window) &&
        length(cycle_window) == 2 &&
        cycle_window[1] < cycle_window[2]),
    "must be NULL or a numeric vector of length 2 with min < max"
  )

  # capture aesthetics before any data manipulation
  facet_quo <- rlang::enquo(facet)
  color_quo <- rlang::enquo(color)
  shape_quo <- rlang::enquo(shape)
  linetype_quo <- rlang::enquo(linetype)

  # build the plotting tibble (join, filter, trace/data_type/value, ratios)
  plot_data <- ir_generate_cycles_tibble(
    dataset,
    species = species,
    mass = mass,
    ratio = ratio
  )

  use_dt_facet <- use_data_type_facet(facet_quo, data_type_as_facet, plot_data)
  # y axis label: NULL when data_type is shown via the facet strip, otherwise the
  # data type(s) being plotted
  y_lab <- if (use_dt_facet) {
    NULL
  } else {
    paste(unique(plot_data$data_type), collapse = " / ")
  }

  # one line per cycle trace (incl. ratio traces); standard/sample are separate
  group_cols <- intersect(
    c("uidx", "analysis", "channel", "type", "trace"),
    names(plot_data)
  )

  # cycle window: applied after the trace factor is built (in the tibble) so it
  # keeps its full levels and the colour mapping stays stable when zoomed. An
  # empty window (no data points inside it) is fine - the line still interpolates
  # from the bracketing points just outside the window (see apply_plot_window()).
  if (!is.null(cycle_window)) {
    plot_data <- apply_plot_window(
      plot_data,
      "cycle",
      cycle_window,
      group_cols
    )
  }

  # optionally drop trace factor levels (e.g. outside the window) not visible
  plot_data <- apply_drop_unused_levels(
    plot_data,
    drop_unused_levels,
    "cycle",
    cycle_window,
    group_cols
  )

  # validate aesthetic expressions against the actual plot data
  check_aes_expr(color_quo, "color", plot_data)
  check_aes_expr(shape_quo, "shape", plot_data)
  check_aes_expr(linetype_quo, "linetype", plot_data)

  # base plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::aes(
      x = !!sym("cycle"),
      y = !!sym("value"),
      group = paste(!!!rlang::syms(group_cols))
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(y = y_lab) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_width(1),
      expand = if (!is.null(cycle_window)) FALSE else ggplot2::waiver()
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n_y_breaks),
      labels = if (scientific) label_scientific() else ggplot2::waiver()
    )

  # additional aesthetics; when colouring by trace, share a colour across traces
  # of the same species/mass (e.g. "CO2: 45" and "CO2: 45/44")
  if (rlang::quo_is_symbol(color_quo, "trace")) {
    color_values <- build_trace_color_values(plot_data, color_values)
  }
  p <- add_color_aes(p, color_quo, color_values, plot_data, drop_unused_levels)
  if (!rlang::quo_is_null(shape_quo)) {
    p <- p + ggplot2::aes(shape = !!shape_quo)
  }
  if (!rlang::quo_is_null(linetype_quo)) {
    p <- p + ggplot2::aes(linetype = !!linetype_quo)
  }

  # facets
  p <- add_data_type_or_facets(
    p,
    facet_quo,
    use_dt_facet,
    plot_data,
    scales,
    nrow,
    ncol,
    ...,
    geometry_set = !missing(nrow) || !missing(ncol)
  )

  # cycle window: clip display to the requested range
  if (!is.null(cycle_window)) {
    p <- p + ggplot2::coord_cartesian(xlim = cycle_window)
  }

  p <- p + ir_default_theme()
  # place the data_type facet strip outside the y axis (on the left, next to the
  # per-row y-axis values) when it is used as a facet row
  if (use_dt_facet) {
    p <- p + ggplot2::theme(strip.placement = "outside")
  }

  return(p)
}
