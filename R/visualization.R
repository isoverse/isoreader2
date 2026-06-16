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
#' @param text_size base font size in points (default: `16`)
#' @param facet_text_size font size for facet strip labels in points. Default
#'   `NULL` leaves the strip labels at the inherited base size; set a number to
#'   override it.
#' @return a `ggplot2` theme object
#' @export
ir_default_theme <- function(text_size = 16, facet_text_size = NULL) {
  check_arg(
    text_size,
    is.numeric(text_size) && length(text_size) == 1L,
    "must be a single number"
  )
  check_arg(
    facet_text_size,
    is.null(facet_text_size) ||
      (is.numeric(facet_text_size) && length(facet_text_size) == 1L),
    "must be NULL or a single number"
  )
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank()
    )
  if (!is.null(facet_text_size)) {
    theme <- theme +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = facet_text_size)
      )
  }
  theme
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

# internal: if a `trace` column is present and not already a factor, convert it
# to a factor with levels sorted by the numerical mass number at the end of the
# trace label (e.g. "CO2: 44" -> 44), mirroring how `mass` is sorted. Traces
# without a trailing number sort last.
sort_trace_factor <- function(plot_data) {
  if ("trace" %in% names(plot_data) && !is.factor(plot_data$trace)) {
    trace_levels <- unique(plot_data$trace)
    # extract the trailing mass number as the sort key (NA for traces without
    # one, which then sort last); coercion NAs are expected, so silence them
    trace_mass <- suppressWarnings(
      as.numeric(sub("^.* (\\d+\\.?\\d*)$", "\\1", trace_levels))
    )
    trace_levels <- trace_levels[order(
      trace_mass,
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

# internal: add the colour aesthetic and a matching colour scale. When the
# colour column is a factor (e.g. `mass` or `trace`), all of its levels are kept
# (`drop = FALSE`) so that the colour mapping stays stable when the plotted data
# is a subset of the full dataset (e.g. zoomed to a window) instead of
# re-coloring the remaining groups. The manual palette is only used when it
# supplies enough colours for *all* levels; otherwise the default discrete scale
# (which generates as many hues as needed) is used, also keeping unused levels.
add_color_aes <- function(p, color_quo, color_values, plot_data) {
  if (rlang::quo_is_null(color_quo)) {
    return(p)
  }
  p <- p + ggplot2::aes(color = !!color_quo)
  color_vals <- rlang::eval_tidy(color_quo, plot_data)
  is_factor <- is.factor(color_vals)
  # for factors count *all* levels (not just those present) so the manual-vs-
  # default decision and the palette size match the full dataset
  n_colors <- if (is_factor) {
    nlevels(color_vals)
  } else {
    dplyr::n_distinct(color_vals)
  }
  if (!is.null(color_values) && length(color_values) >= n_colors) {
    p <- p + scale_color_manual(values = color_values, drop = !is_factor)
  } else if (is_factor) {
    p <- p + ggplot2::scale_color_discrete(drop = FALSE)
  }
  return(p)
}

# internal: subset `plot_data` to a display `window` (length-2 c(min, max)) along
# column `col`, additionally keeping, per line (grouped by `group_cols`), the
# single data point just below and just above the window. Those bracketing points
# let the clipped lines interpolate correctly across the window edges and give
# correct y autoscaling there. Because the bracketing points are found relative to
# the window bounds (not to the in-window points), a window that contains no data
# points of its own is fully supported - the line is simply drawn between the
# bracketing points on either side. Assumes `window` is already validated as
# min < max.
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

# internal: filter plot data to the requested `species` and/or `mass` values
# (compared as character, so numeric/character work interchangeably). Errors
# informatively (listing what is available) if a selection leaves no data.
filter_plot_data <- function(plot_data, species, mass, .env = caller_env()) {
  filter_by <- function(plot_data, col, values) {
    if (is.null(values)) {
      return(plot_data)
    }
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
  plot_data <- filter_by(plot_data, "species", species)
  plot_data <- filter_by(plot_data, "mass", mass)
  plot_data
}

#' Plot scan data
#'
#' Plots scan data from an [ir_aggregate_isofiles()] result or a plain data
#' frame. When `dataset` is an `ir_aggregated_data` object, the `$scans`
#' dataset is inner-joined with `$metadata` (bringing in all metadata columns
#' not already present in `$scans`) before plotting. The plot data must contain
#' `x`, `scan_type`, `x_units`, `mass`, and an `intensity.*` column — an error
#' is thrown if any are missing. The intensity unit suffix becomes the y axis
#' label; `scan_type` and `x_units` are combined for the x axis label. If
#' `mass` is not already a factor it is converted to one with levels sorted in
#' numerical order.
#'
#' @param dataset an `ir_aggregated_data` object from [ir_aggregate_isofiles()]
#'   or a plain data frame with `x`, `scan_type`, `x_units`, `mass`, and an
#'   `intensity.*` column
#' @param scan_type which scan type to plot (e.g. `"high voltage"`). Required
#'   when the data contains more than one scan type; an error lists the
#'   available types. If the data contains only one scan type, the parameter
#'   must either be `NULL` or match that type exactly.
#' @param species optional vector to filter the displayed data to specific
#'   species (e.g. `"CO2"` or `c("N2", "CO2")`); default `NULL` shows all species.
#' @param mass optional vector to filter the displayed data to specific masses
#'   (e.g. `44` or `c(44, 45)`); default `NULL` shows all masses.
#' @param facet column or expression to facet by (default: `file_name`). A
#'   plain column or expression (e.g. `file_name` or `paste(species, mass)`) is
#'   faceted with [ggplot2::facet_wrap()]; a two-sided formula (e.g.
#'   `species ~ mass`) is faceted with [ggplot2::facet_grid()]. Set to `NULL`
#'   to suppress faceting.
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
#'   `trace`, the per-species/mass trace identifier, e.g. `"CO2: 44"`)
#' @param linetype column or expression for the linetype aesthetic (default:
#'   `NULL`, i.e. no linetype aesthetic)
#' @param color_values named or unnamed character vector of colours passed to
#'   [ggplot2::scale_color_manual()], or `NULL` to use the ggplot2 default
#'   colour palette (default: [palette.colors()])
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
#' @param theme ggplot2 theme to apply (default: [ir_default_theme()])
#' @return a `ggplot` object. To further customize the plot by adding ggplot2
#'   layers (e.g. `+ ggplot2::labs(...)`), attach ggplot2 with
#'   `library(ggplot2)` first.
#' @export
ir_plot_scans <- function(
  dataset,
  scan_type = NULL,
  species = NULL,
  mass = NULL,
  facet = file_name,
  scales = "free",
  nrow = NULL,
  ncol = 1,
  color = trace,
  linetype = NULL,
  color_values = palette.colors(),
  scientific = FALSE,
  x_window = NULL,
  n_x_breaks = 5,
  n_y_breaks = 5,
  theme = ir_default_theme(),
  ...
) {
  # safety checks
  if (!missing(dataset) && is(dataset, "ir_isofiles")) {
    cli_abort(
      c(
        "{.field dataset} contains raw isofiles object and cannot be plotted directly",
        "i" = "aggregate the data first with {.code ir_aggregate_isofiles(dataset)}"
      )
    )
  }
  check_arg(
    dataset,
    !missing(dataset) &&
      (is.data.frame(dataset) || is(dataset, "ir_aggregated_data")),
    "must be a data frame or a set of aggregated isofiles"
  )
  check_arg(
    scan_type,
    is.null(scan_type) || rlang::is_scalar_character(scan_type),
    "must be NULL or a single string"
  )
  check_arg(
    species,
    is.null(species) || is.character(species) || is.numeric(species),
    "must be NULL or a character/numeric vector"
  )
  check_arg(
    mass,
    is.null(mass) || is.character(mass) || is.numeric(mass),
    "must be NULL or a character/numeric vector"
  )
  check_arg(
    color_values,
    is.null(color_values) || is.character(color_values),
    "must be NULL or a character vector of colours"
  )
  check_arg(scientific, rlang::is_bool(scientific), "must be TRUE or FALSE")
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

  # capture aesthetics before any data manipulation
  facet_quo <- rlang::enquo(facet)
  color_quo <- rlang::enquo(color)
  linetype_quo <- rlang::enquo(linetype)

  # prepare plot data
  if (is(dataset, "ir_aggregated_data")) {
    if (
      !"scans" %in% names(dataset) ||
        ncol(dataset$scans) == 0 ||
        nrow(dataset$scans) == 0
    ) {
      cli_abort(
        c(
          "no scans available in the provided {.field dataset}",
          "i" = "make sure you are reading scan isofiles and the aggregator includes columns from {.field scans}"
        )
      )
    }
    # join in metadata columns not already in scans
    meta_extra_cols <- setdiff(names(dataset$metadata), names(dataset$scans))
    plot_data <- dplyr::inner_join(
      dataset$scans,
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
    cli_abort("no data to plot (0 rows)")
  }

  # require scan_type, x_units, x, and mass columns
  required_cols <- c("scan_type", "x_units", "x", "mass")
  missing_cols <- setdiff(required_cols, names(plot_data))
  if (length(missing_cols) > 0) {
    cli_abort(
      c(
        "scan data is missing required {qty(length(missing_cols))}column{?s}: {.field {missing_cols}}",
        "i" = "make sure the aggregator includes {.field scan_type}, {.field x_units}, {.field x}, and {.field mass}"
      )
    )
  }

  # filter to the requested species / mass
  plot_data <- filter_plot_data(plot_data, species, mass)

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

  # require an intensity.UNITS column
  intensity_cols <- grep("^intensity\\.", names(plot_data), value = TRUE)
  if (length(intensity_cols) == 0) {
    cli_abort(
      c(
        "no intensity column found in scan data",
        "i" = "expected a column whose name matches {.code intensity.*}"
      )
    )
  }
  intensity_col <- intensity_cols[1]
  intensity_units <- sub("^intensity\\.", "", intensity_col)

  # derive axis labels
  x_lab <- paste0(
    unique(plot_data$scan_type)[1],
    " [",
    unique(plot_data$x_units)[1],
    "]"
  )
  y_lab <- paste0("intensity [", intensity_units, "]")

  # sort mass as a factor in numerical order
  if (!is.factor(plot_data$mass)) {
    mass_levels <- plot_data$mass |>
      unique() |>
      as.numeric() |>
      sort(na.last = TRUE) |>
      as.character()
    plot_data <- plot_data |>
      dplyr::mutate(mass = factor(.data$mass, levels = mass_levels))
  }

  # sort trace as a factor in numerical order of the trailing mass number
  plot_data <- sort_trace_factor(plot_data)

  # x window: applied after the mass/trace factors are built (above) so they keep
  # their full levels and the colour mapping stays stable when zoomed. An empty
  # window (no data points inside it) is fine - the line still interpolates from
  # the bracketing points just outside the window (see apply_plot_window()).
  if (!is.null(x_window)) {
    plot_data <- apply_plot_window(
      plot_data,
      "x",
      x_window,
      c("uidx", "analysis", "species", "channel", "mass")
    )
  }

  # validate aesthetic expressions against the actual plot data
  check_aes_expr(color_quo, "color", plot_data)
  check_aes_expr(linetype_quo, "linetype", plot_data)

  # group aesthetic: always set to ensure one line per scan trace
  group_cols <- intersect(
    c("uidx", "analysis", "species", "mass"),
    names(plot_data)
  )

  # base plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::aes(
      x = !!sym("x"),
      y = !!sym(intensity_col),
      group = interaction(!!!rlang::syms(group_cols))
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = x_lab, y = y_lab) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n_x_breaks),
      expand = if (!is.null(x_window)) FALSE else c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n_y_breaks),
      labels = if (scientific) label_scientific() else ggplot2::waiver(),
      # autoscaled to a window: headroom on both ends; otherwise the default
      expand = if (!is.null(x_window)) {
        ggplot2::expansion(mult = c(0.05, 0.05))
      } else {
        ggplot2::waiver()
      }
    )

  # include 0 in the y range, unless an x window is set (then autoscale to it)
  if (is.null(x_window)) {
    p <- p + ggplot2::expand_limits(y = 0)
  }

  # additional aesthetics
  p <- add_color_aes(p, color_quo, color_values, plot_data)
  if (!rlang::quo_is_null(linetype_quo)) {
    p <- p + ggplot2::aes(linetype = !!linetype_quo)
  }

  # facets
  p <- add_facets(
    p,
    facet_quo,
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

  p <- p + theme

  return(p)
}

#' Plot continuous flow data
#'
#' Plots chromatographic trace data from an [ir_aggregate_isofiles()] result or
#' a plain data frame. When `dataset` is an `ir_aggregated_data` object, the
#' `$traces` dataset is inner-joined with `$metadata` (bringing in all metadata
#' columns not already present in `$traces`) before plotting. The plot data must
#' contain `time.s`, `mass`, and an `intensity.*` column — an error is thrown if
#' any are missing. The intensity unit suffix becomes the y axis label. If
#' `mass` is not already a factor it is converted to one with levels sorted in
#' numerical order.
#'
#' @param dataset an `ir_aggregated_data` object from [ir_aggregate_isofiles()]
#'   or a plain data frame with `time.s`, `mass`, and an `intensity.*` column
#' @param species optional vector to filter the displayed data to specific
#'   species (e.g. `"CO2"` or `c("N2", "CO2")`); default `NULL` shows all species.
#' @param mass optional vector to filter the displayed data to specific masses
#'   (e.g. `44` or `c(44, 45)`); default `NULL` shows all masses.
#' @param facet column or expression to facet by (default: `file_name`). A
#'   plain column or expression (e.g. `file_name` or `paste(species, mass)`) is
#'   faceted with [ggplot2::facet_wrap()]; a two-sided formula (e.g.
#'   `species ~ mass`) is faceted with [ggplot2::facet_grid()]. Set to `NULL`
#'   to suppress faceting.
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
#'   `trace`, the per-species/mass trace identifier, e.g. `"CO2: 44"`)
#' @param linetype column or expression for the linetype aesthetic (default:
#'   `NULL`, i.e. no linetype aesthetic)
#' @param color_values named or unnamed character vector of colours passed to
#'   [ggplot2::scale_color_manual()], or `NULL` to use the ggplot2 default
#'   colour palette (default: [palette.colors()])
#' @param scientific whether to format y axis labels in scientific notation
#'   (default: `FALSE`)
#' @param ... additional arguments passed on to [ggplot2::facet_wrap()] or
#'   [ggplot2::facet_grid()] (e.g. `labeller`)
#' @param time_window optional numeric vector of length 2 giving the time axis
#'   display window `c(min, max)` in seconds (must have `min < max`). The data
#'   point just outside each edge of the window is retained so the clipped lines
#'   interpolate correctly across the window boundaries and y autoscales correctly
#'   at the edges; [ggplot2::coord_cartesian()] clips the display. A window that
#'   contains no data points of its own is allowed (the line is drawn between the
#'   bracketing points). Default `NULL` shows the full time range.
#' @param short_time_labels whether to use compact time axis labels with no
#'   space between value and unit and abbreviated units (`hr`, `m`, `s`)
#'   (default: `FALSE`)
#' @param n_time_breaks desired number of time axis tick marks (default: `5`)
#' @param n_y_breaks desired number of y axis tick marks (default: `5`)
#' @param theme ggplot2 theme to apply (default: [ir_default_theme()])
#' @return a `ggplot` object. To further customize the plot by adding ggplot2
#'   layers (e.g. `+ ggplot2::labs(...)`), attach ggplot2 with
#'   `library(ggplot2)` first.
#' @export
ir_plot_continuous_flow <- function(
  dataset,
  species = NULL,
  mass = NULL,
  facet = file_name,
  scales = "free",
  nrow = NULL,
  ncol = 1,
  color = trace,
  linetype = NULL,
  color_values = palette.colors(),
  scientific = FALSE,
  time_window = NULL,
  short_time_labels = FALSE,
  n_time_breaks = 5,
  n_y_breaks = 5,
  theme = ir_default_theme(),
  ...
) {
  # safety checks
  if (!missing(dataset) && is(dataset, "ir_isofiles")) {
    cli_abort(
      c(
        "{.arg dataset} is a raw isofiles object and cannot be plotted directly",
        "i" = "aggregate it first with {.code ir_aggregate_isofiles(dataset)}"
      )
    )
  }
  check_arg(
    dataset,
    !missing(dataset) &&
      (is.data.frame(dataset) || is(dataset, "ir_aggregated_data")),
    "must be a data frame or a set of aggregated isofiles"
  )
  check_arg(
    species,
    is.null(species) || is.character(species) || is.numeric(species),
    "must be NULL or a character/numeric vector"
  )
  check_arg(
    mass,
    is.null(mass) || is.character(mass) || is.numeric(mass),
    "must be NULL or a character/numeric vector"
  )
  check_arg(
    color_values,
    is.null(color_values) || is.character(color_values),
    "must be NULL or a character vector of colours"
  )
  check_arg(scientific, rlang::is_bool(scientific), "must be TRUE or FALSE")
  check_arg(
    short_time_labels,
    rlang::is_bool(short_time_labels),
    "must be TRUE or FALSE"
  )
  check_arg(
    time_window,
    is.null(time_window) ||
      (is.numeric(time_window) &&
        length(time_window) == 2 &&
        time_window[1] < time_window[2]),
    "must be NULL or a numeric vector of length 2 with min < max"
  )
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

  # capture aesthetics before any data manipulation
  facet_quo <- rlang::enquo(facet)
  color_quo <- rlang::enquo(color)
  linetype_quo <- rlang::enquo(linetype)

  # prepare plot data
  if (is(dataset, "ir_aggregated_data")) {
    if (
      !"traces" %in% names(dataset) ||
        ncol(dataset$traces) == 0 ||
        nrow(dataset$traces) == 0
    ) {
      cli_abort(
        c(
          "no traces available in the provided {.field dataset}",
          "i" = "make sure you are reading continuous flow isofiles and the aggregator includes columns from {.field traces}"
        )
      )
    }
    meta_extra_cols <- setdiff(names(dataset$metadata), names(dataset$traces))
    plot_data <- dplyr::inner_join(
      dataset$traces,
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
    cli_abort("no data to plot (0 rows)")
  }

  # require time.s and mass columns
  missing_cols <- setdiff(c("time.s", "mass"), names(plot_data))
  if (length(missing_cols) > 0) {
    cli_abort(
      c(
        "trace data is missing required {qty(length(missing_cols))}column{?s}: {.field {missing_cols}}",
        "i" = "make sure the aggregator includes {.field time.s} and {.field mass}"
      )
    )
  }
  time_col <- "time.s"
  time_units <- "s"

  # filter to the requested species / mass
  plot_data <- filter_plot_data(plot_data, species, mass)

  # detect intensity column
  intensity_cols <- grep("^intensity\\.", names(plot_data), value = TRUE)
  if (length(intensity_cols) == 0) {
    cli_abort(
      c(
        "no intensity column found in trace data",
        "i" = "expected a column whose name matches {.code intensity.*}"
      )
    )
  }
  intensity_col <- intensity_cols[1]
  intensity_units <- sub("^intensity\\.", "", intensity_col)

  # sort mass as a factor in numerical order
  if (!is.factor(plot_data$mass)) {
    mass_levels <- as.character(sort(
      unique(as.numeric(plot_data$mass)),
      na.last = TRUE
    ))
    plot_data <- dplyr::mutate(
      plot_data,
      mass = factor(.data$mass, levels = mass_levels)
    )
  }

  # sort trace as a factor in numerical order of the trailing mass number
  plot_data <- sort_trace_factor(plot_data)

  # time window: applied after the mass/trace factors are built (above) so they
  # keep their full levels and the colour mapping stays stable when zoomed. An
  # empty window (no data points inside it) is fine - the line still interpolates
  # from the bracketing points just outside the window (see apply_plot_window()).
  if (!is.null(time_window)) {
    plot_data <- apply_plot_window(
      plot_data,
      time_col,
      time_window,
      c("uidx", "analysis", "species", "channel", "mass")
    )
  }

  # validate aesthetic expressions against the actual plot data
  check_aes_expr(color_quo, "color", plot_data)
  check_aes_expr(linetype_quo, "linetype", plot_data)

  # group aesthetic: always set to ensure one line per trace
  group_cols <- intersect(
    c("uidx", "analysis", "species", "mass"),
    names(plot_data)
  )

  # axis labels
  x_lab <- paste0("time [", time_units, "]")
  y_lab <- paste0("intensity [", intensity_units, "]")

  # base plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::aes(
      x = !!sym(time_col),
      y = !!sym(intensity_col),
      group = interaction(!!!rlang::syms(group_cols))
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
      labels = if (scientific) label_scientific() else ggplot2::waiver(),
      # 0 pinned to the bottom when included; both-ends headroom when zoomed in
      expand = if (!is.null(time_window)) {
        ggplot2::expansion(mult = c(0.05, 0.05))
      } else {
        ggplot2::expansion(mult = c(0, 0.05))
      }
    )

  # include 0 in the y range, unless a time window is set (then autoscale to it)
  if (is.null(time_window)) {
    p <- p + ggplot2::expand_limits(y = 0)
  }

  # additional aesthetics
  p <- add_color_aes(p, color_quo, color_values, plot_data)
  if (!rlang::quo_is_null(linetype_quo)) {
    p <- p + ggplot2::aes(linetype = !!linetype_quo)
  }

  # facets
  p <- add_facets(
    p,
    facet_quo,
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

  p <- p + theme

  return(p)
}

#' Plot dual inlet cycle data
#'
#' Plots cycle data from an [ir_aggregate_isofiles()] result or a plain data
#' frame. When `dataset` is an `ir_aggregated_data` object, the `$cycles`
#' dataset is inner-joined with `$metadata` (bringing in all metadata columns
#' not already present in `$cycles`) before plotting. The plot data must contain
#' `cycle`, `type`, `mass`, and an `intensity.*` column — an error is thrown if
#' any are missing. The intensity unit suffix becomes the y axis label. If
#' `mass` is not already a factor it is converted to one with levels sorted in
#' numerical order.
#'
#' @param dataset an `ir_aggregated_data` object from [ir_aggregate_isofiles()]
#'   or a plain data frame with `cycle`, `type`, `mass`, and an
#'   `intensity.*` column
#' @param species optional vector to filter the displayed data to specific
#'   species (e.g. `"CO2"` or `c("N2", "CO2")`); default `NULL` shows all species.
#' @param mass optional vector to filter the displayed data to specific masses
#'   (e.g. `44` or `c(44, 45)`); default `NULL` shows all masses.
#' @param facet column or expression to facet by (default: `file_name`). A
#'   plain column or expression (e.g. `file_name` or `paste(species, mass)`) is
#'   faceted with [ggplot2::facet_wrap()]; a two-sided formula (e.g.
#'   `species ~ mass`) is faceted with [ggplot2::facet_grid()]. Set to `NULL`
#'   to suppress faceting.
#' @param scales whether facet scales should be `"free"` (default), `"fixed"`,
#'   `"free_x"`, or `"free_y"`; passed on to [ggplot2::facet_wrap()] /
#'   [ggplot2::facet_grid()].
#' @param nrow,ncol number of rows and columns of facet panels (`nrow` default
#'   `NULL` lets ggplot2 choose; `ncol` default `1` stacks the panels in a
#'   single column). Only applies when `facet` is a single variable or
#'   expression (faceted with [ggplot2::facet_wrap()]); ignored when `facet` is a
#'   formula (faceted with [ggplot2::facet_grid()]), with a warning if you set
#'   them explicitly.
#' @param color column or expression for the colour aesthetic (default: `mass`)
#' @param shape column or expression for the point shape aesthetic (default:
#'   `type`, distinguishing `"standard"` from `"sample"` cycles)
#' @param linetype column or expression for the linetype aesthetic (default:
#'   `species`)
#' @param color_values named or unnamed character vector of colours passed to
#'   [ggplot2::scale_color_manual()], or `NULL` to use the ggplot2 default
#'   colour palette (default: [palette.colors()])
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
#' @param theme ggplot2 theme to apply (default: [ir_default_theme()])
#' @param ... additional arguments passed on to [ggplot2::facet_wrap()] or
#'   [ggplot2::facet_grid()] (e.g. `labeller`)
#' @return a `ggplot` object. To further customize the plot by adding ggplot2
#'   layers (e.g. `+ ggplot2::labs(...)`), attach ggplot2 with
#'   `library(ggplot2)` first.
#' @export
ir_plot_dual_inlet <- function(
  dataset,
  species = NULL,
  mass = NULL,
  facet = file_name,
  scales = "free",
  nrow = NULL,
  ncol = 1,
  color = trace,
  shape = type,
  linetype = NULL,
  color_values = palette.colors(),
  scientific = FALSE,
  cycle_window = NULL,
  n_y_breaks = 5,
  theme = ir_default_theme(),
  ...
) {
  # safety checks
  if (!missing(dataset) && is(dataset, "ir_isofiles")) {
    cli_abort(
      c(
        "{.arg dataset} is a raw isofiles object and cannot be plotted directly",
        "i" = "aggregate it first with {.code ir_aggregate_isofiles(dataset)}"
      )
    )
  }
  check_arg(
    dataset,
    !missing(dataset) &&
      (is.data.frame(dataset) || is(dataset, "ir_aggregated_data")),
    "must be a data frame or a set of aggregated isofiles"
  )
  check_arg(
    species,
    is.null(species) || is.character(species) || is.numeric(species),
    "must be NULL or a character/numeric vector"
  )
  check_arg(
    mass,
    is.null(mass) || is.character(mass) || is.numeric(mass),
    "must be NULL or a character/numeric vector"
  )
  check_arg(
    color_values,
    is.null(color_values) || is.character(color_values),
    "must be NULL or a character vector of colours"
  )
  check_arg(scientific, rlang::is_bool(scientific), "must be TRUE or FALSE")
  check_arg(
    n_y_breaks,
    rlang::is_scalar_integerish(n_y_breaks) && n_y_breaks > 0,
    "must be a positive whole number"
  )
  check_arg(scales, rlang::is_scalar_character(scales), "must be a string")
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

  # prepare plot data
  if (is(dataset, "ir_aggregated_data")) {
    if (
      !"cycles" %in% names(dataset) ||
        ncol(dataset$cycles) == 0 ||
        nrow(dataset$cycles) == 0
    ) {
      cli_abort(
        c(
          "no cycles available in the provided {.field dataset}",
          "i" = "make sure you are reading dual inlet isofiles and the aggregator includes columns from {.field cycles}"
        )
      )
    }
    meta_extra_cols <- setdiff(names(dataset$metadata), names(dataset$cycles))
    plot_data <- dplyr::inner_join(
      dataset$cycles,
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
    cli_abort("no data to plot (0 rows)")
  }

  # require cycle, type, and mass columns
  missing_cols <- setdiff(c("cycle", "type", "mass"), names(plot_data))
  if (length(missing_cols) > 0) {
    cli_abort(
      c(
        "cycle data is missing required {qty(length(missing_cols))}column{?s}: {.field {missing_cols}}",
        "i" = "make sure the aggregator includes {.field cycle}, {.field type}, and {.field mass}"
      )
    )
  }

  # filter to the requested species / mass
  plot_data <- filter_plot_data(plot_data, species, mass)

  # require an intensity.UNITS column
  intensity_cols <- grep("^intensity\\.", names(plot_data), value = TRUE)
  if (length(intensity_cols) == 0) {
    cli_abort(
      c(
        "no intensity column found in cycle data",
        "i" = "expected a column whose name matches {.code intensity.*}"
      )
    )
  }
  intensity_col <- intensity_cols[1]
  intensity_units <- sub("^intensity\\.", "", intensity_col)

  y_lab <- paste0("intensity [", intensity_units, "]")

  # sort mass as a factor in numerical order
  if (!is.factor(plot_data$mass)) {
    mass_levels <- as.character(sort(
      unique(as.numeric(plot_data$mass)),
      na.last = TRUE
    ))
    plot_data <- dplyr::mutate(
      plot_data,
      mass = factor(.data$mass, levels = mass_levels)
    )
  }

  # sort trace as a factor in numerical order of the trailing mass number
  plot_data <- sort_trace_factor(plot_data)

  # cycle window: applied after the mass/trace factors are built (above) so they
  # keep their full levels and the colour mapping stays stable when zoomed. An
  # empty window (no data points inside it) is fine - the line still interpolates
  # from the bracketing points just outside the window (see apply_plot_window()).
  if (!is.null(cycle_window)) {
    plot_data <- apply_plot_window(
      plot_data,
      "cycle",
      cycle_window,
      c("uidx", "analysis", "species", "channel", "mass", "type")
    )
  }

  # validate aesthetic expressions against the actual plot data
  check_aes_expr(color_quo, "color", plot_data)
  check_aes_expr(shape_quo, "shape", plot_data)
  check_aes_expr(linetype_quo, "linetype", plot_data)

  # group aesthetic: always set to ensure one line per cycle trace
  group_cols <- intersect(
    c("uidx", "analysis", "species", "mass", "type"),
    names(plot_data)
  )

  # base plot
  p <- ggplot2::ggplot(plot_data) +
    ggplot2::aes(
      x = !!sym("cycle"),
      y = !!sym(intensity_col),
      group = interaction(!!!rlang::syms(group_cols))
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
      labels = if (scientific) label_scientific() else ggplot2::waiver(),
      # 0 pinned to the bottom when included; both-ends headroom when zoomed in
      expand = if (!is.null(cycle_window)) {
        ggplot2::expansion(mult = c(0.05, 0.05))
      } else {
        ggplot2::expansion(mult = c(0, 0.05))
      }
    )

  # include 0 in the y range, unless a cycle window is set (then autoscale to it)
  if (is.null(cycle_window)) {
    p <- p + ggplot2::expand_limits(y = 0)
  }

  # additional aesthetics
  if (!rlang::quo_is_null(color_quo)) {
    p <- p + ggplot2::aes(color = !!color_quo)
    if (!is.null(color_values)) {
      # only apply the manual palette if it provides enough colours for the
      # number of distinct colour groups; otherwise fall back to the default
      # ggplot2 colour scale (which generates as many distinct hues as needed)
      n_colors <- dplyr::n_distinct(rlang::eval_tidy(color_quo, plot_data))
      if (length(color_values) >= n_colors) {
        p <- p + scale_color_manual(values = color_values)
      }
    }
  }
  if (!rlang::quo_is_null(shape_quo)) {
    p <- p + ggplot2::aes(shape = !!shape_quo)
  }
  if (!rlang::quo_is_null(linetype_quo)) {
    p <- p + ggplot2::aes(linetype = !!linetype_quo)
  }

  # facets
  p <- add_facets(
    p,
    facet_quo,
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

  p <- p + theme

  return(p)
}
