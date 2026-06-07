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
#' @param facet_text_size font size for facet strip labels in points (default: `20`)
#' @return a `ggplot2` theme object
#' @export
ir_default_theme <- function(text_size = 16, facet_text_size = 20) {
  check_arg(
    text_size,
    is.numeric(text_size) && length(text_size) == 1L,
    "must be a single number"
  )
  check_arg(
    facet_text_size,
    is.numeric(facet_text_size) && length(facet_text_size) == 1L,
    "must be a single number"
  )
  ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      strip.text = ggplot2::element_text(size = facet_text_size),
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank()
    )
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
#' @param panel column or expression for faceting (default: `file_name`). Set
#'   to `NULL` to suppress panels.
#' @param color column or expression for the colour aesthetic (default: `mass`)
#' @param linetype column or expression for the linetype aesthetic (default:
#'   `species`)
#' @param color_values named or unnamed character vector of colours passed to
#'   [ggplot2::scale_color_manual()], or `NULL` to use the ggplot2 default
#'   colour palette (default: [palette.colors()])
#' @param scientific whether to format y axis labels in scientific notation
#'   (default: `FALSE`)
#' @param x_window optional numeric vector of length 2 giving the x axis
#'   display window `c(min, max)`. Data just outside the window is retained for
#'   correct y autoscaling at the edges; [ggplot2::coord_cartesian()] clips the
#'   display. Default `NULL` shows the full x range.
#' @param n_x_breaks desired number of x axis tick marks (default: `5`)
#' @param n_y_breaks desired number of y axis tick marks (default: `5`)
#' @param theme ggplot2 theme to apply (default: [ir_default_theme()])
#' @return a `ggplot` object
#' @export
ir_plot_scans <- function(
  dataset,
  scan_type = NULL,
  panel = file_name,
  color = mass,
  linetype = species,
  color_values = palette.colors(),
  scientific = FALSE,
  x_window = NULL,
  n_x_breaks = 5,
  n_y_breaks = 5,
  theme = ir_default_theme()
) {
  # safety checks
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
    color_values,
    is.null(color_values) || is.character(color_values),
    "must be NULL or a character vector of colours"
  )
  check_arg(scientific, rlang::is_bool(scientific), "must be TRUE or FALSE")
  check_arg(
    x_window,
    is.null(x_window) || (is.numeric(x_window) && length(x_window) == 2),
    "must be NULL or a numeric vector of length 2 (min, max)"
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

  # capture aesthetics before any data manipulation
  panel_quo <- rlang::enquo(panel)
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

  # x window: keep data just outside the limits for correct y autoscaling at edges
  if (!is.null(x_window)) {
    x_range <- range(plot_data$x, na.rm = TRUE)
    line_groups <- intersect(
      c("uidx", "analysis", "species", "channel"),
      names(plot_data)
    )
    plot_data <- plot_data |>
      dplyr::arrange(!!!rlang::syms(c(line_groups, "x"))) |>
      dplyr::mutate(
        in_range = .data$x >= x_window[1] & .data$x <= x_window[2]
      ) |>
      dplyr::mutate(
        .by = line_groups,
        just_before = !.data$in_range &
          dplyr::lag(.data$in_range, default = FALSE),
        just_after = !.data$in_range &
          dplyr::lead(.data$in_range, default = FALSE)
      ) |>
      dplyr::filter(.data$in_range | .data$just_before | .data$just_after) |>
      dplyr::select(-"in_range", -"just_before", -"just_after")
    if (nrow(plot_data) == 0) {
      cli_abort(
        c(
          "{.arg x_window} [{x_window[1]}, {x_window[2]}] contains no data",
          "i" = "the x range in the data is [{x_range[1]}, {x_range[2]}]"
        )
      )
    }
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
    mass_levels <- as.character(sort(unique(as.numeric(plot_data$mass)), na.last = TRUE))
    plot_data <- dplyr::mutate(plot_data, mass = factor(.data$mass, levels = mass_levels))
  }

  # validate aesthetic expressions against the actual plot data
  check_aes_expr(panel_quo, "panel", plot_data)
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
      labels = if (scientific) label_scientific() else ggplot2::waiver()
    ) +
    ggplot2::expand_limits(y = 0)

  # additional aesthetics
  if (!rlang::quo_is_null(color_quo)) {
    p <- p + ggplot2::aes(color = !!color_quo)
    if (!is.null(color_values)) {
      p <- p + scale_color_manual(values = color_values)
    }
  }
  if (!rlang::quo_is_null(linetype_quo)) {
    p <- p + ggplot2::aes(linetype = !!linetype_quo)
  }

  # facets
  if (!rlang::quo_is_null(panel_quo)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!panel_quo), scales = "free")
  }

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
#' @param panel column or expression for faceting (default: `file_name`). Set
#'   to `NULL` to suppress panels.
#' @param color column or expression for the colour aesthetic (default: `mass`)
#' @param linetype column or expression for the linetype aesthetic (default:
#'   `species`)
#' @param color_values named or unnamed character vector of colours passed to
#'   [ggplot2::scale_color_manual()], or `NULL` to use the ggplot2 default
#'   colour palette (default: [palette.colors()])
#' @param scientific whether to format y axis labels in scientific notation
#'   (default: `FALSE`)
#' @param time_window optional numeric vector of length 2 giving the time axis
#'   display window `c(min, max)` in seconds. Data just outside the window is
#'   retained for correct y autoscaling at the edges;
#'   [ggplot2::coord_cartesian()] clips the display. Default `NULL` shows the
#'   full time range.
#' @param short_time_labels whether to use compact time axis labels with no
#'   space between value and unit and abbreviated units (`hr`, `m`, `s`)
#'   (default: `FALSE`)
#' @param n_time_breaks desired number of time axis tick marks (default: `5`)
#' @param n_y_breaks desired number of y axis tick marks (default: `5`)
#' @param theme ggplot2 theme to apply (default: [ir_default_theme()])
#' @return a `ggplot` object
#' @export
ir_plot_continuous_flow <- function(
  dataset,
  panel = file_name,
  color = mass,
  linetype = species,
  color_values = palette.colors(),
  scientific = FALSE,
  time_window = NULL,
  short_time_labels = FALSE,
  n_time_breaks = 5,
  n_y_breaks = 5,
  theme = ir_default_theme()
) {
  # safety checks
  check_arg(
    dataset,
    !missing(dataset) &&
      (is.data.frame(dataset) || is(dataset, "ir_aggregated_data")),
    "must be a data frame or a set of aggregated isofiles"
  )
  check_arg(
    color_values,
    is.null(color_values) || is.character(color_values),
    "must be NULL or a character vector of colours"
  )
  check_arg(scientific, rlang::is_bool(scientific), "must be TRUE or FALSE")
  check_arg(short_time_labels, rlang::is_bool(short_time_labels), "must be TRUE or FALSE")
  check_arg(
    time_window,
    is.null(time_window) ||
      (is.numeric(time_window) && length(time_window) == 2),
    "must be NULL or a numeric vector of length 2 (min, max)"
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

  # capture aesthetics before any data manipulation
  panel_quo <- rlang::enquo(panel)
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

  # time window: keep data just outside the limits for correct y autoscaling at edges
  if (!is.null(time_window)) {
    time_range <- range(plot_data[[time_col]], na.rm = TRUE)
    line_groups <- intersect(
      c("uidx", "analysis", "species", "channel"),
      names(plot_data)
    )
    plot_data <- plot_data |>
      dplyr::arrange(!!!rlang::syms(c(line_groups, time_col))) |>
      dplyr::mutate(
        in_range = .data[[time_col]] >= time_window[1] &
          .data[[time_col]] <= time_window[2]
      ) |>
      dplyr::mutate(
        .by = line_groups,
        just_before = !.data$in_range &
          dplyr::lag(.data$in_range, default = FALSE),
        just_after = !.data$in_range &
          dplyr::lead(.data$in_range, default = FALSE)
      ) |>
      dplyr::filter(.data$in_range | .data$just_before | .data$just_after) |>
      dplyr::select(-"in_range", -"just_before", -"just_after")
    if (nrow(plot_data) == 0) {
      cli_abort(
        c(
          "{.arg time_window} [{time_window[1]}, {time_window[2]}] contains no data",
          "i" = "the time range in the data is [{time_range[1]}, {time_range[2]}]"
        )
      )
    }
  }

  # sort mass as a factor in numerical order
  if (!is.factor(plot_data$mass)) {
    mass_levels <- as.character(sort(unique(as.numeric(plot_data$mass)), na.last = TRUE))
    plot_data <- dplyr::mutate(plot_data, mass = factor(.data$mass, levels = mass_levels))
  }

  # validate aesthetic expressions against the actual plot data
  check_aes_expr(panel_quo, "panel", plot_data)
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
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::expand_limits(y = 0)

  # additional aesthetics
  if (!rlang::quo_is_null(color_quo)) {
    p <- p + ggplot2::aes(color = !!color_quo)
    if (!is.null(color_values)) {
      p <- p + scale_color_manual(values = color_values)
    }
  }
  if (!rlang::quo_is_null(linetype_quo)) {
    p <- p + ggplot2::aes(linetype = !!linetype_quo)
  }

  # facets
  if (!rlang::quo_is_null(panel_quo)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!panel_quo), scales = "free")
  }

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
#' @param panel column or expression for faceting (default: `file_name`). Set
#'   to `NULL` to suppress panels.
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
#' @param n_y_breaks desired number of y axis tick marks (default: `5`)
#' @param theme ggplot2 theme to apply (default: [ir_default_theme()])
#' @return a `ggplot` object
#' @export
ir_plot_dual_inlet <- function(
  dataset,
  panel = file_name,
  color = mass,
  shape = type,
  linetype = species,
  color_values = palette.colors(),
  scientific = FALSE,
  n_y_breaks = 5,
  theme = ir_default_theme()
) {
  # safety checks
  check_arg(
    dataset,
    !missing(dataset) &&
      (is.data.frame(dataset) || is(dataset, "ir_aggregated_data")),
    "must be a data frame or a set of aggregated isofiles"
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

  # capture aesthetics before any data manipulation
  panel_quo <- rlang::enquo(panel)
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
    mass_levels <- as.character(sort(unique(as.numeric(plot_data$mass)), na.last = TRUE))
    plot_data <- dplyr::mutate(plot_data, mass = factor(.data$mass, levels = mass_levels))
  }

  # validate aesthetic expressions against the actual plot data
  check_aes_expr(panel_quo, "panel", plot_data)
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
    ggplot2::labs(x = "Cycle", y = y_lab) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(1)) +
    ggplot2::scale_y_continuous(
      breaks = scales::pretty_breaks(n_y_breaks),
      labels = if (scientific) label_scientific() else ggplot2::waiver(),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::expand_limits(y = 0)

  # additional aesthetics
  if (!rlang::quo_is_null(color_quo)) {
    p <- p + ggplot2::aes(color = !!color_quo)
    if (!is.null(color_values)) {
      p <- p + scale_color_manual(values = color_values)
    }
  }
  if (!rlang::quo_is_null(shape_quo)) {
    p <- p + ggplot2::aes(shape = !!shape_quo)
  }
  if (!rlang::quo_is_null(linetype_quo)) {
    p <- p + ggplot2::aes(linetype = !!linetype_quo)
  }

  # facets
  if (!rlang::quo_is_null(panel_quo)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(!!panel_quo), scales = "free")
  }

  p <- p + theme

  return(p)
}
