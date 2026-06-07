# Plot scan data

Plots scan data from an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result or a plain data frame. When `dataset` is an `ir_aggregated_data`
object, the `$scans` dataset is inner-joined with `$metadata` (bringing
in all metadata columns not already present in `$scans`) before
plotting. The plot data must contain `x`, `scan_type`, `x_units`,
`mass`, and an `intensity.*` column — an error is thrown if any are
missing. The intensity unit suffix becomes the y axis label; `scan_type`
and `x_units` are combined for the x axis label. If `mass` is not
already a factor it is converted to one with levels sorted in numerical
order.

## Usage

``` r
ir_plot_scans(
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
)
```

## Arguments

- dataset:

  an `ir_aggregated_data` object from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  or a plain data frame with `x`, `scan_type`, `x_units`, `mass`, and an
  `intensity.*` column

- scan_type:

  which scan type to plot (e.g. `"high voltage"`). Required when the
  data contains more than one scan type; an error lists the available
  types. If the data contains only one scan type, the parameter must
  either be `NULL` or match that type exactly.

- panel:

  column or expression for faceting (default: `file_name`). Set to
  `NULL` to suppress panels.

- color:

  column or expression for the colour aesthetic (default: `mass`)

- linetype:

  column or expression for the linetype aesthetic (default: `species`)

- color_values:

  named or unnamed character vector of colours passed to
  [`ggplot2::scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html),
  or `NULL` to use the ggplot2 default colour palette (default:
  [`palette.colors()`](https://rdrr.io/r/grDevices/palette.html))

- scientific:

  whether to format y axis labels in scientific notation (default:
  `FALSE`)

- x_window:

  optional numeric vector of length 2 giving the x axis display window
  `c(min, max)`. Data just outside the window is retained for correct y
  autoscaling at the edges;
  [`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)
  clips the display. Default `NULL` shows the full x range.

- n_x_breaks:

  desired number of x axis tick marks (default: `5`)

- n_y_breaks:

  desired number of y axis tick marks (default: `5`)

- theme:

  ggplot2 theme to apply (default:
  [`ir_default_theme()`](https://isoreader2.isoverse.org/reference/ir_default_theme.md))

## Value

a `ggplot` object
