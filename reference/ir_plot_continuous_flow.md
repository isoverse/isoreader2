# Plot continuous flow data

Plots chromatographic trace data from an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result or a plain data frame. When `dataset` is an `ir_aggregated_data`
object, the `$traces` dataset is inner-joined with `$metadata` (bringing
in all metadata columns not already present in `$traces`) before
plotting. The plot data must contain `time.s`, `mass`, and an
`intensity.*` column — an error is thrown if any are missing. The
intensity unit suffix becomes the y axis label. If `mass` is not already
a factor it is converted to one with levels sorted in numerical order.

## Usage

``` r
ir_plot_continuous_flow(
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
)
```

## Arguments

- dataset:

  an `ir_aggregated_data` object from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  or a plain data frame with `time.s`, `mass`, and an `intensity.*`
  column

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

- time_window:

  optional numeric vector of length 2 giving the time axis display
  window `c(min, max)` in seconds. Data just outside the window is
  retained for correct y autoscaling at the edges;
  [`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)
  clips the display. Default `NULL` shows the full time range.

- short_time_labels:

  whether to use compact time axis labels with no space between value
  and unit and abbreviated units (`hr`, `m`, `s`) (default: `FALSE`)

- n_time_breaks:

  desired number of time axis tick marks (default: `5`)

- n_y_breaks:

  desired number of y axis tick marks (default: `5`)

- theme:

  ggplot2 theme to apply (default:
  [`ir_default_theme()`](https://isoreader2.isoverse.org/reference/ir_default_theme.md))

## Value

a `ggplot` object
