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
  ...
)
```

## Arguments

- dataset:

  an `ir_aggregated_data` object from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  or a plain data frame with `time.s`, `mass`, and an `intensity.*`
  column

- species:

  optional vector to filter the displayed data to specific species (e.g.
  `"CO2"` or `c("N2", "CO2")`); default `NULL` shows all species.

- mass:

  optional vector to filter the displayed data to specific masses (e.g.
  `44` or `c(44, 45)`); default `NULL` shows all masses.

- facet:

  column or expression to facet by (default: `file_name`). A plain
  column or expression (e.g. `file_name` or `paste(species, mass)`) is
  faceted with
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html);
  a two-sided formula (e.g. `species ~ mass`) is faceted with
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).
  Set to `NULL` to suppress faceting.

- scales:

  whether facet scales should be `"free"` (default), `"fixed"`,
  `"free_x"`, or `"free_y"`; passed on to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  /
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).

- nrow, ncol:

  number of rows and columns of facet panels (`nrow` default `NULL` lets
  ggplot2 choose; `ncol` default `1` stacks the panels in a single
  column). Only applies when `facet` is a single variable or expression
  (faceted with
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html));
  ignored when `facet` is a formula (faceted with
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)),
  with a warning if you set them explicitly.

- color:

  column or expression for the colour aesthetic (default: `trace`, the
  per-species/mass trace identifier, e.g. `"CO2: 44"`)

- linetype:

  column or expression for the linetype aesthetic (default: `NULL`, i.e.
  no linetype aesthetic)

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
  window `c(min, max)` in seconds (must have `min < max`). The data
  point just outside each edge of the window is retained so the clipped
  lines interpolate correctly across the window boundaries and y
  autoscales correctly at the edges;
  [`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)
  clips the display. A window that contains no data points of its own is
  allowed (the line is drawn between the bracketing points). Default
  `NULL` shows the full time range.

- short_time_labels:

  whether to use compact time axis labels with no space between value
  and unit and abbreviated units (`hr`, `m`, `s`) (default: `FALSE`)

- n_time_breaks:

  desired number of time axis tick marks (default: `5`)

- n_y_breaks:

  desired number of y axis tick marks (default: `5`)

- ...:

  additional arguments passed on to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  or
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
  (e.g. `labeller`)

## Value

a `ggplot` object with
[`ir_default_theme()`](https://isoreader2.isoverse.org/reference/ir_default_theme.md)
applied. To customize the plot, add ggplot2 layers on top (e.g.
`+ ggplot2::theme(...)` or `+ ggplot2::labs(...)`); attach ggplot2 with
[`library(ggplot2)`](https://ggplot2.tidyverse.org) first.
