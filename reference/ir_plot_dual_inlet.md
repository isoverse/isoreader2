# Plot dual inlet cycle data

Plots cycle data from an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result or a plain data frame. When `dataset` is an `ir_aggregated_data`
object, the `$cycles` dataset is inner-joined with `$metadata` (bringing
in all metadata columns not already present in `$cycles`) before
plotting. The plot data must contain `cycle`, `type`, `mass`, and an
`intensity.*` column — an error is thrown if any are missing. The
intensity unit suffix becomes the y axis label. If `mass` is not already
a factor it is converted to one with levels sorted in numerical order.

## Usage

``` r
ir_plot_dual_inlet(
  dataset,
  facet = file_name,
  scales = "free",
  nrow = NULL,
  ncol = 1,
  color = trace,
  shape = type,
  linetype = NULL,
  color_values = palette.colors(),
  scientific = FALSE,
  n_y_breaks = 5,
  theme = ir_default_theme(),
  ...
)
```

## Arguments

- dataset:

  an `ir_aggregated_data` object from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  or a plain data frame with `cycle`, `type`, `mass`, and an
  `intensity.*` column

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
  ignored with a warning when `facet` is a formula (faceted with
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)).

- color:

  column or expression for the colour aesthetic (default: `mass`)

- shape:

  column or expression for the point shape aesthetic (default: `type`,
  distinguishing `"standard"` from `"sample"` cycles)

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

- n_y_breaks:

  desired number of y axis tick marks (default: `5`)

- theme:

  ggplot2 theme to apply (default:
  [`ir_default_theme()`](https://isoreader2.isoverse.org/reference/ir_default_theme.md))

- ...:

  additional arguments passed on to
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  or
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
  (e.g. `labeller`)

## Value

a `ggplot` object
