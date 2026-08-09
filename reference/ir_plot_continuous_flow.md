# Plot continuous flow data

Plots chromatographic trace data from an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result or a plain data frame. The data is prepared with
[`ir_generate_traces_tibble()`](https://isoreader2.isoverse.org/reference/ir_generate_tibble.md)
(which, for an `ir_aggregated_data` object, inner-joins the `$traces`
dataset with `$metadata`). The plot data must contain `species`,
`time.s`, `mass`, and an `intensity.*` column — an error is thrown if
any are missing. A `trace` identifier (`"<species>: <mass>"`) and the
matching `color` identifier are always regenerated and the plotted
`value` together with a `data_type` label (`"intensity [UNITS]"`, or
`"ratios"` for ratio rows) are added.

## Usage

``` r
ir_plot_continuous_flow(
  dataset,
  species = NULL,
  mass = everything(),
  ratio = everything(),
  facet = NULL,
  data_type_as_facet = auto(),
  scales = "free",
  nrow = NULL,
  ncol = 1,
  color = color,
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
)
```

## Arguments

- dataset:

  an `ir_aggregated_data` object from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  or a plain data frame with `species`, `time.s`, `mass`, and an
  `intensity.*` column

- species:

  optional vector to filter the displayed data to specific species (e.g.
  `"CO2"` or `c("N2", "CO2")`); default `NULL` shows all species.

- mass:

  which masses to show as intensity traces, as a
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html)
  expression evaluated as if the masses present in the data were column
  names. The default `everything()` shows every mass and `NULL` (or
  [`c()`](https://rdrr.io/r/base/c.html)) shows none; beyond that any
  tidyselect syntax works, e.g. `c("44", "45")` or `44:48` for specific
  masses, `-"45"`/`!"45"` to exclude one, and helpers such as
  `starts_with("4")`, `matches()`, `all_of()`, or `any_of()`. Unlike
  plain tidyselect, numbers select by name rather than by position
  (`44:48` means the masses 44 to 48, not the 44th to 48th mass).
  Selecting a mass that is not in the data is an error that lists the
  available masses; use `any_of()` to ignore missing ones.

- ratio:

  which ratios to additionally show (computed with
  [`ir_calculate_ratios()`](https://isoreader2.isoverse.org/reference/ir_calculate_ratios.md)),
  as a
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html)
  expression evaluated as if the ratio names present in the data were
  column names - the same syntax as `mass`, e.g. `everything()` (the
  default, all available ratios), `NULL` for none,
  `c("45/44", "46/44")`, `-"45/44"`, or `starts_with("45")`. Selecting
  specific ratios when ratios have not been calculated is an error
  pointing to
  [`ir_calculate_ratios()`](https://isoreader2.isoverse.org/reference/ir_calculate_ratios.md)
  (with the default `everything()`, or `NULL`, and no ratios present,
  none are simply added). Ratio rows are plotted on the same `value`
  axis with `data_type = "ratios"`; `data_type` is then used as a facet
  row (see `data_type_as_facet`) to separate them from the intensities.

- facet:

  column or expression to facet by (default: `NULL`, no extra faceting).
  When `data_type` is used as a facet row (see `data_type_as_facet`), a
  single `facet` variable becomes the facet_grid column
  (`data_type ~ facet`) and a `NULL` facet gives `data_type ~ .`.
  Otherwise a plain column or expression (e.g. `file_name` or
  `paste(species, mass)`) is faceted with
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html),
  and a two-sided formula (e.g. `species ~ mass`) is faceted with
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html).
  Set to `NULL` to suppress faceting.

- data_type_as_facet:

  whether the `data_type` column (intensities vs ratios) is used as the
  [`ggplot2::facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
  row variable:
  [`auto()`](https://isoreader2.isoverse.org/reference/auto.md)
  (default) uses it only when more than one data type is present; `TRUE`
  always uses it; `FALSE` never does. When used, the y axis label is
  dropped (the facet strip provides it) and the facet becomes
  `data_type ~ .` (a `NULL` `facet`) or `data_type ~ facet` (a
  single-variable `facet`). It is ignored when `facet` is a two-sided
  formula (a warning is issued if `data_type_as_facet = TRUE` is
  combined with a formula `facet`, since the two are mutually
  exclusive).

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

  column or expression for the colour aesthetic (default: the generated
  `color` column, which holds all the traces that share a species and
  (numerator) mass, e.g. `"N2: 29, 29/28"` for both the intensity trace
  `"N2: 29"` and its ratio trace `"N2: 29/28"`, so they are drawn in the
  same colour while remaining separate lines). The legend for it is
  labelled `trace`; use `color = trace` to give every trace its own
  colour instead.

- linetype:

  column or expression for the linetype aesthetic (default: `NULL`, i.e.
  no linetype aesthetic)

- color_values:

  named or unnamed character vector of colours passed to
  [`ggplot2::scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html),
  or `NULL` to use the ggplot2 default colour palette (default:
  [`palette.colors()`](https://rdrr.io/r/grDevices/palette.html))

- drop_unused_levels:

  whether to drop unused colour factor levels (e.g. traces that are
  absent after zooming to a window) from the colour scale and legend.
  Default `FALSE` keeps every level so the colour mapping stays stable
  across subsets of the same dataset; set to `TRUE` to show only the
  levels actually present in the plotted data. Note that a `color` level
  covers every trace of its species/mass, so it is only dropped when
  none of them is shown.

- scientific:

  whether to format y axis labels in scientific notation (default:
  `FALSE`)

- time_window.s, time_window.min:

  optional numeric vector of length 2 giving the time axis display
  window `c(min, max)`, either in seconds (`time_window.s`) or in
  minutes (`time_window.min`, converted to seconds internally — the
  function always works in seconds). Provide at most one; if both are
  given, `time_window.s` is used. Must have `min < max`. The data point
  just outside each edge of the window is retained so the clipped lines
  interpolate correctly across the window boundaries and y autoscales
  correctly at the edges;
  [`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html)
  clips the display. A window that contains no data points of its own is
  allowed (the line is drawn between the bracketing points). Default
  `NULL` (both) shows the full time range.

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
