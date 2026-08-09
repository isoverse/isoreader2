# Default isoreader2 plotting theme

This theme is always applied by the plotting functions
([`ir_plot_traces()`](https://isoreader2.isoverse.org/reference/ir_plot_traces.md),
[`ir_plot_dual_inlet()`](https://isoreader2.isoverse.org/reference/ir_plot_dual_inlet.md),
[`ir_plot_scans()`](https://isoreader2.isoverse.org/reference/ir_plot_scans.md)).
To customize a plot, add a
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
on top of the returned plot, e.g.
`ir_plot_traces(...) + ggplot2::theme(text = element_text(size = 20))`.

## Usage

``` r
ir_default_theme(text_size = 16)
```

## Arguments

- text_size:

  base font size in points (default: `16`)

## Value

a `ggplot2` theme object
