# Automatic / default behavior

A sentinel that requests automatic behavior for an argument (currently
the `data_type_as_facet` argument of the plotting functions
[`ir_plot_continuous_flow()`](https://isoreader2.isoverse.org/reference/ir_plot_continuous_flow.md),
[`ir_plot_dual_inlet()`](https://isoreader2.isoverse.org/reference/ir_plot_dual_inlet.md),
[`ir_plot_scans()`](https://isoreader2.isoverse.org/reference/ir_plot_scans.md)).
It is the default for those arguments; pass `TRUE`/`FALSE` to override
the automatic choice.

## Usage

``` r
auto()
```

## Value

an opaque sentinel of class `ir_auto`
