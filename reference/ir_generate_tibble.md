# Generate the tibble used by the plotting functions

These helpers build the exact flat tibble that
[`ir_plot_continuous_flow()`](https://isoreader2.isoverse.org/reference/ir_plot_continuous_flow.md)
(`ir_generate_traces_tibble()`),
[`ir_plot_dual_inlet()`](https://isoreader2.isoverse.org/reference/ir_plot_dual_inlet.md)
(`ir_generate_cycles_tibble()`), and
[`ir_plot_scans()`](https://isoreader2.isoverse.org/reference/ir_plot_scans.md)
(`ir_generate_scans_tibble()`) plot, so it can be inspected or used
independently of producing a plot. The `dataset` is prepared exactly as
for the plotting functions (an `ir_aggregated_data` object has its
`traces` / `cycles` / `scans` dataset inner-joined with `$metadata`; a
plain data frame is used as is), filtered by `species`, and then split
into intensity rows and (optionally) ratio rows, each augmented with
three columns:

- `trace` - the identifier `"<species>: <mass>"` for intensity rows
  (e.g. `"CO2: 44"`) or `"<species>: <ratio_name>"` for ratio rows (e.g.
  `"CO2: 45/44"`), always (re)generated and returned as a factor sorted
  by species and numerical (numerator) mass.

- `data_type` - `"intensity [UNITS]"` (e.g. `"intensity [mV]"`) for the
  intensity rows, or `"ratios"` for ratio rows.

- `value` - the value to plot: the intensity for intensity rows, or the
  (optionally fold-clamped) ratio for ratio rows.

## Usage

``` r
ir_generate_traces_tibble(dataset, species = NULL, mass = NULL, ratio = NULL)

ir_generate_cycles_tibble(dataset, species = NULL, mass = NULL, ratio = NULL)

ir_generate_scans_tibble(dataset, species = NULL, mass = NULL, ratio = NULL)
```

## Arguments

- dataset:

  an `ir_aggregated_data` object from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  or a plain data frame with the required columns (see the matching
  plotting function)

- species:

  optional vector to filter to specific species (e.g. `"CO2"` or
  `c("N2", "CO2")`); default `NULL` keeps all species.

- mass:

  which masses to include as intensity traces: `NULL` (default) for all
  masses, a vector (e.g. `44` or `c(44, 45)`) for specific masses, or a
  zero-length vector (`numeric(0)`/`character(0)`) for none. Note that
  [`c()`](https://rdrr.io/r/base/c.html) is `NULL` in R, i.e. all
  masses.

- ratio:

  which ratios to include (computed with
  [`ir_calculate_ratios()`](https://isoreader2.isoverse.org/reference/ir_calculate_ratios.md)):
  `NULL` (default) for all available ratios, a character vector of ratio
  names (e.g. `c("45/44", "46/44")`) for specific ones, or
  `character(0)` for none. Requesting specific ratio names when ratios
  have not been calculated is an error pointing to
  [`ir_calculate_ratios()`](https://isoreader2.isoverse.org/reference/ir_calculate_ratios.md);
  with `ratio = NULL` and no ratios present none are simply added.

## Value

a tibble with the prepared data plus the `trace`, `data_type`, and
`value` columns described above.
