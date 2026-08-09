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
four columns:

- `trace` - the identifier `"<species>: <mass>"` for intensity rows
  (e.g. `"CO2: 44"`) or `"<species>: <ratio_name>"` for ratio rows (e.g.
  `"CO2: 45/44"`), always (re)generated and returned as a factor sorted
  by species and numerical (numerator) mass. This is what the plotting
  functions group their lines by. Rows whose `species` is `NA` get the
  bare mass/ratio name instead (`"44"`, `"45/44"`) rather than an
  `"NA: "` prefix.

- `color` - the colour identifier, a factor listing every trace that
  shares a species and (numerator) mass: both `"CO2: 45"` and
  `"CO2: 45/44"` get `"CO2: 45, 45/44"` (or `"45, 45/44"` for an `NA`
  species). Mapping the colour aesthetic to this column (the plotting
  functions' default) is what draws an intensity trace and its ratios in
  the same colour while keeping them separate lines. Its levels follow
  the `trace` order, so the legend runs by ascending species/mass.

- `data_type` - `"intensity [UNITS]"` (e.g. `"intensity [mV]"`) for the
  intensity rows, or `"ratios"` for ratio rows.

- `value` - the value to plot: the intensity for intensity rows, or the
  (optionally fold-clamped) ratio for ratio rows.

## Usage

``` r
ir_generate_traces_tibble(
  dataset,
  species = NULL,
  mass = everything(),
  ratio = everything()
)

ir_generate_cycles_tibble(
  dataset,
  species = NULL,
  mass = everything(),
  ratio = everything()
)

ir_generate_scans_tibble(
  dataset,
  species = NULL,
  mass = everything(),
  ratio = everything()
)
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

  which masses to include as intensity traces, as a
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html)
  expression evaluated as if the masses present in the data were column
  names. The default `everything()` includes every mass and `NULL` (or
  [`c()`](https://rdrr.io/r/base/c.html)) includes none; beyond that any
  tidyselect syntax works, e.g. `c("44", "45")` or `44:48` for specific
  masses, `-"45"`/`!"45"` to exclude one, and helpers such as
  `starts_with("4")`, `matches()`, `all_of()`, or `any_of()`. Unlike
  plain tidyselect, numbers select by name rather than by position
  (`44:48` means the masses 44 to 48, not the 44th to 48th mass).
  Selecting a mass that is not in the data is an error that lists the
  available masses; use `any_of()` to ignore missing ones.

- ratio:

  which ratios to include (computed with
  [`ir_calculate_ratios()`](https://isoreader2.isoverse.org/reference/ir_calculate_ratios.md)),
  as a
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html)
  expression evaluated as if the ratio names present in the data were
  column names - the same syntax as `mass`, e.g. `everything()` (the
  default, all available ratios), `NULL` for none,
  `c("45/44", "46/44")`, `-"45/44"`, or `starts_with("45")`. Selecting
  specific ratios when ratios have not been calculated is an error
  pointing to
  [`ir_calculate_ratios()`](https://isoreader2.isoverse.org/reference/ir_calculate_ratios.md);
  with the default `everything()` (or `NULL`) and no ratios present,
  none are simply added.

## Value

a tibble with the prepared data plus the `trace`, `color`, `data_type`,
and `value` columns described above.
