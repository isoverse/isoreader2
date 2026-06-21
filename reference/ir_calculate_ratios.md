# Calculate isotope ratios

Calculate intensity ratios of each mass relative to a base mass for
every measurement in an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result. Ratios are added directly to the `traces` (continuous flow),
`cycles` (dual inlet), and/or `scans` data present in the aggregated
data as two extra columns: `ratio_name` (e.g. `"29/28"`) and `ratio`
(the intensity of that mass divided by the intensity of the base mass of
the same species at the same `time.s`/`cycle`/`x` position within every
`uidx` and `analysis`). Base mass rows are kept and have `NA` in both
columns. Calling this function again recomputes (overwrites) the
`ratio_name`/`ratio` columns.

## Usage

``` r
ir_calculate_ratios(aggregated_data, ...)
```

## Arguments

- aggregated_data:

  datasets aggregated from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  (must include at least one of `traces`, `cycles`, or `scans`)

- ...:

  named base masses for individual species, e.g. `SO2 = 64, N2 = 28`.
  Species not listed here use their numerically lowest measured mass as
  the base mass.

## Value

the `aggregated_data` with `ratio_name` and `ratio` columns added to
each of the `traces`, `cycles`, and/or `scans` datasets that is present.
Both columns are `NA` for base mass rows (and for any species whose
requested base mass could not be found).

## Details

The base mass for a species is, by default, the numerically lowest mass
measured for that species. Override it for individual species via `...`
(e.g. `SO2 = 64`, `N2 = 28`).
