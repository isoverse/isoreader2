# Aggregate data from isofiles

This function allows dynamic aggregation and validation of data read by
[`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md).
Like
[`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md),
it is designed to be fail save by safely catching errors and reporting
back on them (see
[`ir_get_problems()`](https://isoreader2.isoverse.org/reference/problems.md)).
This function should work out of the box for most files without
additional modification of the `aggregator`.

## Usage

``` r
ir_aggregate_isofiles(
  isofiles,
  aggregator = "standard",
  intensity_units = c("mV", "V", "fA", "pA", "nA", "µA", "mA", "A", "cps"),
  show_progress = is_interactive(),
  show_problems = TRUE
)
```

## Arguments

- isofiles:

  the isotope data files/archives read in by
  [`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md)

- aggregator:

  typically the name of a registered aggregator (see all with
  `ir_get_option("aggregators")`), default is the "standard" aggregator
  included in the package (`ir_get_aggregator("standard")`). Other
  options are "minimal" (`ir_get_aggregator("minimal")`) and "extended"
  (`ir_get_aggregator("extended")`). The `aggregator` parameter can can
  also directly be an aggregator tibble (created/modified with
  [`ir_start_aggregator()`](https://isoreader2.isoverse.org/reference/ir_aggregator.md)
  and/or
  [`ir_add_to_aggregator()`](https://isoreader2.isoverse.org/reference/ir_aggregator.md))
  that defines which data should be aggregated and how.

- intensity_units:

  target intensity unit to convert traces/cycles/scans to before
  aggregation, one of `"mV"`, `"V"`, `"fA"`, `"pA"`, `"nA"`, `"µA"`,
  `"mA"`, `"A"`, `"cps"`. Only applied when the aggregator includes any
  of those datasets. Default is `"mV"`.

- show_progress:

  whether to show a progress bar, by default always enabled when running
  interactively e.g. inside Positron or RStudio (and disabled in a
  notebook), turn off with `show_progress = FALSE`

- show_problems:

  whether to show problems encountered along the way (rather than just
  keeping track of them with
  [`ir_get_problems()`](https://isoreader2.isoverse.org/reference/problems.md)).
  Set to `show_problems = FALSE` to turn off the live printout. Either
  way, all encountered problems can be retrieved with running
  [`ir_get_problems()`](https://isoreader2.isoverse.org/reference/problems.md)
  for the returned list

## Value

a list of merged dataframes collected from the `isofiles` based on the
`aggregator` definitions
