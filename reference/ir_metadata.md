# Filter, mutate, or join the metadata of aggregated isofiles

These functions operate on the `$metadata` component of an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result. `ir_filter_metadata()` additionally cascades the filter to all
other datasets: `traces`, `cycles`, and `scans` are filtered by the
remaining `uidx` + `analysis` combinations; `resistors` and `problems`
are filtered by the remaining `uidx` values. After filtering, columns
that are entirely `NA` across all remaining rows are dropped from every
dataset. All three functions also clear the *not-aggregated* column
information (columns present in the source files but not included in the
aggregator) from every dataset, since that information is no longer
meaningful after the metadata has been modified.

## Usage

``` r
ir_filter_metadata(aggregated_data, ...)

ir_mutate_metadata(aggregated_data, ...)

ir_join_metadata(aggregated_data, y, by)
```

## Arguments

- aggregated_data:

  datasets aggregated from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)

- ...:

  passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
  or
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  respectively

- y:

  data frame to join to the metadata

- by:

  character vector of columns to join by (passed to
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html))

## Value

the `aggregated_data` object with an updated `$metadata`

## Functions

- `ir_filter_metadata()`: filter rows of the metadata (and cascade to
  other datasets)

- `ir_mutate_metadata()`: add or modify columns in the metadata

- `ir_join_metadata()`: left-join additional columns into the metadata
