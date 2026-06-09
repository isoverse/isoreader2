# Filter, mutate, or join the metadata of isofiles

These functions modify the metadata of either an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result (`ir_aggregated_data`) or a collection of isofiles read with
[`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md)
(`ir_isofiles`).

## Usage

``` r
ir_filter_metadata(isofiles, ...)

ir_mutate_metadata(isofiles, ...)

ir_join_metadata(isofiles, y, by)
```

## Arguments

- isofiles:

  datasets aggregated from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  (`ir_aggregated_data`) or a collection of isofiles from
  [`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md)
  (`ir_isofiles`)

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

the `isofiles` object (of the same type as the input) with updated
metadata

## Details

For `ir_aggregated_data`, the operation is applied once to the combined
`$metadata` data frame. For `ir_filter_metadata()`, the filter then
cascades to all other datasets: `traces`, `cycles`, and `scans` are
filtered by the remaining `uidx` + `analysis` combinations; `resistors`
and `problems` are filtered by the remaining `uidx` values.

For `ir_isofiles`, the same operation is instead applied **individually
to each row** (i.e. to each file's own nested datasets), since an
`ir_isofiles` object has no combined metadata to operate on. Within each
row, the filter cascade uses whichever linking columns are present
(typically `analysis`). For `ir_filter_metadata()`, any file whose
metadata ends up with 0 rows after the filter is removed from the
`ir_isofiles` collection entirely.

Operating on an unaggregated `ir_isofiles` object is supported for
convenience, but is **significantly slower** than operating on an
`ir_aggregated_data` result, because the operation has to be carried out
separately on every file rather than once on the combined metadata. For
anything beyond small collections, prefer aggregating first with
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
and then applying these functions to the result.

After filtering, columns that are entirely `NA` across all remaining
rows are dropped from every (non-empty) dataset. All three functions
also clear the *not-aggregated* column information (columns present in
the source files but not included in the aggregator) from every dataset,
since that information is no longer meaningful after the metadata has
been modified.

## Functions

- `ir_filter_metadata()`: filter rows of the metadata (and cascade to
  the other datasets)

- `ir_mutate_metadata()`: add or modify columns in the metadata

- `ir_join_metadata()`: left-join additional columns into the metadata
