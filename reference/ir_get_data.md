# Get data frame from aggregated data

Retrieve a specific subset of the aggregated data into a single data
frame by specifying which columns to take from each dataset (metadata,
traces, cycles, scans, resistors, vendor_data_table) using
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
syntax. If data from more than one dataset is selected (e.g. some
columns from `traces` AND some from `resistors`), the datasets are
combined with an
[`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
using the columns listed in `by` (only the ones actually in the
datasets). Joins that would lead to duplicated data entries (i.e.
many-to-many joins) are not allowed and will throw an error to avoid
unexpected replications of individual datapoints. If you really want to
do such a join, you'll have to do it manually.

## Usage

``` r
ir_get_data(
  aggregated_data,
  metadata = c("file_name"),
  traces = NULL,
  cycles = NULL,
  scans = NULL,
  resistors = NULL,
  vendor_data_table = NULL,
  by = c("uidx", "analysis", "config", "species", "channel", "mass")
)

ir_get_metadata(aggregated_data, metadata = everything())

ir_get_resistors(
  aggregated_data,
  metadata = c("file_name"),
  by = c("uidx", "analysis")
)

ir_get_traces(
  aggregated_data,
  metadata = c("file_name"),
  by = c("uidx", "analysis")
)

ir_get_cycles(
  aggregated_data,
  metadata = c("file_name"),
  by = c("uidx", "analysis")
)

ir_get_scans(
  aggregated_data,
  metadata = c("file_name"),
  by = c("uidx", "config")
)

ir_get_vendor_data_table(
  aggregated_data,
  metadata = c("file_name"),
  by = c("uidx", "analysis")
)
```

## Arguments

- aggregated_data:

  datasets aggregated from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)

- metadata:

  columns to get from the aggregated `metadata`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- traces:

  columns to get from the aggregated `traces`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- cycles:

  columns to get from the aggregated `cycles`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- scans:

  columns to get from the aggregated `scans`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- resistors:

  columns to get from the aggregated `resistors`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- vendor_data_table:

  columns to get from the aggregated `vendor_data_table`, all
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  syntax is supported

- by:

  character vector of column names used as join keys when combining data
  from more than one dataset (default covers the standard linking
  columns; only keys actually present in both datasets are used)

## Value

a tibble

## Functions

- `ir_get_metadata()`: shortcut for retrieving all `metadata` columns
  (i.e. `metadata = dplyr::everything()`)

- `ir_get_resistors()`: shortcut for retrieving all `resistors` columns
  (i.e. `resistors = dplyr::everything()`), keyed by the selected
  `metadata`

- `ir_get_traces()`: shortcut for retrieving all `traces` columns (i.e.
  `traces = dplyr::everything()`), keyed by the selected `metadata`

- `ir_get_cycles()`: shortcut for retrieving all `cycles` columns (i.e.
  `cycles = dplyr::everything()`), keyed by the selected `metadata`

- `ir_get_scans()`: shortcut for retrieving all `scans` columns (i.e.
  `scans = dplyr::everything()`), keyed by the selected `metadata`

- `ir_get_vendor_data_table()`: shortcut for retrieving all
  `vendor_data_table` columns (i.e.
  `vendor_data_table = dplyr::everything()`), keyed by the selected
  `metadata`. Only available when aggregated with the `"extended"`
  aggregator.
