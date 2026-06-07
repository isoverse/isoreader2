# Get data frame from aggregated data

Retrieve a specific subset of the aggregated data into a single data
frame by specifying which columns to take from each dataset (metadata,
traces, cycles, scans, resistors) using
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
  by = c("uidx", "analysis", "config", "species", "channel", "mass")
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

## Value

a tibble
