# Combine aggregated isofile data

Combine multiple
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
results into a single `ir_aggregated_data` object by row-binding each of
the contained datasets (`metadata`, `traces`, `cycles`, `scans`,
`resistors`, `vendor_data_table`, `problems`, ...) with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
Datasets present in only some of the objects are combined as well
(missing columns are filled with `NA`). The file index `uidx` is
re-numbered across the inputs so each file stays uniquely identified
(and its datasets stay correctly linked) in the combined object.

## Usage

``` r
# S3 method for class 'ir_aggregated_data'
c(...)
```

## Arguments

- ...:

  `ir_aggregated_data` objects to combine

## Value

a single combined `ir_aggregated_data` object
