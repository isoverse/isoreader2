# Save and load aggregated isofile data

`ir_save_aggregated_data()` serializes an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result to a parquet file. Empty datasets (no columns) are dropped. The
`condition` column of `problems` is set to `NULL` per row because R
condition objects cannot be stored in parquet.
`ir_load_aggregated_data()` reads the file back and returns an
`ir_aggregated_data` object.

## Usage

``` r
ir_save_aggregated_data(aggregated_data, file)

ir_load_aggregated_data(file)
```

## Arguments

- aggregated_data:

  datasets aggregated from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)

- file:

  path to the parquet file (`.parquet` extension added if absent)

## Value

`ir_save_aggregated_data()` returns `aggregated_data` invisibly;
`ir_load_aggregated_data()` returns an `ir_aggregated_data` object.

## Details

Requires the suggested arrow package.

## Functions

- `ir_save_aggregated_data()`: save aggregated data to a parquet file

- `ir_load_aggregated_data()`: load aggregated data from a parquet file
