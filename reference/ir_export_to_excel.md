# Export data to Excel

Exports a data frame or an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result to an Excel file. Each dataset in the aggregated data becomes its
own sheet. The `include` parameter controls which datasets are exported
(default: all non-empty datasets). Recursively creates the output
directory if it does not exist.

## Usage

``` r
ir_export_to_excel(
  data,
  file,
  include = "metadata",
  dbl_digits = 2,
  int_format = "0",
  dbl_format = sprintf(sprintf("%%.%sf", dbl_digits), 0),
  show_progress = is_interactive()
)
```

## Arguments

- data:

  a data frame or an `ir_aggregated_data` object from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)

- file:

  path to the `.xlsx` file (`.xlsx` extension added if absent)

- include:

  for `ir_aggregated_data` only: character vector of dataset names to
  include as sheets. Default `"metadata"` exports only the metadata.

- dbl_digits:

  number of decimal places shown for double columns (all digits are
  stored; this only affects display formatting in Excel)

- int_format:

  Excel number format string for integer columns

- dbl_format:

  Excel number format string for double columns (derived automatically
  from `dbl_digits` if not set)

- show_progress:

  whether to show a progress indicator

## Value

`data` invisibly, for use in pipes

## Details

Requires the openxlsx package (suggested). If not installed, one
installation attempt from CRAN is made automatically.
