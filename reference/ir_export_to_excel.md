# Export data to Excel

Exports one or more data frames / tibbles (typically retrieved with the
`ir_get_*()` functions, e.g.
[`ir_get_metadata()`](https://isoreader2.isoverse.org/reference/ir_get_data.md),
[`ir_get_traces()`](https://isoreader2.isoverse.org/reference/ir_get_data.md))
to an Excel file, one sheet per data frame. Pass the data frames as
`...`: **named** arguments use the name as the sheet name, **unnamed**
arguments are placed in a sheet named after their position (e.g. the 3rd
unnamed data frame goes into `"Sheet3"`).

## Usage

``` r
ir_export_to_excel(
  ...,
  file,
  dbl_digits = 2,
  int_format = "0",
  dbl_format = sprintf(sprintf("%%.%sf", dbl_digits), 0),
  show_progress = is_interactive()
)
```

## Arguments

- ...:

  one or more data frames / tibbles to export, one per sheet. Named
  arguments set the sheet name; unnamed arguments use
  `"Sheet{position}"`.

- file:

  path to the `.xlsx` file (`.xlsx` extension added if absent)

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

the exported data invisibly (the single data frame if one was provided,
otherwise the list of data frames), for use in pipes

## Details

This function only accepts data frames. To store a complete
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result use
[`ir_save_aggregated_data()`](https://isoreader2.isoverse.org/reference/ir_storage.md)
instead.

Requires the openxlsx package. If not installed, one installation
attempt from CRAN is made automatically.

## Examples

``` r
if (FALSE) { # \dontrun{
agg <- ir_examples_folder() |>
  ir_find_continuous_flow() |>
  ir_read_isofiles() |>
  ir_aggregate_isofiles()
ir_export_to_excel(
  metadata = ir_get_metadata(agg),
  traces = ir_get_traces(agg),
  file = "my_export.xlsx"
)
} # }
```
