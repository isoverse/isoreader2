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

Requires the suggested openxlsx package.

## Examples

``` r
# \donttest{
if (requireNamespace("openxlsx", quietly = TRUE)) {
  agg <- ir_examples_folder() |>
    ir_find_continuous_flow() |>
    ir_read_isofiles() |>
    ir_aggregate_isofiles()
  ir_export_to_excel(
    metadata = ir_get_metadata(agg),
    traces = ir_get_traces(agg),
    file = file.path(tempdir(), "my_export.xlsx")
  )
}
#> ✔ [1ms] ir_extract_isofiles() is finished, 0 files/archives required
#> (re-)extraction
#> ✔ [473ms] ir_read_isofiles() finished reading 2 isotope data files/archives
#> ✔ [249ms] ir_aggregate_isofiles() aggregated metadata (2) and traces (24.5k,
#> intensity in mV) from 2 files using the standard aggregator
#> ✔ [5ms] ir_get_data() retrieved 2 records from metadata
#> ✔ [10ms] ir_get_data() retrieved 24.5k records from the combination of metadata
#> (2) and traces (24.5k) via uidx and analysis
#> ✔ [1s] ir_export_to_excel() exported 2 rows of metadata and 24.5k rows of
#> traces to /tmp/Rtmp305jct/my_export.xlsx
# }
```
