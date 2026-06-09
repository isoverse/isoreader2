# Save and load isofiles

`ir_save_isofiles()` serializes a collection of isofiles read with
[`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md)
to an RDS file using
[`readr::write_rds()`](https://readr.tidyverse.org/reference/read_rds.html),
storing the whole `ir_isofiles` object as-is (including all nested
datasets and condition objects) without any changes.
`ir_load_isofiles()` reads the file back with
[`readr::read_rds()`](https://readr.tidyverse.org/reference/read_rds.html)
and returns the `ir_isofiles` object exactly as it was saved.

## Usage

``` r
ir_save_isofiles(isofiles, file)

ir_load_isofiles(file)
```

## Arguments

- isofiles:

  a collection of isofiles from
  [`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md)

- file:

  path to the RDS file (`.rds` extension added if absent)

## Value

`ir_save_isofiles()` returns `isofiles` invisibly; `ir_load_isofiles()`
returns an `ir_isofiles` object.

## Details

This operates at the unaggregated `ir_isofiles` level. To store an
aggregated result instead, use
[`ir_save_aggregated_data()`](https://isoreader2.isoverse.org/reference/ir_storage.md)
/
[`ir_load_aggregated_data()`](https://isoreader2.isoverse.org/reference/ir_storage.md).

## Functions

- `ir_save_isofiles()`: save isofiles to an RDS file

- `ir_load_isofiles()`: load isofiles from an RDS file
