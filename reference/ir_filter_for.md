# Filter isofiles by measurement type

Convenience wrappers around
[`ir_filter_metadata()`](https://isoreader2.isoverse.org/reference/ir_metadata.md)
that keep only the files of a single measurement type (using the
metadata `type` column): continuous flow (`"cf"`), dual inlet (`"di"`),
or scan (`"scan"`). Like
[`ir_filter_metadata()`](https://isoreader2.isoverse.org/reference/ir_metadata.md)
they work on both `ir_isofiles` (from
[`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md))
and `ir_aggregated_data` (from
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md))
objects, cascade to the other datasets, and drop any file whose metadata
ends up empty.

## Usage

``` r
ir_filter_for_continuous_flow(isofiles)

ir_filter_for_dual_inlet(isofiles)

ir_filter_for_scans(isofiles)
```

## Arguments

- isofiles:

  a collection of isofiles from
  [`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md)
  (`ir_isofiles`) or datasets aggregated from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  (`ir_aggregated_data`)

## Value

the `isofiles` object filtered to the requested measurement type

## Functions

- `ir_filter_for_continuous_flow()`: keep only continuous flow files
  (`type == "cf"`)

- `ir_filter_for_dual_inlet()`: keep only dual inlet files
  (`type == "di"`)

- `ir_filter_for_scans()`: keep only scan files (`type == "scan"`)
