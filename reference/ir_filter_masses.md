# Filter isofiles by mass

Keeps only the requested masses in the `traces`, `cycles`, and `scans`
data of either an
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
result (`ir_aggregated_data`) or a collection of isofiles read with
[`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md)
(`ir_isofiles`). Like
[`ir_filter_metadata()`](https://isoreader2.isoverse.org/reference/ir_metadata.md)
it works on both object types and returns the same type it was given.

## Usage

``` r
ir_filter_masses(isofiles, mass)
```

## Arguments

- isofiles:

  datasets aggregated from
  [`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
  (`ir_aggregated_data`) or a collection of isofiles from
  [`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md)
  (`ir_isofiles`)

- mass:

  which masses to keep, as a
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html)
  expression evaluated as if the masses present in the data were column
  names

  - the same syntax as the `mass` argument of
    [`ir_plot_traces()`](https://isoreader2.isoverse.org/reference/ir_plot_traces.md).
    E.g. `c("44", "45")` or `44:48` for specific masses, `-"45"`/`!"45"`
    to exclude one, `everything()` for all of them, and helpers such as
    `starts_with("4")`, `matches()`, `all_of()`, or `any_of()`. Unlike
    plain tidyselect, numbers select by name rather than by position
    (`44:48` means the masses 44 to 48, not the 44th to 48th mass).
    Selecting a mass that is not in the data is an error that lists the
    available masses; use `any_of()` to ignore missing ones.

## Value

the `isofiles` object (of the same type as the input) with only the
selected masses

## Details

Any metadata record (`uidx` + `analysis`) that has no data left in *any*
of `traces`/`cycles`/`scans` afterwards is removed from the metadata,
and the removal cascades to the other datasets exactly as in
[`ir_filter_metadata()`](https://isoreader2.isoverse.org/reference/ir_metadata.md)
(`resistors` and `problems` are restricted to the remaining `uidx`). For
an `ir_isofiles` object, a file whose metadata ends up empty is dropped
from the collection entirely. Note that this also drops records that
never had any `traces`/`cycles`/`scans` data to begin with (e.g. a file
that failed to read) - check
[`ir_get_problems()`](https://isoreader2.isoverse.org/reference/problems.md)
before filtering if you need to keep track of those.

The `mass` selection is always resolved **once against the whole
object** (all files of an `ir_isofiles` collection, and all of
`traces`/`cycles`/`scans` of an aggregated dataset combined), so
selecting a mass that only some of the files or datasets contain works
and simply leaves the others without data.

Ratios calculated with
[`ir_calculate_ratios()`](https://isoreader2.isoverse.org/reference/ir_calculate_ratios.md)
live in the `ratio_name` / `ratio` columns of the rows of their
**numerator** mass, so they are kept or removed with that mass:
filtering to `mass = 44` also removes the `"45/44"` ratio (which sits on
the mass 45 rows), while `mass = c(44, 45)` keeps it. `resistors` are
never filtered by mass since they describe the instrument configuration
rather than measured data.

As for the metadata operations, columns that are entirely `NA` across
all remaining rows are dropped from every (non-empty) dataset and the
*not-aggregated* column information is cleared.

## Examples

``` r
if (FALSE) { # \dontrun{
# keep only the CO2 masses
dataset |> ir_filter_masses(44:46)

# keep everything except mass 45
dataset |> ir_filter_masses(-"45")
} # }
```
