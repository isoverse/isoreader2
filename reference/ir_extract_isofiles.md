# run the isoextract executable on a vector of file paths this is usually not called directly

run the isoextract executable on a vector of file paths this is usually
not called directly

## Usage

``` r
ir_extract_isofiles(
  file_paths,
  pretty_json = FALSE,
  show_progress = is_interactive(),
  show_problems = TRUE
)

ir_get_isoextract_version()
```

## Arguments

- file_paths:

  paths to the isodat file(s), single value or vector of paths. Use
  [`ir_find_isofiles()`](https://isoreader2.isoverse.org/reference/ir_find_isofiles.md)
  to get files in a folder.

- pretty_json:

  whether to write the JSON output in human-readable pretty-printed
  format (default: `FALSE`). Useful for debugging; has no effect on the
  data read back by
  [`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md).
  Note that pretty-printed files are larger than compact ones.

- show_progress:

  whether to show a progress bar, by default always enabled when running
  interactively e.g. inside Positron or RStudio (and disabled in a
  notebook), turn off with `show_progress = FALSE`

- show_problems:

  whether to show problems encountered along the way (rather than just
  keeping track of them with
  [`ir_get_problems()`](https://isoreader2.isoverse.org/reference/problems.md)).
  Set to `show_problems = FALSE` to turn off the live printout. Either
  way, all encountered problems can be retrieved with running
  [`ir_get_problems()`](https://isoreader2.isoverse.org/reference/problems.md)
  for the returned list

## Value

called for its side effect of running isoextract to write a `.json`
sidecar file next to each input file; returns `NULL` invisibly

## Functions

- `ir_get_isoextract_version()`: return the version of the installed
  `isoextract` executable as a
  [numeric_version](https://rdrr.io/r/base/numeric_version.html), or
  `NULL` if it is not installed (or does not report a recognizable
  version)
