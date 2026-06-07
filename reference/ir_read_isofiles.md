# Read isotope data files

Read isotope data files

## Usage

``` r
ir_read_isofiles(
  file_paths,
  show_progress = is_interactive(),
  show_problems = TRUE,
  reextract = FALSE
)
```

## Arguments

- file_paths:

  paths to the isodat file(s), single value or vector of paths. Use
  [`ir_find_isofiles()`](https://isoreader2.isoverse.org/reference/ir_find_isofiles.md)
  to get files in a folder.

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

- reextract:

  whether to re-extract files (uses isoextract to read files from
  scratch), if FALSE (default) only extract files not previously
  extracted

## Value

a tibble data frame where each row holds the file path and nested
tibbles of datasets extracted from the isodat files. Use
[`ir_aggregate_isofiles()`](https://isoreader2.isoverse.org/reference/ir_aggregate_isofiles.md)
to aggregate data safely across files.
