# run the isoextract executable on a vector of file paths this is usually not called directly

run the isoextract executable on a vector of file paths this is usually
not called directly

## Usage

``` r
ir_extract_isofiles(
  file_paths,
  show_progress = is_interactive(),
  show_problems = TRUE
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
