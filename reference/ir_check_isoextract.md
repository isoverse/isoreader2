# Check for the isoextract executable

By default, this will install isoextract if it is missing or outdated.
This function runs automatically when needed and does not usually need
to be called directly by the user.

## Usage

``` r
ir_check_isoextract(
  install_if_missing = !on_cran(),
  reinstall_if_outdated = !on_cran(),
  reinstall_always = FALSE,
  min_version = "0.2.1",
  show_version = TRUE,
  source =
    paste0("https://github.com/isoverse/IsofileExtractor/releases/download/isoextract-v",
    min_version),
  check_isosolfs = FALSE,
  ...
)
```

## Arguments

- install_if_missing:

  install isoextract if it's missing

- reinstall_if_outdated:

  install isoextract if it's outdated (i.e. not at least `min_version`)

- reinstall_always:

  whether to (re-)install no matter what

- min_version:

  the minimum version number required

- show_version:

  whether to print the installed isoextract version after a successful
  check (default: `TRUE`)

- source:

  the URL (or local path) where to find isoextract, by default this is
  the latests release of the executables on github

- check_isosolfs:

  whether to also ensure the `isosolfs` helper executable is installed.
  `isosolfs` is required to read Qtegra notebooks (`.imexp` files) and
  is released alongside isoextract; the same `install_if_missing` /
  `reinstall_if_outdated` / `reinstall_always` / `show_version` settings
  are applied to it. Default is currently `FALSE` because isosolfs has
  not been released yet (enabling it would abort all reads); set to
  `TRUE` (or this will become the default) once isosolfs is available.

- ...:

  passed on to `download.file` if (re-) installing isoextract (and
  isosolfs)

## Value

called for its side effect of ensuring a working isoextract executable
(at least `min_version`) is installed — and, when
`check_isosolfs = TRUE`, isosolfs as well; returns `NULL` invisibly and
aborts if a required executable cannot be made available
