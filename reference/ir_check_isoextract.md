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
  min_version = "0.2.0",
  source =
    paste0("https://github.com/isoverse/IsofileExtractor/releases/download/isoextract-v",
    min_version),
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

- source:

  the URL (or local path) where to find isoextract, by default this is
  the latests release of the executables on github

- ...:

  passed on to `download.file` if (re-) installing isoextract
