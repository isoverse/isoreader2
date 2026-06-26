# Check for the isoextract executables

By default, these will install the executable if it is missing or
outdated. They run automatically when needed and do not usually need to
be called directly by the user. In particular, `ir_check_isoextract()`
calls `ir_check_isosolfs()` automatically (unless
`check_isosolfs = FALSE`), so `ir_check_isosolfs()` rarely needs to be
called on its own.

## Usage

``` r
ir_check_isoextract(
  install_if_missing = !on_cran(),
  reinstall_if_outdated = !on_cran(),
  reinstall_always = FALSE,
  min_version = "0.3.1",
  show_version = TRUE,
  ask_permission = TRUE,
  source =
    paste0("https://github.com/isoverse/IsofileExtractor/releases/download/isoextract-v",
    min_version),
  check_isosolfs = TRUE,
  ...
)

ir_check_isosolfs(
  install_if_missing = !on_cran(),
  reinstall_if_outdated = !on_cran(),
  reinstall_always = FALSE,
  min_version = "1.0.0",
  show_version = TRUE,
  ask_permission = TRUE,
  source =
    paste0("https://github.com/isoverse/IsofileExtractor/releases/download/isosolfs-v",
    min_version),
  ...
)
```

## Arguments

- install_if_missing:

  install the executable if it's missing

- reinstall_if_outdated:

  install the executable if it's outdated (i.e. not at least
  `min_version`)

- reinstall_always:

  whether to (re-)install no matter what

- min_version:

  the minimum version number required

- show_version:

  whether to print the installed version after a successful check
  (default: `TRUE`)

- ask_permission:

  whether to ask for the user's permission before downloading a missing
  or outdated executable (default: `TRUE`). The prompt only appears in
  interactive sessions and only when a download is actually needed; if
  it is declined - or the session is not interactive - no download is
  attempted and the function aborts with instructions. Set to `FALSE` to
  allow the download without prompting (e.g. in scripts). When
  `ir_check_isoextract()` downloads isoextract with the user's consent
  it passes `ask_permission = FALSE` on to `ir_check_isosolfs()` so the
  user is not asked a second time.

- source:

  the URL (or local path) where to find the executable, by default this
  is the latest release of the executables on github

- check_isosolfs:

  whether to also ensure the `isosolfs` helper executable is installed
  (default: `TRUE`), by calling `ir_check_isosolfs()`. `isosolfs` is
  required to read Qtegra notebooks (`.imexp` files) and is released
  alongside isoextract; the same `install_if_missing` /
  `reinstall_if_outdated` / `reinstall_always` / `show_version` settings
  are applied to it.

- ...:

  passed on to `download.file` if (re-) installing the executable(s)

## Value

called for its side effect of ensuring a working executable (at least
`min_version`) is installed — and, for `ir_check_isoextract()` when
`check_isosolfs = TRUE`, isosolfs as well; returns `NULL` invisibly and
aborts if a required executable cannot be made available

## Functions

- `ir_check_isosolfs()`: ensure the `isosolfs` helper executable (used
  to read Qtegra `.imexp` notebooks) is installed. Released alongside
  isoextract and called automatically by `ir_check_isoextract()`, so it
  rarely needs to be called directly.
