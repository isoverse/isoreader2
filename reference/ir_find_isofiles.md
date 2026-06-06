# Find isodat files

Finds isodat files with the specified extensions in one or more folders.

## Usage

``` r
ir_find_isofiles(
  folder,
  types = c("dxf", "cf", "iarc", "larc", "bch", "imexp", "caf", "did", "scn"),
  pattern = NULL,
  recursive = TRUE
)

ir_find_continuous_flow(folder, pattern = NULL, recursive = TRUE)

ir_find_dual_inlet(folder, pattern = NULL, recursive = TRUE)

ir_find_scans(folder, pattern = NULL, recursive = TRUE)
```

## Arguments

- folder:

  path to a folder with isodat files, or a character vector of folder
  paths

- types:

  file extensions to include (without leading dot), default is all
  supported types:
  `c("dxf", "cf", "iarc", "larc", "bch", "imexp", "caf", "did", "scn")`

- pattern:

  provide a name pattern to find only specific files

- recursive:

  whether to find files recursively

## Value

a sorted character vector of unique paths that correspond to the
original data files (without `.json` suffixes if those are the versions
of the files that are present)

## Functions

- `ir_find_continuous_flow()`: finds continuous flow files (`.dxf`,
  `.cf`)

- `ir_find_dual_inlet()`: finds dual inlet files (`.did`, `.caf`)

- `ir_find_scans()`: finds scan files (`.scn`)

## Examples

``` r
ir_find_continuous_flow(system.file("extdata", package = "isoreader2"))
#> Error in ir_find_isofiles(folder, types = c("dxf", "cf", "iarc", "larc",     "bch", "imexp"), pattern = pattern, recursive = recursive): argument folder ("") must point to an existing directory
ir_find_dual_inlet(system.file("extdata", package = "isoreader2"))
#> Error in ir_find_isofiles(folder, types = c("did", "caf"), pattern = pattern,     recursive = recursive): argument folder ("") must point to an existing directory
ir_find_scans(system.file("extdata", package = "isoreader2"))
#> Error in ir_find_isofiles(folder, types = "scn", pattern = pattern, recursive = recursive): argument folder ("") must point to an existing directory
```
