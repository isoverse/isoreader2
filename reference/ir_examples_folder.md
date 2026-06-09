# Path to the bundled example files

Returns the path to the folder with the example isodat files bundled
with the package. This is a convenience wrapper around
`system.file("extdata", package = "isoreader2")` for use in examples and
for getting started.

## Usage

``` r
ir_examples_folder()
```

## Value

the path to the example files folder as a single string

## Examples

``` r
ir_examples_folder() |> ir_find_scans()
#> [1] "/home/runner/work/_temp/Library/isoreader2/extdata/background_scan_example.scn"
#> [2] "/home/runner/work/_temp/Library/isoreader2/extdata/full_scan_example.scn"      
#> [3] "/home/runner/work/_temp/Library/isoreader2/extdata/peak_shape_scan_example.scn"
#> [4] "/home/runner/work/_temp/Library/isoreader2/extdata/time_scan_example.scn"      
```
