# Bundled example files

`ir_examples_folder()` returns the path to the folder with the example
isodat files bundled with the package (a convenience wrapper around
`system.file("extdata", package = "isoreader2")`). `ir_copy_examples()`
copies those example files into a local `folder` so they can be read,
re-extracted, or modified without touching the read-only package
installation.

## Usage

``` r
ir_examples_folder()

ir_copy_examples(folder = "examples")
```

## Arguments

- folder:

  target directory to copy the example files into (default
  `"examples"`); created if it does not exist

## Value

`ir_examples_folder()` returns the path to the example files folder as a
single string.

`ir_copy_examples()` invisibly returns the path to the created examples
folder

## Functions

- `ir_copy_examples()`: copy the bundled example files into a local
  `folder`, creating it if necessary and only copying files that do not
  already exist there (existing files are left untouched). Only the
  original data files are copied, not their bundled `.json` sidecars, so
  reading from the copied folder re-extracts them from scratch (requires
  the isoextract executable).

## Examples

``` r
ir_examples_folder() |> ir_find_scans()
#> [1] "/home/runner/work/_temp/Library/isoreader2/extdata/full_scan_example.scn"       
#> [2] "/home/runner/work/_temp/Library/isoreader2/extdata/peak_shape_scan_example.scn" 
#> [3] "/home/runner/work/_temp/Library/isoreader2/extdata/peak_shape_scan_example2.scn"
#> [4] "/home/runner/work/_temp/Library/isoreader2/extdata/time_scan_example.scn"       
# copy the bundled examples into a temporary folder and find them
ir_copy_examples(folder = file.path(tempdir(), "examples")) |>
  ir_find_continuous_flow()
#> ✔ [3ms] ir_copy_examples() copied 7 example files to /tmp/RtmpPc7qkV/examples
#> [1] "/tmp/RtmpPc7qkV/examples/continuous_flow_ea_example.dxf"
#> [2] "/tmp/RtmpPc7qkV/examples/continuous_flow_gc_example.cf" 
```
