# Package options

These options are best set via `ir_options()` and queried via
`ir_get_option()`. However, the base functions
[`options()`](https://rdrr.io/r/base/options.html) and
[`getOption()`](https://rdrr.io/r/base/options.html) work as well but
require an `isoreader2.` prefix (the package name and a dot) for the
option name. Setting an option to a value of `NULL` means that the
default is used. `ir_get_options()` is available as an additional
convenience function to retrieve a subset of options with a regular
expression pattern.

## Usage

``` r
ir_options(...)

ir_get_options(pattern = NULL)

ir_get_option(x)
```

## Arguments

- ...:

  set package options, syntax identical to
  [`options()`](https://rdrr.io/r/base/options.html)

- pattern:

  to retrieve multiple options (as a list) with a shared pattern

- x:

  name of the specific option to retrieve

## Functions

- `ir_options()`: set/get option values

- `ir_get_options()`: get a subset of option values that fit a pattern

- `ir_get_option()`: retrieve the current value of one option (option
  must be defined for the package)

## Options for the isoreader2 package

- `dev_mode`: developer mode provides more verbose output

## Examples

``` r
# All default options
ir_get_options()
#> $dev_mode
#> [1] FALSE
#> 
```
