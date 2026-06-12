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

## Value

`ir_options()` and `ir_get_options()` return a named list of option
values; `ir_get_option()` returns the value of the single requested
option.

## Functions

- `ir_options()`: set/get option values

- `ir_get_options()`: get a subset of option values that fit a pattern

- `ir_get_option()`: retrieve the current value of one option (option
  must be defined for the package)

## Options for the isoreader2 package

- `aggregators`: data aggregators for pulling data out of raw files. The
  list of available aggregators is accessible via
  `ir_get_option("aggregators")`. Individiual aggregators are available
  via the shortcut helper function `ir_get_aggregator("standard")`.
  Register new/overwrite existing aggregators via
  [`ir_register_aggregator()`](https://isoreader2.isoverse.org/reference/ir_aggregator.md).

- `debug`: turn on debug mode

- `auto_use_ansi`: whether to automatically enable correct rendering of
  stylized (ansi) output in HTML reports from notebooks that call
  [`library(isoir)`](https://rdrr.io/r/base/library.html). Can be turned
  off by calling `isoir::ir_options(auto_use_ansi = FALSE)` **before**
  call [`library(isoir)`](https://rdrr.io/r/base/library.html).

## Examples

``` r
# All default options
ir_get_options()
#> $aggregators
#> $aggregators$metadata
#> ────────────────────────────── Aggregator metadata ─────────────────────────────
#> Dataset metadata:
#>  → file_name = as.character(sub(file_name, pattern = "\\.[^.]+$", replacement =
#> ""))
#>  → analysis = as.integer(analysis)
#>  → timestamp = as.POSIXct(timestamp)
#> 
#> $aggregators$minimal
#> ────────────────────────────── Aggregator minimal ──────────────────────────────
#> Dataset metadata:
#>  → file_name = as.character(sub(file_name, pattern = "\\.[^.]+$", replacement =
#> ""))
#>  → analysis = as.integer(analysis)
#>  → timestamp = as.POSIXct(timestamp)
#> Dataset traces:
#>  → analysis = as.integer(analysis)
#>  → species = as.character(species)
#>  → mass = as.character(mass)
#>  → trace = as.character(sprintf(species, mass, fmt = "%s: %s"))
#>  → time.s = as.numeric(time.s)
#>  → (intensity\\..*) = as.numeric(all_matches("(intensity\\..*)"))
#> Dataset cycles:
#>  → analysis = as.integer(analysis)
#>  → species = as.character(species)
#>  → cycle = as.integer(cycle)
#>  → type = as.character(type)
#>  → mass = as.character(mass)
#>  → trace = as.character(sprintf(species, mass, fmt = "%s: %s"))
#>  → (intensity\\..*) = as.numeric(all_matches("(intensity\\..*)"))
#> Dataset scans:
#>  → analysis = as.integer(analysis)
#>  → species = as.character(species)
#>  → mass = as.character(mass)
#>  → trace = as.character(sprintf(species, mass, fmt = "%s: %s"))
#>  → x = as.numeric(x)
#>  → (intensity\\..*) = as.numeric(all_matches("(intensity\\..*)"))
#> 
#> $aggregators$standard
#> ────────────────────────────── Aggregator standard ─────────────────────────────
#> Dataset metadata:
#>  → file_name = as.character(sub(file_name, pattern = "\\.[^.]+$", replacement =
#> ""))
#>  → analysis = as.integer(analysis)
#>  → timestamp = as.POSIXct(timestamp)
#>  → (.*) = as.character(all_matches("(.*)"))
#> Dataset traces:
#>  → analysis = as.integer(analysis)
#>  → species = as.character(species)
#>  → mass = as.character(mass)
#>  → trace = as.character(sprintf(species, mass, fmt = "%s: %s"))
#>  → time.s = as.numeric(time.s)
#>  → (intensity\\..*) = as.numeric(all_matches("(intensity\\..*)"))
#> Dataset cycles:
#>  → analysis = as.integer(analysis)
#>  → species = as.character(species)
#>  → cycle = as.integer(cycle)
#>  → type = as.character(type)
#>  → mass = as.character(mass)
#>  → trace = as.character(sprintf(species, mass, fmt = "%s: %s"))
#>  → (intensity\\..*) = as.numeric(all_matches("(intensity\\..*)"))
#> Dataset scans:
#>  → analysis = as.integer(analysis)
#>  → species = as.character(species)
#>  → mass = as.character(mass)
#>  → trace = as.character(sprintf(species, mass, fmt = "%s: %s"))
#>  → x = as.numeric(x)
#>  → (intensity\\..*) = as.numeric(all_matches("(intensity\\..*)"))
#> 
#> $aggregators$extended
#> ────────────────────────────── Aggregator extended ─────────────────────────────
#> Dataset metadata:
#>  → file_name = as.character(sub(file_name, pattern = "\\.[^.]+$", replacement =
#> ""))
#>  → analysis = as.integer(analysis)
#>  → timestamp = as.POSIXct(timestamp)
#>  → (.*) = as.character(all_matches("(.*)"))
#> Dataset traces:
#>  → analysis = as.integer(analysis)
#>  → species = as.character(species)
#>  → mass = as.character(mass)
#>  → trace = as.character(sprintf(species, mass, fmt = "%s: %s"))
#>  → time.s = as.numeric(time.s)
#>  → (intensity\\..*) = as.numeric(all_matches("(intensity\\..*)"))
#>  → channel = as.integer(channel)
#>  → config = as.integer(config)
#> Dataset cycles:
#>  → analysis = as.integer(analysis)
#>  → species = as.character(species)
#>  → cycle = as.integer(cycle)
#>  → type = as.character(type)
#>  → mass = as.character(mass)
#>  → trace = as.character(sprintf(species, mass, fmt = "%s: %s"))
#>  → (intensity\\..*) = as.numeric(all_matches("(intensity\\..*)"))
#>  → channel = as.integer(channel)
#> Dataset scans:
#>  → analysis = as.integer(analysis)
#>  → species = as.character(species)
#>  → mass = as.character(mass)
#>  → trace = as.character(sprintf(species, mass, fmt = "%s: %s"))
#>  → x = as.numeric(x)
#>  → (intensity\\..*) = as.numeric(all_matches("(intensity\\..*)"))
#>  → channel = as.integer(channel)
#> Dataset resistors:
#>  → species = as.character(species)
#>  → config = as.integer(config)
#>  → channel = as.integer(channel)
#>  → mass = as.character(mass)
#>  → cup = as.integer(cup)
#>  → resistance.Ohm = as.numeric(resistance.Ohm)
#>  → nominal.Ohm = as.numeric(nominal.Ohm)
#> Dataset vendor_data_table:
#>  → (.*) = keep_as_is(all_matches("(.*)"))
#> 
#> 
#> $debug
#> [1] FALSE
#> 
#> $auto_use_ansi
#> [1] TRUE
#> 
```
