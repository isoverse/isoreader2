# Combine isofiles

Combine multiple collections of isofiles (read by
[`ir_read_isofiles()`](https://isoreader2.isoverse.org/reference/ir_read_isofiles.md))
into a single `ir_isofiles` object by row-binding them with
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
This preserves the object structure and type.

## Usage

``` r
# S3 method for class 'ir_isofiles'
c(...)
```

## Arguments

- ...:

  `ir_isofiles` objects to combine

## Value

a single combined `ir_isofiles` object
