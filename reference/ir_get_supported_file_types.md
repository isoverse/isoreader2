# Get supported file types

Get supported file types

## Usage

``` r
ir_get_supported_file_types()
```

## Value

a tibble of the file types supported by this package

## Examples

``` r
ir_get_supported_file_types()
#> # A tibble: 9 × 3
#>   file_type min_isoextract_version vendor_software
#>   <chr>     <chr>                  <chr>          
#> 1 dxf       0.3.0                  Isodat         
#> 2 cf        0.3.0                  Isodat         
#> 3 iarc      0.3.0                  IonOS          
#> 4 larc      0.3.0                  LyticOS        
#> 5 bch       0.3.0                  Callisto       
#> 6 imexp     0.3.0                  Qtegra         
#> 7 did       0.3.0                  Isodat         
#> 8 caf       0.3.0                  Isodat         
#> 9 scn       0.3.0                  Isodat         
```
