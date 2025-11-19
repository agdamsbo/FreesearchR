# List dataset contained in a package

List dataset contained in a package

## Usage

``` r
list_pkg_data(pkg)
```

## Arguments

- pkg:

  Name of the package, must be installed.

## Value

a `character` vector or `NULL`.

## Examples

``` r
list_pkg_data("ggplot2")
#>  [1] "diamonds"       "economics"      "economics_long" "faithfuld"     
#>  [5] "luv_colours"    "midwest"        "mpg"            "msleep"        
#>  [9] "presidential"   "seals"          "txhousing"     
#> attr(,"package")
#> [1] "ggplot2"
```
