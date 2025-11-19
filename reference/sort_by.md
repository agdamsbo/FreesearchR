# Drop-in replacement for the base::sort_by with option to remove NAs

Drop-in replacement for the base::sort_by with option to remove NAs

## Usage

``` r
sort_by(x, y, na.rm = FALSE, ...)
```

## Arguments

- x:

  x

- y:

  y

- na.rm:

  remove NAs

- ...:

  passed to base_sort_by

## Value

vector

## Examples

``` r
sort_by(c("Multivariable", "Univariable"), c("Univariable", "Minimal", "Multivariable"))
#> [1] "Univariable"   NA              "Multivariable"
```
