# Get missingsness fraction

Get missingsness fraction

## Usage

``` r
missing_fraction(data)
```

## Arguments

- data:

  data

## Value

numeric vector

## Examples

``` r
c(NA, 1:10, rep(NA, 3)) |> missing_fraction()
#> [1] 0.2857143
```
