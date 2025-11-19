# Removes columns with completenes below cutoff

Removes columns with completenes below cutoff

## Usage

``` r
remove_empty_cols(data, cutoff = 0.7)
```

## Arguments

- data:

  data frame

- cutoff:

  numeric

## Value

data frame

## Examples

``` r
data.frame(a = 1:10, b = NA, c = c(2, NA)) |> remove_empty_cols(cutoff = .5)
#>     a  c
#> 1   1  2
#> 2   2 NA
#> 3   3  2
#> 4   4 NA
#> 5   5  2
#> 6   6 NA
#> 7   7  2
#> 8   8 NA
#> 9   9  2
#> 10 10 NA
```
