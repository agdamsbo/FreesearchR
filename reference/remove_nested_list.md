# Very simple function to remove nested lists, like when uploading .rds

Very simple function to remove nested lists, like when uploading .rds

## Usage

``` r
remove_nested_list(data)
```

## Arguments

- data:

  data

## Value

data.frame

## Examples

``` r
dplyr::tibble(a = 1:10, b = rep(list("a"), 10)) |> remove_nested_list()
#> # A tibble: 10 × 1
#>        a
#>    <int>
#>  1     1
#>  2     2
#>  3     3
#>  4     4
#>  5     5
#>  6     6
#>  7     7
#>  8     8
#>  9     9
#> 10    10
dplyr::tibble(a = 1:10, b = rep(list(c("a", "b")), 10)) |> as.data.frame()
#>     a    b
#> 1   1 a, b
#> 2   2 a, b
#> 3   3 a, b
#> 4   4 a, b
#> 5   5 a, b
#> 6   6 a, b
#> 7   7 a, b
#> 8   8 a, b
#> 9   9 a, b
#> 10 10 a, b
```
