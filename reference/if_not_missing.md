# Return if available

Return if available

## Usage

``` r
if_not_missing(data, default = NULL)
```

## Arguments

- data:

  vector

- default:

  assigned value for missings

## Value

vector

## Examples

``` r
NULL |> if_not_missing("new")
#> [1] "new"
c(2, "a", NA) |> if_not_missing()
#> [1] "2" "a"
"See" |> if_not_missing()
#> [1] "See"
```
