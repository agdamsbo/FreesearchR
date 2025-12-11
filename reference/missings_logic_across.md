# Converting all variables to logicals by missing status

Converting all variables to logicals by missing status

## Usage

``` r
missings_logic_across(data, exclude = NULL)
```

## Arguments

- data:

  data

- exclude:

  character vector of variable names to be excluded

## Value

data frame

## Examples

``` r
mtcars |> missings_logic_across("cyl")
#> # A tibble: 32 × 11
#>    mpg     cyl disp  hp    drat  wt    qsec  vs    am    gear  carb 
#>    <lgl> <dbl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl> <lgl>
#>  1 FALSE     6 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  2 FALSE     6 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  3 FALSE     4 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  4 FALSE     6 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  5 FALSE     8 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  6 FALSE     6 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  7 FALSE     8 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  8 FALSE     4 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#>  9 FALSE     4 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> 10 FALSE     6 FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> # ℹ 22 more rows
## gtsummary::trial |>
##   missings_logic_across() |>
##   gtsummary::tbl_summary()
```
