# Get possible regression models

Get possible regression models

## Usage

``` r
possible_functions(data, design = c("cross-sectional"))
```

## Arguments

- data:

  data

## Value

character vector

## Examples

``` r
mtcars |>
  default_parsing() |>
  dplyr::pull("cyl") |>
  possible_functions(design = "cross-sectional")
#> [1] "Ordinal logistic regression model"

mtcars |>
  default_parsing() |>
  dplyr::select("cyl") |>
  possible_functions(design = "cross-sectional")
#> [1] "Ordinal logistic regression model"
```
