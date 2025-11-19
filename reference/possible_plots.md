# Get possible regression models

Get possible regression models

## Usage

``` r
possible_plots(data)
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
  possible_plots()
#> Error in supported_plots(): object 'i18n' not found

mtcars |>
  default_parsing() |>
  dplyr::select("mpg") |>
  possible_plots()
#> Error in supported_plots(): object 'i18n' not found
```
