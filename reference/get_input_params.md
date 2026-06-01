# Get the function parameters based on the selected function description

Get the function parameters based on the selected function description

## Usage

``` r
get_input_params(data)
```

## Arguments

- data:

  vector

## Value

list

## Examples

``` r
ls <- mtcars |>
  default_parsing() |>
  dplyr::pull(mpg) |>
  possible_plots() |>
  (\(.x){
    .x[[1]]
  })() |>
  get_input_params()
#> Error in available_plots(): object 'i18n' not found
```
