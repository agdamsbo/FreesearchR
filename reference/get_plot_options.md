# Get the function options based on the selected function description

Get the function options based on the selected function description

## Usage

``` r
get_plot_options(data)
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
  get_plot_options()
#> Error in supported_plots(): object 'i18n' not found
```
