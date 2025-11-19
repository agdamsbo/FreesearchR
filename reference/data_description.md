# Ultra short data dascription

Ultra short data dascription

## Usage

``` r
data_description(data, data_text = "Data")
```

## Arguments

- data:

## Value

character vector

## Examples

``` r
data.frame(
  sample(1:8, 20, TRUE),
  sample(c(1:8, NA), 20, TRUE)
) |> data_description()
#> Error in data_description(data.frame(sample(1:8, 20, TRUE), sample(c(1:8,     NA), 20, TRUE))): object 'i18n' not found
```
