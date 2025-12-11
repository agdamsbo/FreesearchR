# Print label, and if missing print variable name for plots

Print label, and if missing print variable name for plots

## Usage

``` r
get_label(data, var = NULL)
```

## Arguments

- data:

  vector or data frame

- var:

  variable name. Optional.

## Value

character string

## Examples

``` r
mtcars |> get_label(var = "mpg")
#> [1] "mpg"
mtcars |> get_label()
#> [1] "mtcars"
mtcars$mpg |> get_label()
#> [1] "mtcars$mpg"
gtsummary::trial |> get_label(var = "trt")
#> [1] "Chemotherapy Treatment"
gtsummary::trial$trt |> get_label()
#> [1] "Chemotherapy Treatment"
1:10 |> get_label()
#> [1] "1:10"
```
