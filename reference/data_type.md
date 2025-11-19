# Data type assessment.

These are more overall than the native typeof. This is used to assess a
more meaningful "clinical" data type.

## Usage

``` r
data_type(data)
```

## Arguments

- data:

  vector or data.frame. if data frame, each column is evaluated.

## Value

outcome type

## Examples

``` r
mtcars |>
  default_parsing() |>
  lapply(data_type)
#> $mpg
#> [1] "continuous"
#> 
#> $cyl
#> [1] "categorical"
#> 
#> $disp
#> [1] "continuous"
#> 
#> $hp
#> [1] "continuous"
#> 
#> $drat
#> [1] "continuous"
#> 
#> $wt
#> [1] "continuous"
#> 
#> $qsec
#> [1] "continuous"
#> 
#> $vs
#> [1] "dichotomous"
#> 
#> $am
#> [1] "dichotomous"
#> 
#> $gear
#> [1] "categorical"
#> 
#> $carb
#> [1] "categorical"
#> 
mtcars |>
  default_parsing() |>
  data_type()
#>           mpg           cyl          disp            hp          drat 
#>  "continuous" "categorical"  "continuous"  "continuous"  "continuous" 
#>            wt          qsec            vs            am          gear 
#>  "continuous"  "continuous" "dichotomous" "dichotomous" "categorical" 
#>          carb 
#> "categorical" 
c(1, 2) |> data_type()
#> [1] "dichotomous"
1 |> data_type()
#> [1] "monotone"
c(rep(NA, 10)) |> data_type()
#> [1] "empty"
sample(1:100, 50) |> data_type()
#> [1] "continuous"
factor(letters[1:20]) |> data_type()
#> [1] "categorical"
as.Date(1:20) |> data_type()
#> [1] "datetime"
```
