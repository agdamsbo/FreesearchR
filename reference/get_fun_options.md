# Get the function options based on the selected function description

Get the function options based on the selected function description

## Usage

``` r
get_fun_options(data)
```

## Arguments

- data:

  vector

## Value

list

## Examples

``` r
mtcars |>
  default_parsing() |>
  dplyr::pull(mpg) |>
  possible_functions(design = "cross-sectional") |>
  (\(.x){
    .x[[1]]
  })() |>
  get_fun_options()
#> $lm
#> $lm$descr
#> [1] "Linear regression model"
#> 
#> $lm$design
#> [1] "cross-sectional"
#> 
#> $lm$out.type
#> [1] "continuous"
#> 
#> $lm$fun
#> [1] "stats::lm"
#> 
#> $lm$args.list
#> NULL
#> 
#> $lm$formula.str
#> [1] "{outcome.str}~{paste(vars,collapse='+')}"
#> 
#> $lm$table.fun
#> [1] "gtsummary::tbl_regression"
#> 
#> $lm$table.args.list
#> $lm$table.args.list$exponentiate
#> [1] FALSE
#> 
#> 
#> 
```
