# Variable filter test wrapper

Variable filter test wrapper

## Usage

``` r
vars_filter_validate(before, after)
```

## Arguments

- before:

  data before

- after:

  data after

## Value

vector

## Examples

``` r
vars_filter_validate(mtcars, mtcars[1:6])
#> Error in vars_filter_validate(mtcars, mtcars[1:6]): could not find function "vars_filter_validate"
vars_filter_validate(mtcars, mtcars[0])
#> Error in vars_filter_validate(mtcars, mtcars[0]): could not find function "vars_filter_validate"
```
