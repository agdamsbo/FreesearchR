# Easily subset by data type function

Easily subset by data type function

## Usage

``` r
subset_types(data, types, type.fun = data_type)
```

## Arguments

- data:

  data

- types:

  desired types

- type.fun:

  function to get type. Default is outcome_type

## Value

vector

## Examples

``` r
default_parsing(mtcars) |> subset_types("ordinal")
#> data frame with 0 columns and 32 rows
default_parsing(mtcars) |> subset_types(c("dichotomous", "categorical"))
#>    cyl    vs    am gear carb
#> 1    6 FALSE  TRUE    4    4
#> 2    6 FALSE  TRUE    4    4
#> 3    4  TRUE  TRUE    4    1
#> 4    6  TRUE FALSE    3    1
#> 5    8 FALSE FALSE    3    2
#> 6    6  TRUE FALSE    3    1
#> 7    8 FALSE FALSE    3    4
#> 8    4  TRUE FALSE    4    2
#> 9    4  TRUE FALSE    4    2
#> 10   6  TRUE FALSE    4    4
#> 11   6  TRUE FALSE    4    4
#> 12   8 FALSE FALSE    3    3
#> 13   8 FALSE FALSE    3    3
#> 14   8 FALSE FALSE    3    3
#> 15   8 FALSE FALSE    3    4
#> 16   8 FALSE FALSE    3    4
#> 17   8 FALSE FALSE    3    4
#> 18   4  TRUE  TRUE    4    1
#> 19   4  TRUE  TRUE    4    2
#> 20   4  TRUE  TRUE    4    1
#> 21   4  TRUE FALSE    3    1
#> 22   8 FALSE FALSE    3    2
#> 23   8 FALSE FALSE    3    2
#> 24   8 FALSE FALSE    3    4
#> 25   8 FALSE FALSE    3    2
#> 26   4  TRUE  TRUE    4    1
#> 27   4 FALSE  TRUE    5    2
#> 28   4  TRUE  TRUE    5    2
#> 29   8 FALSE  TRUE    5    4
#> 30   6 FALSE  TRUE    5    6
#> 31   8 FALSE  TRUE    5    8
#> 32   4  TRUE  TRUE    4    2
#' default_parsing(mtcars) |> subset_types("factor",class)
```
