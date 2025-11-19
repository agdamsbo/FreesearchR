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
#> # A tibble: 32 × 0
default_parsing(mtcars) |> subset_types(c("dichotomous", "categorical"))
#> # A tibble: 32 × 5
#>    cyl   vs    am    gear  carb 
#>    <fct> <lgl> <lgl> <fct> <fct>
#>  1 6     FALSE TRUE  4     4    
#>  2 6     FALSE TRUE  4     4    
#>  3 4     TRUE  TRUE  4     1    
#>  4 6     TRUE  FALSE 3     1    
#>  5 8     FALSE FALSE 3     2    
#>  6 6     TRUE  FALSE 3     1    
#>  7 8     FALSE FALSE 3     4    
#>  8 4     TRUE  FALSE 4     2    
#>  9 4     TRUE  FALSE 4     2    
#> 10 6     TRUE  FALSE 4     4    
#> # ℹ 22 more rows
#' default_parsing(mtcars) |> subset_types("factor",class)
```
