# Determine if any variable in data frame character and contains recognized delimiters

Determine if any variable in data frame character and contains
recognized delimiters

## Usage

``` r
is_splittable(data)
```

## Arguments

- data:

  vector or data.frame

## Value

logical

## Examples

``` r
any(apply(mtcars, 2, is_splittable))
#> [1] FALSE
is_splittable(mtcars)
#>   mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb 
#> FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE 
```
