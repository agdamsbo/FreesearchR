# Easily get variable icon based on data type or class

Easily get variable icon based on data type or class

## Usage

``` r
get_var_icon(data, class.type = c("class", "type"))
```

## Arguments

- data:

  variable or data frame

- class.type:

  "type" or "class". Default is "class"

## Value

svg icon

## Examples

``` r
mtcars[1] |> get_var_icon("class")
#> $mpg
#> 
default_parsing(mtcars) |> get_var_icon()
#> $mpg
#> 
#> $cyl
#> 
#> $disp
#> 
#> $hp
#> 
#> $drat
#> 
#> $wt
#> 
#> $qsec
#> 
#> $vs
#> 
#> $am
#> 
#> $gear
#> 
#> $carb
#> 
```
