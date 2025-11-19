# Get data type icons

Get data type icons

## Usage

``` r
type_icons(x)
```

## Arguments

- x:

  character vector of data classes

## Value

list

## Examples

``` r
"ordinal" |> type_icons()
default_parsing(mtcars) |> sapply(data_type) |> type_icons()
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
#> Warning: coercing argument of type 'character' to logical
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
