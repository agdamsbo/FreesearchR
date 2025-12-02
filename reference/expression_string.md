# Deparses expression as string, substitutes native pipe and adds assign

Deparses expression as string, substitutes native pipe and adds assign

## Usage

``` r
expression_string(data, assign.str = "")
```

## Arguments

- data:

  expression

## Value

string

## Examples

``` r
list(
  as.symbol(paste0("mtcars$", "mpg")),
  rlang::call2(.fn = "select", !!!list(c("cyl", "di  sp")), .ns = "dplyr"),
  rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
) |>
  merge_expression() |>
  expression_string()
#> [1] "mtcars$mpg |>dplyr::select(c('cyl', 'di  sp')) |>FreesearchR::default_parsing()"
```
