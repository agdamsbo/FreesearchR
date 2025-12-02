# Reduce character vector with the native pipe operator or character string

Reduce character vector with the native pipe operator or character
string

## Usage

``` r
pipe_string(data, collapse = "|>\n")
```

## Arguments

- data:

  list

## Value

character string

## Examples

``` r
list(
  "mtcars",
  rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
  rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
) |>
  lapply(expression_string) |>
  pipe_string() |>
  expression_string("data<-")
#> [1] "data<-mtcars|>\ndplyr::select(c('cyl', 'disp'))|>\nFreesearchR::default_parsing()"
```
