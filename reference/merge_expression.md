# Merge list of expressions

Merge list of expressions

## Usage

``` r
merge_expression(data)
```

## Arguments

- data:

  list

## Value

expression

## Examples

``` r
list(
  rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
  rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
) |> merge_expression()
#> dplyr::select(c("cyl", "disp")) %>% FreesearchR::default_parsing()
```
