# Easily round log scale limits for nice plots

Easily round log scale limits for nice plots

## Usage

``` r
limit_log(data, fun, ...)
```

## Arguments

- data:

  data

- fun:

  rounding function (floor/ceiling)

- ...:

  ignored

## Value

numeric vector

## Examples

``` r
limit_log(-.1, floor)
#> [1] 0.7
limit_log(.1, ceiling)
#> [1] 2
limit_log(-2.1, ceiling)
#> [1] 0.008
limit_log(2.1, ceiling)
#> [1] 200
```
