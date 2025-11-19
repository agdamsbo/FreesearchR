# Checks if elements in vector are equally spaced as indication of ID

Checks if elements in vector are equally spaced as indication of ID

## Usage

``` r
is_consecutive(data)
```

## Arguments

- data:

  vector

## Value

logical

## Examples

``` r
1:10 |> is_consecutive()
#> [1] TRUE
sample(1:100,40) |> is_consecutive()
#> [1] FALSE
```
