# Matches pattern to vector based on match type

Matches pattern to vector based on match type

## Usage

``` r
grepl_fix(data, pattern, type = c("prefix", "infix", "suffix"))
```

## Arguments

- data:

  vector

- pattern:

  pattern(s) to match. Character vector of length 1 or more.

- type:

  type of match. can be one of "prefix","infix" or "suffix".

## Value

logical vector

## Examples

``` r
c("id", "age", "weight_0", "weight_1") |> grepl_fix(pattern = c("_0", "_1"), type = "suffix")
#> [1] FALSE FALSE  TRUE  TRUE
```
