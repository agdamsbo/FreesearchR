# Validate REDCap token

Validate REDCap token

## Usage

``` r
is_valid_token(token, pattern_env = NULL, nchar = 32)
```

## Arguments

- token:

  token

- pattern_env:

  pattern

## Value

logical

## Examples

``` r
token <- paste(sample(c(1:9, LETTERS[1:6]), 32, TRUE), collapse = "")
is_valid_token(token)
#> [1] TRUE
```
