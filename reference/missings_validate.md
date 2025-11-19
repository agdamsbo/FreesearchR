# Validate function of missingness in data

Validate function of missingness in data

## Usage

``` r
missings_validate(data)
```

## Arguments

- data:

  data set

## Value

data.frame

## Examples

``` r
df <- mtcars
df[1, 2:4] <- NA
missings_validate(df)
#>   p_miss
#> 1   0.85
```
