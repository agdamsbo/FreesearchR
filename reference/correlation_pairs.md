# Determine significant correlations in the data set

Determine significant correlations in the data set

## Usage

``` r
correlation_pairs(data, threshold = 0.8)
```

## Arguments

- data:

  data.frame

- threshold:

  correlation threshold

## Value

data.frame

## Examples

``` r
correlation_pairs(mtcars)
#> Error in cor$r %>% as.table(): could not find function "%>%"
correlation_pairs(mtcars,.9)
#> Error in cor$r %>% as.table(): could not find function "%>%"
correlation_pairs(mtcars[c(1:4),])
#> [1] var1 var2 cor 
#> <0 rows> (or 0-length row.names)
```
