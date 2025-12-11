# Pairwise comparison of missings across covariables

Pairwise comparison of missings across covariables

## Usage

``` r
compare_missings(
  data,
  by_var,
  max_level = 20,
  type = c("predictors", "outcome")
)
```

## Arguments

- data:

  data frame

- by_var:

  variable to stratify by missingness

## Value

gtsummary list object
