# Make unique variable names

Helper function to create new variable names that are unique given a set
of existing names (in a data set, for example). If a variable name
already exists, a number will be appended.

## Usage

``` r
unique_names(new, existing = character())
```

## Arguments

- new:

  a vector of proposed new variable names

- existing:

  a vector of existing variable names

## Value

a vector of unique new variable names

## Examples

``` r
unique_names(c("var_x", "var_y", "var_x"), c("var_x", "var_z"))
#> [1] "var_x_1" "var_y"   "var_x_2"
```
