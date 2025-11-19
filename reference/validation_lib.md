# Validation library

Validation library

## Usage

``` r
validation_lib(name = NULL)
```

## Arguments

- name:

  Index name

## Value

list

## Examples

``` r
validation_lib()
#> Error in validation_lib(): could not find function "validation_lib"
validation_lib("missings")
#> Error in validation_lib("missings"): could not find function "validation_lib"
```
