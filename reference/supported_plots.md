# Implemented functions

Library of supported functions. The list name and "descr" element should
be unique for each element on list.

- descr: Plot description

- primary.type: Primary variable data type (continuous, dichotomous or
  ordinal)

- secondary.type: Secondary variable data type (continuous, dichotomous
  or ordinal)

- secondary.extra: "none" or NULL to have option to choose none.

- tertiary.type: Tertiary variable data type (continuous, dichotomous or
  ordinal)

## Usage

``` r
supported_plots()
```

## Value

list

## Examples

``` r
supported_plots() |> str()
#> Error in supported_plots(): object 'i18n' not found
```
