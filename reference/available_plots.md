# Implemented functions

Library of supported functions. The list name and "descr" element should
be unique for each element on list.

- fun: the plotting function

- fun.args: default parameters for the plotting function

- descr: Plot description

- note: Short note/description of the function for displaying in ui and
  docs

- primary.type: Primary variable data type (see
  [data_type](https://agdamsbo.github.io/FreesearchR/reference/data_type.md))

- base: holds a list of parameters for plot input fields generation
  Secondary and tertiary variable input fields are mandatory.

## Usage

``` r
available_plots()
```

## Value

list

## Examples

``` r
available_plots() |> str()
#> Error in available_plots(): object 'i18n' not found
```
