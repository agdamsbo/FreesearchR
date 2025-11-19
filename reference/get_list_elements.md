# Subset elements from list of lists

General function to sub-setting details stored in list dictionaries.

## Usage

``` r
get_list_elements(name, element, dict = cut_methods())
```

## Arguments

- name:

  list name to lookup

- element:

  element to get

- dict:

  dictionary to use

## Value

named vector

## Examples

``` r
get_list_elements(c("top", "bottom"), "descr")
#> Error in cut_methods(): object 'i18n' not found
```
