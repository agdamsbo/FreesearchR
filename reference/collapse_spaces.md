# Substitue spaces/tabs with single space excluding text within quotes

Written assisted by Claude.ai. It is long and possibly too complicated,
but it works

## Usage

``` r
collapse_spaces(x, preserve_newlines = TRUE)
```

## Arguments

- x:

  character string

- preserve_newlines:

  flag to preserve new lines

## Value

character string

## Examples

``` r
collapse_spaces(c("cyl", "di sp","s e   d","d  e'dl  e'"))
#> Error in collapse_spaces(c("cyl", "di sp", "s e   d", "d  e'dl  e'")): could not find function "collapse_spaces"
```
