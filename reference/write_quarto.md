# Wrapper to save data in RDS, load into specified qmd and render

Wrapper to save data in RDS, load into specified qmd and render

## Usage

``` r
write_quarto(data, ...)
```

## Arguments

- data:

  list to pass to qmd

- ...:

  Passed to
  [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html)

## Value

output file name
