# Area proportional venn diagrams

This is slightly modified from
https://gist.github.com/danlooo/d23d8bcf8856c7dd8e86266097404ded

This functions uses eulerr::euler to plot area proportional venn
diagramms but plots it using ggplot2

## Usage

``` r
ggeulerr(combinations, show_quantities = TRUE, show_labels = TRUE, ...)
```

## Arguments

- combinations:

  set relationships as a named numeric vector, matrix, or data.frame(See
  [`eulerr::euler`](https://jolars.github.io/eulerr/reference/euler.html))

- show_quantities:

  whether to show number of intersecting elements

- show_labels:

  whether to show set names

- ...:

  further arguments passed to eulerr::euler
