# Wrapping

Wrapping

## Usage

``` r
wrap_plot_list(
  data,
  tag_levels = NULL,
  title = NULL,
  axis.font.family = NULL,
  guides = "collect",
  axes = "collect",
  axis_titles = "collect",
  ...
)
```

## Arguments

- data:

  list of ggplot2 objects

- tag_levels:

  passed to patchwork::plot_annotation if given. Default is NULL

- title:

  panel title

- guides:

  passed to patchwork::wrap_plots()

- axes:

  passed to patchwork::wrap_plots()

- axis_titles:

  passed to patchwork::wrap_plots()

- ...:

  passed to patchwork::wrap_plots()

## Value

list of ggplot2 objects
