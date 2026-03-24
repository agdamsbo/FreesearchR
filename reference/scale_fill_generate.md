# Discrete and Continuous Fill Scale Using generate_colors

Drop-in replacement for
[`viridis::scale_fill_viridis()`](https://sjmgarnier.github.io/viridis/reference/scale_viridis.html)
that works with any palette supported by
[`generate_colors()`](https://agdamsbo.github.io/FreesearchR/reference/generate_colors.md).

## Usage

``` r
scale_fill_generate(palette = "viridis", discrete = TRUE, ...)

scale_color_generate(palette = "viridis", discrete = TRUE, ...)
```

## Arguments

- palette:

  Passed to
  [`generate_colors()`](https://agdamsbo.github.io/FreesearchR/reference/generate_colors.md).
  Either a palette name string or a function.

- discrete:

  `logical`. If `TRUE` (default), a discrete scale is returned. If
  `FALSE`, a continuous scale is returned.

- ...:

  Additional arguments passed to
  [`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
  (discrete) or
  [`ggplot2::scale_fill_gradientn()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)
  (continuous).

## See also

`scale_color_generate()`,
[`generate_colors()`](https://agdamsbo.github.io/FreesearchR/reference/generate_colors.md),
[`continuous_colors()`](https://agdamsbo.github.io/FreesearchR/reference/continuous_colors.md)

## Examples

``` r
library(ggplot2)

# Discrete
ggplot(mtcars, aes(x = wt, y = mpg, fill = factor(cyl))) +
  geom_col() +
  scale_fill_generate(palette = "Set1")


# Continuous
ggplot(mtcars, aes(x = wt, y = mpg, fill = mpg)) +
  geom_point(shape = 21, size = 3) +
  scale_fill_generate(palette = "viridis", discrete = FALSE)


ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  scale_color_generate(palette = "Set1")
```
