# Create a Continuous Color Function from a Palette

Wraps
[`generate_colors`](https://agdamsbo.github.io/FreesearchR/reference/generate_colors.md)
into a function that accepts a value between 0 and 1 and returns the
corresponding color. Useful for mapping continuous variables to colors.

## Usage

``` r
continuous_colors(palette = "viridis", n = 256, ...)
```

## Arguments

- palette:

  Passed directly to
  [`generate_colors()`](https://agdamsbo.github.io/FreesearchR/reference/generate_colors.md).
  Either a palette name string or a function.

- n:

  `integer`. Resolution of the underlying color ramp — higher values
  give smoother gradients. Defaults to 256.

- ...:

  Additional arguments passed to
  [`generate_colors()`](https://agdamsbo.github.io/FreesearchR/reference/generate_colors.md).

## Value

A function that takes a numeric vector of values in `[0, 1]` and returns
a character vector of hex colors.

## See also

[`generate_colors()`](https://agdamsbo.github.io/FreesearchR/reference/generate_colors.md)

## Examples

``` r
pal <- continuous_colors("viridis")
pal(0)    # first color
#> [1] "#440154"
pal(1)    # last color
#> [1] "#FDE725"
pal(0.5)  # midpoint
#> [1] "#21908C"

# Map a continuous variable to colors
values <- seq(0, 1, length.out = 10)
pal(values)
#>  [1] "#440154" "#482878" "#3E4989" "#31688E" "#25828E" "#1E9D89" "#35B779"
#>  [8] "#6CCD59" "#B4DD2B" "#FDE725"

# Works with any palette generate_colors() accepts
pal <- continuous_colors("plasma", direction = -1)
pal <- continuous_colors(\(n) hcl.colors(n, palette = "Blue-Red"))
```
