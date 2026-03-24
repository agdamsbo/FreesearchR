# Easily plot single euler diagrams

Easily plot single euler diagrams

## Usage

``` r
plot_euler_single(data, color.palette = "viridis")
```

## Value

ggplot2 object

## Examples

``` r
data.frame(
  A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
  B = sample(c("A", "C"), 50, TRUE),
  C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE),
  D = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
) |> plot_euler_single()
#> Scale for fill is already present.
#> Adding another scale for fill, which will replace the existing scale.

mtcars[c("vs", "am")] |> plot_euler_single("magma")
#> Scale for fill is already present.
#> Adding another scale for fill, which will replace the existing scale.
```
