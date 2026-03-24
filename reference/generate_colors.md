# Generate N Colors from a Specified Color Palette

A flexible wrapper around multiple color palette libraries, returning N
colors as a character vector of hex codes. Supports palettes from
viridisLite, base R grDevices, and RColorBrewer.

## Usage

``` r
generate_colors(n, palette = "viridis", ...)
```

## Arguments

- n:

  `integer`. Number of colors to generate. Must be a positive integer.

- palette:

  `character(1)`. Name of the color palette to use. Case-insensitive.
  Supported options:

  **viridisLite**

  :   `"viridis"`, `"magma"`, `"plasma"`, `"inferno"`, `"cividis"`,
      `"mako"`, `"rocket"`, `"turbo"`

  **grDevices**

  :   `"hcl"`, `"rainbow"`, `"heat"`, `"terrain"`, `"topo"`

  **RColorBrewer**

  :   Any palette name from
      [`RColorBrewer::brewer.pal.info`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html),
      e.g. `"Set1"`, `"Blues"`, `"Dark2"`. If `n` exceeds the palette
      maximum, colors are interpolated via
      [`colorRampPalette`](https://rdrr.io/r/grDevices/colorRamp.html).

- ...:

  Additional arguments passed to the underlying palette function. For
  example, `alpha`, `direction`, `begin`, `end` are forwarded to
  [`viridis`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html);
  `palette` is forwarded to
  [`hcl.colors`](https://rdrr.io/r/grDevices/palettes.html).

## Value

A `character` vector of length `n` containing hex color codes (e.g.
`"#440154FF"`).

## See also

[`viridis`](https://sjmgarnier.github.io/viridisLite/reference/viridis.html),
[`hcl.colors`](https://rdrr.io/r/grDevices/palettes.html),
[`brewer.pal`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html)

## Examples

``` r
# viridisLite palettes
generate_colors(5, "viridis")
#> [1] "#440154FF" "#3B528BFF" "#21908CFF" "#5DC863FF" "#FDE725FF"
generate_colors(5, "plasma")
#> [1] "#0D0887FF" "#7E03A8FF" "#CC4678FF" "#F89441FF" "#F0F921FF"
generate_colors(5, "viridis", alpha = 0.8, direction = -1)
#> [1] "#FDE725CC" "#5DC863CC" "#21908CCC" "#3B528BCC" "#440154CC"

# Base R grDevices
generate_colors(5, "rainbow")
#> [1] "#FF0000" "#CCFF00" "#00FF66" "#0066FF" "#CC00FF"
generate_colors(8, "hcl", palette = "Dark 3")
#> Warning: NAs introduced by coercion
#> [1] "#E16A86FF" "#C7821CFF" "#909800FF" "#00A846FF" "#00AD9AFF" "#00A2D3FF"
#> [7] "#9183E6FF" "#D766C9FF"

# RColorBrewer
generate_colors(5, "Set1")
#> [1] "#E41A1C" "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"
generate_colors(5, "Blues")
#> [1] "#EFF3FF" "#BDD7E7" "#6BAED6" "#3182BD" "#08519C"
generate_colors(12, "Set1")  # interpolates beyond palette max of 9
#>  [1] "#E41A1C" "#66628D" "#419486" "#5A9D5A" "#91569A" "#D96D3B" "#FFAD12"
#>  [8] "#F6EF32" "#B6742A" "#D26D7A" "#DD87B4" "#999999"

# Drop-in replacement for viridisLite::viridis()
# generate_colors(n = length(levels(data_orig[[pri]])), palette = "viridis")
```
