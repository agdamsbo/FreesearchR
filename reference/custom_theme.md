# Custom theme based on unity

Custom theme based on unity

## Usage

``` r
custom_theme(
  ...,
  version = 5,
  primary = FreesearchR_colors("primary"),
  secondary = FreesearchR_colors("secondary"),
  bootswatch = "united",
  base_font = bslib::font_google("Montserrat"),
  heading_font = bslib::font_google("Public Sans", wght = "700"),
  code_font = bslib::font_google("Open Sans"),
  success = FreesearchR_colors("success"),
  info = FreesearchR_colors("info"),
  warning = FreesearchR_colors("warning"),
  danger = FreesearchR_colors("danger")
)
```

## Arguments

- ...:

  everything passed on to bslib::bs_theme()

## Value

theme list
