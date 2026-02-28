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
  base_font = bslib::font_face(family = "Montserrat", src =
    "url('/fonts/Montserrat-Regular.ttf') format('truetype')"),
  heading_font = bslib::font_face(family = "PublicSans", src =
    "url('/fonts/PublicSans-Bold.ttf') format('truetype')"),
  code_font = bslib::font_face(family = "OpenSans", src =
    "url('/fonts/OpenSans-Regular.ttf') format('truetype')"),
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
