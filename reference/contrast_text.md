# Contrast Text Color

Calculates the best contrast text color for a given background color.

## Usage

``` r
contrast_text(
  background,
  light_text = "white",
  dark_text = "black",
  threshold = 0.5,
  method = "perceived_2",
  ...
)
```

## Arguments

- background:

  A hex/named color value that represents the background.

- light_text:

  A hex/named color value that represents the light text color.

- dark_text:

  A hex/named color value that represents the dark text color.

- threshold:

  A numeric value between 0 and 1 that is used to determine the
  luminance threshold of the background color for text color.

- method:

  A character string that specifies the method for calculating the
  luminance. Three different methods are available:
  c("relative","perceived","perceived_2")

- ...:

  parameter overflow. Ignored.

## Value

A character string that contains the best contrast text color.

## Details

This function aids in deciding the font color to print on a given
background. The function is based on the example provided by teppo:
https://stackoverflow.com/a/66669838/21019325. The different methods
provided are based on the methods outlined in the StackOverflow thread:
https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color

## Examples

``` r
contrast_text(c("#F2F2F2", "blue"))
#> [1] "black" "white"

contrast_text(c("#F2F2F2", "blue"), method="relative")
#> [1] "black" "white"
```
