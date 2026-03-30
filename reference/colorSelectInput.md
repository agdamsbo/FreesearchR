# A selectizeInput customized for named vectors of color names supported by [`generate_colors`](https://agdamsbo.github.io/FreesearchR/reference/generate_colors.md)

A selectizeInput customized for named vectors of color names supported
by
[`generate_colors`](https://agdamsbo.github.io/FreesearchR/reference/generate_colors.md)

## Usage

``` r
colorSelectInput(
  inputId,
  label,
  choices,
  selected = NULL,
  previews = 4,
  ...,
  placeholder = ""
)
```

## Arguments

- inputId:

  passed to
  [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)

- label:

  passed to
  [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)

- choices:

  A named `vector` from which fields should be populated

- selected:

  default selection

- previews:

  number of preview colors. Default is 4.

- ...:

  passed to
  [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)

- placeholder:

  passed to
  [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)
  options

- onInitialize:

  passed to
  [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)
  options

## Value

a [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)
dropdown element

## Examples

``` r
if (shiny::interactive()) {
top_palettes <- c(
"Perceptual (blue-yellow)"   = "viridis",
"Perceptual (fire)"          = "plasma",
"Colour-blind friendly"      = "Okabe-Ito",
"Qualitative (bold)"         = "Dark 2",
"Qualitative (paired)"       = "Paired",
"Sequential (blues)"         = "Blues",
"Diverging (red-blue)"       = "RdBu",
"Tableau style"              = "Tableau 10",
"Pastel"                     = "Pastel 1",
"Rainbow"                    = "rainbow"
)
  shinyApp(
    ui = fluidPage(
    titlePanel("Color Palette Select Test"),
    colorSelectInput(
      inputId  = "palette",
      label    = "Color palette",
      choices  = top_palettes,
      selected = "viridis"
    ),
    verbatimTextOutput("selected")
    ),
    server = function(input, output, session) {
    output$selected <- renderPrint(input$palette)
    }
  )
}
#> Error: 'interactive' is not an exported object from 'namespace:shiny'
```
