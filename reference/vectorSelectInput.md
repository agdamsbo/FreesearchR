# A selectizeInput customized for named vectors

A selectizeInput customized for named vectors

## Usage

``` r
vectorSelectInput(
  inputId,
  label,
  choices,
  selected = "",
  ...,
  placeholder = "",
  onInitialize
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
shinyApp(
  ui = fluidPage(
    shiny::uiOutput("select"),
    tableOutput("data")
  ),
  server = function(input, output) {
    output$select <- shiny::renderUI({
      vectorSelectInput(
        inputId = "variable", label = "Variable:",
        data = c(
          "Cylinders" = "cyl",
          "Transmission" = "am",
          "Gears" = "gear"
        )
      )
    })

    output$data <- renderTable(
      {
        mtcars[, c("mpg", input$variable), drop = FALSE]
      },
      rownames = TRUE
    )
  }
)
}
#> Error: 'interactive' is not an exported object from 'namespace:shiny'
```
