# Create new column

This module allow to enter an expression to create a new column in a
`data.frame`.

## Usage

``` r
create_column_ui(id)

create_column_server(
  id,
  data_r = reactive(NULL),
  allowed_operations = list_allowed_operations()
)

allowed_operations()

modal_create_column(
  id,
  title = i18n$t("Create a new column"),
  easyClose = TRUE,
  size = "l",
  footer = NULL
)

winbox_create_column(
  id,
  title = i18n$t("Create a new column"),
  options = shinyWidgets::wbOptions(),
  controls = shinyWidgets::wbControls()
)
```

## Arguments

- id:

  Module's ID.

- data_r:

  A [`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)
  function returning a `data.frame`.

- allowed_operations:

  A `list` of allowed operations, see below for details.

- title:

  An optional title for the dialog.

- easyClose:

  If `TRUE`, the modal dialog can be dismissed by clicking outside the
  dialog box, or be pressing the Escape key. If `FALSE` (the default),
  the modal dialog can't be dismissed in those ways; instead it must be
  dismissed by clicking on a
  [`modalButton()`](https://rdrr.io/pkg/shiny/man/modalDialog.html), or
  from a call to
  [`removeModal()`](https://rdrr.io/pkg/shiny/man/showModal.html) on the
  server.

- size:

  One of `"s"` for small, `"m"` (the default) for medium, `"l"` for
  large, or `"xl"` for extra large. Note that `"xl"` only works with
  Bootstrap 4 and above (to opt-in to Bootstrap 4+, pass
  [`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html)
  to the `theme` argument of a page container like
  [`fluidPage()`](https://rdrr.io/pkg/shiny/man/fluidPage.html)).

- footer:

  UI for footer. Use `NULL` for no footer.

- options:

  List of options, see
  [`wbOptions()`](https://dreamrs.github.io/shinyWidgets/reference/wbOptions.html).

- controls:

  List of controls, see
  [`wbControls()`](https://dreamrs.github.io/shinyWidgets/reference/wbControls.html).

## Value

A [`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)
function returning the data.

## Note

User can only use a subset of function: (, c, :, ~, +, -, \*, ^, %%,
%/%, /, ==, \>, \<, !=, \<=, \>=, &, \|, is.na, ifelse, any, all, abs,
sign, sqrt, ceiling, floor, trunc, cummax, cummin, cumprod, cumsum, exp,
expm1, log, log10, log2, log1p, cos, cosh, sin, sinh, tan, tanh, acos,
acosh, asin, asinh, atan, atanh, cospi, sinpi, tanpi, gamma, lgamma,
digamma, trigamma, round, signif, max, min, range, prod, sum, length,
pmin, pmax, mean, paste, paste0, substr, nchar, trimws, gsub, sub,
grepl, as.numeric, as.character, as.integer, as.Date, as.POSIXct,
as.factor, factor. You can add more operations using the
`allowed_operations` argument, for example if you want to allow to use
package lubridate, you can do:

    c(list_allowed_operations(), getNamespaceExports("lubridate"))

## Examples

``` r
library(shiny)
library(reactable)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5L, preset = "bootstrap"),
  shinyWidgets::html_dependency_winbox(),
  tags$h2("Create new column"),
  fluidRow(
    column(
      width = 4,
      create_column_ui("inline"),
      actionButton("modal", "Or click here to open a modal to create a column"),
      tags$br(), tags$br(),
      actionButton("winbox", "Or click here to open a WinBox to create a column")
    ),
    column(
      width = 8,
      reactableOutput(outputId = "table"),
      verbatimTextOutput("code")
    )
  )
)
#> Error in create_column_ui("inline"): object 'i18n' not found

server <- function(input, output, session) {

  rv <- reactiveValues(data = MASS::Cars93[, c(1, 3, 4, 5, 6, 10)])

  # inline mode
  data_inline_r <- create_column_server(
    id = "inline",
    data_r = reactive(rv$data)
  )
  observeEvent(data_inline_r(), rv$data <- data_inline_r())

  # modal window mode
  observeEvent(input$modal, modal_create_column("modal"))
  data_modal_r <- create_column_server(
    id = "modal",
    data_r = reactive(rv$data)
  )
  observeEvent(data_modal_r(), rv$data <- data_modal_r())

  # WinBox window mode
  observeEvent(input$winbox, winbox_create_column("winbox"))
  data_winbox_r <- create_column_server(
    id = "winbox",
    data_r = reactive(rv$data)
  )
  observeEvent(data_winbox_r(), rv$data <- data_winbox_r())

  # Show result
  output$table <- renderReactable({
    data <- req(rv$data)
    reactable(
      data = data,
      bordered = TRUE,
      compact = TRUE,
      striped = TRUE
    )
  })

  output$code <- renderPrint({
    attr(rv$data, "code")
  })
}

if (interactive())
  shinyApp(ui, server)
```
