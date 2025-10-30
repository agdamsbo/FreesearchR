library(shiny)
library(reactable)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5L, preset = "bootstrap"),
  shinyWidgets::html_dependency_winbox(),
  tags$h2("Split string"),
  fluidRow(
    column(
      width = 4,
      actionButton("modal", "Or click here to open a modal to create a column")
    ),
    column(
      width = 8,
      reactableOutput(outputId = "table"),
      verbatimTextOutput("code")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    data = MASS::Cars93[, c(1, 3, 4, 5, 6, 10)],
    out = NULL
  )

  # modal window mode
  observeEvent(input$modal, modal_string_split("modal"))

  rv$out <- create_column_server(
    id = "modal",
    data_r = reactive(rv$data)
  )


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

if (interactive()) {
  shinyApp(ui, server)
}
