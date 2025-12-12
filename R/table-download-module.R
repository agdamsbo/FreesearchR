table_download_ui <- function(id, title = "Table", ...) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4(title),
    shiny::helpText(i18n$t("Choose your favourite output file format for further work, and download, when the analyses are done.")),
    shiny::br(),
    shiny::br(),
    shiny::selectInput(
      inputId = ns("output_format"),
      label = "Output format",
      selected = NULL,
      choices = list(
        "MS Word" = "docx",
        "Compatible (rtf)" = "rtf"
      )
    ),
    shiny::br(),
    shiny::uiOutput(ns("download_button_container")),
    # shiny::downloadButton(
    #   outputId = ns("act_table"),
    #   label = "Download table",
    #   icon = shiny::icon("download")
    # ),
    shiny::br()
  )
}

table_download_server <- function(id, data, file_name = "table", ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      output$download_button_container <- shiny::renderUI({
        # Check if data exists and is valid
        if (!is.null(data()) && (inherits(data(), "gt_tbl") || inherits(data(), "gtsummary"))) {
          shiny::downloadButton(
            outputId = ns("act_table"),
            label = i18n$t("Download table"),
            icon = shiny::icon("download")
          )
        } else {
          # Return NULL to show nothing
          NULL
        }
      })


      output$act_table <- shiny::downloadHandler(
        filename = function() {
          paste0("report.", input$output_format)
        },
        content = function(file) {
          shiny::req(data())
          type <- input$output_format
          table <- data()

          shiny::withProgress(message = i18n$t("Generating the report. Hold on for a moment.."), {
            tryCatch(
              {
                # browser()
                if (inherits(table, "gtsummary")) {
                  table <- gtsummary::as_gt(table)
                }
                out <- gt::gtsave(
                  data = table,
                  filename = file # Save to the file path provided by downloadHandler
                )
                # This only works locally and was disabled
                # if (type == "docx") {
                #   out |> doconv::docx_update()
                # } else {
                #   out
                # }

                out
              },
              error = function(err) {
                shiny::showNotification(paste0(i18n$t("Error: "), err), type = "error")
              }
            )
          })
        }
      )
    }
  )
}


# In your UI
table_download_demo <- function() {
  ui <- fluidPage(
    table_download_ui(id = "my_table", title = "Download Results")
  )

  # In your server
  server <- function(input, output, session) {
    # Your data as a reactive
    my_table_data <- reactive({
      # This should return a gt or gtsummary table
      mtcars |>
        gt::gt() |>
        gt::tab_header("My Table")
    })

    # Call the module server - THIS IS CRITICAL
    table_download_server(
      id = "my_table", # Must match the UI id
      data = my_table_data # Pass the reactive (without parentheses)
    )
  }
  shiny::shinyApp(ui, server)
}

# table_download_demo()
