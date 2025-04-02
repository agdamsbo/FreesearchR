plot_download_ui <- regression_ui <- function(id, ...) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyWidgets::noUiSliderInput(
      inputId = ns("plot_height"),
      label = "Plot height (mm)",
      min = 50,
      max = 300,
      value = 100,
      step = 1,
      format = shinyWidgets::wNumbFormat(decimals = 0),
      color = datamods:::get_primary_color()
    ),
    shinyWidgets::noUiSliderInput(
      inputId = ns("plot_width"),
      label = "Plot width (mm)",
      min = 50,
      max = 300,
      value = 100,
      step = 1,
      format = shinyWidgets::wNumbFormat(decimals = 0),
      color = datamods:::get_primary_color()
    ),
    shiny::selectInput(
      inputId = ns("plot_type"),
      label = "File format",
      choices = list(
        "png",
        "tiff",
        "eps",
        "pdf",
        "jpeg",
        "svg"
      )
    ),
    shiny::br(),
    # Button
    shiny::downloadButton(
      outputId = ns("download_plot"),
      label = "Download plot",
      icon = shiny::icon("download")
    )
  )
}

plot_download_server <- function(id,
                                 data,
                                 file_name = "reg_plot",
                                 ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # ns <- session$ns



      output$download_plot <- shiny::downloadHandler(
        filename = paste0(file_name, ".", input$plot_type),
        content = function(file) {
          shiny::withProgress(message = "Saving the plot. Hold on for a moment..", {
            ggplot2::ggsave(
              filename = file,
              plot = data,
              width = input$plot_width,
              height = input$plot_height,
              dpi = 300,
              units = "mm", scale = 2
            )
          })
        }
      )
    }
  )
}
