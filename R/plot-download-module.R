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
      output$download_plot <- shiny::downloadHandler(
        filename = function() {
          paste0(file_name, ".", input$plot_type)
        },
        content = function(file) {
          shiny::withProgress(message = "Saving the plot. Hold on for a moment..", {
            ggplot2::ggsave(
              filename = file,
              plot = data,
              width = input$plot_width,
              height = input$plot_height,
              dpi = 300,
              units = "mm",
              scale = 2
            )
          })
        }
      )
    }
  )
}


plot_download_demo_app <- function() {

  ui <- bslib::page_fillable(
    title = "Plot Download Demo",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Download Settings",
        plot_download_ui(id = "plot_dwn")
      ),
      bslib::card(
        bslib::card_header("Sample Plot"),
        shiny::plotOutput("demo_plot", height = "500px")
      )
    )
  )

  server <- function(input, output, session) {

    # Create a sample ggplot
    sample_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(x = wt, y = mpg, color = factor(cyl))) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_smooth(method = "lm", se = TRUE) +
      ggplot2::labs(
        title = "Car Weight vs MPG",
        x = "Weight (1000 lbs)",
        y = "Miles per Gallon",
        color = "Cylinders"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        legend.position = "bottom"
      )

    # Display the plot
    output$demo_plot <- shiny::renderPlot({
      sample_plot
    })

    # Connect to download module
    plot_download_server(
      id = "plot_dwn",
      data = sample_plot,
      file_name = "mtcars_plot"
    )
  }

  shiny::shinyApp(ui, server)
}

# Run the demo
# plot_download_demo_app()
