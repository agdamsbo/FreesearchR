#' Data correlations evaluation module
#'
#' @param id Module id
#'
#' @name data-missings
#' @returns Shiny ui module
#' @export
data_missings_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    gt::gt_output(outputId = ns("missings_table")),
    shiny::plotOutput(outputId = ns("missings_plot"))
  )
}


#'
#' @param data data
#' @param output.format output format
#'
#' @name data-missings
#' @returns shiny server module
#' @export
data_missings_server <- function(id,
                                 data,
                                 ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # ns <- session$ns

      rv <- shiny::reactiveValues(
        data = NULL
      )

      rv$data <- if (is.reactive(data)) data else reactive(data)

      output$missings_plot <- shiny::renderPlot({
        visdat::vis_dat(rv$data(),palette = "cb_safe")
      })
    }
  )
}


missing_demo_app <- function() {
  ui <- shiny::fluidPage(
    shiny::actionButton(
      inputId = "modal_missings",
      label = "Browse data",
      width = "100%",
      disabled = FALSE
    )#,
    # data_missings_ui("data")
  )
  server <- function(input, output, session) {
    data_demo <- mtcars
    data_demo[2:4, "cyl"] <- NA

    observeEvent(input$modal_missings, {
      tryCatch(
        {
          modal_data_missings(data = data_demo, id = "modal_missings")
        },
        error = function(err) {
          showNotification(paste0("We encountered the following error browsing your data: ", err), type = "err")
        }
      )
    })
  }
  shiny::shinyApp(ui, server)
}

missing_demo_app()


modal_data_missings <- function(data,
                                title = "Show missing pattern",
                                easyClose = TRUE,
                                size = "xl",
                                footer = NULL,
                                ...) {

  datar <- if (is.reactive(data)) data else reactive(data)

  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    tags$div(
      shiny::renderPlot({
        visdat::vis_dat(datar())+
          # ggplot2::theme_void() +
          ggplot2::theme(
            # legend.position = "none",
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            # axis.text.y = element_blank(),
            # axis.title.y = element_blank(),
            text = ggplot2::element_text(size = 15),
            # axis.text = ggplot2::element_blank(),
            # panel.background = ggplot2::element_rect(fill = "white"),
            # plot.background = ggplot2::element_rect(fill = "white"),
            # panel.border = ggplot2::element_blank()
            plot.title = ggplot2::element_blank()
          )
      })
    ),
    easyClose = easyClose,
    size = size,
    footer = footer
  ))
}
