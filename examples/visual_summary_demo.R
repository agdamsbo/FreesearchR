visual_summary_demo_app <- function() {
  ui <- shiny::fluidPage(
    shiny::actionButton(
      inputId = "modal_missings",
      label = "Visual summary",
      width = "100%",
      disabled = FALSE
    )
  )
  server <- function(input, output, session) {
    data_demo <- mtcars
    data_demo[sample(1:32, 10), "cyl"] <- NA
    data_demo[sample(1:32, 8), "vs"] <- NA
    data_demo$gear <- factor(data_demo$gear)

    visual_summary_server(id = "data", data = shiny::reactive(data_demo),summary.fun=class)

    observeEvent(input$modal_missings, {
      tryCatch(
        {
          modal_visual_summary(id = "data")
        },
        error = function(err) {
          showNotification(paste0("We encountered the following error browsing your data: ", err), type = "err")
        }
      )
    })
  }
  shiny::shinyApp(ui, server)
}

visual_summary_demo_app()
