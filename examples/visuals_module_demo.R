visuals_demo_app <- function() {
  ui <- bslib::page_fixed(
    do.call(
      bslib::navset_bar,
      c(
        data_visuals_ui("visuals"),
        shiny::tagList(
          bslib::nav_spacer(),
          bslib::nav_panel(
            title = "Notes",
            shiny::fluidRow(
              shiny::column(width = 2),
              shiny::column(
                width = 8,
                shiny::markdown("Look, it **works**!"),
                shiny::column(width = 2)
              )
            )
          )
        )
      )
    )
  )
  server <- function(input, output, session) {
    pl <- data_visuals_server("visuals", data = shiny::reactive(default_parsing(mtcars)))
  }
  shiny::shinyApp(ui, server)
}

if (FALSE){
visuals_demo_app()
}
