#' Correlations plot demo app
#'
#' @returns shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' cor_demo_app()
#' }
cor_demo_app <- function() {
  ui <- shiny::fluidPage(
    shiny::sliderInput(
      inputId = "cor_cutoff",
      label = "Correlation cut-off",
      min = 0,
      max = 1,
      step = .1,
      value = .7,
      ticks = FALSE
    ),
    data_correlations_ui("data", height = 600)
  )
  server <- function(input, output, session) {
    data_correlations_server("data", data = shiny::reactive(default_parsing(mtcars)), cutoff = shiny::reactive(input$cor_cutoff))
  }
  shiny::shinyApp(ui, server)
}
