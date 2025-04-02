#' Regression module
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#' regression_demo_app()
#' }
regression_demo_app <- function() {
  ui <- bslib::page_fixed(
    do.call(
      bslib::navset_bar,
      regression_ui("regression")
    )
  )
  server <- function(input, output, session) {
    regression_server("regression", data = default_parsing(mtcars[1:3]))
  }
  shiny::shinyApp(ui, server)
}
