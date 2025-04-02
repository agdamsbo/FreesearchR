#' Download demo
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#' download_demo_app()
#' }
download_demo_app <- function() {
  ui <- bslib::page_fixed(
    bslib::nav_panel(
      title = "test",
      bslib::navset_bar(
        sidebar = bslib::sidebar(
          bslib::accordion(
            do.call(
              bslib::accordion_panel,
              c(
                list(
                  value = "acc_download",
                  title = "Download",
                  icon = bsicons::bs_icon("download")
                ),
                plot_download_ui("regression")
              )
            )
          )
        )
      )
    )
  )
  server <- function(input, output, session) {
    plot_download_server(
      id = "regression",
      data = {
        lm(mpg ~ ., default_parsing(mtcars)) |>
          gtsummary::tbl_regression() |>
          plot(colour = "variable")
      }
    )
  }
  shiny::shinyApp(ui, server)
}
