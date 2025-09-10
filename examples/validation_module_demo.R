#' Data and analyses validation demo
#'
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#' validation_demo_app()
#' }
validation_demo_app <- function() {
  ui <- shiny::fluidPage(
    shiny::tags$h2("Validation"),
    IDEAFilter::IDEAFilter_ui("data_filter"),
    shiny::br(),
    DT::DTOutput("data_final"),
    shiny::br(),
    validation_ui("validation_demo_2")
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      data_original = shiny::reactive(mtcars),
      data_filtered = NULL
    )

    rv_validation <- shiny::reactiveValues(
      obs_filter = NULL,
      vars_filter = NULL
    )

    data_filter <- IDEAFilter::IDEAFilter(
      id = "data_filter",
      data = mtcars,
      verbose = TRUE
    )

    shiny::observeEvent(
      data_filter(),
      {
        rv$data_filtered <- data_filter()
      }
    )

    output$data_final <- DT::renderDT(
      data_filter()
    )

    shiny::observeEvent(
      list(
        data_filter()
      ),
      {
        to_make_validation <- data_filter()
        ## Validation
        if (!is.null(to_make_validation)) {
          validation <-
            make_validation(
              ls = validation_lib("obs_filter"),
              list(
                x = mtcars,
                y = to_make_validation
              )
            )

        rv_validation$vars_filter <- validation

        validation_server(id = "validation_demo_2", data = rv_validation$vars_filter)

        }


      }
    )

    # shiny::observeEvent(
    #   list(
    #     shiny::reactive(rv_validation$vars_filter)(),
    #     data_filter()
    #   ),
    #   {
    #     # browser()
    #     # to_make_alert <- shiny::isolate(rv_validation$vars_filter)
    #     to_make_alert <- shiny::reactive(rv_validation$vars_filter)()
    #     if (!is.null(rv_validation$vars_filter)) {
    #       validation_server(id = "validation_demo_2", data = to_make_alert)
    #     }
    #   }
    # )
  }
  shiny::shinyApp(ui, server)
}


#' Title
#'
#' @returns
#' @export
#'
#' @examples
#' validation_nr_demo_app()
validation_nr_demo_app <- function() {
  ui <- shiny::fluidPage(
    shiny::tags$h2("Validation"),
    shiny::br(),
    validation_ui("validation_demo_1", max_height = "30px"),
    shiny::br(),
    validation_ui("validation_demo_2")
  )

  server <- function(input, output, session) {
    df_original <- mtcars

    df_obs <- mtcars |> dplyr::filter(mpg > 20)

    df_vars <- df_obs[1:6]

    val1 <- purrr::map2(
      .x = validation_lib()[1],
      .y = list(list(x = df_original, y = df_obs)),
      make_validation
    )

    val2 <- make_validation(
      ls = validation_lib()[[2]],
      list(x = df_original, y = df_vars)
    )

    validation_server(id = "validation_demo_1", data = val1)

    validation_server(id = "validation_demo_2", data = val2)
  }
  shiny::shinyApp(ui, server)
}
