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
    gt::gt_output(outputId = ns("missings_table"))
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
                                 variable,
                                 ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # ns <- session$ns

      datar <- if (is.reactive(data)) data else reactive(data)
      variabler <- if (is.reactive(variable)) variable else reactive(variable)

      rv <- shiny::reactiveValues(
        data = NULL,
        table = NULL
      )

      rv$data <- shiny::reactive({
        df_tbl <- datar()
        by_var <- variabler()

        tryCatch(
          {
            out <- compare_missings(df_tbl,by_var)
          },
          error = function(err) {
            showNotification(paste0("Error: ", err), type = "err")
          }
        )

        out
      })

      output$missings_table <- gt::render_gt({
        shiny::req(datar)
        shiny::req(variabler)

        if (is.null(variabler()) || variabler() == "" || !variabler() %in% names(datar())) {
          tbl <- rv$data()
          if (anyNA(datar())){
            title <- i18n$t("No variable chosen for analysis")
          } else {
          title <- i18n$t("No missing observations")
          }
        } else {
          tbl <- rv$data()|>
            gtsummary::bold_p()
          title <- glue::glue(i18n$t("Missing vs non-missing observations in the variable **'{variabler()}'**"))
        }

        out <-  tbl |>
          gtsummary::as_gt() |>
          gt::tab_header(title = gt::md(title))

        rv$table <- out

        out
      })

      return(reactive(rv$table))
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
    ),
    shiny::selectInput(
      inputId = "missings_var",
      label = "Select variable to stratify analysis", choices = c("cyl", "vs")
    ),
    data_missings_ui("data")
  )
  server <- function(input, output, session) {
    data_demo <- mtcars
    data_demo[sample(1:32, 10), "cyl"] <- NA
    data_demo[sample(1:32, 8), "vs"] <- NA

    data_missings_server(id = "data", data = data_demo, variable = shiny::reactive(input$missings_var))

    visual_summary_server(id = "visual", data = data_demo)

    observeEvent(input$modal_missings, {
      tryCatch(
        {
          modal_visual_summary(id = "visual")
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

#' Pairwise comparison of missings across covariables
#'
#' @param data data frame
#' @param by_var variable to stratify by missingness
#'
#' @returns gtsummary list object
#' @export
#'
compare_missings <- function(data,by_var){
  if (!is.null(by_var) && by_var != "" && by_var %in% names(data)) {
    data[[by_var]] <- ifelse(is.na(data[[by_var]]), "Missing", "Non-missing")

    out <- gtsummary::tbl_summary(data, by = by_var) |>
      gtsummary::add_p()
  } else {
    out <- gtsummary::tbl_summary(data)
  }
  out
}
