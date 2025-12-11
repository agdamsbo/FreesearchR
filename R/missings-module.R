#' Data correlations evaluation module
#'
#' @param id Module id
#' @param ... additional UI elements to show before the table overview
#'
#' @name data-missings
#' @returns Shiny ui module
#' @export
data_missings_ui <- function(id, ...) {
  ns <- shiny::NS(id)

  list(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        bslib::accordion(
          id = ns("acc_mis"),
          open = "acc_chars",
          multiple = FALSE,
          bslib::accordion_panel(
            value = "acc_pan_mis",
            title = "Settings",
            icon = bsicons::bs_icon("x-circle"),
            shiny::uiOutput(ns("missings_method")),
            shiny::uiOutput(ns("missings_var")),
            shiny::helpText(i18n$t("Evaluate missingness by either comparing missing values across variables (optionally grouped by af categorical or dichotomous variable) or compare variables grouped by the missing status (missing or not) of an outcome variable. If there is a significant difference i the missingness, this may cause a bias in you data and should be considered carefully interpreting the data and analyses as data may not be missing at random.")),
            shiny::br(),
            shiny::actionButton(
              inputId = ns("act_miss"),
              label = i18n$t("Evaluate"),
              width = "100%",
              icon = shiny::icon("calculator"),
              disabled = FALSE
            )
          )
        )
      ),
      ...,
      gt::gt_output(outputId = ns("missings_table"))
    )
  )
}

## This should really just be rebuild to only contain a function

#'
#' @param data data
#' @param output.format output format
#'
#' @name data-missings
#' @returns shiny server module
#' @export
data_missings_server <- function(id,
                                 data,
                                 max_level = 20,
                                 ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      datar <- if (is.reactive(data)) data else reactive(data)

      rv <- shiny::reactiveValues(
        data = NULL,
        table = NULL
      )

      ## Notes
      ##
      ## Code export is still missing
      ## Direct table export would be nice

      shiny::observe(
        output$missings_method <- shiny::renderUI({
          shiny::req(data())
          vectorSelectInput(
            inputId = ns("missings_method"),
            label = i18n$t("Analysis method for missingness overview"),
            choices = setNames(
              c(
                "predictors",
                "outcome"
              ),
              c(
                i18n$t("Overview of missings across variables"),
                i18n$t("Overview of difference in variables by missing status in outcome")
              )
            )
          )
        })
      )

      shiny::observe({
        output$missings_var <- shiny::renderUI({
          shiny::req(datar())
          shiny::req(input$missings_method)
          # browser()
          if (input$missings_method == "predictors") {
            label <- i18n$t("Select a variable for grouped overview")
            df <- data_type_filter(data(), type = c("categorical", "dichotomous"))
            col_subset <- c("none", names(df))
          } else {
            label <- i18n$t("Select outcome variable for overview")
            df <- datar()[apply(datar(), 2, anyNA)]
            col_subset <- names(df)
          }
          columnSelectInput(
            inputId = ns("missings_var"),
            label = label,
            data = df,
            col_subset = col_subset,
            none_label = i18n$t("No variable")
          )
        })
      })


      shiny::observeEvent(
        list(input$act_miss),
        {
          shiny::req(datar())
          shiny::req(input$missings_var)
          # browser()
          df_tbl <- datar()
          by_var <- input$missings_var

          parameters <- list(
            by_var = by_var,
            max_level = max_level,
            type = input$missings_method
          )

          tryCatch(
            {
              shiny::withProgress(message = i18n$t("Calculating. Hold tight for a moment.."), {
              out <- do.call(
                compare_missings,
                modifyList(parameters, list(data = df_tbl))
              )
              })
            },
            error = function(err) {
              showNotification(paste0("Error: ", err), type = "err")
            }
          )

          if (is.null(input$missings_var) || input$missings_var == "" || !input$missings_var %in% names(datar()) || input$missings_var == "none") {
            # if (is.null(variabler()) || variabler() == "" || !variabler() %in% names(data()) || variabler() == "none") {
            # tbl <- rv$data()
            if (anyNA(datar())) {
              if (input$missings_method == "predictors") {
                title <- i18n$t("Overview of missing observations")
              } else {
                title <- i18n$t("No outcome measure chosen")
              }
            } else {
              title <- i18n$t("No missing observations")
            }
          } else {
            ## Due to reactivity, the table updates too quickly. this mitigates that issue..


            if (input$missings_var == "predictors") {
              title <- glue::glue(i18n$t("Missings across variables by the variable **'{input$missings_var}'**"))
            } else {
              title <- glue::glue(i18n$t("Missing vs non-missing observations in the variable **'{input$missings_var}'**"))
            }
          }

          attr(out, "tbl_title") <- title

          rv$data <- shiny::reactive(out)
        }
      )

      shiny::observeEvent(
        list(
          # input$act_miss
          rv$data
        ),
        {
          output$missings_table <- gt::render_gt({
            shiny::req(rv$data)
            # shiny::req(input$missings_var)
            # browser()
            if ("p.value" %in% names(rv$data()[["table_body"]])) {
              tbl <- rv$data() |>
                gtsummary::bold_p()
            } else {
              tbl <- rv$data()
            }


            out <- tbl |>
              gtsummary::as_gt() |>
              gt::tab_header(title = gt::md(attr(tbl, "tbl_title")))

            attr(out, "strat_var") <- input$missings_var

            rv$table <- out

            out
          })
        }
      )

      return(shiny::reactive(rv$table))
    }
  )
}


missing_demo_app <- function() {
  ui <- do.call(
    bslib::page,
    c(
      list(
        title = i18n$t("Missings"),
        icon = bsicons::bs_icon("x-circle")
      ),
      data_missings_ui(id = "data")
    )
  )
  server <- function(input, output, session) {
    data_demo <- mtcars
    data_demo[sample(1:32, 10), "cyl"] <- NA
    data_demo[sample(1:32, 8), "vs"] <- NA

    data_missings_server(id = "data", data = data_demo)

    # visual_summary_server(id = "visual", data = data_demo)

    # observeEvent(input$modal_missings, {
    #   tryCatch(
    #     {
    #       modal_visual_summary(id = "visual")
    #     },
    #     error = function(err) {
    #       showNotification(paste0("We encountered the following error browsing your data: ", err), type = "err")
    #     }
    #   )
    # })
  }
  shiny::shinyApp(ui, server)
}

# missing_demo_app()

#' Pairwise comparison of missings across covariables
#'
#' @param data data frame
#' @param by_var variable to stratify by missingness
#'
#' @returns gtsummary list object
#' @export
#'
compare_missings <- function(
  data,
  by_var,
  max_level = 20,
  type = c("predictors", "outcome")
) {
  type <- match.arg(type)

  if (!is.null(by_var) && by_var != "" && by_var %in% names(data)) {
    data <- data |>
      lapply(\(.x){
        if (is.factor(.x)) {
          cut_var(.x, breaks = 20, type = "top")
        } else {
          .x
        }
      }) |>
      dplyr::bind_cols()

    if (type == "predictors") {
      data <- missings_logic_across(data, exclude = by_var)
    } else {
      data[[by_var]] <- ifelse(is.na(data[[by_var]]), "Missing", "Non-missing")
    }

    out <- gtsummary::tbl_summary(data, by = by_var) |>
      gtsummary::add_p()
  } else {
    if (type == "predictors") {
      data <- missings_logic_across(data)
    }

    out <- gtsummary::tbl_summary(data)
  }

  out
}

#' Converting all variables to logicals by missing status
#'
#' @param data data
#' @param exclude character vector of variable names to be excluded
#'
#' @returns data frame
#' @export
#'
#' @examples
#' mtcars |> missings_logic_across("cyl")
#' ## gtsummary::trial |>
#' ##   missings_logic_across() |>
#' ##   gtsummary::tbl_summary()
missings_logic_across <- function(data, exclude = NULL) {
  # This function includes a approach way to preserve variable labels
  names(data) |>
    lapply(\(.x){
      # browser()
      # Saving original labels
      lab <- REDCapCAST::get_attr(data[[.x]], attr = "label")
      if (!.x %in% exclude) {
        out <- is.na(data[[.x]])
      } else {
        out <- data[[.x]]
      }
      if (!is.na(lab)) {
        # Restoring original labels, if not NA
        REDCapCAST::set_attr(data = out, label = lab, attr = "label", overwrite = TRUE)
      } else {
        out
      }
    }) |>
    dplyr::bind_cols(.name_repair = "unique_quiet") |>
    setNames(names(data))
}
