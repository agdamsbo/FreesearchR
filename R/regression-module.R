### On rewriting this module
###
### This module (and the plotting module) should be rewritten to allow for
### dynamically defining variable-selection for model evaluation.
### The principle of having a library of supported functions is fine, but should
### be expanded.
###
###

# list(
#   lm = list(
#     descr = "Linear regression model",
#     design = "cross-sectional",
#     parameters=list(
#       fun = "stats::lm",
#       args.list = NULL
#     ),
#     variables = list(
#       outcome.str = list(
#         fun = "columnSelectInput",
#         multiple = FALSE,
#         label = "Select the dependent/outcome variable."
#       )
#     ),
#     out.type = "continuous",
#     formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
#     table.fun = "gtsummary::tbl_regression",
#     table.args.list = list(exponentiate = FALSE)
#   ))
#
#   Regarding the regression model, it really should be the design selection,
#   that holds the input selection information, as this is what is deciding
#   the number and type of primary inputs.
#
#   Cross-sectional: outcome
#   MMRM: outcome, random effect (id, time)
#   Survival: time, status, strata(?)
#
#



regression_ui <- function(id, ...) {
  ns <- shiny::NS(id)

  shiny::tagList(
    title = "",
    sidebar = bslib::sidebar(
      shiny::uiOutput(outputId = ns("data_info"), inline = TRUE),
      bslib::accordion(
        open = "acc_reg",
        multiple = FALSE,
        bslib::accordion_panel(
          value = "acc_reg",
          title = "Regression",
          icon = bsicons::bs_icon("calculator"),
          shiny::uiOutput(outputId = ns("outcome_var")),
          # shiny::selectInput(
          #   inputId = "design",
          #   label = "Study design",
          #   selected = "no",
          #   inline = TRUE,
          #   choices = list(
          #     "Cross-sectional" = "cross-sectional"
          #   )
          # ),
          shiny::uiOutput(outputId = ns("regression_type")),
          shiny::radioButtons(
            inputId = ns("add_regression_p"),
            label = "Add p-value",
            inline = TRUE,
            selected = "yes",
            choices = list(
              "Yes" = "yes",
              "No" = "no"
            )
          ),
          shiny::radioButtons(
            inputId = ns("all"),
            label = "Specify covariables",
            inline = TRUE, selected = 2,
            choiceNames = c(
              "Yes",
              "No"
            ),
            choiceValues = c(1, 2)
          ),
          shiny::conditionalPanel(
            condition = "input.all==1",
            shiny::uiOutput(outputId = ns("regression_vars")),
            shiny::helpText("If none are selected, all are included."),
            shiny::tags$br(),
            ns = ns
          ),
          bslib::input_task_button(
            id = ns("load"),
            label = "Analyse",
            icon = bsicons::bs_icon("pencil"),
            label_busy = "Working...",
            icon_busy = fontawesome::fa_i("arrows-rotate",
              class = "fa-spin",
              "aria-hidden" = "true"
            ),
            type = "secondary",
            auto_reset = TRUE
          ),
          shiny::helpText("Press 'Analyse' to create the regression model and after changing parameters."),
          shiny::tags$br()
        ),
        do.call(
          bslib::accordion_panel,
          c(
            list(
              value = "acc_plot",
              title = "Coefficient plot",
              icon = bsicons::bs_icon("bar-chart-steps"),
              shiny::tags$br(),
              shiny::uiOutput(outputId = ns("plot_model"))
            ),
            # plot_download_ui(ns("reg_plot_download"))
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
          )
        ),
        bslib::accordion_panel(
          value = "acc_checks",
          title = "Checks",
          icon = bsicons::bs_icon("clipboard-check"),
          shiny::uiOutput(outputId = ns("plot_checks"))
        )
      )
    ),
    bslib::nav_panel(
      title = "Regression table",
      gt::gt_output(outputId = ns("table2"))
    ),
    bslib::nav_panel(
      title = "Coefficient plot",
      shiny::plotOutput(outputId = ns("regression_plot"), height = "80vh")
    ),
    bslib::nav_panel(
      title = "Model checks",
      shiny::plotOutput(outputId = ns("check"), height = "90vh")
    )
  )
}


regression_server <- function(id,
                              data,
                              ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      rv <- shiny::reactiveValues(
        data = NULL,
        plot = NULL,
        check = NULL,
        list = list()
      )

      data_r <- shiny::reactive({
        if (shiny::is.reactive(data)) {
          data()
        } else {
          data
        }
      })

      output$data_info <- shiny::renderUI({
        shiny::req(regression_vars())
        shiny::req(data_r())
        data_description(data_r()[regression_vars()])
      })

      ##############################################################################
      #########
      #########  Input fields
      #########
      ##############################################################################

      ## Keep these "old" selection options as a simple alternative to the modification pane


      output$regression_vars <- shiny::renderUI({
        columnSelectInput(
          inputId = ns("regression_vars"),
          selected = NULL,
          label = "Covariables to include",
          data = data_r(),
          multiple = TRUE
        )
      })

      output$outcome_var <- shiny::renderUI({
        columnSelectInput(
          inputId = ns("outcome_var"),
          selected = NULL,
          label = "Select outcome variable",
          data = data_r(),
          multiple = FALSE
        )
      })

      output$regression_type <- shiny::renderUI({
        shiny::req(input$outcome_var)
        shiny::selectizeInput(
          inputId = ns("regression_type"),
          label = "Choose regression analysis",
          ## The below ifelse statement handles the case of loading a new dataset
          choices = possible_functions(
            data = dplyr::select(
              data_r(),
              ifelse(input$outcome_var %in% names(data_r()),
                input$outcome_var,
                names(data_r())[1]
              )
            ), design = "cross-sectional"
          ),
          multiple = FALSE
        )
      })

      output$factor_vars <- shiny::renderUI({
        shiny::selectizeInput(
          inputId = ns("factor_vars"),
          selected = colnames(data_r())[sapply(data_r(), is.factor)],
          label = "Covariables to format as categorical",
          choices = colnames(data_r()),
          multiple = TRUE
        )
      })

      ## Collected regression variables
      regression_vars <- shiny::reactive({
        if (is.null(input$regression_vars)) {
          out <- colnames(data_r())
        } else {
          out <- unique(c(input$regression_vars, input$outcome_var))
        }
        return(out)
      })

      output$strat_var <- shiny::renderUI({
        columnSelectInput(
          inputId = ns("strat_var"),
          selected = "none",
          label = "Select variable to stratify baseline",
          data = data_r(),
          col_subset = c(
            "none",
            names(data_r())[unlist(lapply(data_r(), data_type)) %in% c("dichotomous", "categorical", "ordinal")]
          )
        )
      })


      output$plot_model <- shiny::renderUI({
        shiny::req(rv$list$regression$tables)
        shiny::selectInput(
          inputId = ns("plot_model"),
          selected = 1,
          label = "Select models to plot",
          choices = names(rv$list$regression$tables),
          multiple = TRUE
        )
      })

      ##############################################################################
      #########
      #########  Regression analysis
      #########
      ##############################################################################

      shiny::observeEvent(
        input$load,
        {
          shiny::req(input$outcome_var)

          rv$list$regression$models <- NULL

          tryCatch(
            {
              ## Which models to create should be decided by input
              ## Could also include
              ##   imputed or
              ##   minimally adjusted
              model_lists <- list(
                "Univariable" = "regression_model_uv_list",
                "Multivariable" = "regression_model_list"
              ) |>
                lapply(\(.fun){
                  parameters=list(
                    data = data_r()[regression_vars()],
                  outcome.str = input$outcome_var,
              fun.descr = input$regression_type
                  )

                  do.call(
                    .fun,
                    parameters
                  )
                })

              rv$list$regression$params <- get_fun_options(input$regression_type) |>
                (\(.x){
                  .x[[1]]
                })()

              rv$list$regression$models <- model_lists
            },
            error = function(err) {
              showNotification(paste0("Creating regression models failed with the following error: ", err), type = "err")
            }
          )
        }
      )

      ##############################################################################
      #########
      #########  Model checks
      #########
      ##############################################################################

      shiny::observeEvent(
        list(
          rv$list$regression$models
        ),
        {
          shiny::req(rv$list$regression$models)
          tryCatch(
            {
              rv$check <- lapply(rv$list$regression$models, \(.x){
                .x$model
              }) |>
                purrr::pluck("Multivariable") |>
                performance::check_model()
            },
            # warning = function(warn) {
            #   showNotification(paste0(warn), type = "warning")
            # },
            error = function(err) {
              showNotification(paste0("Running model assumptions checks failed with the following error: ", err), type = "err")
            }
          )
        }
      )

      rv$check_plot <- shiny::reactive(plot(rv$check))

      output$plot_checks <- shiny::renderUI({
        shiny::req(rv$list$regression$models)
        shiny::req(rv$check_plot)

        ## Implement correct plotting
        names <- sapply(rv$check_plot(), \(.i){
          # .i$labels$title
          get_ggplot_label(.i, "title")
        })

        vectorSelectInput(
          inputId = ns("plot_checks"),
          selected = 1,
          label = "Select checks to plot",
          choices = names,
          multiple = TRUE
        )
      })

      output$check <- shiny::renderPlot(
        {
          shiny::req(rv$check_plot)
          shiny::req(input$plot_checks)

          p <- rv$check_plot() +
            # patchwork::wrap_plots() +
            patchwork::plot_annotation(title = "Multivariable regression model checks")


          layout <- sapply(seq_len(length(p)), \(.x){
            patchwork::area(.x, 1)
          })

          out <- p + patchwork::plot_layout(design = Reduce(c, layout))

          index <- match(
            input$plot_checks,
            sapply(rv$check_plot(), \(.i){
              get_ggplot_label(.i, "title")
            })
          )

          ls <- list()

          for (i in index) {
            p <- out[[i]] +
              ggplot2::theme(axis.text = ggplot2::element_text(size = 10),
                             axis.title = ggplot2::element_text(size = 12),
                             legend.text = ggplot2::element_text(size = 12),
                             plot.subtitle = ggplot2::element_text(size = 12),
                             plot.title = ggplot2::element_text(size = 18))
            ls <- c(ls, list(p))
          }
          # browser()
          tryCatch(
            {
              patchwork::wrap_plots(ls, ncol = if (length(ls) == 1) 1 else 2)
            },
            error = function(err) {
              showNotification(err, type = "err")
            }
          )
        },
        alt = "Assumptions testing of the multivariable regression model"
      )

### Creating the regression table
      shiny::observeEvent(
        input$load,
        {
          shiny::req(rv$list$regression$models)
          ## To avoid plotting old models on fail/error
          rv$list$regression$tables <- NULL

          tryCatch(
            {
              parameters <- list(
                add_p = input$add_regression_p == "no"
              )

              out <- lapply(rv$list$regression$models, \(.x){
                .x$model
              }) |>
                purrr::map(\(.x){
                  do.call(
                    regression_table,
                    append_list(.x,parameters,"x")
                  )
})

              # if (input$add_regression_p == "no") {
              #   out <- out |>
              #     lapply(\(.x){
              #       .x |>
              #         gtsummary::modify_column_hide(
              #           column = "p.value"
              #         )
              #     })
              # }

              rv$list$regression$models |>
                purrr::imap(\(.x,.i){
                  rv$list$regression$models[[.i]][["code_table"]] <- paste(
                  .x$code,
                  expression_string(rlang::call2(.fn = "regression_table",!!!parameters,.ns = "FreesearchR"),assign.str=NULL),sep="|>\n")
                })

              list(
                rv$code$import,
                rlang::call2(.fn = "select",!!!list(input$import_var),.ns = "dplyr"),
                rlang::call2(.fn = "default_parsing",.ns = "FreesearchR")
              ) |>
                merge_expression() |>
                expression_string()

              rv$list$regression$tables <- out

              rv$list$input <- input
            },
            warning = function(warn) {
              showNotification(paste0(warn), type = "warning")
            },
            error = function(err) {
              showNotification(paste0("Creating a regression table failed with the following error: ", err), type = "err")
            }
          )
        }
      )

      output$table2 <- gt::render_gt({
        shiny::req(rv$list$regression$tables)
        rv$list$regression$tables |>
          tbl_merge() |>
          gtsummary::as_gt() |>
          gt::tab_header(gt::md(glue::glue("**Table 2: {rv$list$regression$params$descr}**")))
      })

      ##############################################################################
      #########
      #########  Coefficients plot
      #########
      ##############################################################################

      shiny::observeEvent(list(
        input$plot_model,
        rv$list$regression
      ), {
        shiny::req(input$plot_model)

        tryCatch(
          {
            p <- merge_long(
              rv$list$regression,
              sort_by(
                input$plot_model,
                c("Univariable", "Minimal", "Multivariable"),
                na.rm = TRUE
              )
            ) |>
              (\(.x){
                if (length(input$plot_model) > 1) {
                  plot.tbl_regression(
                    x = .x,
                    colour = "model",
                    dodged = TRUE
                  ) +
                    ggplot2::theme(legend.position = "bottom") +
                    ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))
                } else {
                  plot.tbl_regression(
                    x = .x,
                    colour = "variable"
                  ) +
                    ggplot2::theme(legend.position = "none")
                }
              })()

            rv$plot <- p +
              ggplot2::scale_y_discrete(labels = scales::label_wrap(15)) +
              gg_theme_shiny()
          },
          error = function(err) {
            showNotification(paste0(err), type = "err")
          }
        )
      })


      output$regression_plot <- shiny::renderPlot(
        {
          shiny::req(input$plot_model)

          rv$plot
        },
        alt = "Regression coefficient plot"
      )

      # plot_download_server(
      #   id = ns("reg_plot_download"),
      #   data = shiny::reactive(rv$plot)
      # )

      output$download_plot <- shiny::downloadHandler(
        filename = paste0("regression_plot.", input$plot_type),
        content = function(file) {
          shiny::withProgress(message = "Saving the plot. Hold on for a moment..", {
            ggplot2::ggsave(
              filename = file,
              plot = rv$plot,
              width = input$plot_width,
              height = input$plot_height,
              dpi = 300,
              units = "mm", scale = 2
            )
          })
        }
      )

      ##############################################################################
      #########
      #########  Output
      #########
      ##############################################################################

      return(shiny::reactive({
        return(rv$list)
      }))
    }
  )
}


