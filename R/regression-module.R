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
    # title = "",
    bslib::nav_panel(
      title = i18n$t("Regression table"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::uiOutput(outputId = ns("data_info"), inline = TRUE),
          bslib::accordion(
            id = "acc_reg",
            open = "acc_reg",
            multiple = FALSE,
            bslib::accordion_panel(
              value = "acc_pan_reg",
              title = i18n$t("Regression"),
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
              # shiny::radioButtons(
              #   inputId = ns("all"),
              #   label = i18n$t("Specify covariables"),
              #   inline = TRUE,
              #   selected = 2,
              #   choiceNames = c(
              #     "Yes",
              #     "No"
              #   ),
              #   choiceValues = c(1, 2)
              # ),
              shiny::uiOutput(outputId = ns("all_vars")),
              shiny::conditionalPanel(
                condition = "input.all==1",
                shiny::uiOutput(outputId = ns("regression_vars")),
                shiny::helpText(i18n$t("If none are selected, all are included.")),
                shiny::tags$br(),
                ns = ns
              ),
              bslib::input_task_button(
                id = ns("load"),
                label = i18n$t("Analyse"),
                icon = bsicons::bs_icon("pencil"),
                label_busy = i18n$t("Working..."),
                icon_busy = fontawesome::fa_i("arrows-rotate",
                  class = "fa-spin",
                  "aria-hidden" = "true"
                ),
                type = "secondary",
                auto_reset = TRUE
              ),
              shiny::helpText(i18n$t("Press 'Analyse' to create the regression model and after changing parameters.")),
              shiny::tags$br(),
              shiny::uiOutput(outputId = ns("add_regression_p")),
              # shiny::tags$br(),
              # shiny::radioButtons(
              #   inputId = ns("tbl_theme"),
              #   label = "Show p-value",
              #   inline = TRUE,
              #   selected = "jama",
              #   choices = list(
              #     "JAMA" = "jama",
              #     "Lancet" = "lancet",
              #     "NEJM" = "nejm"
              #   )
              # ),
              shiny::tags$br()
            )
          )
        ),
        gt::gt_output(outputId = ns("table2"))
      )
    ),
    bslib::nav_panel(
      title = i18n$t("Coefficient plot"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::accordion(
            id = "acc_coef_plot",
            open = "acc_pan_coef_plot",
            multiple = FALSE,
            do.call(
              bslib::accordion_panel,
              c(
                list(
                  value = "acc_pan_coef_plot",
                  title = "Coefficients plot",
                  icon = bsicons::bs_icon("bar-chart-steps"),
                  shiny::tags$br(),
                  shiny::uiOutput(outputId = ns("plot_model"))
                ),
                # plot_download_ui(ns("reg_plot_download"))
                shiny::tagList(
                  shinyWidgets::noUiSliderInput(
                    inputId = ns("plot_height"),
                    label = i18n$t("Plot height (mm)"),
                    min = 50,
                    max = 300,
                    value = 100,
                    step = 1,
                    format = shinyWidgets::wNumbFormat(decimals = 0),
                    color = datamods:::get_primary_color()
                  ),
                  shinyWidgets::noUiSliderInput(
                    inputId = ns("plot_width"),
                    label = i18n$t("Plot width (mm)"),
                    min = 50,
                    max = 300,
                    value = 100,
                    step = 1,
                    format = shinyWidgets::wNumbFormat(decimals = 0),
                    color = datamods:::get_primary_color()
                  ),
                  shiny::selectInput(
                    inputId = ns("plot_type"),
                    label = i18n$t("File format"),
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
                    label = i18n$t("Download plot"),
                    icon = shiny::icon("download")
                  )
                )
              )
            )
          )
        ),
        shiny::plotOutput(outputId = ns("regression_plot"), height = "80vh")
      )
    ),
    bslib::nav_panel(
      title = i18n$t("Model checks"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::accordion(
            id = "acc_checks",
            open = "acc_pan_checks",
            multiple = FALSE,
            bslib::accordion_panel(
              value = "acc_pan_checks",
              title = "Checks",
              icon = bsicons::bs_icon("clipboard-check"),
              shiny::uiOutput(outputId = ns("plot_checks"))
            )
          )
        ),
        shiny::plotOutput(outputId = ns("check"), height = "90vh")
      )
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

      ## Update on laguage change

      shiny::observe({
        bslib::accordion_panel_update(id = "acc_reg", target = "acc_pan_reg", title = i18n$t("Regression"))
        bslib::accordion_panel_update(id = "acc_coef_plot", target = "acc_pan_coef_plot", title = i18n$t("Coefficients plot"))
        bslib::accordion_panel_update(id = "acc_checks", target = "acc_pan_checks", title = i18n$t("Checks"))
      })

      output$all_vars <- shiny::renderUI(
        shiny::radioButtons(
          inputId = ns("all"),
          label = i18n$t("Specify covariables"),
          inline = TRUE,
          selected = 2,
          choiceNames = c(
            i18n$t("Yes"),
            i18n$t("No")
          ),
          choiceValues = c(1, 2)
        ),
      )

      output$add_regression_p <- shiny::renderUI(
        shiny::radioButtons(
        inputId = ns("add_regression_p"),
        label = i18n$t("Show p-value"),
        inline = TRUE,
        selected = "yes",
        choiceNames = c(
          i18n$t("Yes"),
          i18n$t("No")
        ),
        choiceValues = c("yes", "no")
      ))



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
          label = i18n$t("Select outcome variable"),
          data = data_r(),
          multiple = FALSE
        )
      })

      output$regression_type <- shiny::renderUI({
        shiny::req(input$outcome_var)
        shiny::selectizeInput(
          inputId = ns("regression_type"),
          label = i18n$t("Choose regression analysis"),
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
          label = i18n$t("Covariables to format as categorical"),
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
          label = i18n$t("Select variable to stratify baseline"),
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
          label = i18n$t("Select models to plot"),
          choices = names(rv$list$regression$tables),
          multiple = TRUE
        )
      })

      ##############################################################################
      #########
      #########  Regression models
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
                  parameters <- list(
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
              showNotification(paste(i18n$t("Creating regression models failed with the following error:"), err), type = "err")
            }
          )
        }
      )



      shiny::observeEvent(
        list(
          data_r(),
          regression_vars()
        ),
        {
          rv$list$regression$tables <- NULL
        }
      )

      ##############################################################################
      #########
      #########  Regression table
      #########
      ##############################################################################

      ### Creating the regression table
      shiny::observeEvent(
        input$load,
        {
          shiny::req(rv$list$regression$models)
          ## To avoid plotting old models on fail/error
          rv$list$regression$tables <- NULL

          # browser()
          tryCatch(
            {
              parameters <- list(
                p.values = input$add_regression_p == "no"
              )

              out <- lapply(rv$list$regression$models, \(.x){
                .x$model
              }) |>
                purrr::map(\(.x){
                  do.call(
                    regression_table,
                    append_list(.x, parameters, "x")
                  )
                })

              rv$list$regression$models |>
                purrr::imap(\(.x, .i){
                  rv$list$regression$models[[.i]][["code_table"]] <- paste(
                    .x$code,
                    expression_string(rlang::call2(.fn = "regression_table", !!!parameters, .ns = "FreesearchR"), assign.str = NULL),
                    sep = "|>\n"
                  )
                })

              rv$list$regression$tables <- out
              rv$list$input <- input
            },
            warning = function(warn) {
              showNotification(paste0(warn), type = "warning")
            },
            error = function(err) {
              showNotification(paste(i18n$t("Creating a regression table failed with the following error:"), err), type = "err")
            }
          )
        }
      )

      ## Consider creating merged table with theming and then passing object
      ## to render.

      output$table2 <- gt::render_gt({
        ## Print checks if a regression table is present
        if (!is.null(rv$list$regression$tables)) {
          # gtsummary::theme_gtsummary_journal(journal = "jama")
          merged <- rv$list$regression$tables |>
            tbl_merge()

          if (input$add_regression_p == "no") {
            merged <- merged |>
              gtsummary::modify_column_hide(column = dplyr::starts_with("p.value"))
          }

          out <- merged |>
            gtsummary::as_gt() |>
            gt::tab_header(gt::md(glue::glue("**Table 2: {rv$list$regression$params$descr}**")))

          # rv$list$regression$table_merged <- out

          out
        } else {
          return(NULL)
        }
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
          shiny::withProgress(message = i18n$t("Saving the plot. Hold on for a moment.."), {
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
              showNotification(paste(i18n$t("Running model assumptions checks failed with the following error:"), err), type = "err")
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
          label = i18n$t("Select checks to plot"),
          choices = names,
          multiple = TRUE
        )
      })

      output$check <- shiny::renderPlot(
        {
          shiny::req(rv$check_plot)
          shiny::req(input$plot_checks)

          ## Print checks if a regression table is present
          if (!is.null(rv$list$regression$tables)) {
            p <- rv$check_plot() +
              # patchwork::wrap_plots() +
              patchwork::plot_annotation(title = i18n$t("Multivariable regression model checks"))


            layout <- sapply(seq_len(length(p)), \(.x){
              patchwork::area(.x, 1)
            })

            p_list <- p + patchwork::plot_layout(design = Reduce(c, layout))

            index <- match(
              input$plot_checks,
              sapply(rv$check_plot(), \(.i){
                get_ggplot_label(.i, "title")
              })
            )

            ls <- list()

            for (i in index) {
              p <- p_list[[i]] +
                ggplot2::theme(
                  axis.text = ggplot2::element_text(size = 10),
                  axis.title = ggplot2::element_text(size = 12),
                  legend.text = ggplot2::element_text(size = 12),
                  plot.subtitle = ggplot2::element_text(size = 12),
                  plot.title = ggplot2::element_text(size = 18)
                )
              ls <- c(ls, list(p))
            }
            # browser()
            tryCatch(
              {
                out <- patchwork::wrap_plots(ls, ncol = if (length(ls) == 1) 1 else 2)
              },
              error = function(err) {
                showNotification(err, type = "err")
              }
            )

            out
          } else {
            return(NULL)
          }
        },
        alt = "Assumptions testing of the multivariable regression model"
      )

      ##############################################################################
      #########
      #########  Output
      #########
      ##############################################################################

      return(shiny::reactive({
        rv$list
      }))
    }
  )
}
