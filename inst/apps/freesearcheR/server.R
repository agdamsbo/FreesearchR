library(readr)
library(MASS)
library(stats)
library(gt)
library(openxlsx2)
library(haven)
library(readODS)
require(shiny)
library(bslib)
library(assertthat)
library(dplyr)
library(quarto)
library(here)
library(broom)
library(broom.helpers)
# library(REDCapCAST)
library(easystats)
library(esquisse)
library(patchwork)
library(DHARMa)
library(apexcharter)
library(toastui)
library(datamods)
library(data.table)
library(IDEAFilter)
library(shinyWidgets)
library(DT)
library(gtsummary)
# library(freesearcheR)

# source("functions.R")

data(mtcars)
trial <- gtsummary::trial |> default_parsing()

# light <- custom_theme()
#
# dark <- custom_theme(bg = "#000",fg="#fff")


server <- function(input, output, session) {
  ## Listing files in www in session start to keep when ending and removing
  ## everything else.
  files.to.keep <- list.files("www/")

  output$docs_file <- shiny::renderUI({
    # shiny::includeHTML("www/docs.html")
    shiny::HTML(readLines("www/docs.html"))
  })

  ##############################################################################
  #########
  #########  Night mode (just very popular, not really needed)
  #########
  ##############################################################################

  # observeEvent(input$dark_mode,{
  #   session$setCurrentTheme(
  #   if (isTRUE(input$dark_mode)) dark else light
  # )})

  # observe({
  #   if(input$dark_mode==TRUE)
  #     session$setCurrentTheme(bs_theme_update(theme = custom_theme(version = 5)))
  #   if(input$dark_mode==FALSE)
  #     session$setCurrentTheme(bs_theme_update(theme = custom_theme(version = 5, bg = "#000",fg="#fff")))
  # })


  ##############################################################################
  #########
  #########  Setting reactive values
  #########
  ##############################################################################

  rv <- shiny::reactiveValues(
    list = list(),
    ds = NULL,
    local_temp = NULL,
    ready = NULL,
    test = "no",
    data_original = NULL,
    data_temp = NULL,
    data = NULL,
    data_filtered = NULL,
    models = NULL,
    code = list()
  )

  ##############################################################################
  #########
  #########  Data import section
  #########
  ##############################################################################

  data_file <- import_file_server(
    id = "file_import",
    show_data_in = "popup",
    trigger_return = "change",
    return_class = "data.frame"
  )

  shiny::observeEvent(data_file$data(), {
    shiny::req(data_file$data())
    rv$data_temp <- data_file$data()
    rv$code <- append_list(data = data_file$code(), list = rv$code, index = "import")
  })

  from_redcap <- m_redcap_readServer(
    id = "redcap_import"
  )

  shiny::observeEvent(from_redcap$data(), {
    # rv$data_original <- purrr::pluck(data_redcap(), "data")()
    rv$data_temp <- from_redcap$data()
    rv$code <- append_list(data = from_redcap$code(),list = rv$code,index = "import")
  })

  output$redcap_prev <- DT::renderDT(
    {
      DT::datatable(head(from_redcap$data(), 5),
        # DT::datatable(head(purrr::pluck(data_redcap(), "data")(), 5),
        caption = "First 5 observations"
      )
    },
    server = TRUE
  )

  from_env <- datamods::import_globalenv_server(
    id = "env",
    trigger_return = "change",
    btn_show_data = FALSE,
    reset = reactive(input$hidden)
  )

  shiny::observeEvent(from_env$data(), {
    shiny::req(from_env$data())

    rv$data_temp <- from_env$data()
    # rv$code <- append_list(data = from_env$code(),list = rv$code,index = "import")
  })

  output$import_var <- shiny::renderUI({
    shiny::req(rv$data_temp)

    preselect <- names(rv$data_temp)[sapply(rv$data_temp, missing_fraction) <= input$complete_cutoff / 100]

    shinyWidgets::virtualSelectInput(
      inputId = "import_var",
      label = "Select variables to include",
      selected = preselect,
      choices = names(rv$data_temp),
      updateOn = "change",
      multiple = TRUE,
      search = TRUE,
      showValueAsTags = TRUE
    )
  })


  shiny::observeEvent(
    eventExpr = list(
      input$import_var
    ),
    handlerExpr = {
      shiny::req(rv$data_temp)

      rv$data_original <- rv$data_temp |>
        dplyr::select(input$import_var) |>
        default_parsing()

      rv$code$import <- rv$code$import |>
        deparse() |>
        paste(collapse="") |>
        paste("|>
        dplyr::select(",paste(input$import_var,collapse=","),") |>
        freesearcheR::default_parsing()") |>
        (\(.x){
          paste0("data <- ",.x)
        })()

      rv$code$filter <- NULL
      rv$code$modify <- NULL
    }
  )


  shiny::observeEvent(rv$data_original, {
    if (is.null(rv$data_original) | NROW(rv$data_original) == 0) {
      shiny::updateActionButton(inputId = "act_start", disabled = TRUE)
    } else {
      shiny::updateActionButton(inputId = "act_start", disabled = FALSE)
    }
  })

  ##############################################################################
  #########
  #########  Data modification section
  #########
  ##############################################################################

  shiny::observeEvent(
    eventExpr = list(
      rv$data_original,
      input$complete_cutoff
    ),
    handlerExpr = {
      shiny::req(rv$data_original)

      rv$data <- rv$data_original
    }
  )

  ## For now this solution work, but I would prefer to solve this with the above
  shiny::observeEvent(input$reset_confirm,
    {
      if (isTRUE(input$reset_confirm)) {
        shiny::req(rv$data_original)
        rv$data <- rv$data_original
        rv$code$filter <- NULL
        rv$code$modify <- NULL
      }
    },
    ignoreNULL = TRUE
  )


  shiny::observeEvent(input$data_reset, {
    shinyWidgets::ask_confirmation(
      cancelOnDismiss = TRUE,
      inputId = "reset_confirm",
      title = "Please confirm data reset?",
      type = "warning"
    )
  })

  # shiny::observeEvent(input$reset_confirm, {
  #   rv$data <- rv$data_original |> default_parsing()
  # })

  #########  Overview

  data_summary_server(
    id = "data_summary",
    data = shiny::reactive({
      rv$data_filtered
    }),
    color.main = "#2A004E",
    color.sec = "#C62300",
    pagination = 20
  )

  #########
  #########  Modifications
  #########

  ## Using modified version of the datamods::cut_variable_server function
  ## Further modifications are needed to have cut/bin options based on class of variable
  ## Could be defined server-side

  shiny::observeEvent(
    input$modal_variables,
    modal_update_variables("modal_variables", title = "Update and select variables")
  )


  #########  Create factor

  shiny::observeEvent(
    input$modal_cut,
    modal_cut_variable("modal_cut", title = "Create new factor")
  )

  data_modal_cut <- cut_variable_server(
    id = "modal_cut",
    data_r = shiny::reactive(rv$data)
  )

  shiny::observeEvent(data_modal_cut(), {
    rv$data <- data_modal_cut()
    rv$code$modify[[length(rv$code$modify)+1]] <- attr(rv$data,"code")
    })

  #########  Modify factor

  shiny::observeEvent(
    input$modal_update,
    datamods::modal_update_factor(id = "modal_update", title = "Reorder factor levels")
  )

  data_modal_update <- datamods::update_factor_server(
    id = "modal_update",
    data_r = reactive(rv$data)
  )

  shiny::observeEvent(data_modal_update(), {
    shiny::removeModal()
    rv$data <- data_modal_update()
    rv$code$modify[[length(rv$code$modify)+1]] <- attr(rv$data,"code")
  })

  #########  Create column

  shiny::observeEvent(
    input$modal_column,
    datamods::modal_create_column(
      id = "modal_column",
      footer = "This window is aimed at advanced users and require some R-experience!",
      title = "Create new variables"
    )
  )
  data_modal_r <- datamods::create_column_server(
    id = "modal_column",
    data_r = reactive(rv$data)
  )
  shiny::observeEvent(
    data_modal_r(),
    {
      rv$data <- data_modal_r()
      rv$code$modify[[length(rv$code$modify)+1]] <- attr(rv$data,"code")
    }
  )

  #########  Show result
  tryCatch(
    {
      output$table_mod <- toastui::renderDatagrid({
        shiny::req(rv$data)
        # data <- rv$data
        toastui::datagrid(
          # data = rv$data # ,
          data = data_filter(),
          pagination = 10
          # bordered = TRUE,
          # compact = TRUE,
          # striped = TRUE
        )
      })
    },
    warning = function(warn) {
      showNotification(paste0(warn), type = "warning")
    },
    error = function(err) {
      showNotification(paste0(err), type = "err")
    }
  )

  # output$code <- renderPrint({
  #   attr(rv$data, "code")
  # })

  # updated_data <- datamods::update_variables_server(
  updated_data <- update_variables_server(
    id = "modal_variables",
    data = reactive(rv$data),
    return_data_on_init = FALSE
  )

  output$original_str <- renderPrint({
    str(rv$data_original)
  })

  output$modified_str <- renderPrint({
    str(as.data.frame(rv$data_filtered) |>
      REDCapCAST::set_attr(
        label = NULL,
        attr = "code"
      ))
  })

  shiny::observeEvent(updated_data(), {
    rv$data <- updated_data()
  })

  # IDEAFilter has the least cluttered UI, but might have a License issue
  data_filter <- IDEAFilter::IDEAFilter("data_filter", data = reactive(rv$data), verbose = TRUE)

  shiny::observeEvent(
    list(
      shiny::reactive(rv$data),
      shiny::reactive(rv$data_original),
      data_filter(),
      regression_vars(),
      input$complete_cutoff
    ),
    {
      rv$data_filtered <- data_filter()

      rv$list$data <- data_filter() |>
        REDCapCAST::fct_drop()
    }
  )

  shiny::observeEvent(
    list(
      shiny::reactive(rv$data),
      shiny::reactive(rv$data_original),
      data_filter(),
      shiny::reactive(rv$data_filtered)
    ),
    {
      out <- gsub(
        "filter", "dplyr::filter",
        gsub(
          "\\s{2,}", " ",
          paste0(
            capture.output(attr(rv$data_filtered, "code")),
            collapse = " "
          )
        )
      )

      out <- strsplit(out, "%>%") |>
        unlist() |>
        (\(.x){
          paste(c("data", .x[-1]), collapse = "|> \n ")
        })()

      rv$code <- append_list(data = out, list = rv$code, index = "filter")
    }
  )

  output$code_import <- shiny::renderPrint({
    cat(rv$code$import)
  })

  output$code_data <- shiny::renderPrint({
    ls <- rv$code$modify |> unique()
    out <- paste("data |> \n",
                 sapply(ls,\(.x) paste(deparse(.x),collapse=",")),
                 collapse="|> \n")
    cat(out)
  })

  output$code_filter <- shiny::renderPrint({
    cat(rv$code$filter)
  })

  ##############################################################################
  #########
  #########  Data analyses Inputs
  #########
  ##############################################################################

  ## Keep these "old" selection options as a simple alternative to the modification pane

  output$include_vars <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "include_vars",
      selected = NULL,
      label = "Covariables to include",
      choices = colnames(rv$data_filtered),
      multiple = TRUE
    )
  })

  output$outcome_var <- shiny::renderUI({
    shiny::selectInput(
      inputId = "outcome_var",
      selected = NULL,
      label = "Select outcome variable",
      choices = colnames(rv$data_filtered),
      multiple = FALSE
    )
  })

  output$regression_type <- shiny::renderUI({
    shiny::req(input$outcome_var)
    shiny::selectizeInput(
      inputId = "regression_type",
      label = "Choose regression analysis",
      ## The below ifelse statement handles the case of loading a new dataset
      choices = possible_functions(
        data = dplyr::select(
          rv$data_filtered,
          ifelse(input$outcome_var %in% names(rv$data_filtered),
            input$outcome_var,
            names(rv$data_filtered)[1]
          )
        ), design = "cross-sectional"
      ),
      multiple = FALSE
    )
  })

  output$factor_vars <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "factor_vars",
      selected = colnames(rv$data_filtered)[sapply(rv$data_filtered, is.factor)],
      label = "Covariables to format as categorical",
      choices = colnames(rv$data_filtered),
      multiple = TRUE
    )
  })

  ## Collected regression variables
  regression_vars <- shiny::reactive({
    if (is.null(input$include_vars)) {
      out <- colnames(rv$data_filtered)
    } else {
      out <- unique(c(input$include_vars, input$outcome_var))
    }
    return(out)
  })

  output$strat_var <- shiny::renderUI({
    shiny::selectInput(
      inputId = "strat_var",
      selected = "none",
      label = "Select variable to stratify baseline",
      choices = c(
        "none",
        rv$data_filtered |>
          (\(.x){
            lapply(.x, \(.c){
              if (identical("factor", class(.c))) {
                .c
              }
            }) |>
              dplyr::bind_cols()
          })() |>
          colnames()
      ),
      multiple = FALSE
    )
  })


  output$plot_model <- shiny::renderUI({
    shiny::req(rv$list$regression$tables)
    shiny::selectInput(
      inputId = "plot_model",
      selected = "none",
      label = "Select models to plot",
      choices = names(rv$list$regression$tables),
      multiple = TRUE
    )
  })


  ##############################################################################
  #########
  #########  Descriptive evaluations
  #########
  ##############################################################################

  shiny::observeEvent(
    # ignoreInit = TRUE,
    list(
      shiny::reactive(rv$list$data),
      shiny::reactive(rv$data),
      shiny::reactive(rv$data_original),
      data_filter(),
      input$strat_var,
      input$include_vars,
      input$complete_cutoff,
      input$add_p
    ),
    {
      shiny::req(input$strat_var)
      shiny::req(rv$list$data)

      if (input$strat_var == "none" | !input$strat_var %in% names(rv$list$data)) {
        by.var <- NULL
      } else {
        by.var <- input$strat_var
      }

      rv$list$table1 <-
        rv$list$data |>
        baseline_table(
          fun.args =
            list(
              by = by.var
            )
        ) |>
        (\(.x){
          if (!is.null(by.var)) {
            .x |> gtsummary::add_overall()
          } else {
            .x
          }
        })() |>
        (\(.x){
          if (input$add_p == "yes" & !is.null(by.var)) {
            .x |>
              gtsummary::add_p() |>
              gtsummary::bold_p()
          } else {
            .x
          }
        })()

      # gtsummary::as_kable(rv$list$table1) |>
      #   readr::write_lines(file="./www/_table1.md")
    }
  )

  output$outcome_var_cor <- shiny::renderUI({
    columnSelectInput(
      inputId = "outcome_var_cor",
      selected = "none",
      data = rv$list$data,
      label = "Select outcome variable",
      col_subset = c(
        "none",
        colnames(rv$list$data)
      ),
      multiple = FALSE
    )
  })

  output$table1 <- gt::render_gt({
    shiny::req(rv$list$table1)

    rv$list$table1 |>
      gtsummary::as_gt() |>
      gt::tab_header(gt::md("**Table 1: Baseline Characteristics**"))
  })

  data_correlations_server(
    id = "correlations",
    data = shiny::reactive({
      shiny::req(rv$list$data)
      out <- rv$list$data
      if (!is.null(input$outcome_var_cor) && input$outcome_var_cor != "none") {
        out <- out[!names(out) %in% input$outcome_var_cor]
      }
      out
    }),
    cutoff = shiny::reactive(input$cor_cutoff)
  )

  ##############################################################################
  #########
  #########  Data visuals
  #########
  ##############################################################################

  pl <- data_visuals_server("visuals", data = shiny::reactive(rv$data))

  ##############################################################################
  #########
  #########  Regression model analyses
  #########
  ##############################################################################

  shiny::observeEvent(
    input$load,
    {
      shiny::req(input$outcome_var)
      # browser()
      # Assumes all character variables can be formatted as factors
      # data <- data_filter$filtered() |>
      tryCatch(
        {
          ## Which models to create should be decided by input
          ## Could also include
          ##   imputed or
          ##   minimally adjusted
          model_lists <- list(
            "Univariable" = regression_model_uv_list,
            "Multivariable" = regression_model_list
          ) |>
            lapply(\(.fun){
              ls <- do.call(
                .fun,
                c(
                  list(data = rv$list$data |>
                    (\(.x){
                      .x[regression_vars()]
                    })()),
                  list(outcome.str = input$outcome_var),
                  list(fun.descr = input$regression_type)
                )
              )
            })

          # browser()

          rv$list$regression$params <- get_fun_options(input$regression_type) |>
            (\(.x){
              .x[[1]]
            })()

          rv$list$regression$models <- model_lists

          # names(rv$list$regression)

          # rv$models <- lapply(model_lists, \(.x){
          #   .x$model
          # })
        },
        warning = function(warn) {
          showNotification(paste0(warn), type = "warning")
        },
        error = function(err) {
          showNotification(paste0("Creating regression models failed with the following error: ", err), type = "err")
        }
      )
    }
  )

  shiny::observeEvent(
    ignoreInit = TRUE,
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
        warning = function(warn) {
          showNotification(paste0(warn), type = "warning")
        },
        error = function(err) {
          showNotification(paste0("Running model assumptions checks failed with the following error: ", err), type = "err")
        }
      )
    }
  )

  output$check <- shiny::renderPlot(
    {
      shiny::req(rv$check)
      # browser()
      # p <- plot(rv$check) +
      #   patchwork::plot_annotation(title = "Multivariable regression model checks")

      p <- plot(rv$check) +
        patchwork::plot_annotation(title = "Multivariable regression model checks")

      for (i in seq_len(length(p))) {
        p[[i]] <- p[[i]] + gg_theme_shiny()
      }

      p

      # p + patchwork::plot_layout(ncol = 1, design = ggplot2::waiver())

      # Generate checks in one column
      # layout <- sapply(seq_len(length(p)), \(.x){
      #   patchwork::area(.x, 1)
      # })
      #
      # p + patchwork::plot_layout(design = Reduce(c, layout))

      # patchwork::wrap_plots(ncol=1) +
      # patchwork::plot_annotation(title = 'Multivariable regression model checks')
    },
    height = 600,
    alt = "Assumptions testing of the multivariable regression model"
  )


  shiny::observeEvent(
    input$load,
    {
      shiny::req(rv$list$regression$models)
      tryCatch(
        {
          out <- lapply(rv$list$regression$models, \(.x){
            .x$model
          }) |>
            purrr::map(regression_table)

          if (input$add_regression_p == "no") {
            out <- out |>
              lapply(\(.x){
                .x |>
                  gtsummary::modify_column_hide(
                    column = "p.value"
                  )
              })
          }

          rv$list$regression$tables <- out

          # rv$list$regression$table <- out |>
          #   tbl_merge()

          # gtsummary::as_kable(rv$list$regression$table) |>
          #   readr::write_lines(file="./www/_regression_table.md")

          rv$list$input <- input
        },
        warning = function(warn) {
          showNotification(paste0(warn), type = "warning")
        },
        error = function(err) {
          showNotification(paste0("Creating a regression table failed with the following error: ", err), type = "err")
        }
      )
      rv$ready <- "ready"
    }
  )

  output$table2 <- gt::render_gt({
    shiny::req(rv$list$regression$tables)
    rv$list$regression$tables |>
      tbl_merge() |>
      gtsummary::as_gt() |>
      gt::tab_header(gt::md(glue::glue("**Table 2: {rv$list$regression$params$descr}**")))
  })

  output$regression_plot <- shiny::renderPlot(
    {
      # shiny::req(rv$list$regression$plot)
      shiny::req(input$plot_model)

      out <- merge_long(rv$list$regression, input$plot_model) |>
        plot.tbl_regression(
          colour = "variable",
          facet_col = "model"
        )

      out +
        ggplot2::scale_y_discrete(labels = scales::label_wrap(15)) +
        gg_theme_shiny()

      # rv$list$regression$tables$Multivariable |>
      #   plot(colour = "variable") +
      #   ggplot2::scale_y_discrete(labels = scales::label_wrap(15)) +
      #   gg_theme_shiny()
    },
    height = 500,
    alt = "Regression coefficient plot"
  )

  shiny::conditionalPanel(
    condition = "output.uploaded == 'yes'",
  )

  ##############################################################################
  #########
  #########  Page navigation
  #########
  ##############################################################################

  shiny::observeEvent(input$act_start, {
    bslib::nav_select(id = "main_panel", selected = "Data")
  })

  ##############################################################################
  #########
  #########  Reactivity
  #########
  ##############################################################################

  output$uploaded <- shiny::reactive({
    if (is.null(rv$ds)) {
      "no"
    } else {
      "yes"
    }
  })

  shiny::outputOptions(output, "uploaded", suspendWhenHidden = FALSE)

  output$ready <- shiny::reactive({
    if (is.null(rv$ready)) {
      "no"
    } else {
      "yes"
    }
  })

  shiny::outputOptions(output, "ready", suspendWhenHidden = FALSE)

  # Reimplement from environment at later time
  # output$has_input <- shiny::reactive({
  #   if (rv$input) {
  #     "yes"
  #   } else {
  #     "no"
  #   }
  # })

  # shiny::outputOptions(output, "has_input", suspendWhenHidden = FALSE)

  ##############################################################################
  #########
  #########  Downloads
  #########
  ##############################################################################

  # Could be rendered with other tables or should show progress
  # Investigate quarto render problems
  # On temp file handling: https://github.com/quarto-dev/quarto-cli/issues/3992
  output$report <- downloadHandler(
    filename = shiny::reactive({
      paste0("report.", input$output_type)
    }),
    content = function(file, type = input$output_type) {
      # shiny::req(rv$list$regression)
      ## Notification is not progressing
      ## Presumably due to missing

      # Simplified for .rmd output attempt
      format <- ifelse(type == "docx", "word_document", "odt_document")

      shiny::withProgress(message = "Generating the report. Hold on for a moment..", {
        rv$list |>
          write_rmd(
            output_format = format,
            input = file.path(getwd(), "www/report.rmd")
          )

        # write_quarto(
        #   output_format = type,
        #   input = file.path(getwd(), "www/report.qmd")
        # )
      })
      file.rename(paste0("www/report.", type), file)
    }
  )

  output$data_modified <- downloadHandler(
    filename = shiny::reactive({
      paste0("modified_data.", input$data_type)
    }),
    content = function(file, type = input$data_type) {
      if (type == "rds") {
        readr::write_rds(rv$list$data, file = file)
      } else if (type == "dta") {
        haven::write_dta(as.data.frame(rv$list$data), path = file)
      } else if (type == "csv") {
        readr::write_csv(rv$list$data, file = file)
      }
    }
  )

  ##############################################################################
  #########
  #########  Clearing the session on end
  #########
  ##############################################################################

  session$onSessionEnded(function() {
    cat("Session Ended\n")
    files <- list.files("www/")
    lapply(files[!files %in% files.to.keep], \(.x){
      unlink(paste0("www/", .x), recursive = FALSE)
      print(paste(.x, "deleted"))
    })
  })
}
