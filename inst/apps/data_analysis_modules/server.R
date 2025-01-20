library(readr)
library(MASS)
library(stats)
library(gtsummary)
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
library(patchwork)
library(DHARMa)
library(apexcharter)
library(toastui)
library(datamods)
library(data.table)
library(IDEAFilter)
library(shinyWidgets)
library(DT)
# library(freesearcheR)

# source("functions.R")



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
    data = NULL,
    data_filtered = NULL,
    models = NULL,
    check = NULL
  )

  ##############################################################################
  #########
  #########  Data import section
  #########
  ##############################################################################

  consider.na <- c("NA", "\"\"", "")

  data_file <- datamods::import_file_server(
    id = "file_import",
    show_data_in = "popup",
    trigger_return = "change",
    return_class = "data.frame",
    read_fns = list(
      ods = function(file) {
        readODS::read_ods(path = file, na = consider.na)
      },
      dta = function(file) {
        haven::read_dta(file = file, .name_repair = "unique_quiet")
      },
      csv = function(file) {
        readr::read_csv(file = file, na = consider.na)
      },
      # xls = function(file){
      #   openxlsx2::read_xlsx(file = file, na.strings = consider.na,)
      # },
      # xlsx = function(file){
      #   openxlsx2::read_xlsx(file = file, na.strings = consider.na,)
      # },
      rds = function(file) {
        readr::read_rds(file = file)
      }
    )
  )

  shiny::observeEvent(data_file$data(), {
    shiny::req(data_file$data())
    rv$data_original <- data_file$data()
  })

  data_redcap <- m_redcap_readServer(
    id = "redcap_import",
    output.format = "list"
  )

  shiny::observeEvent(data_redcap(), {
    rv$data_original <- purrr::pluck(data_redcap(), "data")()
  })

  output$redcap_prev <- DT::renderDT(
    {
      DT::datatable(head(purrr::pluck(data_redcap(), "data")(), 5),
        caption = "First 5 observations"
      )
    },
    server = TRUE
  )

  from_env <- import_globalenv_server(
    id = "env",
    trigger_return = "change",
    btn_show_data = FALSE,
    reset = reactive(input$hidden)
  )

  shiny::observeEvent(from_env$data(), {
    shiny::req(from_env$data())
    rv$data_original <- from_env$data()
  })


  ##############################################################################
  #########
  #########  Data modification section
  #########
  ##############################################################################

  shiny::observeEvent(rv$data_original, {
    rv$data <- rv$data_original |> default_parsing()
  })

  shiny::observeEvent(input$data_reset, {
    shinyWidgets::ask_confirmation(
      inputId = "reset_confirm",
      title = "Please confirm data reset?"
    )
  })

  shiny::observeEvent(input$reset_confirm, {
    rv$data <- rv$data_original |> default_parsing()
  })

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

  #########  Create factor

  shiny::observeEvent(
    input$modal_cut,
    modal_cut_variable("modal_cut")
  )
  data_modal_cut <- cut_variable_server(
    id = "modal_cut",
    data_r = shiny::reactive(rv$data)
  )
  shiny::observeEvent(data_modal_cut(), rv$data <- data_modal_cut())

  #########  Modify factor

  shiny::observeEvent(
    input$modal_update,
    datamods::modal_update_factor(id = "modal_update")
  )
  data_modal_update <- datamods::update_factor_server(
    id = "modal_update",
    data_r = reactive(rv$data)
  )
  shiny::observeEvent(data_modal_update(), {
    shiny::removeModal()
    rv$data <- data_modal_update()
  })

  #########  Create column

  shiny::observeEvent(
    input$modal_column,
    datamods::modal_create_column(id = "modal_column")
  )
  data_modal_r <- datamods::create_column_server(
    id = "modal_column",
    data_r = reactive(rv$data)
  )
  shiny::observeEvent(
    data_modal_r(),
    {
      rv$data <- data_modal_r()
    }
  )

  #########  Show result

  output$table_mod <- toastui::renderDatagrid({
    shiny::req(rv$data)
    # data <- rv$data
    toastui::datagrid(
      # data = rv$data # ,
      data = data_filter(), pagination = 30,
      # bordered = TRUE,
      # compact = TRUE,
      # striped = TRUE
    )
  })

  output$code <- renderPrint({
    attr(rv$data, "code")
  })

  # updated_data <- datamods::update_variables_server(
  updated_data <- update_variables_server(
    id = "vars_update",
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
      base_vars(),
      input$complete_cutoff
    ),
    {
      rv$data_filtered <- data_filter()

      rv$list$data <- data_filter() |>
        REDCapCAST::fct_drop.data.frame() |>
        (\(.x){
          .x[base_vars()]
        })() |>
        janitor::remove_empty(
          which = "cols",
          cutoff = input$complete_cutoff / 100
        )
    }
  )

  output$filtered_code <- shiny::renderPrint({
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

    cat(out)
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
      # selected = colnames(rv$data_filtered)[sapply(rv$data_filtered, is.factor)],
      label = "Choose regression analysis",
      choices = possible_functions(data = dplyr::select(rv$data_filtered, input$outcome_var), design = "cross-sectional"),
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

  base_vars <- shiny::reactive({
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
        rv$data_filtered[base_vars()] |>
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

  ## Have a look at column filters at some point
  ## There should be a way to use the filtering the filter data for further analyses
  ## Disabled for now, as the JS is apparently not isolated
  # output$data_table <-
  #   DT::renderDT(
  #     {
  #       DT::datatable(ds()[base_vars()])
  #     },
  #     server = FALSE
  #   )
  #
  # output$data.classes <- gt::render_gt({
  #   shiny::req(input$file)
  #   data.frame(matrix(sapply(ds(), \(.x){
  #     class(.x)[1]
  #   }), nrow = 1)) |>
  #     stats::setNames(names(ds())) |>
  #     gt::gt()
  # })


  ### Outputs

  # shiny::observeEvent(data_filter(), {
  #   rv$data_filtered <- data_filter()
  # })

  # shiny::observeEvent(
  #   shiny::reactive(rv$data_filtered),
  #   {
  #     rv$list$data <- rv$data_filtered |>
  #       # dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor)) |>
  #       REDCapCAST::fct_drop.data.frame() |>
  #       # factorize(vars = input$factor_vars) |>
  #       remove_na_attr()
  #
  #     # rv$list$data <- data
  #     # rv$list$data <- data[base_vars()]
  #   }
  # )

  #   shiny::observe({
  #     if (input$strat_var == "none") {
  #       by.var <- NULL
  #     } else {
  #       by.var <- input$strat_var
  #     }
  #
  #     rv$list$table1 <- rv$list$data |>
  #       baseline_table(
  #         fun.args =
  #           list(
  #             by = by.var
  #           )
  #       ) |>
  #       (\(.x){
  #         if (!is.null(by.var)) {
  #           .x |> gtsummary::add_overall()
  #         } else {
  #           .x
  #         }
  #       })() |>
  #       (\(.x){
  #         if (input$add_p == "yes") {
  #           .x |>
  #             gtsummary::add_p() |>
  #             gtsummary::bold_p()
  #         } else {
  #           .x
  #         }
  #       })()
  #   })
  #
  #     output$table1 <- gt::render_gt(
  #       rv$list$table1 |>
  #         gtsummary::as_gt() |>
  # gt::tab_header(shiny::md("**Table 1. Patient Characteristics**"))
  #     )

  ##############################################################################
  #########
  #########  Data analyses results
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
      input$add_p,
      input$complete_cutoff
    ),
    {
      shiny::req(input$strat_var)
      shiny::req(rv$list$data)

      if (input$strat_var == "none") {
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
          if (input$add_p == "yes") {
            .x |>
              gtsummary::add_p() |>
              gtsummary::bold_p()
          } else {
            .x
          }
        })()
    }
  )


  output$table1 <- gt::render_gt({
    shiny::req(rv$list$table1)

    rv$list$table1 |>
      gtsummary::as_gt() |>
      gt::tab_header(gt::md("**Table 1: Baseline Characteristics**"))
  })

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
                  list(data = rv$list$data),
                  list(outcome.str = input$outcome_var),
                  list(fun.descr = input$regression_type)
                )
              )
            })

          # browser()

          rv$list$regression$options <- get_fun_options(input$regression_type) |>
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

  output$check <- shiny::renderPlot({
    shiny::req(rv$check)
    p <- plot(rv$check) +
      patchwork::plot_annotation(title = "Multivariable regression model checks")
    p
    # Generate checks in one column
    # layout <- sapply(seq_len(length(p)), \(.x){
    #   patchwork::area(.x, 1)
    # })
    #
    # p + patchwork::plot_layout(design = Reduce(c, layout))

    # patchwork::wrap_plots(ncol=1) +
    # patchwork::plot_annotation(title = 'Multivariable regression model checks')
  })


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

          rv$list$regression$table <- out |>
            tbl_merge()

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
    shiny::req(rv$list$regression$table)
    rv$list$regression$table |>
      gtsummary::as_gt() |>
      gt::tab_header(gt::md(glue::glue("**Table 2: {rv$list$regression$options$descr}**")))
  })


  shiny::conditionalPanel(
    condition = "output.uploaded == 'yes'",
  )

  # observeEvent(input$act_start, {
  #   nav_show(id = "overview",target = "Import"
  #   )
  # })

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
      shiny::req(rv$list$regression)
      ## Notification is not progressing
      ## Presumably due to missing
      shiny::withProgress(message = "Generating the report. Hold on for a moment..", {
        rv$list |>
          write_quarto(
            output_format = type,
            input = file.path(getwd(), "www/report.qmd")
          )
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
      } else {
        haven::write_dta(as.data.frame(rv$list$data), path = file)
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
