library(readr)
library(MASS)
library(stats)
library(gt)
# library(openxlsx2)
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
# library(esquisse)
library(patchwork)
library(DHARMa)
library(apexcharter)
library(toastui)
library(datamods)
library(IDEAFilter)
library(shinyWidgets)
library(DT)
library(data.table)
library(gtsummary)
# library(FreesearchR)

# source("functions.R")

data(starwars)
data(mtcars)
mtcars_date <- mtcars |> append_column(as.Date(sample(1:365, nrow(mtcars))), "rand_dates")
mtcars_date$date <- as.Date(sample(seq_len(365),nrow(mtcars)))
data(trial)


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
    regression = list(),
    ds = NULL,
    local_temp = NULL,
    ready = NULL,
    test = "no",
    data_original = NULL,
    data_temp = NULL,
    data = NULL,
    data_variables = NULL,
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
    rv$code <- modifyList(x = rv$code, list(import = data_file$code()))
  })

  from_redcap <- m_redcap_readServer(
    id = "redcap_import"
  )

  shiny::observeEvent(from_redcap$data(), {
    rv$data_temp <- from_redcap$data()
    rv$code <- modifyList(x = rv$code, list(import = from_redcap$code()))
  })

  ## This is used to ensure the reactive data is retrieved
  output$redcap_prev <- DT::renderDT(
    {
      DT::datatable(head(from_redcap$data(), 5),
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
    rv$code <- modifyList(x = rv$code, list(import = from_env$name()))
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

  output$data_loaded <- shiny::reactive({
    !is.null(rv$data_temp)
  })

  shiny::observeEvent(input$source, {
    rv$data_temp <- NULL
  })

  shiny::outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  shiny::observeEvent(
    eventExpr = list(
      input$import_var,
      input$complete_cutoff,
      rv$data_temp
    ),
    handlerExpr = {
      shiny::req(rv$data_temp)
      shiny::req(input$import_var)
      # browser()
      temp_data <- rv$data_temp
      if (all(input$import_var %in% names(temp_data))) {
        temp_data <- temp_data |> dplyr::select(input$import_var)
      }

      rv$data_original <- temp_data |>
        default_parsing()

      rv$code$import <- rv$code$import |>
        expression_string(assign.str = "df <-")

      rv$code$format <- list(
        "df",
        rlang::expr(dplyr::select(dplyr::all_of(!!input$import_var))),
        rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
      ) |>
        lapply(expression_string) |>
        pipe_string() |>
        expression_string(assign.str = "df <-")

      rv$code$filter <- NULL
      rv$code$modify <- NULL
    }, ignoreNULL = FALSE
  )

  output$data_info_import <- shiny::renderUI({
    shiny::req(rv$data_original)
    data_description(rv$data_original)
  })

  ## Activating action buttons on data imported
  shiny::observeEvent(rv$data_original, {
    if (is.null(rv$data_original) | NROW(rv$data_original) == 0) {
      shiny::updateActionButton(inputId = "act_start", disabled = TRUE)
      shiny::updateActionButton(inputId = "modal_browse", disabled = TRUE)
      shiny::updateActionButton(inputId = "act_eval", disabled = TRUE)
    } else {
      shiny::updateActionButton(inputId = "act_start", disabled = FALSE)
      shiny::updateActionButton(inputId = "modal_browse", disabled = FALSE)
      shiny::updateActionButton(inputId = "act_eval", disabled = FALSE)
    }
  })

  ##############################################################################
  #########
  #########  Data modification section
  #########
  ##############################################################################

  shiny::observeEvent(
    eventExpr = list(
      rv$data_original
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
        rv$code$variables <- NULL
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


  #########
  #########  Modifications
  #########

  ## Using modified version of the datamods::cut_variable_server function
  ## Further modifications are needed to have cut/bin options based on class of variable
  ## Could be defined server-side

  output$data_info <- shiny::renderUI({
    shiny::req(data_filter())
    data_description(data_filter(), "The filtered data")
  })

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
    rv$code$modify[[length(rv$code$modify) + 1]] <- attr(rv$data, "code")
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
    rv$code$modify[[length(rv$code$modify) + 1]] <- attr(rv$data, "code")
  })

  #########  Create column

  shiny::observeEvent(
    input$modal_column,
    modal_create_column(
      id = "modal_column",
      footer = "This window is aimed at advanced users and require some R-experience!",
      title = "Create new variables"
    )
  )
  data_modal_r <- create_column_server(
    id = "modal_column",
    data_r = reactive(rv$data)
  )
  shiny::observeEvent(
    data_modal_r(),
    {
      rv$data <- data_modal_r()
      rv$code$modify[[length(rv$code$modify) + 1]] <- attr(rv$data, "code")
    }
  )

  #########  Subset, rename, reclass

  updated_data <- update_variables_server(
    id = "modal_variables",
    data = shiny::reactive(rv$data),
    return_data_on_init = FALSE
  )

  shiny::observeEvent(updated_data(), {
    rv$data <- updated_data()
    rv$code$modify[[length(rv$code$modify) + 1]] <- attr(rv$data, "code")
  })

  ### Column filter
  ### Completely implemented, but it takes a little considering where in the
  ### data flow to implement, as it will act destructively on previous
  ### manipulations

  output$column_filter <- shiny::renderUI({
    shiny::req(rv$data)
    # c("dichotomous", "ordinal", "categorical", "datatime", "continuous")
    shinyWidgets::virtualSelectInput(
      inputId = "column_filter",
      label = "Select variable types to include",
      selected = unique(data_type(rv$data)),
      choices = unique(data_type(rv$data)),
      updateOn = "change",
      multiple = TRUE,
      search = FALSE,
      showValueAsTags = TRUE
    )
  })

  shiny::observe({
    # shiny::req(input$column_filter)
    out <- data_type_filter(rv$data, input$column_filter)
    rv$data_variables <- out
    if (!is.null(input$column_filter)) {
      rv$code$variables <- attr(out, "code")
    }
    # rv$code$modify[[length(rv$code$modify) + 1]] <- attr(rv$data, "code")
  })


  #########  Data filter
  # IDEAFilter has the least cluttered UI, but might have a License issue
  data_filter <- IDEAFilter::IDEAFilter("data_filter",
    data = shiny::reactive(rv$data_variables),
    verbose = TRUE
  )

  shiny::observeEvent(
    list(
      shiny::reactive(rv$data_variables),
      shiny::reactive(rv$data_original),
      data_filter(),
      # regression_vars(),
      input$complete_cutoff
    ),
    {
      ###  Save filtered data
      rv$data_filtered <- data_filter()

      ###  Save filtered data
      ###  without empty factor levels
      rv$list$data <- data_filter() |>
        REDCapCAST::fct_drop() |>
        (\(.x){
          .x[!sapply(.x, is.character)]
        })()

      ## This looks messy!! But it works as intended for now

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
          paste(c("df <- df", .x[-1], "REDCapCAST::fct_drop()"),
            collapse = "|> \n "
          )
        })()

      rv$code <- append_list(data = out, list = rv$code, index = "filter")
    }
  )

  #########  Data preview

  ###  Overview

  data_summary_server(
    id = "data_summary",
    data = shiny::reactive({
      rv$data_filtered
    }),
    color.main = "#2A004E",
    color.sec = "#C62300",
    pagination = 10
  )

  observeEvent(input$modal_browse, {
    show_data(REDCapCAST::fct_drop(rv$data_filtered), title = "Uploaded data overview", type = "modal")
  })

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


  ##############################################################################
  #########
  #########  Code export
  #########
  ##############################################################################

  ## This really should be collapsed to only one call, but I'll leave it for now
  ## as a working example of dynamically defining outputs and rendering.

  # output$code_import <- shiny::renderPrint({
  #   shiny::req(rv$code$import)
  #   cat(c("#Data import\n", rv$code$import))
  # })

  output$code_import <- shiny::renderUI({
    prismCodeBlock(paste0("#Data import\n", rv$code$import))
  })

  output$code_import <- shiny::renderUI({
    prismCodeBlock(paste0("#Data import formatting\n", rv$code$format))
  })

  output$code_data <- shiny::renderUI({
    shiny::req(rv$code$modify)
    # browser()
    ## This will create three lines for each modification
    # ls <- rv$code$modify
    ## This will remove all non-unique entries
    # ls <- rv$code$modify |> unique()
    ## This will only remove all non-repeating entries
    ls <- rv$code$modify[!is_identical_to_previous(rv$code$modify)]

    out <- ls |>
      lapply(expression_string) |>
      pipe_string() |>
      expression_string(assign.str = "df <- df |>\n")

    prismCodeBlock(paste0("#Data modifications\n", out))
  })

  output$code_variables <- shiny::renderUI({
    shiny::req(rv$code$variables)
    out <- expression_string(rv$code$variables, assign.str = "df <- df |>\n")
    prismCodeBlock(paste0("#Variables filter\n", out))
  })

  output$code_filter <- shiny::renderUI({
    shiny::req(rv$code$filter)
    prismCodeBlock(paste0("#Data filter\n", rv$code$filter))
  })

  output$code_table1 <- shiny::renderUI({
    shiny::req(rv$code$table1)
    prismCodeBlock(paste0("#Data characteristics table\n", rv$code$table1))
  })


  ## Just a note to self
  ## This is a very rewarding couple of lines marking new insights to dynamically rendering code
  shiny::observe({
    rv$regression()$regression$models |> purrr::imap(\(.x, .i){
      output[[paste0("code_", tolower(.i))]] <- shiny::renderUI({
        prismCodeBlock(paste0(paste("#", .i, "regression model\n"), .x$code_table))
      })
    })
  })


  ##############################################################################
  #########
  #########  Data analyses Inputs
  #########
  ##############################################################################

  output$strat_var <- shiny::renderUI({
    columnSelectInput(
      inputId = "strat_var",
      selected = "none",
      label = "Select variable to stratify baseline",
      data = shiny::reactive(rv$data_filtered)(),
      col_subset = c(
        "none",
        names(rv$data_filtered)[unlist(lapply(rv$data_filtered, data_type)) %in% c("dichotomous", "categorical", "ordinal")]
      )
    )
  })

  ##############################################################################
  #########
  #########  Descriptive evaluations
  #########
  ##############################################################################


  output$data_info_nochar <- shiny::renderUI({
    shiny::req(rv$list$data)
    data_description(rv$list$data, data_text = "The dataset without text variables")
  })

  shiny::observeEvent(
    list(
      input$act_eval
    ),
    {
      shiny::req(input$strat_var)
      shiny::req(rv$list$data)

      parameters <- list(
        by.var = input$strat_var,
        add.p = input$add_p == "yes",
        add.overall = TRUE
      )

      shiny::withProgress(message = "Creating the table. Hold on for a moment..", {
        rv$list$table1 <- rlang::exec(create_baseline, !!!append_list(rv$list$data, parameters, "data"))
      })

      rv$code$table1 <- glue::glue("FreesearchR::create_baseline(data,{list2str(parameters)})")
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

  pl <- data_visuals_server("visuals", data = shiny::reactive(rv$list$data))

  ##############################################################################
  #########
  #########  Regression model analyses
  #########
  ##############################################################################

  rv$regression <- regression_server("regression", data = shiny::reactive(rv$list$data))

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
      # browser()
      # Simplified for .rmd output attempt
      format <- ifelse(type == "docx", "word_document", "odt_document")

      # browser()
      rv$list$regression <- rv$regression()

      shiny::withProgress(message = "Generating the report. Hold on for a moment..", {
        tryCatch(
          {
            rv$list |>
              write_rmd(
                output_format = format,
                input = file.path(getwd(), "www/report.rmd")
              )
          },
          error = function(err) {
            showNotification(paste0("We encountered the following error creating your report: ", err), type = "err")
          }
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
