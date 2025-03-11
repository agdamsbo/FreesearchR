#' Shiny module to browser and export REDCap data
#'
#' @param id Namespace id
#' @param include_title logical to include title
#'
#' @rdname redcap_read_shiny_module
#'
#' @return shiny ui element
#' @export
m_redcap_readUI <- function(id, include_title = TRUE) {
  ns <- shiny::NS(id)

  server_ui <- shiny::tagList(
    # width = 6,
    shiny::tags$h4("REDCap server"),
    shiny::textInput(
      inputId = ns("uri"),
      label = "Web address",
      value = "https://redcap.your.institution/"
    ),
    shiny::helpText("Format should be either 'https://redcap.your.institution/' or 'https://your.institution/redcap/'"),
    shiny::textInput(
      inputId = ns("api"),
      label = "API token",
      value = ""
    ),
    shiny::helpText("The token is a string of 32 numbers and letters."),
    shiny::actionButton(
      inputId = ns("data_connect"),
      label = "Connect",
      icon = shiny::icon("link", lib = "glyphicon"),
      # width = NULL,
      disabled = TRUE
    ),
    shiny::br(),
    shiny::br(),
    tags$div(
      id = ns("connect-placeholder"),
      shinyWidgets::alert(
        id = ns("connect-result"),
        status = "info",
        tags$p(phosphoricons::ph("info", weight = "bold"), "Please fill in server address (URI) and API token, then press 'Connect'.")
      ),
      dismissible = TRUE
    ),
    shiny::br()
  )


  params_ui <-
    shiny::tagList(
      # width = 6,
      shiny::tags$h4("Data import parameters"),
      shiny::helpText("Options here will show, when API and uri are typed"),
      shiny::uiOutput(outputId = ns("fields")),
      shiny::uiOutput(outputId = ns("data_type")),
      shiny::uiOutput(outputId = ns("fill")),
      shinyWidgets::switchInput(
        inputId = "do_filter",
        label = "Apply filter?",
        value = FALSE,
        inline = FALSE,
        onLabel = "YES",
        offLabel = "NO"
      ),
      shiny::conditionalPanel(
        condition = "input.do_filter",
        shiny::uiOutput(outputId = ns("arms")),
        shiny::textInput(
          inputId = ns("filter"),
          label = "Optional filter logic (e.g., ⁠[gender] = 'female')"
        )
      )
    )


  shiny::fluidPage(
    if (include_title) shiny::tags$h3("Import data from REDCap"),
    bslib::layout_columns(
      server_ui,
      params_ui,
      col_widths = bslib::breakpoints(
        sm = c(12, 12),
        md = c(12, 12)
      )
    ),
    shiny::column(
      width = 12,
      # shiny::actionButton(inputId = ns("import"), label = "Import"),
      ## TODO: Use busy indicator like on download to have button activate/deactivate
      shiny::actionButton(
        inputId = ns("data_import"),
        label = "Import",
        icon = shiny::icon("download", lib = "glyphicon"),
        width = "100%",
        disabled = TRUE
      ),
      # bslib::input_task_button(
      #   id = ns("data_import"),
      #   label = "Import",
      #   icon = shiny::icon("download", lib = "glyphicon"),
      #   label_busy = "Just a minute...",
      #   icon_busy = fontawesome::fa_i("arrows-rotate",
      #     class = "fa-spin",
      #     "aria-hidden" = "true"
      #   ),
      #   type = "primary",
      #   auto_reset = TRUE#,state="busy"
      # ),
      shiny::br(),
      shiny::br(),
      shiny::helpText("Press 'Import' to get data from the REDCap server. Check the preview below before proceeding."),
      shiny::br(),
      shiny::br()
    )
  )
}


#' @rdname redcap_read_shiny_module
#'
#' @return shiny server module
#' @export
#'
m_redcap_readServer <- function(id) {
  module <- function(input, output, session) {
    ns <- session$ns

    data_rv <- shiny::reactiveValues(
      dd_status = NULL,
      data_status = NULL,
      uri = NULL,
      project_name = NULL,
      info = NULL,
      arms = NULL,
      dd_list = NULL,
      data = NULL,
      rep_fields = NULL,
      imported = NULL
    )

    shiny::observeEvent(list(input$api, input$uri), {
      uri <- paste0(ifelse(endsWith(input$uri, "/"), input$uri, paste0(input$uri, "/")), "api/")

      if (is_valid_redcap_url(uri) & is_valid_token(input$api)) {
        data_rv$uri <- uri
        shiny::updateActionButton(inputId = "data_connect", disabled = FALSE)
      } else {
        shiny::updateActionButton(inputId = "data_connect", disabled = TRUE)
      }
    })


    tryCatch(
      {
        shiny::observeEvent(
          list(
            input$data_connect
          ),
          {
            shiny::req(input$api)
            shiny::req(data_rv$uri)

            parameters <- list(
              redcap_uri = data_rv$uri,
              token = input$api
            )

            # browser()
            imported <- try(rlang::exec(REDCapR::redcap_metadata_read, !!!parameters), silent = TRUE)

            ## TODO: Simplify error messages
            if (inherits(imported, "try-error") || NROW(imported) < 1 || ifelse(is.list(imported), !isTRUE(imported$success), FALSE)) {
              if (ifelse(is.list(imported), !isTRUE(imported$success), FALSE)) {
                mssg <- imported$raw_text
              } else {
                mssg <- attr(imported, "condition")$message
              }

              datamods:::insert_error(mssg = mssg, selector = "connect")
              data_rv$dd_status <- "error"
              data_rv$dd_list <- NULL
            } else if (isTRUE(imported$success)) {
              data_rv$dd_status <- "success"

              data_rv$info <- REDCapR::redcap_project_info_read(
                redcap_uri = data_rv$uri,
                token = input$api
              )$data

              datamods:::insert_alert(
                selector = ns("connect"),
                status = "success",
                include_data_alert(see_data_text = "Click to see data dictionary",
                  dataIdName = "see_data",
                  extra = tags$p(tags$b(phosphoricons::ph("check", weight = "bold"), "Connected to server!"), tags$p(paste0(data_rv$info$project_title, " loaded."))),
                  btn_show_data = TRUE
                )
              )

              data_rv$dd_list <- imported
            }
          },
          ignoreInit = TRUE
        )
      },
      warning = function(warn) {
        showNotification(paste0(warn), type = "warning")
      },
      error = function(err) {
        showNotification(paste0(err), type = "err")
      }
    )

    shiny::observeEvent(input$see_data, {
      datamods::show_data(
        purrr::pluck(data_rv$dd_list, "data"),
        title = "Data dictionary",
        type = "modal",
        show_classes = FALSE,
        tags$b("Preview:")
      )
    })

    arms <- shiny::reactive({
      shiny::req(input$api)
      shiny::req(data_rv$uri)

      REDCapR::redcap_event_read(
        redcap_uri = data_rv$uri,
        token = input$api
      )$data
    })

    output$fields <- shiny::renderUI({
      shiny::req(data_rv$dd_list)
      shinyWidgets::virtualSelectInput(
        inputId = ns("fields"),
        label = "Select variables to import:",
        choices = purrr::pluck(data_rv$dd_list, "data") |>
          dplyr::select(field_name, form_name) |>
          (\(.x){
            split(.x$field_name, REDCapCAST::as_factor(.x$form_name))
          })(),
        updateOn = "change",
        multiple = TRUE,
        search = TRUE,
        showValueAsTags = TRUE
      )
    })

    output$data_type <- shiny::renderUI({
      shiny::req(data_rv$info)
      if (isTRUE(data_rv$info$has_repeating_instruments_or_events)) {
        vectorSelectInput(
          inputId = ns("data_type"),
          label = "Select the data format to import",
          choices = c(
            "Wide data (One row for each subject)" = "wide",
            "Long data for project with repeating instruments (default REDCap)" = "long"
          ),
          selected = "wide",
          multiple = FALSE
        )
      }
    })

    output$fill <- shiny::renderUI({
      shiny::req(data_rv$info)
      shiny::req(input$data_type)

      ## Get repeated field
      data_rv$rep_fields <- data_rv$dd_list$data$field_name[
        data_rv$dd_list$data$form_name %in% repeated_instruments(
          uri = data_rv$uri,
          token = input$api
        )
      ]

      if (input$data_type == "long" && isTRUE(any(input$fields %in% data_rv$rep_fields))) {
        vectorSelectInput(
          inputId = ns("fill"),
          label = "Fill missing values?",
          choices = c(
            "Yes, fill missing, non-repeated values" = "yes",
            "No, leave the data as is" = "no"
          ),
          selected = "yes",
          multiple = FALSE
        )
      }
    })

    shiny::observeEvent(input$fields, {
      if (is.null(input$fields) | length(input$fields) == 0) {
        shiny::updateActionButton(inputId = "data_import", disabled = TRUE)
      } else {
        shiny::updateActionButton(inputId = "data_import", disabled = FALSE)
      }
    })

    output$arms <- shiny::renderUI({
      vectorSelectInput(
        inputId = ns("arms"),
        selected = NULL,
        label = "Filter by events/arms",
        choices = stats::setNames(arms()[[3]], arms()[[1]]),
        multiple = TRUE
      )
    })

    shiny::observeEvent(input$data_import, {
      shiny::req(input$fields)
      record_id <- purrr::pluck(data_rv$dd_list, "data")[[1]][1]


      parameters <- list(
        uri = data_rv$uri,
        token = input$api,
        fields = unique(c(record_id, input$fields)),
        events = input$arms,
        raw_or_label = "both",
        filter_logic = input$filter,
        split_forms = if (input$data_type == "long") "none" else "all"
      )

      shiny::withProgress(message = "Downloading REDCap data. Hold on for a moment..", {
        imported <- try(rlang::exec(REDCapCAST::read_redcap_tables, !!!parameters), silent = TRUE)
      })
      code <- rlang::call2(REDCapCAST::read_redcap_tables, !!!parameters)


      if (inherits(imported, "try-error") || NROW(imported) < 1) {
        data_rv$data_status <- "error"
        data_rv$data_list <- NULL
      } else {
        data_rv$data_status <- "success"

        ## The data management below should be separated to allow for changing
        ## "wide"/"long" without re-importing data
        if (input$data_type != "long") {
          # browser()
          out <- imported |>
            # redcap_wider()
            REDCapCAST::redcap_wider()
        } else {
          if (input$fill == "yes") {
            ## Repeated fields


            ## Non-repeated fields in current dataset
            inc_non_rep <- names(imported)[!names(imported) %in% data_rv$rep_fields]

            out <- imported |>
              drop_empty_event() |>
              dplyr::group_by(!!dplyr::sym(names(imported)[1])) |>
              tidyr::fill(inc_non_rep) |>
              dplyr::ungroup()
          } else {
            out <- imported |>
              drop_empty_event()
          }
        }

        data_rv$data <- out |>
          dplyr::select(-dplyr::ends_with("_complete")) |>
          # dplyr::select(-dplyr::any_of(record_id)) |>
          REDCapCAST::suffix2label()
      }
    })

    # shiny::observe({
    #   shiny::req(data_rv$imported)
    #
    #   imported <- data_rv$imported
    #
    #
    # })

    return(shiny::reactive(data_rv$data))
  }

  shiny::moduleServer(
    id = id,
    module = module
  )
}

#' @importFrom htmltools tagList tags
#' @importFrom shiny icon getDefaultReactiveDomain
include_data_alert <- function(dataIdName = "see_data",
                               btn_show_data,
                               see_data_text = "Click to see data",
                               extra = NULL,
                               session = shiny::getDefaultReactiveDomain()) {
  if (isTRUE(btn_show_data)) {
    success_message <- tagList(
      extra,
      tags$br(),
      shiny::actionLink(
        inputId = session$ns(dataIdName),
        label = tagList(phosphoricons::ph("book-open-text"), see_data_text)
      )
    )
  }
  return(success_message)
}

# #' REDCap import teal data module
# #'
# #' @rdname redcap_read_shiny_module
# tdm_redcap_read <- teal::teal_data_module(
#   ui <- function(id) {
#     shiny::fluidPage(
#       m_redcap_readUI(id)
#     )
#   },
#   server = function(id) {
#     m_redcap_readServer(id, output.format = "teal")
#   }
# )


#' Test if url is valid format for REDCap API
#'
#' @param url url
#'
#' @returns logical
#' @export
#'
#' @examples
#' url <- c(
#'   "www.example.com",
#'   "redcap.your.inst/api/",
#'   "https://redcap.your.inst/api/",
#'   "https://your.inst/redcap/api/",
#'   "https://www.your.inst/redcap/api/"
#' )
#' is_valid_redcap_url(url)
is_valid_redcap_url <- function(url) {
  pattern <- "https://[^ /$.?#].[^\\s]*/api/$"
  stringr::str_detect(url, pattern)
}

#' Validate REDCap token
#'
#' @param token token
#' @param pattern_env pattern
#'
#' @returns logical
#' @export
#'
#' @examples
#' token <- paste(sample(c(1:9, LETTERS[1:6]), 32, TRUE), collapse = "")
#' is_valid_token(token)
is_valid_token <- function(token, pattern_env = NULL, nchar = 32) {
  checkmate::assert_character(token, any.missing = TRUE, len = 1)

  if (!is.null(pattern_env)) {
    checkmate::assert_character(pattern_env,
      any.missing = FALSE,
      len = 1
    )
    pattern <- pattern_env
  } else {
    pattern <- glue::glue("^([0-9A-Fa-f]{<nchar>})(?:\\n)?$",
      .open = "<",
      .close = ">"
    )
  }

  if (is.na(token)) {
    out <- FALSE
  } else if (is.null(token)) {
    out <- FALSE
  } else if (nchar(token) == 0L) {
    out <- FALSE
  } else if (!grepl(pattern, token, perl = TRUE)) {
    out <- FALSE
  } else {
    out <- TRUE
  }
  out
}

#' Get names of repeated instruments
#'
#' @param uri REDCap database uri
#' @param token database token
#'
#' @returns vector
#' @export
#'
repeated_instruments <- function(uri, token) {
  instruments <- REDCapR::redcap_event_instruments(redcap_uri = uri, token = token)
  unique(instruments$data$form[duplicated(instruments$data$form)])
}

#' Drop empty events from REDCap export
#'
#' @param data data
#' @param event "redcap_event_name", "redcap_repeat_instrument" or
#' "redcap_repeat_instance"
#'
#' @returns data.frame
#' @export
#'
drop_empty_event <- function(data, event = "redcap_event_name") {
  generics <- c(names(data)[1], "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance")

  filt <- split(data, data[[event]]) |>
    lapply(\(.x){
      dplyr::select(.x, -tidyselect::all_of(generics)) |>
        REDCapCAST::all_na()
    }) |>
    unlist()

  data[data[[event]] %in% names(filt)[!filt], ]
}


#' Test app for the redcap_read_shiny_module
#'
#' @rdname redcap_read_shiny_module
#'
#' @examples
#' \dontrun{
#' redcap_demo_app()
#' }
redcap_demo_app <- function() {
  ui <- shiny::fluidPage(
    m_redcap_readUI("data"),
    DT::DTOutput("data_summary")
  )
  server <- function(input, output, session) {
    data_val <- shiny::reactiveValues(data = NULL)


    data_val$data <- m_redcap_readServer(id = "data")

    output$data_summary <- DT::renderDataTable(
      {
        shiny::req(data_val$data)
        data_val$data()
      },
      options = list(
        scrollX = TRUE,
        pageLength = 5
      ),
    )
  }
  shiny::shinyApp(ui, server)
}
