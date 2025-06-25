#' Shiny module to browser and export REDCap data
#'
#' @param id Namespace id
#' @param include_title logical to include title
#'
#' @rdname redcap_read_shiny_module
#'
#' @return shiny ui element
#' @export
m_redcap_readUI <- function(id, title = TRUE, url = NULL) {
  ns <- shiny::NS(id)

  if (isTRUE(title)) {
    title <- shiny::tags$h4(
      "Import data from REDCap",
      class = "redcap-module-title"
    )
  }

  server_ui <- shiny::tagList(
    shiny::tags$h4("REDCap server"),
    shiny::textInput(
      inputId = ns("uri"),
      label = "Web address",
      value = if_not_missing(url, "https://redcap.your.institution/"),
      width = "100%"
    ),
    shiny::helpText("Format should be either 'https://redcap.your.institution/' or 'https://your.institution/redcap/'"),
    # shiny::textInput(
    #   inputId = ns("api"),
    #   label = "API token",
    #   value = "",
    #   width = "100%"
    # ),
    shiny::passwordInput(
      inputId = ns("api"),
      label = "API token",
      value = "",
      width = "100%"
    ),
    shiny::helpText("The token is a string of 32 numbers and letters."),
    shiny::br(),
    shiny::br(),
    shiny::actionButton(
      inputId = ns("data_connect"),
      label = "Connect",
      icon = shiny::icon("link", lib = "glyphicon"),
      width = "100%",
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

  filter_ui <-
    shiny::tagList(
      # width = 6,
      shiny::uiOutput(outputId = ns("arms")),
      shiny::textInput(
        inputId = ns("filter"),
        label = "Optional filter logic (e.g., ⁠[gender] = 'female')"
      )
    )

  params_ui <-
    shiny::tagList(
      shiny::tags$h4("Data import parameters"),
      shiny::tags$div(
        style = htmltools::css(
          display = "grid",
          gridTemplateColumns = "1fr 50px",
          gridColumnGap = "10px"
        ),
        shiny::uiOutput(outputId = ns("fields")),
        shiny::tags$div(
          class = "shiny-input-container",
          shiny::tags$label(
            class = "control-label",
            `for` = ns("dropdown_params"),
            "...",
            style = htmltools::css(visibility = "hidden")
          ),
          shinyWidgets::dropMenu(
            shiny::actionButton(
              inputId = ns("dropdown_params"),
              label = shiny::icon("filter"),
              width = "50px"
            ),
            filter_ui
          )
        )
      ),
      shiny::helpText("Select fields/variables to import and click the funnel to apply optional filters"),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::uiOutput(outputId = ns("data_type")),
      shiny::uiOutput(outputId = ns("fill")),
      shiny::actionButton(
        inputId = ns("data_import"),
        label = "Import",
        icon = shiny::icon("download", lib = "glyphicon"),
        width = "100%",
        disabled = TRUE
      ),
      shiny::tags$br(),
      shiny::tags$br(),
      tags$div(
        id = ns("retrieved-placeholder"),
        shinyWidgets::alert(
          id = ns("retrieved-result"),
          status = "info",
          tags$p(phosphoricons::ph("info", weight = "bold"), "Please specify data to download, then press 'Import'.")
        ),
        dismissible = TRUE
      )
    )


  shiny::fluidPage(
    title = title,
    server_ui,
    # shiny::uiOutput(ns("params_ui")),
    shiny::conditionalPanel(
      condition = "output.connect_success == true",
      params_ui,
      ns = ns
    ),
    shiny::br()
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
      code = NULL
    )

    shiny::observeEvent(list(input$api, input$uri), {
      shiny::req(input$api)
      shiny::req(input$uri)
      if (!is.null(input$uri)) {
        uri <- paste0(ifelse(endsWith(input$uri, "/"), input$uri, paste0(input$uri, "/")), "api/")
      } else {
        uri <- input$uri
      }

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
            shiny::withProgress(
              {
                imported <- try(rlang::exec(REDCapR::redcap_metadata_read, !!!parameters), silent = TRUE)
              },
              message = paste("Connecting to", data_rv$uri)
            )

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
                include_data_alert(
                  see_data_text = "Click to see data dictionary",
                  dataIdName = "see_dd",
                  extra = tags$p(
                    tags$b(phosphoricons::ph("check", weight = "bold"), "Connected to server!"),
                    glue::glue("The {data_rv$info$project_title} project is loaded.")
                  ),
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

    output$connect_success <- shiny::reactive(identical(data_rv$dd_status, "success"))
    shiny::outputOptions(output, "connect_success", suspendWhenHidden = FALSE)


    shiny::observeEvent(input$see_dd, {
      show_data(
        purrr::pluck(data_rv$dd_list, "data"),
        title = "Data dictionary",
        type = "modal",
        show_classes = FALSE,
        tags$b("Preview:")
      )
    })

    shiny::observeEvent(input$see_data, {
      show_data(
        # purrr::pluck(data_rv$dd_list, "data"),
        data_rv$data,
        title = "Imported data set",
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
        label = "Select fields/variables to import:",
        choices = purrr::pluck(data_rv$dd_list, "data") |>
          dplyr::select(field_name, form_name) |>
          (\(.x){
            split(.x$field_name, REDCapCAST::as_factor(.x$form_name))
          })(),
        updateOn = "change",
        multiple = TRUE,
        search = TRUE,
        showValueAsTags = TRUE,
        width = "100%"
      )
    })

    output$data_type <- shiny::renderUI({
      shiny::req(data_rv$info)
      if (isTRUE(data_rv$info$has_repeating_instruments_or_events)) {
        vectorSelectInput(
          inputId = ns("data_type"),
          label = "Specify the data format",
          choices = c(
            "Wide data (One row for each subject)" = "wide",
            "Long data for project with repeating instruments (default REDCap)" = "long"
          ),
          selected = "wide",
          multiple = FALSE,
          width = "100%"
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
          selected = "no",
          multiple = FALSE,
          width = "100%"
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
      if (NROW(arms()) > 0) {
        vectorSelectInput(
          inputId = ns("arms"),
          selected = NULL,
          label = "Filter by events/arms",
          choices = stats::setNames(arms()[[3]], arms()[[1]]),
          multiple = TRUE,
          width = "100%"
        )
      }
    })

    shiny::observeEvent(input$data_import, {
      shiny::req(input$fields)

      # browser()
      record_id <- purrr::pluck(data_rv$dd_list, "data")[[1]][1]


      parameters <- list(
        uri = data_rv$uri,
        token = input$api,
        fields = unique(c(record_id, input$fields)),
        events = input$arms,
        raw_or_label = "both",
        filter_logic = input$filter,
        split_forms = ifelse(
          input$data_type == "long" && !is.null(input$data_type),
          "none",
          "all"
        )
      )

      shiny::withProgress(message = "Downloading REDCap data. Hold on for a moment..", {
        imported <- try(rlang::exec(REDCapCAST::read_redcap_tables, !!!parameters), silent = TRUE)
      })

      parameters_code <- parameters[c("uri", "fields", "events", "raw_or_label", "filter_logic")]

      code <- rlang::call2(
        "easy_redcap",
        !!!utils::modifyList(
          parameters_code,
          list(
            data_format = ifelse(
              input$data_type == "long" && !is.null(input$data_type),
              "long",
              "wide"
            ),
            project.name = simple_snake(data_rv$info$project_title)
          )
        ),
        .ns = "REDCapCAST"
      )

      if (inherits(imported, "try-error") || NROW(imported) < 1) {
        data_rv$data_status <- "error"
        data_rv$data_list <- NULL
        data_rv$data_message <- imported$raw_text
      } else {
        data_rv$data_status <- "success"
        data_rv$data_message <- "Requested data was retrieved!"

        ## The data management below should be separated to allow for changing
        ## "wide"/"long" without re-importing data

        if (parameters$split_form == "all") {
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

        # browser()
        in_data_check <- parameters$fields %in% names(out) |
          sapply(names(out), \(.x) any(sapply(parameters$fields, \(.y) startsWith(.x, .y))))

        if (!any(in_data_check[-1])) {
          data_rv$data_status <- "warning"
          data_rv$data_message <- "Data retrieved, but it looks like only the ID was retrieved from the server. Please check with your REDCap administrator that you have required permissions for data access."
        }

        if (!all(in_data_check)) {
          data_rv$data_status <- "warning"
          data_rv$data_message <- "Data retrieved, but it looks like not all requested fields were retrieved from the server. Please check with your REDCap administrator that you have required permissions for data access."
        }

        data_rv$code <- code

        data_rv$data <- out |>
          dplyr::select(-dplyr::ends_with("_complete")) |>
          # dplyr::select(-dplyr::any_of(record_id)) |>
          REDCapCAST::suffix2label()
      }
    })

    shiny::observeEvent(
      data_rv$data_status,
      {
        # browser()
        if (identical(data_rv$data_status, "error")) {
          datamods:::insert_error(mssg = data_rv$data_message, selector = ns("retrieved"))
        } else if (identical(data_rv$data_status, "success")) {
          datamods:::insert_alert(
            selector = ns("retrieved"),
            status = data_rv$data_status,
            # tags$p(
            #   tags$b(phosphoricons::ph("check", weight = "bold"), "Success!"),
            #   data_rv$data_message
            # ),
            include_data_alert(
              see_data_text = "Click to see the imported data",
              dataIdName = "see_data",
              extra = tags$p(
                tags$b(phosphoricons::ph("check", weight = "bold"), data_rv$data_message)
              ),
              btn_show_data = TRUE
            )
          )
        } else {
          datamods:::insert_alert(
            selector = ns("retrieved"),
            status = data_rv$data_status,
            tags$p(
              tags$b(phosphoricons::ph("warning", weight = "bold"), "Warning!"),
              data_rv$data_message
            )
          )
        }
      }
    )

    return(list(
      status = shiny::reactive(data_rv$data_status),
      name = shiny::reactive(data_rv$info$project_title),
      info = shiny::reactive(data_rv$info),
      code = shiny::reactive(data_rv$code),
      data = shiny::reactive(data_rv$data)
    ))
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
    m_redcap_readUI("data", url = NULL),
    DT::DTOutput("data"),
    shiny::tags$b("Code:"),
    shiny::verbatimTextOutput(outputId = "code")
  )
  server <- function(input, output, session) {
    data_val <- m_redcap_readServer(id = "data")

    output$data <- DT::renderDataTable(
      {
        shiny::req(data_val$data)
        data_val$data()
      },
      options = list(
        scrollX = TRUE,
        pageLength = 5
      ),
    )
    output$code <- shiny::renderPrint({
      shiny::req(data_val$code)
      data_val$code()
    })
  }
  shiny::shinyApp(ui, server)
}
