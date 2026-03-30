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
    title <- shiny::tags$h4(i18n$t("Import data from REDCap"), class = "redcap-module-title")
  }

  server_ui <- shiny::tagList(
    shiny::tags$h4(i18n$t("REDCap server")),
    shiny::textInput(
      inputId = ns("uri"),
      label = i18n$t("Web address"),
      value = if_not_missing(url, "https://redcap.your.institution/"),
      width = "100%"
    ),
    shiny::helpText(
      i18n$t(
        "Format should be either 'https://redcap.your.institution/' or 'https://your.institution/redcap/'"
      )
    ),
    shiny::br(),
    shiny::br(),
    shiny::passwordInput(
      inputId = ns("api"),
      label = i18n$t("API token"),
      value = "",
      width = "100%"
    ),
    shiny::helpText(i18n$t(
      "The token is a string of 32 numbers and letters."
    )),
    shiny::br(),
    shiny::br(),
    shiny::actionButton(
      inputId = ns("data_connect"),
      label = i18n$t("Connect"),
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
        tags$p(
          phosphoricons::ph("info", weight = "bold"),
          i18n$t("Please fill in web address and API token, then press 'Connect'.")
        )
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
        label = i18n$t("Optional filter logic (e.g., ⁠[gender] = 'female')")
      ),
      uiOutput(ns("filter_feedback"))
    )

  params_ui <-
    shiny::tagList(
      shiny::tags$h4(i18n$t("Data import parameters")),
      shiny::tags$div(
        ####
        #### All below was deactivated to deactivate filtering
        ####
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
      shiny::helpText(
        i18n$t(
          "Select fields/variables to import and click the funnel to apply optional filters"
        )
      ),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::uiOutput(outputId = ns("data_type")),
      shiny::uiOutput(outputId = ns("fill")),
      shiny::actionButton(
        inputId = ns("data_import"),
        label = i18n$t("Import"),
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
          tags$p(
            phosphoricons::ph("info", weight = "bold"),
            "Please specify data to download, then press 'Import'."
          )
        ),
        dismissible = TRUE
      )
    )


  shiny::fluidPage(
    title = title,
    server_ui,
    # shiny::uiOutput(ns("params_ui")),
    shiny::conditionalPanel(condition = "output.connect_success == true", params_ui, ns = ns),
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
      code = NULL,
      filter_valid = NULL
    )

    shiny::observeEvent(list(input$api, input$uri), {
      shiny::req(input$api)
      shiny::req(input$uri)
      if (!is.null(input$uri)) {
        uri <- paste0(ifelse(
          endsWith(input$uri, "/"),
          input$uri,
          paste0(input$uri, "/")
        ), "api/")
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


    tryCatch({
      shiny::observeEvent(list(input$data_connect), {
        shiny::req(input$api)
        shiny::req(data_rv$uri)

        parameters <- list(redcap_uri = data_rv$uri, token = input$api)

        # browser()
        shiny::withProgress({
          imported <- try(rlang::exec(REDCapR::redcap_metadata_read, !!!parameters),
                          silent = TRUE)
        }, message = paste("Connecting to", data_rv$uri))

        ## TODO: Simplify error messages
        if (inherits(imported, "try-error") ||
            NROW(imported) < 1 ||
            ifelse(is.list(imported), !isTRUE(imported$success), FALSE)) {
          if (ifelse(is.list(imported),
                     !isTRUE(imported$success),
                     FALSE)) {
            mssg <- imported$raw_text
          } else {
            mssg <- attr(imported, "condition")$message
          }

          datamods:::insert_error(mssg = mssg, selector = "connect")
          data_rv$dd_status <- "error"
          data_rv$dd_list <- NULL
        } else if (isTRUE(imported$success)) {
          data_rv$dd_status <- "success"

          data_rv$info <- REDCapR::redcap_project_info_read(redcap_uri = data_rv$uri, token = input$api)$data

          datamods:::insert_alert(
            selector = ns("connect"),
            status = "success",
            include_data_alert(
              see_data_text = i18n$t("Click to see data dictionary"),
              dataIdName = "see_dd",
              extra = tags$p(
                tags$b(
                  phosphoricons::ph("check", weight = "bold"),
                  i18n$t("Connected to server!")
                ),
                glue::glue(
                  i18n$t(
                    "The {data_rv$info$project_title} project is loaded."
                  )
                )
              ),
              btn_show_data = TRUE
            )
          )

          data_rv$dd_list <- imported
        }
      }, ignoreInit = TRUE)
    }, warning = function(warn) {
      showNotification(paste0(warn), type = "warning")
    }, error = function(err) {
      showNotification(paste0(err), type = "error")
    })

    output$connect_success <- shiny::reactive(identical(data_rv$dd_status, "success"))
    shiny::outputOptions(output, "connect_success", suspendWhenHidden = FALSE)


    shiny::observeEvent(input$see_dd, {
      show_data(
        purrr::pluck(data_rv$dd_list, "data"),
        title = i18n$t("Data dictionary"),
        type = "modal",
        show_classes = FALSE,
        tags$b(i18n$t("Preview:"))
      )
    })

    shiny::observeEvent(input$see_data, {
      show_data(
        # purrr::pluck(data_rv$dd_list, "data"),
        data_rv$data,
        title = i18n$t("Imported data set"),
        type = "modal",
        show_classes = FALSE,
        tags$b(i18n$t("Preview:"))
      )
    })

    arms <- shiny::reactive({
      shiny::req(input$api)
      shiny::req(data_rv$uri)

      REDCapR::redcap_event_read(redcap_uri = data_rv$uri, token = input$api)$data
    })

    output$fields <- shiny::renderUI({
      shiny::req(data_rv$dd_list)
      shinyWidgets::virtualSelectInput(
        inputId = ns("fields"),
        label = i18n$t("Select fields/variables to import:"),
        choices = purrr::pluck(data_rv$dd_list, "data") |>
          dplyr::select(field_name, form_name) |>
          (\(.x) {
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
          label = i18n$t("Specify the data format"),
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
      data_rv$rep_fields <- data_rv$dd_list$data$field_name[data_rv$dd_list$data$form_name %in% repeated_instruments(uri = data_rv$uri, token = input$api)]

      if (input$data_type == "long" &&
          isTRUE(any(input$fields %in% data_rv$rep_fields))) {
        vectorSelectInput(
          inputId = ns("fill"),
          label = i18n$t("Fill missing values?"),
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


    filter_validation <- reactive({
      val <- trimws(input$filter)
      if (nchar(val) == 0)
        return(NULL)
      validate_redcap_filter(val, purrr::pluck(data_rv$dd_list, "data"))
    })

    output$filter_feedback <- renderUI({
      result <- filter_validation()
      if (is.null(result)) {
        data_rv$filter_valid <- NULL
        return(NULL)
      }

      if (result$valid) {
        data_rv$filter_valid <- TRUE
        tags$span(style = "color: green;", "\u2713 Filter is valid")
      } else {
        data_rv$filter_valid <- FALSE

        tags$span(style = "color: red;",
                  "\u2717 ",
                  line_break(result$message, lineLength = 30))
      }
    })

    shiny::observeEvent(input$data_import, {
      shiny::req(input$fields)

      # browser()
      record_id <- purrr::pluck(data_rv$dd_list, "data")[[1]][1]

      if (!is.null(data_rv$filter_valid)) {
        if (isTRUE(data_rv$filter_valid)) {
          filter <- trimws(input$filter)
        } else {
          filter <- ""
        }
      } else {
        filter <- ""
      }

      parameters <- list(
        uri = data_rv$uri,
        token = input$api,
        fields = unique(c(record_id, input$fields)),
        events = input$arms,
        raw_or_label = "both",
        filter_logic = filter,
        # filter_logic = "",
        split_forms = ifelse(
          input$data_type == "long" && !is.null(input$data_type),
          "none",
          "all"
        )
      )

      shiny::withProgress(message = "Downloading REDCap data. Hold on for a moment..", {
        imported <- try({
          rlang::exec(REDCapCAST::read_redcap_tables, !!!parameters)
          # if (nrow(out)==0){
          #   stop("No data was exported")
          # } else {
          #   out
          # }
        }, # error = function(err) {
        #   showNotification(i18n$t("An error was encountered exporting data. Please review data filter."), type = "error")
        # },
        silent = TRUE)
      })

      # d <- REDCapCAST::apply_factor_labels(data = imported$survey, meta = data_rv$dd_list$data)

      parameters_code <- parameters[c("uri",
                                      "fields",
                                      "events",
                                      "raw_or_label",
                                      "filter_logic")]

      code <- rlang::call2("easy_redcap",
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
                           .ns = "REDCapCAST")

      if (inherits(imported, "try-error") |
          NROW(imported) == 0 |
          (length(imported) == 1 & !is.list(imported))) {
        data_rv$data_status <- "error"
        data_rv$data_list <- NULL
        data_rv$data_message <- i18n$t("An empty data set was imported. Please review data filter.")
        data_rv$data <- NULL
      } else {
        data_rv$data_status <- "success"
        data_rv$data_message <- i18n$t("Requested data was retrieved!")

        ## The data management below should be separated to allow for changing
        ## "wide"/"long" without re-importing data

        if (parameters$split_form == "all") {
          out <- imported |>
            # redcap_wider()
            REDCapCAST::redcap_wider()
        } else {
          if (identical(input$fill, "yes")) {
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

        ## Ensure correct factor labels
        ## It is a little hacky and should be included in the read_redcap_tables, but is lost along the way
        out <- REDCapCAST::apply_factor_labels(data = out, meta = data_rv$dd_list$data)


        in_data_check <- parameters$fields %in% names(out) |
          sapply(names(out), \(.x) any(sapply(
            parameters$fields, \(.y) startsWith(.x, .y)
          )))

        if (!any(in_data_check[-1])) {
          data_rv$data_status <- "warning"
          data_rv$data_message <- i18n$t(
            "Data retrieved, but it looks like only the ID was retrieved from the server. Please check with your REDCap administrator that you have required permissions for data access."
          )
        }

        if (!all(in_data_check)) {
          data_rv$data_status <- "warning"
          data_rv$data_message <- i18n$t(
            "Data retrieved, but it looks like not all requested fields were retrieved from the server. Please check with your REDCap administrator that you have required permissions for data access."
          )
        }

        data_rv$code <- code

        ## Level labels nare lost at this point...
        data_rv$data <- out |>
          dplyr::select(-dplyr::ends_with("_complete")) |>
          # dplyr::select(-dplyr::any_of(record_id)) |>
          REDCapCAST::suffix2label()

      }
    })

    shiny::observeEvent(data_rv$data_status, {
      if (identical(data_rv$data_status, "error")) {
        ## The insert error wouldn't work. Inserted through regular.
        # datamods:::insert_error(mssg = data_rv$data_message,
        #                         selector = ns("retrieved"))
        datamods:::insert_alert(
          selector = ns("retrieved"),
          status = "danger",
          tags$p(
            tags$b(
              phosphoricons::ph("warning", weight = "bold"),
              "Warning!"
            ),
            data_rv$data_message
          )
        )
      } else if (identical(data_rv$data_status, "success")) {
        datamods:::insert_alert(
          selector = ns("retrieved"),
          status = data_rv$data_status,
          # tags$p(
          #   tags$b(phosphoricons::ph("check", weight = "bold"), "Success!"),
          #   data_rv$data_message
          # ),
          include_data_alert(
            see_data_text = i18n$t("Click to see the imported data"),
            dataIdName = "see_data",
            extra = tags$p(tags$b(
              phosphoricons::ph("check", weight = "bold"),
              data_rv$data_message
            )),
            btn_show_data = TRUE
          )
        )
      } else {
        datamods:::insert_alert(
          selector = ns("retrieved"),
          status = data_rv$data_status,
          tags$p(
            tags$b(
              phosphoricons::ph("warning", weight = "bold"),
              "Warning!"
            ),
            data_rv$data_message
          )
        )
      }
    })

    return(
      list(
        status = shiny::reactive(data_rv$data_status),
        name = shiny::reactive(data_rv$info$project_title),
        info = shiny::reactive(data_rv$info),
        code = shiny::reactive(data_rv$code),
        data = shiny::reactive(data_rv$data)
      )
    )
  }

  shiny::moduleServer(id = id, module = module)
}

#' @importFrom htmltools tagList tags
#' @importFrom shiny icon getDefaultReactiveDomain
include_data_alert <- function(dataIdName = "see_data",
                               btn_show_data,
                               see_data_text = "Click to see data",
                               extra = NULL,
                               session = shiny::getDefaultReactiveDomain()) {
  if (isTRUE(btn_show_data)) {
    success_message <- tagList(extra,
                               tags$br(),
                               shiny::actionLink(
                                 inputId = session$ns(dataIdName),
                                 label = tagList(phosphoricons::ph("book-open-text"), see_data_text)
                               ))
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
is_valid_token <- function(token,
                           pattern_env = NULL,
                           nchar = 32) {
  checkmate::assert_character(token, any.missing = TRUE, len = 1)

  if (!is.null(pattern_env)) {
    checkmate::assert_character(pattern_env, any.missing = FALSE, len = 1)
    pattern <- pattern_env
  } else {
    pattern <- glue::glue("^([0-9A-Fa-f]{<nchar>})(?:\\n)?$",
                          .open = "<",
                          .close = ">")
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
  generics <- c(
    names(data)[1],
    "redcap_event_name",
    "redcap_repeat_instrument",
    "redcap_repeat_instance"
  )

  filt <- split(data, data[[event]]) |>
    lapply(\(.x) {
      dplyr::select(.x, -tidyselect::all_of(generics)) |>
        REDCapCAST::all_na()
    }) |>
    unlist()

  data[data[[event]] %in% names(filt)[!filt], ]
}


#' Validate a REDCap server-side filter string against a data dictionary
#'
#' Checks that a REDCap filter expression is syntactically correct and
#' consistent with the field types defined in the project data dictionary.
#' Plain text without field references is always rejected. Multi-clause
#' filters joined by \code{AND} or \code{OR} are supported.
#'
#' @param filter A single character string containing the filter expression,
#'   e.g. \code{"[age] > 18"} or \code{"[cohabitation] = '1' AND [age] > 18"}.
#' @param dictionary A data frame representing the REDCap data dictionary in
#'   API export format, as returned by e.g. \code{REDCapCAST::get_redcap_metadata()}.
#'   Must contain at least the columns \code{field_name} and \code{field_type}.
#'   The columns \code{text_validation_type_or_show_slider_number} and
#'   \code{select_choices_or_calculations} are used when present for stricter
#'   type and choice validation.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{\code{valid}}{Logical. \code{TRUE} if the filter passes all checks.}
#'     \item{\code{message}}{Character. \code{"Filter is valid."} on success, or
#'       a newline-separated string of error messages describing every problem
#'       found.}
#'   }
#'
#' @details
#' Validation rules by field type:
#' \describe{
#'   \item{\code{calc}}{Numeric fields. Value must be an unquoted number.
#'     All comparison operators (\code{=}, \code{!=}, \code{<}, \code{>},
#'     \code{<=}, \code{>=}) are accepted.}
#'   \item{\code{text} with date validation}{Fields with validation type
#'     \code{date_ymd}, \code{date_dmy}, \code{datetime_*}, etc. Value must be
#'     a quoted date/datetime string in \code{'YYYY-MM-DD'} format. All
#'     comparison operators are accepted.}
#'   \item{\code{text} with time validation}{Fields with validation type
#'     \code{time_hh_mm_ss} or \code{time_mm_ss}. Value must be a quoted time
#'     string, e.g. \code{'14:30:00'}. All comparison operators are accepted.}
#'   \item{\code{radio} / \code{dropdown}}{Categorical fields. Value must be a
#'     quoted choice code (e.g. \code{'1'}) that exists in the field's choice
#'     list. Only \code{=} and \code{!=} are accepted.}
#'   \item{\code{text} (plain)}{Free-text fields. Value must be a quoted string.
#'     Only \code{=} and \code{!=} are accepted.}
#' }
#'
#' @examples
#' \dontrun{
#' dict <- REDCapCAST::get_redcap_metadata(
#'   uri    = "https://redcap.example.com/api/",
#'   token  = Sys.getenv("REDCAP_TOKEN")
#' )
#'
#' validate_redcap_filter("[age] > 18", dict)
#' #> list(valid = TRUE, message = "Filter is valid.")
#'
#' validate_redcap_filter("only plain text", dict)
#' #> list(valid = FALSE, message = "Filter must contain at least one field ...")
#'
#' validate_redcap_filter("[cohabitation] = '1' AND [age] > 18", dict)
#' #> list(valid = TRUE, message = "Filter is valid.")
#' }
#'
#' @export
# REDCap filter validation based on data dictionary
#
# REDCap filter format: [field_name] operator value
# Example: [age] > 18
#          [cohabitation] = '1'
#          [inclusion] > '2020-01-01'
#
# Supported field types and their allowed operators/value formats:
#   text (no validation) -> string values, = != operators only
#   text (date_ymd/date_dmy) -> quoted date strings, all comparison operators
#   text (time_hh_mm_ss) -> quoted time strings, all comparison operators
#   text (datetime_*) -> quoted datetime strings, all comparison operators
#   text (autocomplete) -> string values, = != operators only
#   calc -> numeric values, all comparison operators
#   radio/dropdown -> quoted numeric codes, = != operators only

validate_redcap_filter <- function(filter, dictionary) {
  # --- Input checks ---
  if (!is.character(filter) ||
      length(filter) != 1 || nchar(trimws(filter)) == 0) {
    return(list(valid = FALSE, message = "Filter must be a non-empty string."))
  }

  if (!grepl("\\[.+\\]", filter)) {
    return(
      list(valid = FALSE, message = "Filter must contain at least one field reference in [brackets]. Plain text is not accepted.")
    )
  }

  # --- Column names (API export format) ---
  col_field    <- "field_name"
  col_type     <- "field_type"
  col_val_type <- "text_validation_type_or_show_slider_number"
  col_choices  <- "select_choices_or_calculations"

  missing_cols <- setdiff(c(col_field, col_type), names(dictionary))
  if (length(missing_cols) > 0) {
    stop("Dictionary is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  # --- Build lookup index once for O(1) field access ---
  field_idx    <- setNames(seq_len(nrow(dictionary)), dictionary[[col_field]])
  has_val_type <- col_val_type %in% names(dictionary)
  has_choices  <- col_choices  %in% names(dictionary)

  # --- Classify field types ---
  numeric_types    <- c("calc")
  date_validations <- c(
    "date_ymd",
    "date_dmy",
    "datetime_ymd",
    "datetime_dmy",
    "datetime_seconds_ymd",
    "datetime_seconds_dmy"
  )
  time_validations <- c("time_hh_mm_ss", "time_mm_ss")
  categorical_types <- c("radio", "dropdown", "checkbox")
  text_types        <- c("text", "autocomplete")

  num_ops  <- c("=", "!=", "<", ">", "<=", ">=")
  cat_ops  <- c("=", "!=")
  text_ops <- c("=", "!=")

  # --- Parse filter into clauses ---
  # Split on AND/OR (REDCap uses 'and'/'or' or 'AND'/'OR')
  clauses <- trimws(strsplit(filter, "(?i)\\s+(and|or)\\s+", perl = TRUE)[[1]])

  clause_pattern <- "^\\[([^\\]]+)\\]\\s*(=|!=|<=|>=|<|>)\\s*(.+)$"

  errors <- character(0)

  for (clause in clauses) {
    if (!grepl(clause_pattern, clause, perl = TRUE)) {
      errors <- c(
        errors,
        sprintf(
          "Clause '%s' does not match expected format: [field] operator value",
          clause
        )
      )
      next
    }

    parts    <- regmatches(clause, regexec(clause_pattern, clause, perl = TRUE))[[1]]
    field    <- parts[2]
    operator <- parts[3]
    value    <- trimws(parts[4])

    # --- Check field exists using pre-built index ---
    row_i <- field_idx[field]
    if (is.na(row_i)) {
      errors <- c(errors, sprintf("Unknown field: [%s]", field))
      next
    }

    field_type <- dictionary[[col_type]][row_i]
    val_type   <- if (has_val_type)
      dictionary[[col_val_type]][row_i]
    else
      ""
    if (is.na(val_type))
      val_type <- ""

    # --- Determine expected value format and allowed operators ---
    if (field_type %in% numeric_types ||
        grepl("^integer$|^number", val_type)) {
      if (!operator %in% num_ops) {
        errors <- c(
          errors,
          sprintf(
            "[%s] is numeric — operator '%s' is not valid. Use one of: %s",
            field,
            operator,
            paste(num_ops, collapse = ", ")
          )
        )
      }
      if (!grepl("^-?[0-9]+(\\.[0-9]+)?$", value)) {
        errors <- c(
          errors,
          sprintf(
            "[%s] is numeric — value '%s' should be an unquoted number (e.g. 18 or 3.5)",
            field,
            value
          )
        )
      }

    } else if (val_type %in% date_validations) {
      if (!operator %in% num_ops) {
        errors <- c(
          errors,
          sprintf(
            "[%s] is a date — operator '%s' is not valid. Use one of: %s",
            field,
            operator,
            paste(num_ops, collapse = ", ")
          )
        )
      }
      if (!grepl(
        "^'[0-9]{4}-[0-9]{2}-[0-9]{2}(\\s[0-9]{2}:[0-9]{2}(:[0-9]{2})?)?'$",
        value
      )) {
        errors <- c(
          errors,
          sprintf(
            "[%s] is a date — value '%s' should be a quoted date string, e.g. '2020-01-31'",
            field,
            value
          )
        )
      }

    } else if (val_type %in% time_validations) {
      if (!operator %in% num_ops) {
        errors <- c(
          errors,
          sprintf(
            "[%s] is a time — operator '%s' is not valid. Use one of: %s",
            field,
            operator,
            paste(num_ops, collapse = ", ")
          )
        )
      }
      if (!grepl("^'[0-9]{2}:[0-9]{2}(:[0-9]{2})?'$", value)) {
        errors <- c(
          errors,
          sprintf(
            "[%s] is a time — value '%s' should be a quoted time string, e.g. '14:30:00'",
            field,
            value
          )
        )
      }

    } else if (field_type %in% categorical_types) {
      if (!operator %in% cat_ops) {
        errors <- c(
          errors,
          sprintf(
            "[%s] is categorical — operator '%s' is not valid. Use one of: %s",
            field,
            operator,
            paste(cat_ops, collapse = ", ")
          )
        )
      }

      # Validate value is a known choice code
      choices_raw <- if (has_choices)
        dictionary[[col_choices]][row_i]
      else
        NA
      if (!is.na(choices_raw) && nchar(trimws(choices_raw)) > 0) {
        choice_codes <- trimws(gsub(",.+?(\\||$)", "", gsub(
          "^\\s*", "", strsplit(choices_raw, "\\|")[[1]]
        )))
        value_unquoted <- gsub("^'|'$", "", value)
        if (!value_unquoted %in% choice_codes) {
          errors <- c(
            errors,
            sprintf(
              "[%s] is categorical — '%s' is not a valid choice code. Valid codes: %s",
              field,
              value_unquoted,
              paste(choice_codes, collapse = ", ")
            )
          )
        }
      }

      if (!grepl("^'.*'$", value)) {
        errors <- c(errors,
                    sprintf(
                      "[%s] is categorical — value should be quoted, e.g. '1'",
                      field
                    ))
      }

    } else {
      # Plain text field
      if (!operator %in% text_ops) {
        errors <- c(
          errors,
          sprintf(
            "[%s] is a text field — operator '%s' is not valid. Use one of: %s",
            field,
            operator,
            paste(text_ops, collapse = ", ")
          )
        )
      }
      if (!grepl("^'.*'$", value)) {
        errors <- c(
          errors,
          sprintf(
            "[%s] is a text field — value should be quoted, e.g. 'some text'",
            field
          )
        )
      }
    }
  }

  if (length(errors) > 0) {
    return(list(
      valid = FALSE,
      message = paste(errors, collapse = "\n")
    ))
  }

  list(valid = TRUE, message = "Filter is valid.")
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

    output$data <- DT::renderDataTable({
      shiny::req(data_val$data)
      data_val$data()
    }, options = list(scrollX = TRUE, pageLength = 5), )
    output$code <- shiny::renderPrint({
      shiny::req(data_val$code)
      data_val$code()
    })
  }
  shiny::shinyApp(ui, server)
}
