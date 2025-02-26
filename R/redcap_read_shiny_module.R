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
    shiny::tags$h4("REDCap server information"),
    shiny::textInput(
      inputId = ns("uri"),
      label = "URI/Address",
      value = "https://redcap.your.institution/api/"
    ),
    shiny::textInput(
      inputId = ns("api"),
      label = "API token",
      value = ""
    ),
    tags$div(
      id = ns("connect-placeholder"),
      shinyWidgets::alert(
        id = ns("connect-result"),
        status = "info",
        tags$p(phosphoricons::ph("info", weight = "bold"),"Please fill in server address (URI) and API token.")
      ),
      dismissible = TRUE
    )
  )


  params_ui <-
    shiny::tagList(
      # width = 6,
      shiny::tags$h4("Data import parameters"),
      shiny::helpText("Options here will show, when API and uri are typed"),
      shiny::uiOutput(outputId = ns("fields")),
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
      bslib::input_task_button(
        id = ns("data_import"),
        label = "Import",
        icon = shiny::icon("download", lib = "glyphicon"),
        label_busy = "Just a minute...",
        icon_busy = fontawesome::fa_i("arrows-rotate",
          class = "fa-spin",
          "aria-hidden" = "true"
        ),
        type = "primary",
        auto_reset = TRUE#,state="busy"
      ),
      shiny::br(),
      shiny::br(),
      shiny::helpText("Press 'Import' after having specified API token and URI to export data from the REDCap server. A preview will show below the DataDictionary."),
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
      info = NULL,
      arms = NULL,
      dd_list = NULL,
      data = NULL
    )

    # tryCatch(
    #   {
        shiny::observeEvent(
          list(
            input$api,
            input$uri
          ),
          {
            shiny::req(input$api)
            shiny::req(input$uri)

            parameters <- list(
              redcap_uri = input$uri,
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

              datamods:::insert_error(mssg = mssg,selector = "connect")
              data_rv$dd_status <- "error"
              data_rv$dd_list <- NULL
            } else if (isTRUE(imported$success)) {

              datamods:::insert_alert(
                selector = ns("connect"),
                status = "success",
                make_success_alert(
                  dataIdName = "see_data",
                  extra = tags$b(phosphoricons::ph("check", weight = "bold"),"Connected to server! Project data loaded."),
                  btn_show_data = TRUE
                )
              )

              data_rv$dd_status <- "success"
              data_rv$dd_list <- imported
            }
          },
          ignoreInit = TRUE
        )
    #   },
    #   warning = function(warn) {
    #     showNotification(paste0(warn), type = "warning")
    #   },
    #   error = function(err) {
    #     showNotification(paste0(err), type = "err")
    #   }
    # )

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
      shiny::req(input$uri)

      REDCapR::redcap_event_read(
        redcap_uri = input$uri,
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
            split(.x$field_name, .x$form_name)
          })()
        ,
        updateOn = "close",
        multiple = TRUE,
        search = TRUE,
        showValueAsTags = TRUE
      )
    })

    ## TODO: Get activate/inactivate action button to work
    # shiny::observeEvent(input$fields,
    #                     {
    #   if (is.null(input$fields) | length(input$fields)==1){
    #     bslib::update_task_button(id= "data_import", state = "busy")
    #     # datamods:::toggle_widget(inputId = "data_import", enable = FALSE)
    #   } else {
    #     bslib::update_task_button(id= "data_import", state = "ready")
    #     # datamods:::toggle_widget(inputId = "data_import", enable = TRUE)
    #   }
    # })

    output$arms <- shiny::renderUI({
      shiny::selectizeInput(
        inputId = ns("arms"),
        selected = NULL,
        label = "Filter by events/arms",
        choices = arms()[[3]],
        multiple = TRUE
      )
    })

    ## Merge project name in success meassage
    ## Generate Codebook link
    name <- shiny::reactive({
      if (data_rv$dd_status=="success"){
      # browser()
      REDCapR::redcap_project_info_read(
        redcap_uri = input$uri,
        token = input$api
      )$data$project_title}
    })


    shiny::observeEvent(input$data_import, {
      shiny::req(input$fields)
      record_id <- purrr::pluck(data_rv$dd_list, "data")[[1]][1]

      parameters <- list(
        uri = input$uri,
        token = input$api,
        fields = unique(c(record_id, input$fields)),
        events = input$arms,
        raw_or_label = "both",
        filter_logic = input$filter
      )

      imported <- try(rlang::exec(REDCapCAST::read_redcap_tables, !!!parameters), silent = TRUE)
      code <- rlang::call2(REDCapCAST::read_redcap_tables, !!!parameters)


      if (inherits(imported, "try-error") || NROW(imported) < 1) {

        data_rv$data_status <- "error"
        data_rv$data_list <- NULL
      } else {
        data_rv$data_status <- "success"
        data_rv$data <- imported |>
          REDCapCAST::redcap_wider() |>
          dplyr::select(-dplyr::ends_with("_complete")) |>
          dplyr::select(-dplyr::any_of(record_id)) |>
          REDCapCAST::suffix2label()
      }
    })

    return(shiny::reactive(data_rv$data))
  }

  shiny::moduleServer(
    id = id,
    module = module
  )
}

#' @importFrom htmltools tagList tags
#' @importFrom shiny icon getDefaultReactiveDomain
make_success_alert <- function(dataIdName = "see_data",
                               btn_show_data,
                               see_data_text="Click to see data",
                               extra = NULL,
                               session = shiny::getDefaultReactiveDomain()) {
  if (isTRUE(btn_show_data)) {
    success_message <- tagList(
      extra,
      tags$br(),
      shiny::actionLink(
        inputId = session$ns(dataIdName),
        label = tagList(phosphoricons::ph("table"), see_data_text)
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
    toastui::datagridOutput2(outputId = "redcap_prev"),
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
