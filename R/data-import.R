#' data_import_ui <- function(id, include_title = TRUE) {
#'   ns <- shiny::NS(id)
#'
#'   shiny::fluidRow(
#'     shiny::column(width = 2),
#'     shiny::column(
#'       width = 8,
#'       shiny::h4("Choose your data source"),
#'       shiny::br(),
#'       shinyWidgets::radioGroupButtons(
#'         inputId = "source",
#'         selected = "env",
#'         choices = c(
#'           "File upload" = "file",
#'           "REDCap server export" = "redcap",
#'           "Local or sample data" = "env"
#'         ),
#'         width = "100%"
#'       ),
#'       shiny::helpText("Upload a file from your device, get data directly from REDCap or select a sample data set for testing from the app."),
#'       shiny::br(),
#'       shiny::br(),
#'       shiny::conditionalPanel(
#'         condition = "input.source=='file'",
#'         import_file_ui(
#'           id = "file_import",
#'           layout_params = "dropdown",
#'           title = "Choose a datafile to upload",
#'           file_extensions = c(".csv", ".tsv", ".txt", ".xls", ".xlsx", ".rds", ".sas7bdat", ".ods", ".dta")
#'         )
#'       ),
#'       shiny::conditionalPanel(
#'         condition = "input.source=='redcap'",
#'         m_redcap_readUI("redcap_import")
#'       ),
#'       shiny::conditionalPanel(
#'         condition = "input.source=='env'",
#'         import_globalenv_ui(id = "env", title = NULL)
#'       ),
#'       shiny::conditionalPanel(
#'         condition = "input.source=='redcap'",
#'         DT::DTOutput(outputId = "redcap_prev")
#'       ),
#'       shiny::br(),
#'       shiny::br(),
#'       shiny::h5("Specify variables to include"),
#'       shiny::fluidRow(
#'         shiny::column(
#'           width = 6,
#'           shiny::br(),
#'           shiny::p("Filter by completeness threshold and manual selection:"),
#'           shiny::br(),
#'           shiny::br()
#'         ),
#'         shiny::column(
#'           width = 6,
#'           shinyWidgets::noUiSliderInput(
#'             inputId = "complete_cutoff",
#'             label = NULL,
#'             min = 0,
#'             max = 100,
#'             step = 5,
#'             value = 70,
#'             format = shinyWidgets::wNumbFormat(decimals = 0),
#'             color = datamods:::get_primary_color()
#'           ),
#'           shiny::helpText("Filter variables with completeness above the specified percentage."),
#'           shiny::br(),
#'           shiny::br(),
#'           shiny::uiOutput(outputId = "import_var")
#'         )
#'       ),
#'       shiny::br(),
#'       shiny::br(),
#'       shiny::actionButton(
#'         inputId = "act_start",
#'         label = "Start",
#'         width = "100%",
#'         icon = shiny::icon("play"),
#'         disabled = TRUE
#'       ),
#'       shiny::helpText('After importing, hit "Start" or navigate to the desired tab.'),
#'       shiny::br(),
#'       shiny::br(),
#'       shiny::column(width = 2)
#'     )
#'   )
#'   }
#'
#'
#' data_import_server <- function(id) {
#'   module <- function(input, output, session) {
#'     ns <- session$ns
#'
#'     rv <- shiny::reactiveValues(
#'       data_original = NULL,
#'       data_temp = NULL,
#'       data = NULL,
#'       code = list()
#'     )
#'
#'   data_file <- import_file_server(
#'     id = "file_import",
#'     show_data_in = "popup",
#'     trigger_return = "change",
#'     return_class = "data.frame",
#'     read_fns = list(
#'       ods = import_ods,
#'       dta = function(file) {
#'         haven::read_dta(
#'           file = file,
#'           .name_repair = "unique_quiet"
#'         )
#'       },
#'       # csv = function(file) {
#'       #   readr::read_csv(
#'       #     file = file,
#'       #     na = consider.na,
#'       #     name_repair = "unique_quiet"
#'       #     )
#'       # },
#'       csv = import_delim,
#'       tsv = import_delim,
#'       txt = import_delim,
#'       xls = import_xls,
#'       xlsx = import_xls,
#'       rds = function(file) {
#'         readr::read_rds(
#'           file = file,
#'           name_repair = "unique_quiet"
#'         )
#'       }
#'     )
#'   )
#'
#'   shiny::observeEvent(data_file$data(), {
#'     shiny::req(data_file$data())
#'     rv$data_temp <- data_file$data()
#'     rv$code <- append_list(data = data_file$code(), list = rv$code, index = "import")
#'   })
#'
#'   data_redcap <- m_redcap_readServer(
#'     id = "redcap_import" # ,
#'     # output.format = "list"
#'   )
#'
#'   shiny::observeEvent(data_redcap(), {
#'     # rv$data_original <- purrr::pluck(data_redcap(), "data")()
#'     rv$data_temp <- data_redcap()
#'   })
#'
#'   output$redcap_prev <- DT::renderDT(
#'     {
#'       DT::datatable(head(data_redcap(), 5),
#'         # DT::datatable(head(purrr::pluck(data_redcap(), "data")(), 5),
#'         caption = "First 5 observations"
#'       )
#'     },
#'     server = TRUE
#'   )
#'
#'   from_env <- datamods::import_globalenv_server(
#'     id = "env",
#'     trigger_return = "change",
#'     btn_show_data = FALSE,
#'     reset = reactive(input$hidden)
#'   )
#'
#'   shiny::observeEvent(from_env$data(), {
#'     shiny::req(from_env$data())
#'
#'     rv$data_temp <- from_env$data()
#'     # rv$code <- append_list(data = from_env$code(),list = rv$code,index = "import")
#'   })
#'
#'   output$import_var <- shiny::renderUI({
#'     shiny::req(rv$data_temp)
#'
#'     preselect <- names(rv$data_temp)[sapply(rv$data_temp, missing_fraction) <= input$complete_cutoff / 100]
#'
#'     shinyWidgets::virtualSelectInput(
#'       inputId = "import_var",
#'       label = "Select variables to include",
#'       selected = preselect,
#'       choices = names(rv$data_temp),
#'       updateOn = "close",
#'       multiple = TRUE,
#'       search = TRUE,
#'       showValueAsTags = TRUE
#'     )
#'   })
#'
#'
#'   shiny::observeEvent(
#'     eventExpr = list(
#'       input$import_var
#'     ),
#'     handlerExpr = {
#'       shiny::req(rv$data_temp)
#'
#'       rv$data_original <- rv$data_temp |>
#'         dplyr::select(input$import_var) |>
#'         # janitor::clean_names() |>
#'         default_parsing()
#'     }
#'   )
#'
#'   return(shiny::reactive(rv$data_original))
#'
#'   }
#'
#'   shiny::moduleServer(
#'     id = id,
#'     module = module
#'   )
#'
#'   }
#'
#'
#' #' Test app for the data-import module
#' #'
#' #' @rdname data-import
#' #'
#' #' @examples
#' #' \dontrun{
#' #' data_import_demo_app()
#' #' }
#' data_import_demo_app <- function() {
#'   ui <- shiny::fluidPage(
#'     data_import_ui("data")
#'   )
#'   server <- function(input, output, session) {
#'     data_val <- shiny::reactiveValues(data = NULL)
#'
#'
#'     data_val$data <- data_import_server(id = "data")
#'
#'     output$data_summary <- DT::renderDataTable(
#'       {
#'         shiny::req(data_val$data)
#'         data_val$data()
#'       },
#'       options = list(
#'         scrollX = TRUE,
#'         pageLength = 5
#'       ),
#'     )
#'   }
#'   shiny::shinyApp(ui, server)
#' }
