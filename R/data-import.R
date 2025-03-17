data_import_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(width = 2),
    shiny::column(
      width = 8,
      shiny::h4("Choose your data source"),
      shiny::br(),
      shinyWidgets::radioGroupButtons(
        inputId = "source",
        selected = "env",
        choices = c(
          "File upload" = "file",
          "REDCap server export" = "redcap",
          "Local or sample data" = "env"
        ),
        width = "100%"
      ),
      shiny::helpText("Upload a file from your device, get data directly from REDCap or select a sample data set for testing from the app."),
      shiny::br(),
      shiny::br(),
      shiny::conditionalPanel(
        condition = "input.source=='file'",
        import_file_ui(
          id = ns("file_import"),
          layout_params = "dropdown",
          title = "Choose a datafile to upload",
          file_extensions = c(".csv", ".tsv", ".txt", ".xls", ".xlsx", ".rds", ".sas7bdat", ".ods", ".dta")
        )
      ),
      shiny::conditionalPanel(
        condition = "input.source=='redcap'",
        m_redcap_readUI(id = ns("redcap_import"))
      ),
      shiny::conditionalPanel(
        condition = "input.source=='env'",
        import_globalenv_ui(id = ns("env"), title = NULL)
      ),
      shiny::conditionalPanel(
        condition = "input.source=='redcap'",
        DT::DTOutput(outputId = ns("redcap_prev"))
      )
    )
  )
  }


data_import_server <- function(id) {
  module <- function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      data_temp = NULL,
      code = list()
    )

    data_file <- import_file_server(
      id = ns("file_import"),
      show_data_in = "popup",
      trigger_return = "change",
      return_class = "data.frame",
      read_fns = list(
        ods = import_ods,
        dta = function(file) {
          haven::read_dta(
            file = file,
            .name_repair = "unique_quiet"
          )
        },
        csv = import_delim,
        tsv = import_delim,
        txt = import_delim,
        xls = import_xls,
        xlsx = import_xls,
        rds = function(file) {
          readr::read_rds(
            file = file,
            name_repair = "unique_quiet"
          )
        }
      )
    )

    shiny::observeEvent(data_file$data(), {
      shiny::req(data_file$data())
      browser()
      rv$data_temp <- data_file$data()
      rv$code <- append_list(data = data_file$code(), list = rv$code, index = "import")
    })

    data_redcap <- m_redcap_readServer(
      id = "redcap_import"
    )

    shiny::observeEvent(data_redcap(), {
      # rv$data_original <- purrr::pluck(data_redcap(), "data")()
      rv$data_temp <- data_redcap()
    })

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

    return(list(
      # status = reactive(temporary_rv$status),
      # name = reactive(temporary_rv$name),
      # code = reactive(temporary_rv$code),
      data = shiny::reactive(rv$data_temp)
    ))

  }

  shiny::moduleServer(
    id = id,
    module = module
  )

  }


#' Test app for the data-import module
#'
#' @rdname data-import
#'
#' @examples
#' \dontrun{
#' data_import_demo_app()
#' }
data_import_demo_app <- function() {
  ui <- shiny::fluidPage(
    data_import_ui("data_import"),
    toastui::datagridOutput2(outputId = "table"),
    DT::DTOutput("data_summary")
  )
  server <- function(input, output, session) {
    imported <- shiny::reactive(data_import_server(id = "data_import"))

    # output$data_summary <- DT::renderDataTable(
    #   {
    #     shiny::req(data_val$data)
    #     data_val$data
    #   },
    #   options = list(
    #     scrollX = TRUE,
    #     pageLength = 5
    #   )
    # )
    output$table <- toastui::renderDatagrid2({
      req(imported$data)
      toastui::datagrid(
        data = head(imported$data, 5),
        theme = "striped",
        colwidths = "guess",
        minBodyHeight = 250
      )
    })

  }
  shiny::shinyApp(ui, server)
}
