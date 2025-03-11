
#' @title Import data from a file
#'
#' @description Let user upload a file and import data
#'
#' @param preview_data Show or not a preview of the data under the file input.
#' @param file_extensions File extensions accepted by [shiny::fileInput()], can also be MIME type.
#' @param layout_params How to display import parameters : in a dropdown button or inline below file input.
#'
#' @export
#'
#' @name import-file
#'
#' @importFrom shiny NS fileInput actionButton icon
#' @importFrom htmltools tags tagAppendAttributes css tagAppendChild
#' @importFrom shinyWidgets pickerInput numericInputIcon textInputIcon dropMenu
#' @importFrom phosphoricons ph
#' @importFrom toastui datagridOutput2
#'
#' @example examples/from-file.R
import_file_ui <- function(id,
                           title = TRUE,
                           preview_data = TRUE,
                           file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav"),
                           layout_params = c("dropdown", "inline")) {

  ns <- NS(id)

  if (!is.null(layout_params)) {
    layout_params <- match.arg(layout_params)
  }

  if (isTRUE(title)) {
    title <- tags$h4(
      datamods:::i18n("Import a file"),
      class = "datamods-title"
    )
  }


  params_ui <- shiny::fluidRow(
    shiny::column(
      width = 6,
      shinyWidgets::numericInputIcon(
        inputId = ns("skip_rows"),
        label = datamods:::i18n("Rows to skip before reading data:"),
        value = 0,
        min = 0,
        icon = list("n ="),
        size = "sm",
        width = "100%"
      ),
      shiny::tagAppendChild(
        shinyWidgets::textInputIcon(
          inputId = ns("na_label"),
          label = datamods:::i18n("Missing values character(s):"),
          value = "NA,,'',na",
          icon = list("NA"),
          size = "sm",
          width = "100%"
        ),
        shiny::helpText(ph("info"), datamods:::i18n("if several use a comma (',') to separate them"))
      )
    ),
    shiny::column(
      width = 6,
      shinyWidgets::textInputIcon(
        inputId = ns("dec"),
        label = datamods:::i18n("Decimal separator:"),
        value = ".",
        icon = list("0.00"),
        size = "sm",
        width = "100%"
      ),
      selectInputIcon(
        inputId = ns("encoding"),
        label = datamods:::i18n("Encoding:"),
        choices = c("UTF-8"="UTF-8",
                    "Latin1"="latin1"),
        icon = phosphoricons::ph("text-aa"),
        size = "sm",
        width = "100%"
      )
    )
  )

  file_ui <- shiny::tagAppendAttributes(
    shiny::fileInput(
      inputId = ns("file"),
      label = datamods:::i18n("Upload a file:"),
      buttonLabel = datamods:::i18n("Browse..."),
      placeholder = datamods:::i18n("No file selected"),
      accept = file_extensions,
      width = "100%"
    ),
    class = "mb-0"
  )
  if (identical(layout_params, "dropdown")) {
    file_ui <- shiny::tags$div(
      style = htmltools::css(
        display = "grid",
        gridTemplateColumns = "1fr 50px",
        gridColumnGap = "10px"
      ),
      file_ui,
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
            label = ph("gear", title = "Parameters"),
            width = "50px",
            class = "px-1"
          ),
          params_ui
        )
      )
    )
  }
  tags$div(
    class = "datamods-import",
    datamods:::html_dependency_datamods(),
    title,
    file_ui,
    if (identical(layout_params, "inline")) params_ui,
    tags$div(
      class = "hidden",
      id = ns("sheet-container"),
      shinyWidgets::pickerInput(
        inputId = ns("sheet"),
        label = datamods:::i18n("Select sheet to import:"),
        choices = NULL,
        width = "100%"
      )
    ),
    tags$div(
      id = ns("import-placeholder"),
      shinyWidgets::alert(
        id = ns("import-result"),
        status = "info",
        shiny::tags$b(datamods:::i18n("No file selected:")),
        sprintf(datamods:::i18n("You can import %s files"), paste(file_extensions, collapse = ", ")),
        dismissible = TRUE
      )
    ),
    if (isTRUE(preview_data)) {
      toastui::datagridOutput2(outputId = ns("table"))
    },
    uiOutput(
      outputId = ns("container_confirm_btn"),
      style = "margin-top: 20px;"
    ),
    tags$div(
      style = htmltools::css(display = "none"),
      shiny::checkboxInput(
        inputId = ns("preview_data"),
        label = NULL,
        value = isTRUE(preview_data)
      )
    )
  )
}


#' @param read_fns Named list with custom function(s) to read data:
#'  * the name must be the extension of the files to which the function will be applied
#'  * the value must be a function that can have 5 arguments (you can ignore some of them, but you have to use the same names),
#'    passed by user through the interface:
#'    + `file`: path to the file
#'    + `sheet`: for Excel files, sheet to read
#'    + `skip`: number of row to skip
#'    + `dec`: decimal separator
#'    + `encoding`: file encoding
#'    + `na.strings`: character(s) to interpret as missing values.
#'
#' @export
#'
#' @importFrom shiny moduleServer
#' @importFrom htmltools tags tagList
#' @importFrom shiny reactiveValues reactive observeEvent removeUI req
#' @importFrom shinyWidgets updatePickerInput
#' @importFrom readxl excel_sheets
#' @importFrom rio import
#' @importFrom rlang exec fn_fmls_names is_named is_function
#' @importFrom tools file_ext
#' @importFrom utils head
#' @importFrom toastui renderDatagrid2 datagrid
#'
#' @rdname import-file
import_file_server <- function(id,
                               btn_show_data = TRUE,
                               show_data_in = c("popup", "modal"),
                               trigger_return = c("button", "change"),
                               return_class = c("data.frame", "data.table", "tbl_df", "raw"),
                               reset = reactive(NULL),
                               read_fns = list()) {

  if (length(read_fns) > 0) {
    if (!is_named(read_fns))
      stop("import_file_server: `read_fns` must be a named list.", call. = FALSE)
    if (!all(vapply(read_fns, is_function, logical(1))))
      stop("import_file_server: `read_fns` must be list of function(s).", call. = FALSE)
  }

  trigger_return <- match.arg(trigger_return)
  return_class <- match.arg(return_class)

  module <- function(input, output, session) {

    ns <- session$ns
    imported_rv <- reactiveValues(data = NULL, name = NULL)
    temporary_rv <- reactiveValues(data = NULL, name = NULL, status = NULL)

    observeEvent(reset(), {
      temporary_rv$data <- NULL
      temporary_rv$name <- NULL
      temporary_rv$status <- NULL
    })

    output$container_confirm_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        datamods:::button_import()
      }
    })

    observeEvent(input$file, {
      if (isTRUE(is_excel(input$file$datapath))) {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "sheet",
          choices = readxl::excel_sheets(input$file$datapath)
        )
        datamods:::showUI(paste0("#", ns("sheet-container")))
      } else if (isTRUE(is_ods(input$file$datapath))) {
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "sheet",
          choices = readODS::ods_sheets(input$file$datapath)
        )
        datamods:::showUI(paste0("#", ns("sheet-container")))
      } else {
        datamods:::hideUI(paste0("#", ns("sheet-container")))
      }
    })

    observeEvent(list(
      input$file,
      input$sheet,
      input$skip_rows,
      input$dec,
      input$encoding,
      input$na_label
    ), {
      req(input$file)
      # req(input$skip_rows)
      extension <- tools::file_ext(input$file$datapath)
      if (isTRUE(extension %in% names(read_fns))) {
        parameters <- list(
          file = input$file$datapath,
          sheet = input$sheet,
          skip = input$skip_rows,
          dec = input$dec,
          encoding = input$encoding,
          na.strings = datamods:::split_char(input$na_label)
        )
        parameters <- parameters[which(names(parameters) %in% rlang::fn_fmls_names(read_fns[[extension]]))]
        imported <- try(rlang::exec(read_fns[[extension]], !!!parameters), silent = TRUE)
        code <- call2(read_fns[[extension]], !!!modifyList(parameters, list(file = input$file$name)))
      } else {
        if (is_excel(input$file$datapath) || is_ods(input$file$datapath)) {
          req(input$sheet)
          parameters <- list(
            file = input$file$datapath,
            which = input$sheet,
            skip = input$skip_rows,
            na = datamods:::split_char(input$na_label)
          )
        } else if (is_sas(input$file$datapath)) {
          parameters <- list(
            file = input$file$datapath,
            skip = input$skip_rows,
            encoding = input$encoding
          )
        } else {
          parameters <- list(
            file = input$file$datapath,
            skip = input$skip_rows,
            dec = input$dec,
            encoding = input$encoding,
            na.strings = datamods:::split_char(input$na_label)
          )
        }
        imported <- try(rlang::exec(rio::import, !!!parameters), silent = TRUE)
        code <- rlang::call2("import", !!!utils::modifyList(parameters, list(file = input$file$name)), .ns = "rio")
      }

      if (inherits(imported, "try-error")) {
        imported <- try(rlang::exec(rio::import, !!!parameters[1]), silent = TRUE)
        code <- rlang::call2("import", !!!list(file = input$file$name), .ns = "rio")
      }

      if (inherits(imported, "try-error") || NROW(imported) < 1) {

        datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
        datamods:::insert_error(mssg = datamods:::i18n(attr(imported, "condition")$message))
        temporary_rv$status <- "error"
        temporary_rv$data <- NULL
        temporary_rv$name <- NULL
        temporary_rv$code <- NULL

      } else {

        datamods:::toggle_widget(inputId = "confirm", enable = TRUE)

        datamods:::insert_alert(
          selector = ns("import"),
          status = "success",
          datamods:::make_success_alert(
            imported,
            trigger_return = trigger_return,
            btn_show_data = btn_show_data,
            extra = if (isTRUE(input$preview_data)) datamods:::i18n("First five rows are shown below:")
          )
        )
        temporary_rv$status <- "success"
        temporary_rv$data <- imported
        temporary_rv$name <- input$file$name
        temporary_rv$code <- code
      }
    }, ignoreInit = TRUE)

    observeEvent(input$see_data, {
      datamods:::show_data(temporary_rv$data, title = datamods:::i18n("Imported data"), type = show_data_in)
    })

    output$table <- toastui::renderDatagrid2({
      req(temporary_rv$data)
      toastui::datagrid(
        data = head(temporary_rv$data, 5),
        theme = "striped",
        colwidths = "guess",
        minBodyHeight = 250
      )
    })

    observeEvent(input$confirm, {
      imported_rv$data <- temporary_rv$data
      imported_rv$name <- temporary_rv$name
      imported_rv$code <- temporary_rv$code
    })

    if (identical(trigger_return, "button")) {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(imported_rv$name),
        code = reactive(imported_rv$code),
        data = reactive(datamods:::as_out(imported_rv$data, return_class))
      ))
    } else {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(temporary_rv$name),
        code = reactive(temporary_rv$code),
        data = reactive(datamods:::as_out(temporary_rv$data, return_class))
      ))
    }
  }

  moduleServer(
    id = id,
    module = module
  )
}

# utils -------------------------------------------------------------------

is_excel <- function(path) {
  isTRUE(tools::file_ext(path) %in% c("xls", "xlsx"))
}

is_ods <- function(path) {
  isTRUE(tools::file_ext(path) %in% c("ods"))
}

is_sas <- function(path) {
  isTRUE(tools::file_ext(path) %in% c("sas7bdat"))
}

#' Wrapper of data.table::fread to import delim files with few presets
#'
#' @param file file
#' @param encoding encoding
#' @param na.strings na.strings
#'
#' @returns data.frame
#' @export
#'
import_delim <- function(file, skip, encoding, na.strings) {
  data.table::fread(
    file = file,
    na.strings = na.strings,
    skip = skip,
    check.names	= TRUE,
    encoding = encoding,
    data.table = FALSE,
    logical01 = TRUE,
    logicalYN = TRUE,
    keepLeadingZeros = TRUE
  )
}

#' @title Create a select input control with icon(s)
#'
#' @description Extend form controls by adding text or icons before,
#'  after, or on both sides of a classic `selectInput`.
#'
#' @inheritParams shiny::selectInput
#'
#' @return A numeric input control that can be added to a UI definition.
#' @export
#'
#' @importFrom shiny restoreInput
#' @importFrom htmltools tags validateCssUnit css
#'
selectInputIcon <- function(inputId,
                            label,
                            choices,
                            selected = NULL,
                            multiple = FALSE,
                            selectize = TRUE,
                            size = NULL,
                            width = NULL,
                            icon = NULL) {
  selected <- shiny::restoreInput(id = inputId, default = selected)
  tags$div(
    class = "form-group shiny-input-container",
    shinyWidgets:::label_input(inputId, label),
    style = htmltools:::css(width = htmltools:::validateCssUnit(width)),
    tags$div(
      class = "input-group",
      class = shinyWidgets:::validate_size(size),
      shinyWidgets:::markup_input_group(icon, "left", theme_func = shiny::getCurrentTheme),
      shiny::tags$select(
        id = inputId,
        class = "form-control select-input-icon",
        shiny:::selectOptions(choices, selected, inputId, selectize)
      ),
      shinyWidgets:::markup_input_group(icon, "right", theme_func = shiny::getCurrentTheme)
    ),
    shinyWidgets:::html_dependency_input_icons()
  )
}





# library(shiny)
# library(datamods)

ui <- fluidPage(
  # theme = bslib::bs_theme(version = 5L),
  # theme = bslib::bs_theme(version = 5L, preset = "bootstrap"),
  tags$h3("Import data from a file"),
  fluidRow(
    column(
      width = 4,
      import_file_ui(
        id = "myid",
        file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".json"),
        layout_params = "dropdown" #"inline" # or "dropdown"
      )
    ),
    column(
      width = 8,
      tags$b("Import status:"),
      verbatimTextOutput(outputId = "status"),
      tags$b("Name:"),
      verbatimTextOutput(outputId = "name"),
      tags$b("Code:"),
      verbatimTextOutput(outputId = "code"),
      tags$b("Data:"),
      verbatimTextOutput(outputId = "data")
    )
  )
)

server <- function(input, output, session) {

  imported <- import_file_server(
    id = "myid",
    # Custom functions to read data
    read_fns = list(
      xls = function(file, sheet, skip, encoding) {
        readxl::read_xls(path = file, sheet = sheet, skip = skip)
      },
      json = function(file) {
        jsonlite::read_json(file, simplifyVector = TRUE)
      }
    ),
    show_data_in = "modal"
  )

  output$status <- renderPrint({
    imported$status()
  })
  output$name <- renderPrint({
    imported$name()
  })
  output$code <- renderPrint({
    imported$code()
  })
  output$data <- renderPrint({
    imported$data()
  })

}

if (interactive())
  shinyApp(ui, server)


