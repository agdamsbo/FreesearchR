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
#'
import_file_ui <- function(id,
                           title = "",
                           preview_data = TRUE,
                           file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav"),
                           layout_params = c("dropdown", "inline")) {
  ns <- shiny::NS(id)

  if (!is.null(layout_params)) {
    layout_params <- match.arg(layout_params)
  }

  if (isTRUE(title)) {
    title <- shiny::tags$h4(
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
        shiny::helpText(phosphoricons::ph("info"), datamods:::i18n("if several use a comma (',') to separate them"))
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
        choices = c(
          "UTF-8" = "UTF-8",
          "Latin1" = "latin1"
        ),
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
            label = phosphoricons::ph("gear", title = "Parameters"),
            width = "50px",
            class = "px-1"
          ),
          params_ui
        )
      )
    )
  }
  shiny::tags$div(
    class = "datamods-import",
    datamods:::html_dependency_datamods(),
    title,
    file_ui,
    if (identical(layout_params, "inline")) params_ui,
    shiny::tags$div(
      class = "hidden",
      id = ns("sheet-container"),
      shinyWidgets::pickerInput(
        inputId = ns("sheet"),
        label = datamods:::i18n("Select sheet to import:"),
        choices = NULL,
        width = "100%",
        multiple = TRUE
      )
    ),
    shiny::tags$div(
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
    }
    ,
    shiny::uiOutput(
      outputId = ns("container_confirm_btn"),
      style = "margin-top: 20px;"
    )  ,
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
    if (!rlang::is_named(read_fns)) {
      stop("import_file_server: `read_fns` must be a named list.", call. = FALSE)
    }
    if (!all(vapply(read_fns, rlang::is_function, logical(1)))) {
      stop("import_file_server: `read_fns` must be list of function(s).", call. = FALSE)
    }
  }

  trigger_return <- match.arg(trigger_return)
  return_class <- match.arg(return_class)

  module <- function(input, output, session) {
    ns <- session$ns
    imported_rv <- shiny::reactiveValues(data = NULL, name = NULL)
    temporary_rv <- shiny::reactiveValues(data = NULL, name = NULL, status = NULL)

    shiny::observeEvent(reset(), {
      temporary_rv$data <- NULL
      temporary_rv$name <- NULL
      temporary_rv$status <- NULL
    })

    output$container_confirm_btn <- shiny::renderUI({
      if (identical(trigger_return, "button")) {
        datamods:::button_import()
      }
    })

    shiny::observeEvent(input$file, {
      if (isTRUE(is_workbook(input$file$datapath))) {
        if (isTRUE(is_excel(input$file$datapath))) {
          choices <- readxl::excel_sheets(input$file$datapath)
        } else if (isTRUE(is_ods(input$file$datapath))) {
          choices <- readODS::ods_sheets(input$file$datapath)
        }
        selected <- choices[1]

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "sheet",
          choices = choices,
          selected = selected
        )
        datamods:::showUI(paste0("#", ns("sheet-container")))
      } else {
        datamods:::hideUI(paste0("#", ns("sheet-container")))
      }
    })

    observeEvent(
      list(
        input$file,
        input$sheet,
        input$skip_rows,
        input$dec,
        input$encoding,
        input$na_label
      ),
      {
        req(input$file)
        if (is_workbook(input$file$datapath)) shiny::req(input$sheet)
        # browser()

        # browser()
        # req(input$skip_rows)
        extension <- tools::file_ext(input$file$datapath)

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
        code <- rlang::call2(read_fns[[extension]], !!!modifyList(parameters, list(file = input$file$name)))

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
      },
      ignoreInit = TRUE
    )

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

is_workbook <- function(path) {
  is_excel(path) || is_ods(path)
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
    check.names = TRUE,
    encoding = encoding,
    data.table = FALSE,
    logical01 = TRUE,
    logicalYN = TRUE,
    keepLeadingZeros = TRUE
  )
}

import_xls <- function(file, sheet, skip, na.strings) {
  tryCatch(
    {
      # browser()
      sheet |>
        purrr::map(\(.x){
          openxlsx2::read_xlsx(
            file = file,
            sheet = .x,
            skip_empty_rows = TRUE,
            start_row = skip - 1,
            na.strings = na.strings
          )
        }) |>
        purrr::reduce(dplyr::full_join)
    },
    warning = function(warn) {
      showNotification(paste0(warn), type = "warning")
    },
    error = function(err) {
      showNotification(paste0(err), type = "err")
    }
  )
}

import_ods <- function(file, sheet, skip, na.strings) {
  tryCatch(
    {
      sheet |>
        purrr::map(\(.x){
          readODS::read_ods(
            path = file,
            sheet = .x,
            skip = skip,
            na = na.strings
          )
        }) |>
        purrr::reduce(dplyr::full_join)
    },
    warning = function(warn) {
      showNotification(paste0(warn), type = "warning")
    },
    error = function(err) {
      showNotification(paste0(err), type = "err")
    }
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
  shiny::tags$div(
    class = "form-group shiny-input-container",
    shinyWidgets:::label_input(inputId, label),
    style = htmltools:::css(width = htmltools:::validateCssUnit(width)),
    shiny::tags$div(
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


#' Test app for the import_file module
#'
#' @rdname import-file_module
#'
#' @examples
#' \dontrun{
#' import_file_demo_app()
#' }
import_file_demo_app <- function() {
  ui <- shiny::fluidPage(
    # theme = bslib::bs_theme(version = 5L),
    # theme = bslib::bs_theme(version = 5L, preset = "bootstrap"),
    shiny::tags$h3("Import data from a file"),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        import_file_ui(
          id = "myid",
          file_extensions = c(".csv", ".tsv", ".txt", ".xls", ".xlsx", ".rds", ".sas7bdat", ".ods", ".dta"),
          layout_params = "dropdown" # "inline" # or "dropdown"
        )
      ),
      shiny::column(
        width = 8,
        shiny::tags$b("Import status:"),
        shiny::verbatimTextOutput(outputId = "status"),
        shiny::tags$b("Name:"),
        shiny::verbatimTextOutput(outputId = "name"),
        shiny::tags$b("Code:"),
        shiny::verbatimTextOutput(outputId = "code"),
        shiny::tags$b("Data:"),
        shiny::verbatimTextOutput(outputId = "data")
      )
    )
  )
  server <- function(input, output, session) {
    imported <- import_file_server(
      id = "myid",
      show_data_in = "popup",
      trigger_return = "change",
      return_class = "data.frame",
      # Custom functions to read data
      read_fns = list(
        ods = import_ods,
        dta = function(file) {
          haven::read_dta(
            file = file,
            .name_repair = "unique_quiet"
          )
        },
        # csv = function(file) {
        #   readr::read_csv(
        #     file = file,
        #     na = consider.na,
        #     name_repair = "unique_quiet"
        #     )
        # },
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

    output$status <- shiny::renderPrint({
      imported$status()
    })
    output$name <- shiny::renderPrint({
      imported$name()
    })
    output$code <- shiny::renderPrint({
      imported$code()
    })
    output$data <- shiny::renderPrint({
      imported$data()
    })
  }
  shiny::shinyApp(ui, server)
}
