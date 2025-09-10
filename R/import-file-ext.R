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
      "Import a file",
      class = "datamods-title"
    )
  }


  params_ui <- shiny::fluidRow(
    shiny::column(
      width = 6,
      shinyWidgets::numericInputIcon(
        inputId = ns("skip_rows"),
        label = i18n$t("Rows to skip before reading data:"),
        value = 0,
        min = 0,
        icon = list("n ="),
        size = "sm",
        width = "100%"
      ),
      shiny::tagAppendChild(
        shinyWidgets::textInputIcon(
          inputId = ns("na_label"),
          label = i18n$t("Missing values character(s):"),
          value = "NA,,'',na",
          icon = list("NA"),
          size = "sm",
          width = "100%"
        ),
        shiny::helpText(phosphoricons::ph("info"), i18n$t("if several use a comma (',') to separate them"))
      )
    ),
    shiny::column(
      width = 6,
      shinyWidgets::textInputIcon(
        inputId = ns("dec"),
        label = i18n$t("Decimal separator:"),
        value = ".",
        icon = list("0.00"),
        size = "sm",
        width = "100%"
      ),
      selectInputIcon(
        inputId = ns("encoding"),
        label = i18n$t("Encoding:"),
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
      label = i18n$t("Upload a file:"),
      buttonLabel = i18n$t("Browse..."),
      placeholder = "No file selected; maximum file size is 5 mb",
      accept = file_extensions,
      width = "100%",
      ## A solution to allow multiple file upload is being considered
      multiple = FALSE
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
        label = i18n$t("Select sheet to import:"),
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
        shiny::tags$b(i18n$t("No file selected.")),
        # shiny::textOutput(ns("trans_format_text")),
        # This is the easiest solution, though not gramatically perfect
        i18n$t("You can choose between these file types:"), paste(file_extensions,collapse=', '),
        # sprintf("You can import %s files", paste(file_extensions, collapse = ", ")),
        dismissible = TRUE
      )
    ),
    if (isTRUE(preview_data)) {
      toastui::datagridOutput2(outputId = ns("table"))
    },
    shiny::uiOutput(
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
                               reset = reactive(NULL)) {
  read_fns <- list(
    ods = "import_ods",
    dta = "import_dta",
    csv = "import_delim",
    tsv = "import_delim",
    txt = "import_delim",
    xls = "import_xls",
    xlsx = "import_xls",
    rds = "import_rds"
  )

  trigger_return <- match.arg(trigger_return)
  return_class <- match.arg(return_class)

  module <- function(input, output, session) {
    ns <- session$ns
    imported_rv <- shiny::reactiveValues(data = NULL, name = NULL)
    temporary_rv <- shiny::reactiveValues(data = NULL, name = NULL, status = NULL, sheets = 1)

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

    # ## Translations
    # shiny::observe({
    #   output$trans_format_text <- shiny::renderText(glue::glue(i18n$t("You can import {file_extensions_text} files")))
    # })

    shiny::observeEvent(input$file, {
      ## Several steps are taken to ensure no errors on changed input file
      temporary_rv$sheets <- 1
      if (isTRUE(is_workbook(input$file$datapath))) {
        if (isTRUE(is_excel(input$file$datapath))) {
          temporary_rv$sheets <- readxl::excel_sheets(input$file$datapath)
        } else if (isTRUE(is_ods(input$file$datapath))) {
          temporary_rv$sheets <- readODS::ods_sheets(input$file$datapath)
        }
        selected <- temporary_rv$sheets[1]

        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "sheet",
          selected = selected,
          choices = temporary_rv$sheets
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

        if (!all(input$sheet %in% temporary_rv$sheets)) {
          sheets <- 1
        } else {
          sheets <- input$sheet
        }

        extension <- tools::file_ext(input$file$datapath)

        parameters <- list(
          file = input$file$datapath,
          sheet = sheets,
          skip = input$skip_rows,
          dec = input$dec,
          encoding = input$encoding,
          na.strings = datamods:::split_char(input$na_label)
        )

        parameters <- parameters[which(names(parameters) %in% rlang::fn_fmls_names(get(read_fns[[extension]])))]
        # parameters <- parameters[which(names(parameters) %in% rlang::fn_fmls_names(read_fns[[extension]]))]
        imported <- try(rlang::exec(read_fns[[extension]], !!!parameters), silent = TRUE)
        code <- rlang::call2(read_fns[[extension]], !!!modifyList(parameters, list(file = input$file$name)), .ns = "FreesearchR")

        if (inherits(imported, "try-error")) {
          imported <- try(rlang::exec(rio::import, !!!parameters[1]), silent = TRUE)
          code <- rlang::call2("import", !!!list(file = input$file$name), .ns = "rio")
        }

        if (inherits(imported, "try-error") || NROW(imported) < 1) {
          datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
          datamods:::insert_error(mssg = i18n$t(attr(imported, "condition")$message))
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
              extra = if (isTRUE(input$preview_data)) i18n$t("First five rows are shown below:")
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
      tryCatch(
        {
          datamods:::show_data(default_parsing(temporary_rv$data), title = i18n$t("Imported data"), type = show_data_in)
        },
        # warning = function(warn) {
        #     showNotification(warn, type = "warning")
        # },
        error = function(err) {
          showNotification(err, type = "err")
        }
      )
    })

    output$table <- toastui::renderDatagrid2({
      req(temporary_rv$data)
      tryCatch(
        {
          toastui::datagrid(
            data = setNames(head(temporary_rv$data, 5), make.names(names(temporary_rv$data), unique = TRUE)),
            theme = "striped",
            colwidths = "guess",
            minBodyHeight = 250
          )
        },
        error = function(err) {
          showNotification(err, type = "err")
        }
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


# File import functions ---------------------------------------------------

#' Wrapper to ease data file import
#'
#' @param file path to the file
#' @param sheet for Excel files, sheet to read
#' @param skip number of row to skip
#' @param encoding file encoding
#' @param na.strings character(s) to interpret as missing values.
#'
#'
#' @name import-file-type
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


#' @name import-file-type
#'
#' @returns data.frame
#' @export
#'
import_xls <- function(file, sheet, skip, na.strings) {
  tryCatch(
    {
      ## If sheet is null, this allows purrr::map to run
      if (is.null(sheet)) sheet <- 1

      sheet |>
        purrr::map(\(.x){
          readxl::read_excel(
            path = file,
            sheet = .x,
            na = na.strings,
            skip = skip,
            .name_repair = "unique_quiet",
            trim_ws = TRUE
          )

          # openxlsx2::read_xlsx(
          #   file = file,
          #   sheet = .x,
          #   skip_empty_rows = TRUE,
          #   start_row = skip - 1,
          #   na.strings = na.strings
          # )
        }) |>
        purrr::reduce(dplyr::full_join)
    },
    # warning = function(warn) {
    #   showNotification(paste0(warn), type = "warning")
    # },
    error = function(err) {
      showNotification(paste0(err), type = "err")
    }
  )
}


#' @name import-file-type
#'
#' @returns data.frame
#' @export
#'
import_ods <- function(file, sheet, skip, na.strings) {
  tryCatch(
    {
      if (is.null(sheet)) sheet <- 1
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
    # warning = function(warn) {
    #   showNotification(paste0(warn), type = "warning")
    # },
    error = function(err) {
      showNotification(paste0(err), type = "err")
    }
  )
}

#' @name import-file-type
#'
#' @returns data.frame
#' @export
#'
import_dta <- function(file) {
  haven::read_dta(
    file = file,
    .name_repair = "unique_quiet"
  )
}

#' @name import-file-type
#'
#' @returns data.frame
#' @export
#'
import_rds <- function(file) {
  readr::read_rds(
    file = file
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
      return_class = "data.frame"
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
