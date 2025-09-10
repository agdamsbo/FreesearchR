#' @title Create new column
#'
#' @description
#' This module allow to enter an expression to create a new column in a `data.frame`.
#'
#'
#' @param id Module's ID.
#'
#' @return A [shiny::reactive()] function returning the data.
#'
#' @note User can only use a subset of function: `r paste(list_allowed_operations(), collapse=", ")`.
#'  You can add more operations using the `allowed_operations` argument, for  example if you want to allow to use package lubridate, you can do:
#'  ```r
#'  c(list_allowed_operations(), getNamespaceExports("lubridate"))
#'  ```
#'
#' @export
#'
#' @importFrom htmltools tagList tags css
#'
#' @name create-column
#'
#' @example examples/create_column_module_demo.R
create_column_ui <- function(id) {
  ns <- NS(id)
  htmltools::tagList(
    # datamods:::html_dependency_datamods(),
    # html_dependency_FreesearchR(),
   shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "FreesearchR/inst/assets/css/FreesearchR.css")
  ),
    # tags$head(
    #   # Note the wrapping of the string in HTML()
    #   tags$style(HTML("
    #   /* modified from esquisse for data types */
    #   .btn-column-categorical {
    #     background-color: #EF562D;
    #     color: #FFFFFF;
    #   }
    #   .btn-column-continuous {
    #     background-color: #0C4C8A;
    #     color: #FFFFFF;
    #   }
    #   .btn-column-dichotomous {
    #     background-color: #97D5E0;
    #     color: #FFFFFF;
    #   }
    #   .btn-column-datetime {
    #     background-color: #97D5E0;
    #     color: #FFFFFF;
    #   }
    #   .btn-column-id {
    #     background-color: #848484;
    #     color: #FFFFFF;
    #   }
    #   .btn-column-text {
    #     background-color: #2E2E2E;
    #     color: #FFFFFF;
    #   }"))
    # ),
    fluidRow(
      column(
        width = 6,
        textInput(
          inputId = ns("new_column"),
          label = i18n$t("New column name:"),
          value = "new_column1",
          width = "100%"
        )
      ),
      column(
        width = 6,
        shinyWidgets::virtualSelectInput(
          inputId = ns("group_by"),
          label = i18n$t("Group calculation by:"),
          choices = NULL,
          multiple = TRUE,
          disableSelectAll = TRUE,
          hasOptionDescription = TRUE,
          width = "100%"
        )
      )
    ),
    shiny::textAreaInput(
      inputId = ns("expression"),
      label = i18n$t("Enter an expression to define new column:"),
      value = "",
      width = "100%",
      rows = 6
    ),
    tags$i(
      class = "d-block",
      phosphoricons::ph("info"),
      i18n$t("Click on a column name to add it to the expression:")
    ),
    uiOutput(outputId = ns("columns")),
    uiOutput(outputId = ns("feedback")),
    tags$div(
      style = htmltools::css(
        display = "grid",
        gridTemplateColumns = "3fr 1fr",
        columnGap = "10px",
        margin = "10px 0"
      ),
      actionButton(
        inputId = ns("compute"),
        label = tagList(
          phosphoricons::ph("gear"), i18n$t("Create column")
        ),
        class = "btn-outline-primary",
        width = "100%"
      ),
      actionButton(
        inputId = ns("remove"),
        label = tagList(
          phosphoricons::ph("trash")
        ),
        class = "btn-outline-danger",
        width = "100%"
      )
    )
  )
}

#' @param data_r A [shiny::reactive()] function returning a `data.frame`.
#' @param allowed_operations A `list` of allowed operations, see below for details.
#'
#' @export
#'
#' @rdname create-column
#'
create_column_server <- function(id,
                                 data_r = reactive(NULL),
                                 allowed_operations = list_allowed_operations()) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      info_alert <- shinyWidgets::alert(
        status = "info",
        phosphoricons::ph("question"),
        i18n$t("Choose a name for the column to be created or modified,"),
        i18n$t("then enter an expression before clicking on the button above to validate or on "),
        phosphoricons::ph("trash"), i18n$t("to delete it.")
      )

      rv <- reactiveValues(
        data = NULL,
        feedback = info_alert
      )

      observeEvent(input$hidden, rv$feedback <- info_alert)

      bindEvent(observe({
        data <- data_r()
        shinyWidgets::updateVirtualSelect(
          inputId = "group_by",
          choices = make_choices_with_infos(data)
        )
      }), data_r(), input$hidden)

      observeEvent(data_r(), rv$data <- data_r())

      output$feedback <- renderUI(rv$feedback)

      output$columns <- renderUI({
        data <- req(rv$data)
        mapply(
          label = names(data),
          data = data,
          FUN = btn_column,
          MoreArgs = list(inputId = ns("add_column")),
          SIMPLIFY = FALSE
        )
      })

      observeEvent(input$add_column, {
        updateTextAreaInput(
          session = session,
          inputId = "expression",
          value = paste0(input$expression, input$add_column)
        )
      })

      observeEvent(input$new_column, {
        if (input$new_column == "") {
          rv$feedback <- shinyWidgets::alert(
            status = "warning",
            phosphoricons::ph("warning"), i18n$t("New column name cannot be empty")
          )
        }
      })

      observeEvent(input$remove, {
        rv$data[[input$new_column]] <- NULL
      })
      observeEvent(input$compute, {
        rv$feedback <- try_compute_column(
          expression = input$expression,
          name = input$new_column,
          rv = rv,
          allowed_operations = allowed_operations,
          by = input$group_by
        )
      })

      return(reactive(rv$data))
    }
  )
}

#' @export
#'
#' @rdname create-column
# @importFrom methods getGroupMembers
list_allowed_operations <- function() {
  c(
    "(", "c",
    # getGroupMembers("Arith"),
    c("+", "-", "*", "^", "%%", "%/%", "/"),
    # getGroupMembers("Compare"),
    c("==", ">", "<", "!=", "<=", ">="),
    # getGroupMembers("Logic"),
    c("&", "|"),
    # getGroupMembers("Math"),
    c(
      "abs", "sign", "sqrt", "ceiling", "floor", "trunc", "cummax",
      "cummin", "cumprod", "cumsum", "exp", "expm1", "log", "log10",
      "log2", "log1p", "cos", "cosh", "sin", "sinh", "tan", "tanh",
      "acos", "acosh", "asin", "asinh", "atan", "atanh", "cospi", "sinpi",
      "tanpi", "gamma", "lgamma", "digamma", "trigamma"
    ),
    # getGroupMembers("Math2"),
    c("round", "signif"),
    # getGroupMembers("Summary"),
    c("max", "min", "range", "prod", "sum", "any", "all"),
    "pmin", "pmax", "mean",
    "paste", "paste0", "substr", "nchar", "trimws",
    "gsub", "sub", "grepl", "ifelse", "length",
    "as.numeric", "as.character", "as.integer", "as.Date", "as.POSIXct",
    "as.factor", "factor"
  )
}


#' @inheritParams shiny::modalDialog
#' @export
#'
#' @importFrom shiny showModal modalDialog textInput
#' @importFrom htmltools tagList
#'
#' @rdname create-column
modal_create_column <- function(id,
                                title = i18n$t("Create a new column"),
                                easyClose = TRUE,
                                size = "l",
                                footer = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    create_column_ui(id),
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = datamods:::genId())
    ),
    easyClose = easyClose,
    size = size,
    footer = footer
  ))
}

#' @inheritParams shinyWidgets::WinBox
#' @export
#'
#' @importFrom shinyWidgets WinBox wbOptions wbControls
#' @importFrom htmltools tagList
#' @rdname create-column
winbox_create_column <- function(id,
                                 title = i18n$t("Create a new column"),
                                 options = shinyWidgets::wbOptions(),
                                 controls = shinyWidgets::wbControls()) {
  ns <- NS(id)
  WinBox(
    title = title,
    ui = tagList(
      create_column_ui(id),
      tags$div(
        style = "display: none;",
        textInput(inputId = ns("hidden"), label = NULL, value = datamods:::genId())
      )
    ),
    options = modifyList(
      shinyWidgets::wbOptions(height = "550px", modal = TRUE),
      options
    ),
    controls = controls,
    auto_height = FALSE
  )
}


try_compute_column <- function(expression,
                               name,
                               rv,
                               allowed_operations,
                               by = NULL) {
  parsed <- try(parse(text = expression, keep.source = FALSE), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    return(datamods:::alert_error(attr(parsed, "condition")$message))
  }
  funs <- unlist(c(extract_calls(parsed), lapply(parsed, extract_calls)), recursive = TRUE)
  if (!are_allowed_operations(funs, allowed_operations)) {
    return(datamods:::alert_error(i18n$t("Some operations are not allowed")))
  }
  if (!isTruthy(by)) {
    result <- try(
      rlang::eval_tidy(rlang::parse_expr(expression), data = rv$data),
      silent = TRUE
    )
  } else {
    result <- try(
      {
        dt <- as.data.table(rv$data)
        new_col <- NULL
        dt[, new_col := rlang::eval_tidy(rlang::parse_expr(expression), data = .SD), by = by]
        dt$new_col
      },
      silent = TRUE
    )
  }
  if (inherits(result, "try-error")) {
    return(alert_error(attr(result, "condition")$message))
  }
  adding_col <- try(rv$data[[name]] <- result, silent = TRUE)
  if (inherits(adding_col, "try-error")) {
    return(alert_error(attr(adding_col, "condition")$message))
  }
  code <- if (!isTruthy(by)) {
    rlang::call2("mutate", !!!rlang::set_names(list(rlang::parse_expr(expression)), name))
  } else {
    rlang::call2(
      "mutate",
      !!!rlang::set_names(list(rlang::parse_expr(expression)), name),
      !!!list(.by = rlang::expr(c(!!!rlang::syms(by))))
    )
  }
  attr(rv$data, "code") <- Reduce(
    f = function(x, y) rlang::expr(!!x %>% !!y),
    x = c(attr(rv$data, "code"), code)
  )
  shinyWidgets::alert(
    status = "success",
    phosphoricons::ph("check"), i18n$t("Column added!")
  )
}

are_allowed_operations <- function(x, allowed_operations) {
  all(
    x %in% allowed_operations
  )
}


extract_calls <- function(exp) {
  if (is.call(exp)) {
    return(list(
      as.character(exp[[1L]]),
      lapply(exp[-1L], extract_calls)
    ))
  }
}

alert_error <- function(text) {
  alert(
    status = "danger",
    phosphoricons::ph("bug"), text
  )
}


btn_column <- function(label, data, inputId) {
  icon <- get_var_icon(data, "class")
  type <- data_type(data)
  tags$button(
    type = "button",
    class = paste0("btn btn-column-", type),
    style = htmltools::css(
      "--bs-btn-padding-y" = ".25rem",
      "--bs-btn-padding-x" = ".5rem",
      "--bs-btn-font-size" = ".75rem",
      "margin-bottom" = "5px"
    ),
    if (!is.null(icon)) icon,
    label,
    onclick = sprintf(
      "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
      inputId, label
    )
  )
}

make_choices_with_infos <- function(data) {
  lapply(
    X = seq_along(data),
    FUN = function(i) {
      nm <- names(data)[i]
      values <- data[[nm]]
      icon <- get_var_icon(values, "class")
      # icon <- if (inherits(values, "character")) {
      #   phosphoricons::ph("text-aa")
      # } else if (inherits(values, "factor")) {
      #   phosphoricons::ph("list-bullets")
      # } else if (inherits(values, c("numeric", "integer"))) {
      #   phosphoricons::ph("hash")
      # } else if (inherits(values, c("Date"))) {
      #   phosphoricons::ph("calendar")
      # } else if (inherits(values, c("POSIXt"))) {
      #   phosphoricons::ph("clock")
      # } else {
      #   NULL
      # }
      description <- if (is.atomic(values)) {
        paste(i18n$t("Unique values:"), data.table::uniqueN(values))
      } else {
        ""
      }
      list(
        label = htmltools::doRenderTags(tagList(
          icon, nm
        )),
        value = nm,
        description = description
      )
    }
  )
}
