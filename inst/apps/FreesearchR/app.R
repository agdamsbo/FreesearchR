

########
#### Current file: /Users/au301842/FreesearchR/app/libs.R
########

library(shiny)
library(shinyjs)
# library(methods)
# library(readr)
# library(MASS)
# library(stats)
# library(gt)
# library(openxlsx2)
# library(haven)
# library(readODS)
# library(bslib)
# library(assertthat)
library(dplyr)
# library(quarto)
# library(here)
# library(broom)
# library(broom.helpers)
# library(easystats)
# library(patchwork)
# library(DHARMa)
# library(apexcharter)
library(toastui)
library(datamods)
# library(IDEAFilter)
library(shinyWidgets)
# library(DT)
# library(data.table)
library(gtsummary)
library(bsicons)
library(rlang)
# library(datamods)
# library(toastui)
# library(phosphoricons)


########
#### Current file: /Users/au301842/FreesearchR/app/functions.R
########



########
#### Current file: /Users/au301842/FreesearchR/R//app_version.R
########

app_version <- function()'25.7.2'


########
#### Current file: /Users/au301842/FreesearchR/R//baseline_table.R
########

#' Print a flexible baseline characteristics table
#'
#' @param data data set
#' @param fun.args list of arguments passed to
#' @param fun function to
#' @param vars character vector of variables to include
#'
#' @return object of standard class for fun
#' @export
#'
#' @examples
#' mtcars |> baseline_table()
#' mtcars |> baseline_table(fun.args = list(by = "gear"))
baseline_table <- function(data, fun.args = NULL, fun = gtsummary::tbl_summary, vars = NULL) {

  out <- do.call(fun, c(list(data = data), fun.args))
  return(out)
}



#' Create a baseline table
#'
#' @param data data
#' @param ... passed as fun.arg to baseline_table()
#' @param strat.var grouping/strat variable
#' @param add.p add comparison/p-value
#' @param add.overall add overall column
#'
#' @returns gtsummary table list object
#' @export
#'
#' @examples
#' mtcars |> create_baseline(by.var = "gear", add.p = "yes" == "yes")
#' create_baseline(default_parsing(mtcars), by.var = "am", add.p = FALSE, add.overall = FALSE, theme = "lancet")
create_baseline <- function(data, ..., by.var, add.p = FALSE, add.overall = FALSE, theme = c("jama", "lancet", "nejm", "qjecon")) {
  theme <- match.arg(theme)

  if (by.var == "none" | !by.var %in% names(data)) {
    by.var <- NULL
  }

  ## These steps are to handle logicals/booleans, that messes up the order of columns
  ## Has been reported and should be fixed soon (02042025)

  if (!is.null(by.var)) {
    if (identical("logical", class(data[[by.var]]))) {
      data[by.var] <- as.character(data[[by.var]])
    }
  }

  suppressMessages(gtsummary::theme_gtsummary_journal(journal = theme))

  args <- list(...)

  parameters <- list(
    data = data,
    fun.args = list(by = by.var, ...)
  )

  out <- do.call(
    baseline_table,
    parameters
  )


  if (!is.null(by.var)) {
    if (isTRUE(add.overall)) {
      out <- out |> gtsummary::add_overall()
    }
    if (isTRUE(add.p)) {
      out <- out |>
        gtsummary::add_p() |>
        gtsummary::bold_p()
    }
  }

  out
}


########
#### Current file: /Users/au301842/FreesearchR/R//contrast_text.R
########

#' @title Contrast Text Color
#' @description Calculates the best contrast text color for a given
#' background color.
#' @param background A hex/named color value that represents the background.
#' @param light_text A hex/named color value that represents the light text
#' color.
#' @param dark_text A hex/named color value that represents the dark text color.
#' @param threshold A numeric value between 0 and 1 that is used to determine
#' the luminance threshold of the background color for text color.
#' @param method A character string that specifies the method for calculating
#' the luminance. Three different methods are available:
#' c("relative","perceived","perceived_2")
#' @param ... parameter overflow. Ignored.
#' @details
#' This function aids in deciding the font color to print on a given background.
#' The function is based on the example provided by teppo:
#' https://stackoverflow.com/a/66669838/21019325.
#' The different methods provided are based on the methods outlined in the
#' StackOverflow thread:
#' https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color
#' @return A character string that contains the best contrast text color.
#' @examples
#' contrast_text(c("#F2F2F2", "blue"))
#'
#' contrast_text(c("#F2F2F2", "blue"), method="relative")
#' @export
#'
#'
contrast_text <- function(background,
                          light_text = 'white',
                          dark_text = 'black',
                          threshold = 0.5,
                          method = "perceived_2",
                          ...) {
  if (method == "relative") {
    luminance <-
      c(c(.2126, .7152, .0722) %*% grDevices::col2rgb(background) / 255)
  } else if (method == "perceived") {
    luminance <-
      c(c(.299, .587, .114) %*% grDevices::col2rgb(background) / 255)
  } else if (method == "perceived_2") {
    luminance <- c(sqrt(colSums((
      c(.299, .587, .114) * grDevices::col2rgb(background)
    ) ^ 2)) / 255)
  }

  ifelse(luminance < threshold,
         light_text,
         dark_text)
}


########
#### Current file: /Users/au301842/FreesearchR/R//correlations-module.R
########

#' Data correlations evaluation module
#'
#' @param id Module id. (Use 'ns("id")')
#'
#' @name data-correlations
#' @returns Shiny ui module
#' @export
data_correlations_ui <- function(id, ...) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::textOutput(outputId = ns("suggest")),
    shiny::plotOutput(outputId = ns("correlation_plot"), ...)
  )
}


#'
#' @param data data
#' @param color.main main color
#' @param color.sec secondary color
#' @param ... arguments passed to toastui::datagrid
#'
#' @name data-correlations
#' @returns shiny server module
#' @export
data_correlations_server <- function(id,
                                     data,
                                     include.class = NULL,
                                     cutoff = .7,
                                     ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # ns <- session$ns

      rv <- shiny::reactiveValues(
        data = NULL
      )

      rv$data <- shiny::reactive({
        shiny::req(data)
        if (!is.null(include.class)) {
          filter <- sapply(data(), class) %in% include.class
          out <- data()[filter]
        } else {
          out <- data()
        }
        # out |> dplyr::mutate(dplyr::across(tidyselect::everything(),as.numeric))
        sapply(out,as.numeric)
        # as.numeric()
      })

      # rv <- list()
      # rv$data <- mtcars

      output$suggest <- shiny::renderPrint({
        shiny::req(rv$data)
        shiny::req(cutoff)
        pairs <- correlation_pairs(rv$data(), threshold = cutoff())

        more <- ifelse(nrow(pairs) > 1, "from each pair ", "")

        if (nrow(pairs) == 0) {
          out <- glue::glue("No variables have a correlation measure above the threshold.")
        } else {
          out <- pairs |>
            apply(1, \(.x){
              glue::glue("'{.x[1]}'x'{.x[2]}'({round(as.numeric(.x[3]),2)})")
            }) |>
            (\(.x){
              glue::glue("The following variable pairs are highly correlated: {sentence_paste(.x)}.\nConsider excluding one {more}from the dataset to ensure variables are independent.")
            })()
        }
        out
      })

      output$correlation_plot <- shiny::renderPlot({
        ggcorrplot::ggcorrplot(cor(rv$data())) +
          # ggplot2::theme_void() +
          ggplot2::theme(
            # legend.position = "none",
            legend.title = ggplot2::element_text(size = 20),
            legend.text = ggplot2::element_text(size = 14),
            # panel.grid.major = element_blank(),
            # panel.grid.minor = element_blank(),
            # axis.text.y = element_blank(),
            # axis.title.y = element_blank(),
            axis.text.x = ggplot2::element_text(size = 20),
            axis.text.y = ggplot2::element_text(size = 20),
            # text = element_text(size = 5),
            # plot.title = element_blank(),
            # panel.background = ggplot2::element_rect(fill = "white"),
            # plot.background = ggplot2::element_rect(fill = "white"),
            panel.border = ggplot2::element_blank()
          )
        # psych::pairs.panels(rv$data())
      })
    }
  )
}

correlation_pairs <- function(data, threshold = .8) {
  data <- as.data.frame(data)[!sapply(as.data.frame(data), is.character)]
  data <- sapply(data,\(.x)if (is.factor(.x)) as.numeric(.x) else .x) |> as.data.frame()
  # data <- data |> dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.numeric))
  cor <- Hmisc::rcorr(as.matrix(data))
  r <- cor$r %>% as.table()
  d <- r |>
    as.data.frame() |>
    dplyr::filter(abs(Freq) > threshold, Freq != 1)

  d[1:2] |>
    apply(1, \(.x){
      sort(unname(.x))
    },
    simplify = logical(1)
    ) |>
    duplicated() |>
    (\(.x){
      d[!.x, ]
    })() |>
    setNames(c("var1", "var2", "cor"))
}

sentence_paste <- function(data, and.str = "and") {
  and.str <- gsub(" ", "", and.str)
  if (length(data) < 2) {
    data
  } else if (length(data) == 2) {
    paste(data, collapse = glue::glue(" {and.str} "))
  } else if (length(data) > 2) {
    paste(paste(data[-length(data)], collapse = ", "), data[length(data)], sep = glue::glue(" {and.str} "))
  }
}






########
#### Current file: /Users/au301842/FreesearchR/R//create-column-mod.R
########

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
          label = i18n("New column name:"),
          value = "new_column1",
          width = "100%"
        )
      ),
      column(
        width = 6,
        shinyWidgets::virtualSelectInput(
          inputId = ns("group_by"),
          label = i18n("Group calculation by:"),
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
      label = i18n("Enter an expression to define new column:"),
      value = "",
      width = "100%",
      rows = 6
    ),
    tags$i(
      class = "d-block",
      phosphoricons::ph("info"),
      datamods::i18n("Click on a column name to add it to the expression:")
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
          phosphoricons::ph("gear"), i18n("Create column")
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
        datamods::i18n("Choose a name for the column to be created or modified,"),
        datamods::i18n("then enter an expression before clicking on the button above to validate or on "),
        phosphoricons::ph("trash"), datamods::i18n("to delete it.")
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
            phosphoricons::ph("warning"), datamods::i18n("New column name cannot be empty")
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
                                title = i18n("Create a new column"),
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
                                 title = i18n("Create a new column"),
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
    return(datamods:::alert_error(datamods::i18n("Some operations are not allowed")))
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
    phosphoricons::ph("check"), datamods::i18n("Column added!")
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
        paste(i18n("Unique values:"), data.table::uniqueN(values))
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


########
#### Current file: /Users/au301842/FreesearchR/R//custom_SelectInput.R
########

#' A selectizeInput customized for data frames with column labels
#'
#' @description
#' Copied and modified from the IDEAFilter package
#' Adds the option to select "none" which is handled later
#'
#' @param inputId passed to \code{\link[shiny]{selectizeInput}}
#' @param label passed to \code{\link[shiny]{selectizeInput}}
#' @param data \code{data.frame} object from which fields should be populated
#' @param selected default selection
#' @param ... passed to \code{\link[shiny]{selectizeInput}}
#' @param col_subset a \code{vector} containing the list of allowable columns to select
#' @param placeholder passed to \code{\link[shiny]{selectizeInput}} options
#' @param onInitialize passed to \code{\link[shiny]{selectizeInput}} options
#' @param none_label label for "none" item
#' @param maxItems max number of items
#'
#' @return a \code{\link[shiny]{selectizeInput}} dropdown element
#'
#' @importFrom shiny selectizeInput
#' @export
#'
columnSelectInput <- function(inputId, label, data, selected = "", ...,
                              col_subset = NULL, placeholder = "", onInitialize, none_label="No variable selected",maxItems=NULL) {
  datar <- if (is.reactive(data)) data else reactive(data)
  col_subsetr <- if (is.reactive(col_subset)) col_subset else reactive(col_subset)

  labels <- Map(function(col) {
    json <- sprintf(
      IDEAFilter:::strip_leading_ws('
    {
      "name": "%s",
      "label": "%s",
      "dataclass": "%s",
      "datatype": "%s"
    }'),
      col,
      attr(datar()[[col]], "label") %||% "",
      IDEAFilter:::get_dataFilter_class(datar()[[col]]),
      data_type(datar()[[col]])
    )
  }, col = names(datar()))

  if (!"none" %in% names(datar())){
    labels <- c("none"=list(sprintf('\n    {\n      \"name\": \"none\",\n      \"label\": \"%s\",\n      \"dataclass\": \"\",\n      \"datatype\": \"\"\n    }',none_label)),labels)
    choices <- setNames(names(labels), labels)
    choices <- choices[match(if (length(col_subsetr()) == 0 || isTRUE(col_subsetr() == "")) names(datar()) else col_subsetr(), choices)]
  } else {
    choices <- setNames(names(datar()), labels)
    choices <- choices[match(if (length(col_subsetr()) == 0 || isTRUE(col_subsetr() == "")) choices else col_subsetr(), choices)]
  }

  shiny::selectizeInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    ...,
    options = c(
      list(render = I("{
        // format the way that options are rendered
        option: function(item, escape) {
          item.data = JSON.parse(item.label);
          return '<div style=\"padding: 3px 12px\">' +
                   '<div><strong>' +
                      escape(item.data.name) + ' ' +
                      '</strong>' +
                      (item.data.dataclass != '' ?
                      '<span style=\"opacity: 0.9;\"><code style=\"color: black;\"> ' +
                        item.data.dataclass +
                      '</code></span>' : '' ) + ' ' +
                      (item.data.datatype != '' ?
                      '<span style=\"opacity: 0.9;\"><code style=\"color: black;\"> ' +
                        item.data.datatype +
                      '</code></span>' : '' ) +
                   '</div>' +
                   (item.data.label != '' ? '<div style=\"line-height: 1em;\"><small>' + escape(item.data.label) + '</small></div>' : '') +
                 '</div>';
        },

        // avoid data vomit splashing on screen when an option is selected
        item: function(item, escape) {
        item.data = JSON.parse(item.label);
        return '<div>' +
                 escape(item.data.name) +
               '</div>';
        }
      }")),
      if (!is.null(maxItems)) list(maxItems=maxItems)
    )
  )
}


#' A selectizeInput customized for named vectors
#'
#' @param inputId passed to \code{\link[shiny]{selectizeInput}}
#' @param label passed to \code{\link[shiny]{selectizeInput}}
#' @param choices A named \code{vector} from which fields should be populated
#' @param selected default selection
#' @param ... passed to \code{\link[shiny]{selectizeInput}}
#' @param placeholder passed to \code{\link[shiny]{selectizeInput}} options
#' @param onInitialize passed to \code{\link[shiny]{selectizeInput}} options
#'
#' @returns a \code{\link[shiny]{selectizeInput}} dropdown element
#' @export
#'
#' @examples
#' if (shiny::interactive()) {
#' shinyApp(
#'   ui = fluidPage(
#'     shiny::uiOutput("select"),
#'     tableOutput("data")
#'   ),
#'   server = function(input, output) {
#'     output$select <- shiny::renderUI({
#'       vectorSelectInput(
#'         inputId = "variable", label = "Variable:",
#'         data = c(
#'           "Cylinders" = "cyl",
#'           "Transmission" = "am",
#'           "Gears" = "gear"
#'         )
#'       )
#'     })
#'
#'     output$data <- renderTable(
#'       {
#'         mtcars[, c("mpg", input$variable), drop = FALSE]
#'       },
#'       rownames = TRUE
#'     )
#'   }
#' )
#' }
vectorSelectInput <- function(inputId,
                              label,
                              choices,
                              selected = "",
                              ...,
                              placeholder = "",
                              onInitialize) {
  datar <- if (shiny::is.reactive(choices)) data else shiny::reactive(choices)

  labels <- sprintf(
    IDEAFilter:::strip_leading_ws('
    {
      "name": "%s",
      "label": "%s"
    }'),
    datar(),
    names(datar()) %||% ""
  )

  choices_new <- stats::setNames(datar(), labels)

  shiny::selectizeInput(
    inputId = inputId,
    label = label,
    choices = choices_new,
    selected = selected,
    ...,
    options = c(
      list(render = I("{
        // format the way that options are rendered
        option: function(item, escape) {
          item.data = JSON.parse(item.label);
          return '<div style=\"padding: 3px 12px\">' +
                   '<div><strong>' +
                      escape(item.data.name) + ' ' +
                   '</strong></div>' +
                   (item.data.label != '' ? '<div style=\"line-height: 1em;\"><small>' + escape(item.data.label) + '</small></div>' : '') +
                 '</div>';
        },

        // avoid data vomit splashing on screen when an option is selected
        item: function(item, escape) {
        item.data = JSON.parse(item.label);
        return '<div>' +
                 escape(item.data.name) +
               '</div>';
        }
      }"))
    )
  )
}




########
#### Current file: /Users/au301842/FreesearchR/R//cut-variable-dates.R
########

#' Extended cutting function with fall-back to the native base::cut
#'
#' @param x an object inheriting from class "hms"
#' @param ... passed on
#'
#' @export
#' @name cut_var
cut_var <- function(x, ...) {
  UseMethod("cut_var")
}

#' @export
#' @name cut_var
cut_var.default <- function(x, ...) {
  base::cut(x, ...)
}

#' @name cut_var
#'
#' @return factor
#' @export
#'
#' @examples
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var(2)
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var("min")
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var(breaks = "hour")
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var(breaks = hms::as_hms(c("01:00:00", "03:01:20", "9:20:20")))
#' d_t <- readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA))
#' f <- d_t |> cut_var(2)
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA)) |> cut_var(breaks = lubridate::as_datetime(c(hms::as_hms(levels(f)), hms::as_hms(max(d_t, na.rm = TRUE) + 1))), right = FALSE)
cut_var.hms <- function(x, breaks, ...) {
  ## as_hms keeps returning warnings on tz(); ignored
  suppressWarnings({
    if (hms::is_hms(breaks)) {
      breaks <- lubridate::as_datetime(breaks)
    }
    x <- lubridate::as_datetime(x)
    out <- cut_var.POSIXt(x, breaks = breaks, ...)
    attr(out, which = "brks") <- hms::as_hms(lubridate::as_datetime(attr(out, which = "brks")))
    attr(out, which = "levels") <- as.character(hms::as_hms(lubridate::as_datetime(attr(out, which = "levels"))))
  })
  out
}

#' @name cut_var
#' @param x an object inheriting from class "POSIXt" or "Date"
#'
#' @examples
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(2)
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = "weekday")
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = "month_only")
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks=NULL,format = "%A-%H")
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks=NULL,format = "%W")
cut_var.POSIXt <- function(x, breaks, right = FALSE, include.lowest = TRUE, start.on.monday = TRUE, ...) {
  breaks_o <- breaks
  args <- list(...)
  # browser()
  if (is.numeric(breaks)) {
    breaks <- quantile(
      x,
      probs = seq(0, 1, 1 / breaks),
      right = right,
      include.lowest = include.lowest,
      na.rm = TRUE
    )
  }

  if ("format" %in% names(args)){
    assertthat::assert_that(is.character(args$format))
    out <- forcats::as_factor(format(x,format=args$format))
  } else if (identical(breaks, "weekday")) {
    ## This is
    ds <- as.Date(1:7) |>
      (\(.x){
        sort_by(format(.x,"%A"),as.numeric(format(.x,"%w")))
      })()

    if (start.on.monday) {
      ds <- ds[c(7, 1:6)]
    }
    out <- factor(weekdays(x), levels = ds) |> forcats::fct_drop()
  } else if (identical(breaks, "month_only")) {
    ## Simplest way to create a vector of all months in order
    ## which will also follow the locale of the machine
    ms <- paste0("1970-", 1:12, "-01") |>
      as.Date() |>
      months()

    out <- factor(months(x), levels = ms) |> forcats::fct_drop()
  } else {
    ## Doesn't really work very well for breaks other than the special character cases as right border is excluded
    out <- base::cut.POSIXt(x, breaks = breaks, right = right, ...) |> forcats::fct_drop()
    # browser()
  }
  l <- levels(out)
  if (is.numeric(breaks_o)) {
    l <- breaks
  } else if (is.character(breaks) && length(breaks) == 1 && !(identical(breaks, "weekday") | identical(breaks, "month_only"))) {
    if (include.lowest) {
      if (right) {
        l <- c(l, min(as.character(x)))
      } else {
        l <- c(l, max(as.character(x)))
      }
    }
  } else if (length(l) < length(breaks_o)) {
    l <- breaks_o
  }

  attr(out, which = "brks") <- l
  out
}

#' @name cut_var
#' @param x an object inheriting from class "POSIXct"
cut_var.POSIXct <- cut_var.POSIXt

#' @name cut_var
#' @param x an object inheriting from class "POSIXct"
#'
#' @examples
#' as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(2)
#' as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = "weekday")
#' as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(format = "%W")
cut_var.Date <- function(x, breaks=NULL, start.on.monday = TRUE, ...) {
  args <- list(...)

  if ("format" %in% names(args)){
    assertthat::assert_that(is.character(args$format))
    out <- forcats::as_factor(format(x,format=args$format))
  } else if (identical(breaks, "weekday")) {
    ds <- as.Date(1:7) |>
      (\(.x){
        sort_by(format(.x,"%A"),as.numeric(format(.x,"%w")))
      })()

    if (start.on.monday) {
      ds <- ds[c(7, 1:6)]
    }
    out <- factor(weekdays(x), levels = ds) |> forcats::fct_drop()
  } else if (identical(breaks, "month_only")) {
    ms <- paste0("1970-", 1:12, "-01") |>
      as.Date() |>
      months()

    out <- factor(months(x), levels = ms) |> forcats::fct_drop()
  } else {
    ## Doesn't really work very well for breaks other than the special character cases as right border is excluded
    out <- base::cut.Date(x, breaks = breaks, ...) |> forcats::fct_drop()
    # browser()
  }
  out
}

#' Test class
#'
#' @param data data
#' @param class.vec vector of class names to test
#'
#' @return factor
#' @export
#'
#' @examples
#' \dontrun{
#' vapply(REDCapCAST::redcapcast_data, \(.x){
#'   is_any_class(.x, c("hms", "Date", "POSIXct", "POSIXt"))
#' }, logical(1))
#' }
is_any_class <- function(data, class.vec) {
  any(class(data) %in% class.vec)
}

#' Test is date/datetime/time
#'
#' @param data data
#'
#' @return factor
#' @export
#'
#' @examples
#' vapply(REDCapCAST::redcapcast_data, is_datetime, logical(1))
is_datetime <- function(data) {
  is_any_class(data, class.vec = c("hms", "Date", "POSIXct", "POSIXt"))
}

#' @title Module to Convert Numeric to Factor
#'
#' @description
#' This module contain an interface to cut a numeric into several intervals.
#'
#'
#' @param id Module ID.
#'
#' @return A [shiny::reactive()] function returning the data.
#' @export
#'
#' @importFrom shiny NS fluidRow column numericInput checkboxInput checkboxInput plotOutput uiOutput
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom toastui datagridOutput2
#'
#' @name cut-variable
#'
cut_variable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      column(
        width = 3,
        shinyWidgets::virtualSelectInput(
          inputId = ns("variable"),
          label = datamods:::i18n("Variable to cut:"),
          choices = NULL,
          width = "100%"
        )
      ),
      column(
        width = 3,
        shiny::uiOutput(ns("cut_method"))
      ),
      column(
        width = 3,
        numericInput(
          inputId = ns("n_breaks"),
          label = datamods:::i18n("Number of breaks:"),
          value = 3,
          min = 2,
          max = 12,
          width = "100%"
        )
      ),
      column(
        width = 3,
        checkboxInput(
          inputId = ns("right"),
          label = datamods:::i18n("Close intervals on the right"),
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("include_lowest"),
          label = datamods:::i18n("Include lowest value"),
          value = TRUE
        )
      )
    ),
    conditionalPanel(
      condition = "input.method == 'fixed'",
      ns = ns,
      uiOutput(outputId = ns("slider_fixed"))
    ),
    plotOutput(outputId = ns("plot"), width = "100%", height = "270px"),
    toastui::datagridOutput2(outputId = ns("count")),
    actionButton(
      inputId = ns("create"),
      label = tagList(phosphoricons::ph("scissors"), datamods:::i18n("Create factor variable")),
      class = "btn-outline-primary float-end"
    ),
    tags$div(class = "clearfix")
  )
}

#' @param data_r A [shiny::reactive()] function returning a `data.frame`.
#'
#' @export
#'
#' @importFrom shiny moduleServer observeEvent reactive req bindEvent renderPlot
#' @importFrom shinyWidgets updateVirtualSelect noUiSliderInput
#' @importFrom toastui renderDatagrid2 datagrid grid_colorbar
#' @importFrom rlang %||% call2 set_names expr syms
#' @importFrom classInt classIntervals
#'
#' @rdname cut-variable
cut_variable_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(data = NULL, new_var_name = NULL)

      bindEvent(observe({
        data <- data_r()
        rv$data <- data
        vars_num <- vapply(data, \(.x){
          is.numeric(.x) || is_datetime(.x)
        }, logical(1))
        vars_num <- names(vars_num)[vars_num]
        shinyWidgets::updateVirtualSelect(
          inputId = "variable",
          choices = vars_num,
          selected = if (isTruthy(input$variable)) input$variable else vars_num[1]
        )
      }), data_r(), input$hidden)

      output$slider_fixed <- renderUI({
        data <- req(data_r())
        variable <- req(input$variable)
        req(hasName(data, variable))

        if (is_datetime(data[[variable]])) {
          brks <- cut_var(data[[variable]],
            breaks = input$n_breaks
          )$brks
        } else {
          brks <- classInt::classIntervals(
            var = data[[variable]],
            n = input$n_breaks,
            style = "quantile"
          )$brks
        }

        if (is_datetime(data[[variable]])) {
          lower <- min(data[[variable]], na.rm = TRUE)
        } else {
          lower <- floor(min(data[[variable]], na.rm = TRUE))
        }

        if (is_datetime(data[[variable]])) {
          upper <- max(data[[variable]], na.rm = TRUE)
        } else {
          upper <- ceiling(max(data[[variable]], na.rm = TRUE))
        }


        shinyWidgets::noUiSliderInput(
          inputId = session$ns("fixed_brks"),
          label = datamods:::i18n("Fixed breaks:"),
          min = lower,
          max = upper,
          value = brks,
          color = datamods:::get_primary_color(),
          width = "100%"
        )
      })

      output$cut_method <- renderUI({
        data <- req(data_r())
        variable <- req(input$variable)

        choices <- c(
          # "fixed",
          # "quantile"
        )

        if (any(c("hms","POSIXct") %in% class(data[[variable]]))) {
          choices <- c(choices, "hour")
        } else if (any(c("POSIXt", "Date") %in% class(data[[variable]]))) {
          choices <- c(
            choices,
            "day",
            "weekday",
            "week",
            # "week_only",
            "month",
            "month_only",
            "quarter",
            "year"
          )
        } else {
          choices <- c(
            choices,
            "fixed",
            "quantile",
            # "sd",
            # "equal",
            # "pretty",
            # "kmeans",
            # "hclust",
            # "bclust",
            # "fisher",
            # "jenks",
            "headtails" # ,
            # "maximum",
            # "box"
          )
        }

        choices <- unique(choices)

        shinyWidgets::virtualSelectInput(
          inputId = session$ns("method"),
          label = datamods:::i18n("Method:"),
          choices = choices,
          selected = NULL,
          width = "100%"
        )
      })


      breaks_r <- reactive({
        data <- req(data_r())
        variable <- req(input$variable)
        req(hasName(data, variable))
        req(input$n_breaks, input$method)
        if (input$method == "fixed") {
          req(input$fixed_brks)
          if (any(c("hms", "POSIXct") %in% class(data[[variable]]))) {
            # cut.POSIXct <- cut.POSIXt
            f <- cut_var(data[[variable]], breaks = input$fixed_brks)
            list(var = f, brks = levels(f))
          } else {
            classInt::classIntervals(
              var = as.numeric(data[[variable]]),
              n = input$n_breaks,
              style = "fixed",
              fixedBreaks = input$fixed_brks
            )
          }
        } else if (input$method == "quantile") {
          req(input$fixed_brks)
          if (any(c("hms", "POSIXt") %in% class(data[[variable]]))) {
            # cut.POSIXct <- cut.POSIXt
            f <- cut_var(data[[variable]], breaks = input$n_breaks)
            list(var = f, brks = levels(f))
          } else {
            classInt::classIntervals(
              var = as.numeric(data[[variable]]),
              n = input$n_breaks,
              style = "quantile"
            )
          }
        } else if (input$method %in% c(
          "day",
          "weekday",
          "week",
          "month",
          "month_only",
          "quarter",
          "year"
        )) {
          # To enable datetime cutting
          # cut.POSIXct <- cut.POSIXt
          f <- cut_var(data[[variable]], breaks = input$method)
          list(var = f, brks = levels(f))
        } else if (input$method %in% c("hour")) {
          # To enable datetime cutting
          # cut.POSIXct <- cut.POSIXt
          f <- cut_var(data[[variable]], breaks = "hour")
          list(var = f, brks = levels(f))
        # } else if (input$method %in% c("week_only")) {
        #   # As a proof of concept a single option to use "format" parameter
        #   # https://www.stat.berkeley.edu/~s133/dates.html
        #   f <- cut_var(data[[variable]], format = "%W")
        #   list(var = f, brks = levels(f))
        } else {
          classInt::classIntervals(
            var = as.numeric(data[[variable]]),
            n = input$n_breaks,
            style = input$method
          )
        }
      })

      output$plot <- renderPlot({
        data <- req(data_r())
        variable <- req(input$variable)
        plot_histogram(data, variable, breaks = breaks_r()$brks, color = datamods:::get_primary_color())
        # plot_histogram(data = breaks_r()$var, breaks = breaks_r()$brks, color = datamods:::get_primary_color())
      })


      data_cutted_r <- reactive({
        req(input$method)
        data <- req(data_r())
        variable <- req(input$variable)


        if (input$method %in% c("day", "weekday", "week", "month", "month_only", "quarter", "year", "hour")) {
          breaks <- input$method
        } else {
          breaks <- breaks_r()$brks
        }

        parameters <- list(
          x = data[[variable]],
          breaks = breaks,
          include.lowest = input$include_lowest,
          right = input$right
        )

        new_variable <- tryCatch(
          {
            rlang::exec(cut_var, !!!parameters)
          },
          error = function(err) {
            showNotification(paste0("We encountered the following error creating your report: ", err), type = "err")
          }
        )

        # new_variable <- do.call(
        #   cut,
        #   parameters
        # )


        data <- append_column(data, column = new_variable, name = paste0(variable, "_cut"), index = "right")

        # setNames(paste0(variable, "_cut"))
        #
        #   data <- dplyr::bind_cols(data, new_variable, .name_repair = "unique_quiet")

        # rv$new_var_name <- names(data)[length(data)]
        # browser()

        # browser()
        code <- rlang::call2(
          "append_column",
          !!!list(
            column = rlang::call2("cut_var",
                                  !!!modifyList(parameters, list(x = as.symbol(paste0("data$", variable)))),
                                  .ns = "FreesearchR"),
            name = paste0(variable, "_cut"), index = "right"
          ),
          .ns = "FreesearchR"
        )
        attr(data, "code") <- code

        # attr(data, "code") <- Reduce(
        #   f = function(x, y) expr(!!x %>% !!y),
        #   x = c(attr(data, "code"), code)
        # )
        data
      })

      output$count <- toastui::renderDatagrid2({
        # shiny::req(rv$new_var_name)
        data <- req(data_cutted_r())
        # variable <- req(input$variable)
        count_data <- as.data.frame(
          table(
            breaks = data[[length(data)]],
            useNA = "ifany"
          ),
          responseName = "count"
        )
        gridTheme <- getOption("datagrid.theme")
        if (length(gridTheme) < 1) {
          datamods:::apply_grid_theme()
        }
        on.exit(toastui::reset_grid_theme())
        grid <- toastui::datagrid(
          data = count_data,
          colwidths = "guess",
          theme = "default",
          bodyHeight = "auto"
        )
        grid <- toastui::grid_columns(grid, className = "font-monospace")
        toastui::grid_colorbar(
          grid,
          column = "count",
          label_outside = TRUE,
          label_width = "40px",
          bar_bg = datamods:::get_primary_color(),
          from = c(0, max(count_data$count) + 1)
        )
      })

      data_returned_r <- observeEvent(input$create, {
        rv$data <- data_cutted_r()
      })
      return(reactive(rv$data))
    }
  )
}



#' @inheritParams shiny::modalDialog
#' @export
#'
#' @importFrom shiny showModal modalDialog textInput
#' @importFrom htmltools tagList
#'
#' @rdname cut-variable
modal_cut_variable <- function(id,
                               title = datamods:::i18n("Convert Numeric to Factor"),
                               easyClose = TRUE,
                               size = "l",
                               footer = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    cut_variable_ui(id),
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = datamods:::genId())
    ),
    easyClose = easyClose,
    size = size,
    footer = footer
  ))
}


#' @importFrom graphics abline axis hist par plot.new plot.window
plot_histogram <- function(data, column=NULL, bins = 30, breaks = NULL, color = "#112466") {
  if (is.vector(data)){
    x <- data
  } else {
  x <- data[[column]]

  }
  x <- as.numeric(x)
  op <- par(mar = rep(1.5, 4))
  on.exit(par(op))
  plot.new()
  plot.window(xlim = range(pretty(x)), ylim = range(pretty(hist(x, breaks = bins, plot = FALSE)$counts)))
  abline(v = pretty(x), col = "#D8D8D8")
  abline(h = pretty(hist(x, breaks = bins, plot = FALSE)$counts), col = "#D8D8D8")
  hist(x, breaks = bins, xlim = range(pretty(x)), xaxs = "i", yaxs = "i", col = color, add = TRUE)
  axis(side = 1, at = pretty(x), pos = 0)
  axis(side = 2, at = pretty(hist(x, breaks = bins, plot = FALSE)$counts), pos = min(pretty(x)))
  abline(v = breaks, col = "#FFFFFF", lty = 1, lwd = 1.5)
  abline(v = breaks, col = "#2E2E2E", lty = 2, lwd = 1.5)
}



########
#### Current file: /Users/au301842/FreesearchR/R//data_plots.R
########

# source(here::here("functions.R"))

#' Data correlations evaluation module
#'
#' @param id Module id. (Use 'ns("id")')
#'
#' @name data-plots
#' @returns Shiny ui module
#' @export
#'
data_visuals_ui <- function(id, tab_title = "Plots", ...) {
  ns <- shiny::NS(id)

  list(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        bslib::accordion(
          multiple = FALSE,
          bslib::accordion_panel(
            title = "Creating plot",
            icon = bsicons::bs_icon("graph-up"),
            shiny::uiOutput(outputId = ns("primary")),
            shiny::helpText('Only non-text variables are available for plotting. Go the "Data" to reclass data to plot.'),
            shiny::tags$br(),
            shiny::uiOutput(outputId = ns("type")),
            shiny::uiOutput(outputId = ns("secondary")),
            shiny::uiOutput(outputId = ns("tertiary")),
            shiny::br(),
            shiny::actionButton(
              inputId = ns("act_plot"),
              label = "Plot",
              width = "100%",
              icon = shiny::icon("palette"),
              disabled = FALSE
            ),
            shiny::helpText('Adjust settings, then press "Plot".')
          ),
          bslib::accordion_panel(
            title = "Download",
            icon = bsicons::bs_icon("download"),
            shinyWidgets::noUiSliderInput(
              inputId = ns("height_slide"),
              label = "Plot height (mm)",
              min = 50,
              max = 300,
              value = 100,
              step = 1,
              format = shinyWidgets::wNumbFormat(decimals = 0),
              color = datamods:::get_primary_color(),
              inline = TRUE
            ),
            # shiny::numericInput(
            #   inputId = ns("height_numeric"),
            #   label = "Plot height (mm)",
            #   min = 50,
            #   max = 300,
            #   value = 100
            # ),
            shinyWidgets::noUiSliderInput(
              inputId = ns("width"),
              label = "Plot width (mm)",
              min = 50,
              max = 300,
              value = 100,
              step = 1,
              format = shinyWidgets::wNumbFormat(decimals = 0),
              color = datamods:::get_primary_color()
            ),
            shiny::selectInput(
              inputId = ns("plot_type"),
              label = "File format",
              choices = list(
                "png",
                "tiff",
                "eps",
                "pdf",
                "jpeg",
                "svg"
              )
            ),
            shiny::br(),
            # Button
            shiny::downloadButton(
              outputId = ns("download_plot"),
              label = "Download plot",
              icon = shiny::icon("download")
            )
          )
        ),
        shiny::p("We have collected a few notes on visualising data and details on the options included in FreesearchR:", shiny::tags$a(
          href = "https://agdamsbo.github.io/FreesearchR/articles/visuals.html",
          "View notes in new tab",
          target = "_blank",
          rel = "noopener noreferrer"
        ))
      ),
      shiny::plotOutput(ns("plot"), height = "70vh"),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::htmlOutput(outputId = ns("code_plot"))
    )
  )
  # )
}


#'
#' @param data data
#' @param ... ignored
#'
#' @name data-plots
#' @returns shiny server module
#' @export
data_visuals_server <- function(id,
                                data,
                                ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      rv <- shiny::reactiveValues(
        plot.params = NULL,
        plot = NULL,
        code = NULL
      )

      # ## --- New attempt
      #
      # rv$plot.params <- shiny::reactive({
      #   get_plot_options(input$type) |> purrr::pluck(1)
      # })
      #
      # c(output,
      #            list(shiny::renderUI({
      #   columnSelectInput(
      #     inputId = ns("primary"),
      #     data = data,
      #     placeholder = "Select variable",
      #     label = "Response variable",
      #     multiple = FALSE
      #   )
      # }),
      # shiny::renderUI({
      #   shiny::req(input$primary)
      #   # browser()
      #
      #   if (!input$primary %in% names(data())) {
      #     plot_data <- data()[1]
      #   } else {
      #     plot_data <- data()[input$primary]
      #   }
      #
      #   plots <- possible_plots(
      #     data = plot_data
      #   )
      #
      #   plots_named <- get_plot_options(plots) |>
      #     lapply(\(.x){
      #       stats::setNames(.x$descr, .x$note)
      #     })
      #
      #   vectorSelectInput(
      #     inputId = ns("type"),
      #     selected = NULL,
      #     label = shiny::h4("Plot type"),
      #     choices = Reduce(c, plots_named),
      #     multiple = FALSE
      #   )
      # }),
      # shiny::renderUI({
      #   shiny::req(input$type)
      #
      #   cols <- c(
      #     rv$plot.params()[["secondary.extra"]],
      #     all_but(
      #       colnames(subset_types(
      #         data(),
      #         rv$plot.params()[["secondary.type"]]
      #       )),
      #       input$primary
      #     )
      #   )
      #
      #   columnSelectInput(
      #     inputId = ns("secondary"),
      #     data = data,
      #     selected = cols[1],
      #     placeholder = "Please select",
      #     label = if (isTRUE(rv$plot.params()[["secondary.multi"]])) "Additional variables" else "Secondary variable",
      #     multiple = rv$plot.params()[["secondary.multi"]],
      #     maxItems = rv$plot.params()[["secondary.max"]],
      #     col_subset = cols,
      #     none_label = "No variable"
      #   )
      # }),
      # shiny::renderUI({
      #   shiny::req(input$type)
      #   columnSelectInput(
      #     inputId = ns("tertiary"),
      #     data = data,
      #     placeholder = "Please select",
      #     label = "Grouping variable",
      #     multiple = FALSE,
      #     col_subset = c(
      #       "none",
      #       all_but(
      #         colnames(subset_types(
      #           data(),
      #           rv$plot.params()[["tertiary.type"]]
      #         )),
      #         input$primary,
      #         input$secondary
      #       )
      #     ),
      #     none_label = "No stratification"
      #   )
      # })
      # )|> setNames(c("primary","type","secondary","tertiary")),keep.null = TRUE)


      output$primary <- shiny::renderUI({
        shiny::req(data())
        columnSelectInput(
          inputId = ns("primary"),
          col_subset = names(data())[sapply(data(), data_type) != "text"],
          data = data,
          placeholder = "Select variable",
          label = "Response variable",
          multiple = FALSE
        )
      })

      # shiny::observeEvent(data, {
      #   if (is.null(data()) | NROW(data()) == 0) {
      #     shiny::updateActionButton(inputId = ns("act_plot"), disabled = TRUE)
      #   } else {
      #     shiny::updateActionButton(inputId = ns("act_plot"), disabled = FALSE)
      #   }
      # })


      output$type <- shiny::renderUI({
        shiny::req(input$primary)
        shiny::req(data())
        # browser()

        if (!input$primary %in% names(data())) {
          plot_data <- data()[1]
        } else {
          plot_data <- data()[input$primary]
        }

        plots <- possible_plots(
          data = plot_data
        )

        plots_named <- get_plot_options(plots) |>
          lapply(\(.x){
            stats::setNames(.x$descr, .x$note)
          })

        vectorSelectInput(
          inputId = ns("type"),
          selected = NULL,
          label = shiny::h4("Plot type"),
          choices = Reduce(c, plots_named),
          multiple = FALSE
        )
      })

      rv$plot.params <- shiny::reactive({
        get_plot_options(input$type) |> purrr::pluck(1)
      })

      output$secondary <- shiny::renderUI({
        shiny::req(input$type)

        cols <- c(
          rv$plot.params()[["secondary.extra"]],
          all_but(
            colnames(subset_types(
              data(),
              rv$plot.params()[["secondary.type"]]
            )),
            input$primary
          )
        )

        columnSelectInput(
          inputId = ns("secondary"),
          data = data,
          selected = cols[1],
          placeholder = "Please select",
          label = if (isTRUE(rv$plot.params()[["secondary.multi"]])) "Additional variables" else "Secondary variable",
          multiple = rv$plot.params()[["secondary.multi"]],
          maxItems = rv$plot.params()[["secondary.max"]],
          col_subset = cols,
          none_label = "No variable"
        )
      })

      output$tertiary <- shiny::renderUI({
        shiny::req(input$type)
        columnSelectInput(
          inputId = ns("tertiary"),
          data = data,
          placeholder = "Please select",
          label = "Grouping variable",
          multiple = FALSE,
          col_subset = c(
            "none",
            all_but(
              colnames(subset_types(
                data(),
                rv$plot.params()[["tertiary.type"]]
              )),
              input$primary,
              input$secondary
            )
          ),
          none_label = "No stratification"
        )
      })

      shiny::observeEvent(input$act_plot,
        {
          if (NROW(data()) > 0) {
            tryCatch(
              {
                parameters <- list(
                  type = rv$plot.params()[["fun"]],
                  pri = input$primary,
                  sec = input$secondary,
                  ter = input$tertiary
                )

                shiny::withProgress(message = "Drawing the plot. Hold tight for a moment..", {
                  rv$plot <- rlang::exec(
                    create_plot,
                    !!!append_list(
                      data(),
                      parameters,
                      "data"
                    )
                  )
                })

                rv$code <- glue::glue("FreesearchR::create_plot(df,{list2str(parameters)})")
              },
              # warning = function(warn) {
              #   showNotification(paste0(warn), type = "warning")
              # },
              error = function(err) {
                showNotification(paste0(err), type = "err")
              }
            )
          }
        },
        ignoreInit = TRUE
      )

      output$code_plot <- shiny::renderUI({
        shiny::req(rv$code)
        prismCodeBlock(paste0("#Plotting\n", rv$code))
      })

      shiny::observeEvent(
        list(
          data()
        ),
        {
          shiny::req(data())

          rv$plot <- NULL
        }
      )

      output$plot <- shiny::renderPlot({
        # shiny::req(rv$plot)
        # rv$plot
        if (!is.null(rv$plot)) {
          rv$plot
        } else {
          return(NULL)
        }
      })

      # shiny::observeEvent(input$height_numeric, {
      #   shinyWidgets::updateNoUiSliderInput(session, ns("height_slide"), value = input$height_numeric)
      # }, ignoreInit = TRUE)
      # shiny::observeEvent(input$height_slide, {
      #   shiny::updateNumericInput(session, ns("height_numeric"), value = input$height_slide)
      # }, ignoreInit = TRUE)


      output$download_plot <- shiny::downloadHandler(
        filename = shiny::reactive({
          paste0("plot.", input$plot_type)
        }),
        content = function(file) {
          if (inherits(rv$plot, "patchwork")) {
            plot <- rv$plot
          } else if (inherits(rv$plot, "ggplot")) {
            plot <- rv$plot
          } else {
            plot <- rv$plot[[1]]
          }
          # browser()
          shiny::withProgress(message = "Drawing the plot. Hold on for a moment..", {
            ggplot2::ggsave(
              filename = file,
              plot = plot,
              width = input$width,
              height = input$height_slide,
              dpi = 300,
              units = "mm", scale = 2
            )
          })
        }
      )


      shiny::observe(
        return(rv$plot)
      )
    }
  )
}

#' Select all from vector but
#'
#' @param data vector
#' @param ... exclude
#'
#' @returns vector
#' @export
#'
#' @examples
#' all_but(1:10, c(2, 3), 11, 5)
all_but <- function(data, ...) {
  data[!data %in% c(...)]
}

#' Easily subset by data type function
#'
#' @param data data
#' @param types desired types
#' @param type.fun function to get type. Default is outcome_type
#'
#' @returns vector
#' @export
#'
#' @examples
#' default_parsing(mtcars) |> subset_types("ordinal")
#' default_parsing(mtcars) |> subset_types(c("dichotomous", "categorical"))
#' #' default_parsing(mtcars) |> subset_types("factor",class)
subset_types <- function(data, types, type.fun = data_type) {
  data[sapply(data, type.fun) %in% types]
}


#' Implemented functions
#'
#' @description
#' Library of supported functions. The list name and "descr" element should be
#' unique for each element on list.
#'
#' -   descr: Plot description
#'
#' -   primary.type: Primary variable data type (continuous, dichotomous or ordinal)
#'
#' -   secondary.type: Secondary variable data type (continuous, dichotomous or ordinal)
#'
#' -   secondary.extra: "none" or NULL to have option to choose none.
#'
#' -   tertiary.type: Tertiary variable data type (continuous, dichotomous or ordinal)
#'
#'
#' @returns list
#' @export
#'
#' @examples
#' supported_plots() |> str()
supported_plots <- function() {
  list(
    plot_hbars = list(
      fun = "plot_hbars",
      descr = "Stacked horizontal bars",
      note = "A classical way of visualising the distribution of an ordinal scale like the modified Ranking Scale and known as Grotta bars",
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = "none"
    ),
    plot_violin = list(
      fun = "plot_violin",
      descr = "Violin plot",
      note = "A modern alternative to the classic boxplot to visualise data distribution",
      primary.type = c("datatime", "continuous", "dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      secondary.extra = "none",
      tertiary.type = c("dichotomous", "categorical")
    ),
    # plot_ridge = list(
    #   descr = "Ridge plot",
    #   note = "An alternative option to visualise data distribution",
    #   primary.type = "continuous",
    #   secondary.type = c("dichotomous" ,"categorical"),
    #   tertiary.type = c("dichotomous" ,"categorical"),
    #   secondary.extra = NULL
    # ),
    plot_sankey = list(
      fun = "plot_sankey",
      descr = "Sankey plot",
      note = "A way of visualising change between groups",
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      secondary.extra = NULL,
      tertiary.type = c("dichotomous", "categorical")
    ),
    plot_scatter = list(
      fun = "plot_scatter",
      descr = "Scatter plot",
      note = "A classic way of showing the association between to variables",
      primary.type = c("datatime", "continuous"),
      secondary.type = c("datatime", "continuous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = NULL
    ),
    plot_box = list(
      fun = "plot_box",
      descr = "Box plot",
      note = "A classic way to plot data distribution by groups",
      primary.type = c("datatime", "continuous", "dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = "none"
    ),
    plot_euler = list(
      fun = "plot_euler",
      descr = "Euler diagram",
      note = "Generate area-proportional Euler diagrams to display set relationships",
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = TRUE,
      secondary.max = 4,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = NULL
    )
  )
}

#' Get possible regression models
#'
#' @param data data
#'
#' @returns character vector
#' @export
#'
#' @examples
#' mtcars |>
#'   default_parsing() |>
#'   dplyr::pull("cyl") |>
#'   possible_plots()
#'
#' mtcars |>
#'   default_parsing() |>
#'   dplyr::select("mpg") |>
#'   possible_plots()
possible_plots <- function(data) {
  # browser()
  # data <- if (is.reactive(data)) data() else data
  if (is.data.frame(data)) {
    data <- data[[1]]
  }

  type <- data_type(data)

  if (type == "unknown") {
    out <- type
  } else {
    out <- supported_plots() |>
      lapply(\(.x){
        if (type %in% .x$primary.type) {
          .x$descr
        }
      }) |>
      unlist()
  }
  unname(out)
}

#' Get the function options based on the selected function description
#'
#' @param data vector
#'
#' @returns list
#' @export
#'
#' @examples
#' ls <- mtcars |>
#'   default_parsing() |>
#'   dplyr::pull(mpg) |>
#'   possible_plots() |>
#'   (\(.x){
#'     .x[[1]]
#'   })() |>
#'   get_plot_options()
get_plot_options <- function(data) {
  descrs <- supported_plots() |>
    lapply(\(.x){
      .x$descr
    }) |>
    unlist()
  supported_plots() |>
    (\(.x){
      .x[match(data, descrs)]
    })()
}



#' Wrapper to create plot based on provided type
#'
#' @param data data.frame
#' @param pri primary variable
#' @param sec secondary variable
#' @param ter tertiary variable
#' @param type plot type (derived from possible_plots() and matches custom function)
#' @param ... ignored for now
#'
#' @name data-plots
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' create_plot(mtcars, "plot_violin", "mpg", "cyl") |> attributes()
create_plot <- function(data, type, pri, sec, ter = NULL, ...) {
  if (!is.null(sec)) {
    if (!any(sec %in% names(data))) {
      sec <- NULL
    }
  }

  if (!is.null(ter)) {
    if (!ter %in% names(data)) {
      ter <- NULL
    }
  }

  parameters <- list(
    pri = pri,
    sec = sec,
    ter = ter,
    ...
  )

  out <- do.call(
    type,
    modifyList(parameters, list(data = data))
  )

  code <- rlang::call2(type, !!!parameters, .ns = "FreesearchR")

  attr(out, "code") <- code
  out
}

#' Print label, and if missing print variable name
#'
#' @param data vector or data frame
#' @param var variable name. Optional.
#'
#' @returns character string
#' @export
#'
#' @examples
#' mtcars |> get_label(var = "mpg")
#' mtcars |> get_label()
#' mtcars$mpg |> get_label()
#' gtsummary::trial |> get_label(var = "trt")
#' gtsummary::trial$trt |> get_label()
#' 1:10 |> get_label()
get_label <- function(data, var = NULL) {
  # data <- if (is.reactive(data)) data() else data
  if (!is.null(var) & is.data.frame(data)) {
    data <- data[[var]]
  }
  out <- REDCapCAST::get_attr(data = data, attr = "label")
  if (is.na(out)) {
    if (is.null(var)) {
      out <- deparse(substitute(data))
    } else {
      if (is.symbol(var)) {
        out <- gsub('\"', "", deparse(substitute(var)))
      } else {
        out <- var
      }
    }
  }
  out
}


#' Line breaking at given number of characters for nicely plotting labels
#'
#' @param data string
#' @param lineLength maximum line length
#' @param fixed flag to force split at exactly the value given in lineLength.
#' Default is FALSE, only splitting at spaces.
#'
#' @returns character string
#' @export
#'
#' @examples
#' "Lorem ipsum... you know the routine" |> line_break()
#' paste(sample(letters[1:10], 100, TRUE), collapse = "") |> line_break(force = TRUE)
line_break <- function(data, lineLength = 20, force = FALSE) {
  if (isTRUE(force)) {
    ## This eats some letters when splitting a sentence... ??
    gsub(paste0("(.{1,", lineLength, "})(\\s|[[:alnum:]])"), "\\1\n", data)
  } else {
    paste(strwrap(data, lineLength), collapse = "\n")
  }
  ## https://stackoverflow.com/a/29847221
}


#' Wrapping
#'
#' @param data list of ggplot2 objects
#' @param tag_levels passed to patchwork::plot_annotation if given. Default is NULL
#' @param title panel title
#' @param ... ignored for argument overflow
#'
#' @returns list of ggplot2 objects
#' @export
#'
wrap_plot_list <- function(data,
                           tag_levels = NULL,
                           title = NULL,
                           axis.font.family = NULL,
                           ...) {
  if (ggplot2::is_ggplot(data[[1]])) {
    if (length(data) > 1) {
      out <- data |>
        (\(.x){
          if (rlang::is_named(.x)) {
            purrr::imap(.x, \(.y, .i){
              .y + ggplot2::ggtitle(.i)
            })
          } else {
            .x
          }
        })() |>
        align_axes() |>
        patchwork::wrap_plots(
          guides = "collect",
          axes = "collect",
          axis_titles = "collect"
        )
      if (!is.null(tag_levels)) {
        out <- out + patchwork::plot_annotation(tag_levels = tag_levels)
      }
      if (!is.null(title)) {
        out <- out +
          patchwork::plot_annotation(
            title = title,
            theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 25))
          )
      }
    } else {
      out <- data[[1]]
    }
  } else {
    cli::cli_abort("Can only wrap lists of {.cls ggplot} objects")
  }

  if (inherits(x = out, what = "patchwork")) {
    out &
      ggplot2::theme(axis.text = ggplot2::element_text(family = axis.font.family))
  } else {
    out +
      ggplot2::theme(axis.text = ggplot2::element_text(family = axis.font.family))
  }
}


#' Aligns axes between plots
#'
#' @param ... ggplot2 objects or list of ggplot2 objects
#'
#' @returns list of ggplot2 objects
#' @export
#'
align_axes <- function(...) {
  # https://stackoverflow.com/questions/62818776/get-axis-limits-from-ggplot-object
  # https://github.com/thomasp85/patchwork/blob/main/R/plot_multipage.R#L150
  if (ggplot2::is_ggplot(..1)) {
    ## Assumes list of ggplots
    p <- list(...)
  } else if (is.list(..1)) {
    ## Assumes list with list of ggplots
    p <- ..1
  } else {
    cli::cli_abort("Can only align {.cls ggplot} objects or a list of them")
  }

  yr <- clean_common_axis(p, "y")

  xr <- clean_common_axis(p, "x")

  suppressWarnings({
    p |> purrr::map(~ .x + ggplot2::xlim(xr) + ggplot2::ylim(yr))
  })
}

#' Extract and clean axis ranges
#'
#' @param p plot
#' @param axis axis. x or y.
#'
#' @returns vector
#' @export
#'
clean_common_axis <- function(p, axis) {
  purrr::map(p, ~ ggplot2::layer_scales(.x)[[axis]]$get_limits()) |>
    unlist() |>
    (\(.x){
      if (is.numeric(.x)) {
        range(.x)
      } else {
        as.character(.x)
      }
    })() |>
    unique()
}


########
#### Current file: /Users/au301842/FreesearchR/R//data-import.R
########

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
        datamods::import_globalenv_ui(id = ns("env"), title = NULL)
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
      return_class = "data.frame"
    )

    shiny::observeEvent(data_file$data(), {
      shiny::req(data_file$data())

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


########
#### Current file: /Users/au301842/FreesearchR/R//data-summary.R
########

#' Data summary module
#'
#' @param id Module id. (Use 'ns("id")')
#'
#' @name data-summary
#' @returns Shiny ui module
#' @export
data_summary_ui <- function(id) {
  ns <- NS(id)

  toastui::datagridOutput(outputId = ns("tbl_summary"))
}


#'
#' @param data data
#' @param color.main main color
#' @param color.sec secondary color
#' @param ... arguments passed to create_overview_datagrid
#'
#' @name data-summary
#' @returns shiny server module
#' @export
data_summary_server <- function(id,
                                data,
                                color.main,
                                color.sec,
                                ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      output$tbl_summary <-
        toastui::renderDatagrid(
          {
            shiny::req(data())
            data() |>
            overview_vars() |>
            create_overview_datagrid(...) |>
            add_sparkline(
              column = "vals",
              color.main = color.main,
              color.sec = color.sec
            )
          }
            )

    }
  )
}

#' Add sparkline to datagrid
#'
#' @param grid grid
#' @param column clumn to transform
#'
#' @returns datagrid
#' @export
#'
#' @examples
#' grid <- mtcars |>
#'   default_parsing() |>
#'   overview_vars() |>
#'   toastui::datagrid() |>
#'   add_sparkline()
#' grid
add_sparkline <- function(grid, column = "vals", color.main = "#2a8484", color.sec = "#84EF84") {
  out <- toastui::grid_sparkline(
    grid = grid,
    column = column,
    renderer = function(data) {
      data_cl <- class(data)
      if (all(sapply(data,is.na))){
        type <- "line"
        ds <- data.frame(x = NA, y = NA)
        horizontal <- FALSE
      } else if (identical(data_cl, "factor")) {
        type <- "column"
        s <- summary(data)
        ds <- data.frame(x = names(s), y = s)
        horizontal <- FALSE
      } else if (identical(data_cl, "logical")) {
        type <- "column"
        s <- table(data)
        ds <- data.frame(x = names(s), y = as.vector(s))
        horizontal <- FALSE
      } else if (any(c("numeric", "integer") %in% data_cl)) {
        if (is_consecutive(data)) {
          type <- "line"
          ds <- data.frame(x = NA, y = NA)
          horizontal <- FALSE
        } else {
          type <- "box"
          ds <- data.frame(x = 1, y = data)
          horizontal <- TRUE
        }
      } else if (any(c("Date", "POSIXct", "POSIXt", "hms", "difftime") %in% data_cl)) {
        type <- "line"
        ds <- data.frame(x = seq_along(data), y = data)
        horizontal <- FALSE
      } else {
        type <- "line"
        ds <- data.frame(x = NA, y = NA)
        horizontal <- FALSE
      }
      apexcharter::apex(
        ds,
        apexcharter::aes(x, y),
        type = type,
        auto_update = TRUE
      ) |>
        apexcharter::ax_chart(sparkline = list(enabled = TRUE)) |>
        apexcharter::ax_plotOptions(
          boxPlot = apexcharter::boxplot_opts(color.upper = color.sec, color.lower = color.main),
          bar = apexcharter::bar_opts(horizontal = horizontal)
        ) |>
        apexcharter::ax_colors(
          c(color.main, color.sec)
        )
    }
  )

  toastui::grid_columns(
    grid = out,
    columns = column,
    minWidth = 200
  )
}

#' Checks if elements in vector are equally spaced as indication of ID
#'
#' @param data vector
#'
#' @returns logical
#' @export
#'
#' @examples
#' 1:10 |> is_consecutive()
#' sample(1:100,40) |> is_consecutive()
is_consecutive <- function(data){
  suppressWarnings(length(unique(diff(as.numeric(data))))==1)
}

#' Create a data overview data.frame ready for sparklines
#'
#' @param data data
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' mtcars |> overview_vars()
overview_vars <- function(data) {
  data <- as.data.frame(data)

  dplyr::tibble(
    icon = get_classes(data),
    class = icon,
    name = names(data),
    n_missing = unname(colSums(is.na(data))),
    p_complete = 1 - n_missing / nrow(data),
    n_unique = get_n_unique(data),
    vals = as.list(data)
  )
}

#' Create a data overview datagrid
#'
#' @param data data
#'
#' @returns datagrid
#' @export
#'
#' @examples
#' mtcars |>
#'   overview_vars() |>
#'   create_overview_datagrid()
create_overview_datagrid <- function(data,...) {
  # browser()
  gridTheme <- getOption("datagrid.theme")
  if (length(gridTheme) < 1) {
    datamods:::apply_grid_theme()
  }
  on.exit(toastui::reset_grid_theme())

  col.names <- names(data)

  std_names <- c(
    "Name" = "name",
    "Icon" = "icon",
    "Class" = "class",
    "Type" = "type",
    "Missings" = "n_missing",
    "Complete" = "p_complete",
    "Unique" = "n_unique",
    "Distribution" = "vals"
  )

  headers <- lapply(col.names, \(.x){
    if (.x %in% std_names) {
      names(std_names)[match(.x, std_names)]
    } else {
      .x
    }
  }) |> unlist()

  grid <- toastui::datagrid(
    data = data,
    theme = "default",
    colwidths = "fit",
    ...
  )

  grid <- toastui::grid_columns(
    grid = grid,
    columns = col.names,
    header = headers,
    resizable = TRUE
  )

  grid <- toastui::grid_columns(
    grid = grid,
    columns = "vals",
    width = 120
  )

  grid <- toastui::grid_columns(
    grid = grid,
    columns = "icon",
    header = " ",
    align = "center",sortable = FALSE,
    width = 40
  )

  grid <- add_class_icon(
    grid = grid,
    column = "icon",
    fun = class_icons
  )

  grid <- toastui::grid_format(
    grid = grid,
    "p_complete",
    formatter = toastui::JS("function(obj) {return (obj.value*100).toFixed(0) + '%';}")
  )

  ## This could obviously be extended, which will added even more complexity.

  grid <- toastui::grid_filters(
    grid = grid,
    column = "name",
    # columns = unname(std_names[std_names!="vals"]),
    showApplyBtn = FALSE,
    showClearBtn = TRUE,
    type = "text"
  )


  return(grid)
}

#' Convert class grid column to icon
#'
#' @param grid grid
#' @param column column
#'
#' @returns datagrid
#' @export
#'
#' @examples
#' mtcars |>
#'   overview_vars() |>
#'   toastui::datagrid() |>
#'   add_class_icon()
add_class_icon <- function(grid, column = "class", fun=class_icons) {
  out <- toastui::grid_format(
    grid = grid,
    column = column,
    formatter = function(value) {
      lapply(
        X = value,
        FUN = fun
      )
    }
  )

  toastui::grid_columns(
    grid = out,
    header = NULL,
    columns = column,
    width = 60
  )
}


#' Get data class icons
#'
#' @param x character vector of data classes
#'
#' @returns list
#' @export
#'
#' @examples
#' "numeric" |> class_icons()|> str()
#' mtcars |> sapply(class) |> class_icons() |> str()
class_icons <- function(x) {
  if (length(x)>1){
    lapply(x,class_icons)
  } else {
  if (identical(x, "numeric")) {
    shiny::icon("calculator")
  } else if (identical(x, "factor")) {
    shiny::icon("chart-simple")
  } else if (identical(x, "integer")) {
    shiny::icon("arrow-down-1-9")
  } else if (identical(x, "character")) {
    shiny::icon("arrow-down-a-z")
  } else if (identical(x, "logical")) {
    shiny::icon("toggle-off")
  } else if (any(c("Date", "POSIXt") %in% x)) {
    shiny::icon("calendar-days")
  } else if (any("POSIXct", "hms") %in% x) {
    shiny::icon("clock")
  } else {
    shiny::icon("table")
  }}
}

#' Get data type icons
#'
#' @param x character vector of data classes
#'
#' @returns list
#' @export
#'
#' @examples
#' "ordinal" |> type_icons()
#' default_parsing(mtcars) |> sapply(data_type) |> type_icons()
type_icons <- function(x) {
  if (length(x)>1){
    lapply(x,class_icons)
  } else {
    if (identical(x, "continuous")) {
      shiny::icon("calculator")
    } else if (identical(x, "categorical")) {
      shiny::icon("chart-simple")
    } else if (identical(x, "ordinal")) {
      shiny::icon("arrow-down-1-9")
    } else if (identical(x, "text")) {
      shiny::icon("arrow-down-a-z")
    } else if (identical(x, "dichotomous")) {
      shiny::icon("toggle-off")
    } else if (identical(x,"datetime")) {
      shiny::icon("calendar-days")
    } else if (identical(x,"id")) {
      shiny::icon("id-card")
    } else {
      shiny::icon("table")
    }
    }
}

#' Easily get variable icon based on data type or class
#'
#' @param data variable or data frame
#' @param class.type "type" or "class". Default is "class"
#'
#' @returns svg icon
#' @export
#'
#' @examples
#' mtcars[1] |> get_var_icon("class")
#' default_parsing(mtcars) |> get_var_icon()
get_var_icon <- function(data,class.type=c("class","type")){
  if (is.data.frame(data)){
    lapply(data,get_var_icon)
  } else {

  class.type <- match.arg(class.type)

  switch(class.type,
         type = {
           type_icons(data_type(data))
         },
         class = {
           class(data)[1] |> class_icons()
         }
  )
}

}


########
#### Current file: /Users/au301842/FreesearchR/R//datagrid-infos-mod.R
########


#' Display a table in a window
#'
#' @param data a data object (either a `matrix` or a `data.frame`).
#' @param title Title to be displayed in window.
#' @param show_classes Show variables classes under variables names in table header.
#' @param type Display table in a pop-up with [shinyWidgets::show_alert()],
#'  in modal window with [shiny::showModal()] or in a WinBox window with [shinyWidgets::WinBox()].
#' @param options Arguments passed to [toastui::datagrid()].
#' @param width Width of the window, only used if `type = "popup"` or `type = "winbox"`.
#' @param ... Additional options, such as `wbOptions = wbOptions()` or `wbControls = wbControls()`.
#'
#' @note
#' If you use `type = "winbox"`, you'll need to use `shinyWidgets::html_dependency_winbox()` somewhere in your UI.
#'
#' @return No value.
#' @export
#'
show_data <- function(data,
                      title = NULL,
                      options = NULL,
                      show_classes = TRUE,
                      type = c("popup", "modal", "winbox"),
                      width = "65%",
                      ...) { # nocov start
  type <- match.arg(type)
  data <- as.data.frame(data)
  args <- list(...)
  gridTheme <- getOption("datagrid.theme")
  if (length(gridTheme) < 1) {
    datamods:::apply_grid_theme()
  }
  on.exit(toastui::reset_grid_theme())

  if (is.null(options))
    options <- list()

  options$height <- 500
  options$minBodyHeight <- 400
  options$data <- data
  options$theme <- "default"
  options$colwidths <- "guess"
  options$guess_colwidths_opts <- list(min_width = 90, max_width = 400, mul = 1, add = 10)
  if (isTRUE(show_classes))
    options$summary <- construct_col_summary(data)
  datatable <- rlang::exec(toastui::datagrid, !!!options)
  datatable <- toastui::grid_columns(datatable, className = "font-monospace")
  if (identical(type, "winbox")) {
    stopifnot(
      "You need shinyWidgets >= 0.8.4" = packageVersion("shinyWidgets") >= "0.8.4"
    )
    wb_options <- if (is.null(args$wbOptions)) {
      shinyWidgets::wbOptions(
        height = "600px",
        width = width,
        modal = TRUE
      )
    } else {
      modifyList(
        shinyWidgets::wbOptions(
          height = "600px",
          width = width,
          modal = TRUE
        ),
        args$wbOptions
      )
    }
    wb_controls <- if (is.null(args$wbControls)) {
      shinyWidgets::wbControls()
    } else {
      args$wbControls
    }
    shinyWidgets::WinBox(
      title = title,
      ui = datatable,
      options = wb_options,
      controls = wb_controls,
      padding = "0 5px"
    )
  } else if (identical(type, "popup")) {
    shinyWidgets::show_alert(
      title = NULL,
      text = tags$div(
        if (!is.null(title)) {
          tagList(
            tags$h3(title),
            tags$hr()
          )
        },
        style = "color: #000 !important;",
        datatable
      ),
      closeOnClickOutside = TRUE,
      showCloseButton = TRUE,
      btn_labels = NA,
      html = TRUE,
      width = width
    )
  } else {
    showModal(modalDialog(
      title = tagList(
        datamods:::button_close_modal(),
        title
      ),
      tags$div(
        style = htmltools::css(minHeight = htmltools::validateCssUnit(options$height)),
        toastui::renderDatagrid2(datatable)
      ),
      size = "xl",
      footer = NULL,
      easyClose = TRUE
    ))
  }
} # nocov end



#' @importFrom htmltools tagList tags css
describe_col_char <- function(x, with_summary = TRUE) {
  tags$div(
    style = htmltools::css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = htmltools::css(fontStyle = "italic"),
      get_var_icon(x),
      # phosphoricons::ph("text-aa"),
      "character"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = htmltools::css(margin = "3px 0")),
        tags$div(
          datamods:::i18n("Unique:"), length(unique(x))
        ),
        tags$div(
          datamods:::i18n("Missing:"), sum(is.na(x))
        ),
        tags$div(
          style = htmltools::css(whiteSpace = "normal", wordBreak = "break-all"),
          datamods:::i18n("Most Common:"), gsub(
            pattern = "'",
            replacement = "\u07F4",
            x = names(sort(table(x), decreasing = TRUE))[1]
          )
        ),
        tags$div(
          "\u00A0"
        )
      )
    }
  )
}

fmt_p <- function(val, tot) {
  paste0(round(val / tot * 100, 1), "%")
}

describe_col_factor <- function(x, with_summary = TRUE) {
  count <- sort(table(x, useNA = "always"), decreasing = TRUE)
  total <- sum(count)
  one <- count[!is.na(names(count))][1]
  two <- count[!is.na(names(count))][2]
  missing <- count[is.na(names(count))]
  tags$div(
    style = htmltools::css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = htmltools::css(fontStyle = "italic"),
      get_var_icon(x),
      # phosphoricons::ph("list-bullets"),
      "factor"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = htmltools::css(margin = "3px 0")),
        tags$div(
          names(one), ":", fmt_p(one, total)
        ),
        tags$div(
          names(two), ":", fmt_p(two, total)
        ),
        tags$div(
          "Missing", ":", fmt_p(missing, total)
        ),
        tags$div(
          "\u00A0"
        )
      )
    }
  )
}

describe_col_num <- function(x, with_summary = TRUE) {
  tags$div(
    style = htmltools::css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = htmltools::css(fontStyle = "italic"),
      get_var_icon(x),
      # phosphoricons::ph("hash"),
      "numeric"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = htmltools::css(margin = "3px 0")),
        tags$div(
          datamods:::i18n("Min:"), round(min(x, na.rm = TRUE), 2)
        ),
        tags$div(
          datamods:::i18n("Mean:"), round(mean(x, na.rm = TRUE), 2)
        ),
        tags$div(
          datamods:::i18n("Max:"), round(max(x, na.rm = TRUE), 2)
        ),
        tags$div(
          datamods:::i18n("Missing:"), sum(is.na(x))
        )
      )
    }
  )
}


describe_col_date <- function(x, with_summary = TRUE) {
  tags$div(
    style = htmltools::css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = htmltools::css(fontStyle = "italic"),
      get_var_icon(x),
      # phosphoricons::ph("calendar"),
      "date"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = htmltools::css(margin = "3px 0")),
        tags$div(
          datamods:::i18n("Min:"), min(x, na.rm = TRUE)
        ),
        tags$div(
          datamods:::i18n("Max:"), max(x, na.rm = TRUE)
        ),
        tags$div(
          datamods:::i18n("Missing:"), sum(is.na(x))
        ),
        tags$div(
          "\u00A0"
        )
      )
    }
  )
}

describe_col_datetime <- function(x, with_summary = TRUE) {
  tags$div(
    style = htmltools::css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = htmltools::css(fontStyle = "italic"),
      get_var_icon(x),
      # phosphoricons::ph("clock"),
      "datetime"
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = htmltools::css(margin = "3px 0")),
        tags$div(
          datamods:::i18n("Min:"), min(x, na.rm = TRUE)
        ),
        tags$div(
          datamods:::i18n("Max:"), max(x, na.rm = TRUE)
        ),
        tags$div(
          datamods:::i18n("Missing:"), sum(is.na(x))
        ),
        tags$div(
          "\u00A0"
        )
      )
    }
  )
}


describe_col_other <- function(x, with_summary = TRUE) {
  tags$div(
    style = htmltools::css(padding = "3px 0", fontSize = "x-small"),
    tags$div(
      style = htmltools::css(fontStyle = "italic"),
      get_var_icon(x),
      # phosphoricons::ph("clock"),
      paste(class(x), collapse = ", ")
    ),
    if (with_summary) {
      tagList(
        tags$hr(style = htmltools::css(margin = "3px 0")),
        tags$div(
          datamods:::i18n("Unique:"), length(unique(x))
        ),
        tags$div(
          datamods:::i18n("Missing:"), sum(is.na(x))
        ),
        tags$div(
          "\u00A0"
        ),
        tags$div(
          "\u00A0"
        )
      )
    }
  )
}

construct_col_summary <- function(data) {
  list(
    position = "top",
    height = 90,
    columnContent = lapply(
      X = setNames(names(data), names(data)),
      FUN = function(col) {
        values <- data[[col]]
        content <- if (inherits(values, "character")) {
          describe_col_char(values)
        } else if (inherits(values, "factor")) {
          describe_col_factor(values)
        } else if (inherits(values, c("numeric", "integer"))) {
          describe_col_num(values)
        } else if (inherits(values, c("Date"))) {
          describe_col_date(values)
        } else if (inherits(values, c("POSIXt"))) {
          describe_col_datetime(values)
        } else {
          describe_col_other(values)
        }
        list(
          template = toastui::JS(
            "function(value) {",
            sprintf(
              "return '%s';",
              gsub(replacement = "", pattern = "\n", x = htmltools::doRenderTags(content))
            ),
            "}"
          )
        )
      }
    )
  )
}


########
#### Current file: /Users/au301842/FreesearchR/R//helpers.R
########

#' Wrapper function to get function from character vector referring to function from namespace. Passed to 'do.call()'
#'
#' @description
#' This function follows the idea from this comment: https://stackoverflow.com/questions/38983179/do-call-a-function-in-r-without-loading-the-package
#' @param x function or function name
#'
#' @return function or character vector
#' @export
#'
#' @examples
#' getfun("stats::lm")
getfun <- function(x) {
  if ("character" %in% class(x)) {
    if (length(grep("::", x)) > 0) {
      parts <- strsplit(x, "::")[[1]]
      requireNamespace(parts[1])
      getExportedValue(parts[1], parts[2])
    }
  } else {
    x
  }
}

#' Wrapper to save data in RDS, load into specified qmd and render
#'
#' @param data list to pass to qmd
#' @param ... Passed to `quarto::quarto_render()`
#'
#' @return output file name
#' @export
#'
write_quarto <- function(data, ...) {
  # Exports data to temporary location
  #
  # I assume this is more secure than putting it in the www folder and deleting
  # on session end

  # temp <- base::tempfile(fileext = ".rds")
  # readr::write_rds(data, file = here)

  readr::write_rds(data, file = "www/web_data.rds")

  ## Specifying a output path will make the rendering fail
  ## Ref: https://github.com/quarto-dev/quarto-cli/discussions/4041
  ## Outputs to the same as the .qmd file
  quarto::quarto_render(
    execute_params = list(data.file = "web_data.rds"),
    # execute_params = list(data.file = temp),
    ...
  )
}

write_rmd <- function(data, ..., params.args=NULL) {
  # Exports data to temporary location
  #
  # I assume this is more secure than putting it in the www folder and deleting
  # on session end

  # temp <- base::tempfile(fileext = ".rds")
  # readr::write_rds(data, file = here)

  readr::write_rds(data, file = "www/web_data.rds")

  ## Specifying a output path will make the rendering fail
  ## Ref: https://github.com/quarto-dev/quarto-cli/discussions/4041
  ## Outputs to the same as the .qmd file
  rmarkdown::render(
    params = modifyList(list(data.file = "web_data.rds",version=app_version()),params.args),
    # execute_params = list(data.file = temp),
    ...
  )
}

#' Flexible file import based on extension
#'
#' @param file file name
#' @param consider.na character vector of strings to consider as NAs
#'
#' @return tibble
#' @export
#'
#' @examples
#' read_input("https://raw.githubusercontent.com/agdamsbo/cognitive.index.lookup/main/data/sample.csv")
read_input <- function(file, consider.na = c("NA", '""', "")) {
  ext <- tools::file_ext(file)

  if (ext == "csv") {
    df <- readr::read_csv(file = file, na = consider.na)
  } else if (ext %in% c("xls", "xlsx")) {
    df <- readxl::read_excel(file = file, na.strings = consider.na)
  } else if (ext == "dta") {
    df <- haven::read_dta(file = file)
  } else if (ext == "ods") {
    df <- readODS::read_ods(path = file)
  } else if (ext == "rds") {
    df <- readr::read_rds(file = file)
  } else {
    stop("Input file format has to be on of:
             '.csv', '.xls', '.xlsx', '.dta', '.ods' or '.rds'")
  }

  df
}

#' Convert string of arguments to list of arguments
#'
#' @description
#' Idea from the answer: https://stackoverflow.com/a/62979238
#'
#' @param string string to convert to list to use with do.call
#'
#' @return list
#' @export
#'
#' @examples
#' argsstring2list("A=1:5,b=2:4")
#'
argsstring2list <- function(string) {
  eval(parse(text = paste0("list(", string, ")")))
}


#' Factorize variables in data.frame
#'
#' @param data data.frame
#' @param vars variables to force factorize
#'
#' @return data.frame
#' @export
#'
#' @examples
#' factorize(mtcars, names(mtcars))
factorize <- function(data, vars) {
  if (!is.null(vars)) {
    data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(vars),
          REDCapCAST::as_factor
        )
      )
  } else {
    data
  }
}

dummy_Imports <- function() {
  list(
    MASS::as.fractions(),
    broom::augment(),
    broom.helpers::all_categorical(),
    here::here(),
    cardx::all_of(),
    parameters::ci(),
    DT::addRow(),
    bslib::accordion()
  )
  # https://github.com/hadley/r-pkgs/issues/828
}


#' Title
#'
#' @param data data
#' @param output.format output
#' @param filename filename
#' @param ... passed on
#'
#' @returns data
#' @export
#'
file_export <- function(data, output.format = c("df", "teal", "list"), filename, ...) {
  output.format <- match.arg(output.format)

  filename <- gsub("-", "_", filename)

  if (output.format == "teal") {
    out <- within(
      teal_data(),
      {
        assign(name, value |>
          dplyr::bind_cols(.name_repair = "unique_quiet") |>
          default_parsing())
      },
      value = data,
      name = filename
    )

    datanames(out) <- filename
  } else if (output.format == "df") {
    out <- data |>
      default_parsing()
  } else if (output.format == "list") {
    out <- list(
      data = data,
      name = filename
    )

    out <- c(out, ...)
  }

  out
}


#' Default data parsing
#'
#' @param data data
#'
#' @returns data.frame or tibble
#' @export
#'
#' @examples
#' mtcars |> str()
#' mtcars |>
#'   default_parsing() |>
#'   str()
#' head(starwars, 5) |> str()
#' starwars |>
#'   default_parsing() |>
#'   head(5) |>
#'   str()
default_parsing <- function(data) {
  name_labels <- lapply(data, \(.x) REDCapCAST::get_attr(.x, attr = "label"))
  # browser()
  out <- data |>
    setNames(make.names(names(data), unique = TRUE)) |>
    ## Temporary step to avoid nested list and crashing
    remove_nested_list() |>
    REDCapCAST::parse_data() |>
    REDCapCAST::as_factor() |>
    REDCapCAST::numchar2fct(numeric.threshold = 8, character.throshold = 10) |>
    REDCapCAST::as_logical() |>
    REDCapCAST::fct_drop()

  set_column_label(out, setNames(name_labels, names(out)), overwrite = FALSE)

  # purrr::map2(
  #   out,
  #   name_labels[names(name_labels) %in% names(out)],
  #   \(.x, .l){
  #     if (!(is.na(.l) | .l == "")) {
  #       REDCapCAST::set_attr(.x, .l, attr = "label")
  #     } else {
  #       attr(x = .x, which = "label") <- NULL
  #       .x
  #     }
  #     # REDCapCAST::set_attr(data = .x, label = .l,attr = "label", overwrite = FALSE)
  #   }
  # ) |> dplyr::bind_cols()
}

#' Remove empty/NA attributes
#'
#' @param data data
#'
#' @returns data of same class as input
#' @export
#'
#' @examples
#' ds <- mtcars |>
#'   lapply(\(.x) REDCapCAST::set_attr(.x, label = NA, attr = "label")) |>
#'   dplyr::bind_cols()
#' ds |>
#'   remove_empty_attr() |>
#'   str()
#' mtcars |>
#'   lapply(\(.x) REDCapCAST::set_attr(.x, label = NA, attr = "label")) |>
#'   remove_empty_attr() |>
#'   str()
#'
remove_empty_attr <- function(data) {
  if (is.data.frame(data)) {
    data |>
      lapply(remove_empty_attr) |>
      dplyr::bind_cols()
  } else if (is.list(data)) {
    data |> lapply(remove_empty_attr)
  } else {
    attributes(data)[is.na(attributes(data))] <- NULL
    data
  }
}

#' Removes columns with completenes below cutoff
#'
#' @param data data frame
#' @param cutoff numeric
#'
#' @returns data frame
#' @export
#'
#' @examples
#' data.frame(a = 1:10, b = NA, c = c(2, NA)) |> remove_empty_cols(cutoff = .5)
remove_empty_cols <- function(data, cutoff = .7) {
  filter <- apply(X = data, MARGIN = 2, FUN = \(.x){
    sum(as.numeric(!is.na(.x))) / length(.x)
  }) >= cutoff
  data[filter]
}


#' Append list with named index
#'
#' @param data data to add to list
#' @param list list
#' @param index index name
#'
#' @returns list
#' @export
#'
#' @examples
#' ls_d <- list(test = c(1:20))
#' ls_d <- list()
#' data.frame(letters[1:20], 1:20) |> append_list(ls_d, "letters")
#' letters[1:20] |> append_list(ls_d, "letters")
append_list <- function(data, list, index) {
  ## This will overwrite and not warn
  ## Not very safe, but convenient to append code to list
  if (index %in% names(list)) {
    list[[index]] <- data
    out <- list
  } else {
    out <- setNames(c(list, list(data)), c(names(list), index))
  }
  out
}


#' Get missingsness fraction
#'
#' @param data data
#'
#' @returns numeric vector
#' @export
#'
#' @examples
#' c(NA, 1:10, rep(NA, 3)) |> missing_fraction()
missing_fraction <- function(data) {
  NROW(data[is.na(data)]) / NROW(data)
}



#' Ultra short data dascription
#'
#' @param data
#'
#' @returns character vector
#' @export
#'
#' @examples
#' data.frame(
#'   sample(1:8, 20, TRUE),
#'   sample(c(1:8, NA), 20, TRUE)
#' ) |> data_description()
data_description <- function(data, data_text = "Data") {
  data <- if (shiny::is.reactive(data)) data() else data

  n <- nrow(data)
  n_var <- ncol(data)
  n_complete <- sum(complete.cases(data))
  p_complete <- n_complete / n

  sprintf(
    "%s has %s observations and %s variables, with %s (%s%%) complete cases.",
    data_text,
    n,
    n_var,
    n_complete,
    signif(100 * p_complete, 3)
  )
}


#' Filter function to filter data set by variable type
#'
#' @param data data frame
#' @param type vector of data types (recognised: data_types)
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' default_parsing(mtcars) |>
#'   data_type_filter(type = c("categorical", "continuous")) |>
#'   attributes()
#' default_parsing(mtcars) |>
#'   data_type_filter(type = NULL) |>
#'   attributes()
#' \dontrun{
#' default_parsing(mtcars) |> data_type_filter(type = c("test", "categorical", "continuous"))
#' }
data_type_filter <- function(data, type) {
  ## Please ensure to only provide recognised data types
  assertthat::assert_that(all(type %in% names(data_types())))

  if (!is.null(type)) {
    out <- data[data_type(data) %in% type]
    code <- rlang::call2("data_type_filter", !!!list(type = type), .ns = "FreesearchR")
    attr(out, "code") <- code
  } else {
    out <- data
  }
  out
}

#' Drop-in replacement for the base::sort_by with option to remove NAs
#'
#' @param x x
#' @param y y
#' @param na.rm remove NAs
#' @param ... passed to base_sort_by
#'
#' @returns vector
#' @export
#'
#' @examples
#' sort_by(c("Multivariable", "Univariable"), c("Univariable", "Minimal", "Multivariable"))
sort_by <- function(x, y, na.rm = FALSE, ...) {
  out <- base::sort_by(x, y, ...)
  if (na.rm == TRUE) {
    out[!is.na(out)]
  } else {
    out
  }
}


get_ggplot_label <- function(data, label) {
  assertthat::assert_that(ggplot2::is_ggplot(data))
  data$labels[[label]]
}


#' Return if available
#'
#' @param data vector
#' @param default assigned value for missings
#'
#' @returns vector
#' @export
#'
#' @examples
#' NULL |> if_not_missing("new")
#' c(2, "a", NA) |> if_not_missing()
#' "See" |> if_not_missing()
if_not_missing <- function(data, default = NULL) {
  if (length(data) > 1) {
    Reduce(c, lapply(data, if_not_missing))
  } else if (is.na(data) || is.null(data)) {
    return(default)
  } else {
    return(data)
  }
}


#' Merge list of expressions
#'
#' @param data list
#'
#' @returns expression
#' @export
#'
#' @examples
#' list(
#'   rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
#'   rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
#' ) |> merge_expression()
merge_expression <- function(data) {
  Reduce(
    f = function(x, y) rlang::expr(!!x %>% !!y),
    x = data
  )
}

#' Reduce character vector with the native pipe operator or character string
#'
#' @param data list
#'
#' @returns character string
#' @export
#'
#' @examples
#' list(
#'   "mtcars",
#'   rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
#'   rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
#' ) |>
#'   lapply(expression_string) |>
#'   pipe_string() |>
#'   expression_string("data<-")
pipe_string <- function(data, collapse = "|>\n") {
  if (is.list(data)) {
    Reduce(
      f = function(x, y) glue::glue("{x}{collapse}{y}"),
      x = data
    )
  } else {
    data
  }
}

#' Deparses expression as string, substitutes native pipe and adds assign
#'
#' @param data expression
#'
#' @returns string
#' @export
#'
#' @examples
#' list(
#'   as.symbol(paste0("mtcars$", "mpg")),
#'   rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
#'   rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
#' ) |>
#'   merge_expression() |>
#'   expression_string()
expression_string <- function(data, assign.str = "") {
  exp.str <- if (is.call(data)) deparse(data) else data
  # browser()
  out <- paste0(assign.str, gsub("%>%", "|>\n", paste(gsub('"', "'", paste(exp.str, collapse = "")), collapse = "")))
  gsub(" |`", "", out)
}


#' Very simple function to remove nested lists, like when uploading .rds
#'
#' @param data data
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' dplyr::tibble(a = 1:10, b = rep(list("a"), 10)) |> remove_nested_list()
#' dplyr::tibble(a = 1:10, b = rep(list(c("a", "b")), 10)) |> as.data.frame()
remove_nested_list <- function(data) {
  data[!sapply(data, is.list)]
}




#' (Re)label columns in data.frame
#'
#' @param data data.frame to be labelled
#' @param label named list or vector
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' ls <- list("mpg" = "", "cyl" = "Cylinders", "disp" = "", "hp" = "", "drat" = "", "wt" = "", "qsec" = "", "vs" = "", "am" = "", "gear" = "", "carb" = "")
#' ls2 <- c("mpg" = "", "cyl" = "Cylinders", "disp" = "", "hp" = "Horses", "drat" = "", "wt" = "", "qsec" = "", "vs" = "", "am" = "", "gear" = "", "carb" = "")
#' ls3 <- c("mpg" = "", "cyl" = "", "disp" = "", "hp" = "Horses", "drat" = "", "wt" = "", "qsec" = "", "vs" = "", "am" = "", "gear" = "", "carb" = "")
#' mtcars |>
#'   set_column_label(ls) |>
#'   set_column_label(ls2) |>
#'   set_column_label(ls3)
#' rlang::expr(FreesearchR::set_column_label(label = !!ls3)) |> expression_string()
set_column_label <- function(data, label, overwrite = TRUE) {
  purrr::imap(data, function(.data, .name) {
    ls <- if (is.list(label)) unlist(label) else label
    ls[ls == ""] <- NA
    if (.name %in% names(ls)) {
      out <- REDCapCAST::set_attr(.data, unname(ls[.name]), attr = "label", overwrite = overwrite)
      remove_empty_attr(out)
    } else {
      .data
    }
  }) |> dplyr::bind_cols(.name_repair = "unique_quiet")
}


#' Append a column to a data.frame
#'
#' @param data data
#' @param column new column (vector) or data.frame with 1 column
#' @param name new name (pre-fix)
#' @param index desired location. May be "left", "right" or numeric index.
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' mtcars |>
#'   dplyr::mutate(mpg_cut = mpg) |>
#'   append_column(mtcars$mpg, "mpg_cutter")
append_column <- function(data, column, name, index = "right") {
  assertthat::assert_that(NCOL(column) == 1)
  assertthat::assert_that(length(index) == 1)

  if (index == "right") {
    index <- ncol(data) + 1
  } else if (index == "left") {
    index <- 1
  } else if (is.numeric(index)) {
    if (index > ncol(data)) {
      index <- ncol(data) + 1
    }
  } else {
    index <- ncol(data) + 1
  }

  ## Identifying potential naming conflicts
  nm_conflicts <- names(data)[startsWith(names(data), name)]
  ## Simple attemt to create new unique name
  if (length(nm_conflicts) > 0) {
    name <- glue::glue("{name}_{length(nm_conflicts)+1}")
  }
  ## If the above not achieves a unique name, the generic approach is used
  if (name %in% names(data)) {
    name <- make.names(c(name, names(data)), unique = TRUE)[1]
  }
  new_df <- setNames(data.frame(column), name)

  list(
    data[seq_len(index - 1)],
    new_df,
    if (!index > ncol(data)) data[index:ncol(data)]
  ) |>
    dplyr::bind_cols()
}



#' Test if element is identical to the previous
#'
#' @param data data. vector, data.frame or list
#' @param no.name logical to remove names attribute before testing
#'
#' @returns logical vector
#' @export
#'
#' @examples
#' c(1, 1, 2, 3, 3, 2, 4, 4) |> is_identical_to_previous()
#' mtcars[c(1, 1, 2, 3, 3, 2, 4, 4)] |> is_identical_to_previous()
#' list(1, 1, list(2), "A", "a", "a") |> is_identical_to_previous()
is_identical_to_previous <- function(data, no.name = TRUE) {
  if (is.data.frame(data)) {
    lagged <- data.frame(FALSE, data[seq_len(length(data) - 1)])
  } else {
    lagged <- c(FALSE, data[seq_len(length(data) - 1)])
  }

  vapply(seq_len(length(data)), \(.x){
    if (isTRUE(no.name)) {
      identical(unname(lagged[.x]), unname(data[.x]))
    } else {
      identical(lagged[.x], data[.x])
    }
  }, FUN.VALUE = logical(1))
}


#' Simplified version of the snakecase packages to_snake_case
#'
#' @param data character string vector
#'
#' @returns vector
#' @export
#'
#' @examples
#' c("foo bar", "fooBar21", "!!Foo'B'a-r", "foo_bar", "F  OO bar") |> simple_snake()
simple_snake <- function(data){
  gsub("[\\s+]","_",gsub("[^\\w\\s:-]", "", tolower(data), perl=TRUE), perl=TRUE)
}


########
#### Current file: /Users/au301842/FreesearchR/R//hosted_version.R
########

hosted_version <- function()'v25.7.2-250704'


########
#### Current file: /Users/au301842/FreesearchR/R//html_dependency_freesearchr.R
########

html_dependency_FreesearchR <- function() {
  htmltools::htmlDependency(
    name = "FreesearchR",
    version = packageVersion("FreesearchR"),
    src = list(href = "FreesearchR", file = "assets"),
    package = "FreesearchR",
    stylesheet = "css/FreesearchR.css"
  )
}


########
#### Current file: /Users/au301842/FreesearchR/R//import-file-ext.R
########

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
      placeholder = datamods:::i18n("No file selected; maximum file size is 5 mb"),
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
      tryCatch(
        {
          datamods:::show_data(default_parsing(temporary_rv$data), title = datamods:::i18n("Imported data"), type = show_data_in)
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


########
#### Current file: /Users/au301842/FreesearchR/R//launch_FreesearchR.R
########

#' Easily launch the FreesearchR app
#'
#' @description
#' All data.frames in the global environment will be accessible through the app.
#'
#' @param ... passed on to `shiny::runApp()`
#'
#' @returns shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' launch_FreesearchR(launch.browser = TRUE)
#' }
launch_FreesearchR <- function(...){
  appDir <- system.file("apps", "FreesearchR", package = "FreesearchR")
  if (appDir == "") {
    stop("Could not find the app directory. Try re-installing `FreesearchR`.", call. = FALSE)
  }

  a <- shiny::runApp(appDir = paste0(appDir,"/app.R"), ...)
  return(invisible(a))
}



########
#### Current file: /Users/au301842/FreesearchR/R//missings-module.R
########

#' Data correlations evaluation module
#'
#' @param id Module id
#'
#' @name data-missings
#' @returns Shiny ui module
#' @export
data_missings_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    gt::gt_output(outputId = ns("missings_table"))
  )
}


#'
#' @param data data
#' @param output.format output format
#'
#' @name data-missings
#' @returns shiny server module
#' @export
data_missings_server <- function(id,
                                 data,
                                 variable,
                                 ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # ns <- session$ns

      datar <- if (is.reactive(data)) data else reactive(data)
      variabler <- if (is.reactive(variable)) variable else reactive(variable)

      rv <- shiny::reactiveValues(
        data = NULL,
        table = NULL
      )

      rv$data <- shiny::reactive({
        df_tbl <- datar()
        by_var <- variabler()

        tryCatch(
          {
            out <- compare_missings(df_tbl,by_var)
          },
          error = function(err) {
            showNotification(paste0("Error: ", err), type = "err")
          }
        )

        out
      })

      output$missings_table <- gt::render_gt({
        shiny::req(datar)
        shiny::req(variabler)

        if (is.null(variabler()) || variabler() == "" || !variabler() %in% names(datar())) {
          if (anyNA(datar())){
            title <- "No variable chosen for analysis"
          } else {
          title <- "No missing observations"
          }
        } else {
          title <- glue::glue("Missing vs non-missing observations in the variable **'{variabler()}'**")
        }

        out <- rv$data() |>
          gtsummary::as_gt() |>
          gt::tab_header(title = gt::md(title))

        rv$table <- out

        out
      })

      return(reactive(rv$table))
    }
  )
}


missing_demo_app <- function() {
  ui <- shiny::fluidPage(
    shiny::actionButton(
      inputId = "modal_missings",
      label = "Browse data",
      width = "100%",
      disabled = FALSE
    ),
    shiny::selectInput(
      inputId = "missings_var",
      label = "Select variable to stratify analysis", choices = c("cyl", "vs")
    ),
    data_missings_ui("data")
  )
  server <- function(input, output, session) {
    data_demo <- mtcars
    data_demo[sample(1:32, 10), "cyl"] <- NA
    data_demo[sample(1:32, 8), "vs"] <- NA

    data_missings_server(id = "data", data = data_demo, variable = shiny::reactive(input$missings_var))

    visual_summary_server(id = "visual", data = data_demo)

    observeEvent(input$modal_missings, {
      tryCatch(
        {
          modal_visual_summary(id = "visual")
        },
        error = function(err) {
          showNotification(paste0("We encountered the following error browsing your data: ", err), type = "err")
        }
      )
    })
  }
  shiny::shinyApp(ui, server)
}

missing_demo_app()

#' Pairwise comparison of missings across covariables
#'
#' @param data data frame
#' @param by_var variable to stratify by missingness
#'
#' @returns gtsummary list object
#' @export
#'
compare_missings <- function(data,by_var){
  if (!is.null(by_var) && by_var != "" && by_var %in% names(data)) {
    data[[by_var]] <- ifelse(is.na(data[[by_var]]), "Missing", "Non-missing")

    out <- gtsummary::tbl_summary(data, by = by_var) |>
      gtsummary::add_p()
  } else {
    out <- gtsummary::tbl_summary(data)
  }
  out
}


########
#### Current file: /Users/au301842/FreesearchR/R//plot_box.R
########

#' Beautiful box plot(s)
#'
#' @param data data frame
#' @param pri primary variable
#' @param sec secondary variable
#' @param ter tertiary variable
#' @param ... passed on to wrap_plot_list
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_box(pri = "mpg", sec = "gear")
#' mtcars |> plot_box(pri = "mpg", sec="cyl")
#' mtcars |>
#'   default_parsing() |>
#'   plot_box(pri = "mpg", sec = "cyl", ter = "gear")
#' mtcars |>
#'   default_parsing() |>
#'   plot_box(pri = "mpg", sec = "cyl", ter = "gear",axis.font.family="mono")
plot_box <- function(data, pri, sec, ter = NULL,...) {
  if (!is.null(ter)) {
    ds <- split(data, data[ter])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.ds){
    plot_box_single(
      data = .ds,
      pri = pri,
      sec = sec
    )
  })

  wrap_plot_list(out,title=glue::glue("Grouped by {get_label(data,ter)}"),...)
}




#' Create nice box-plots
#'
#' @name data-plots
#'
#' @returns ggplot object
#' @export
#'
#' @examples
#' mtcars |> plot_box_single("mpg")
#' mtcars |> plot_box_single("mpg","cyl")
#' gtsummary::trial |> plot_box_single("age","trt")
plot_box_single <- function(data, pri, sec=NULL, seed = 2103) {
  set.seed(seed)

  if (is.null(sec)) {
    sec <- "All"
    data[[sec]] <- sec
  }

  discrete <- !data_type(data[[sec]]) %in% "continuous"

  data |>
    ggplot2::ggplot(ggplot2::aes(x = !!dplyr::sym(pri), y = !!dplyr::sym(sec), fill = !!dplyr::sym(sec), group = !!dplyr::sym(sec))) +
    ggplot2::geom_boxplot(linewidth = 1.8, outliers = FALSE) +
    ## THis could be optional in future
    ggplot2::geom_jitter(color = "black", size = 2, alpha = 0.9, width = 0.1, height = .2) +
    ggplot2::xlab(get_label(data,pri))+
    ggplot2::ylab(get_label(data,sec)) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(discrete = discrete, option = "D") +
    # ggplot2::theme_void() +
    ggplot2::theme_bw(base_size = 24) +
    ggplot2::theme(
      legend.position = "none",
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      # axis.text.y = element_blank(),
      # axis.title.y = element_blank(),
      # text = ggplot2::element_text(size = 20),
      # axis.text = ggplot2::element_blank(),
      # plot.title = element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(colour = "black")
    )
}


########
#### Current file: /Users/au301842/FreesearchR/R//plot_euler.R
########

#' Area proportional venn diagrams
#'
#' @description
#' THis is slightly modified from https://gist.github.com/danlooo/d23d8bcf8856c7dd8e86266097404ded
#'
#' This functions uses eulerr::euler to plot area proportional venn diagramms
#' but plots it using ggplot2
#'
#' @param combinations set relationships as a named numeric vector, matrix, or
#' data.frame(See `eulerr::euler`)
#' @param show_quantities whether to show number of intersecting elements
#' @param show_labels whether to show set names
#' @param ... further arguments passed to eulerr::euler
ggeulerr <- function(
    combinations,
    show_quantities = TRUE,
    show_labels = TRUE,
    ...) {
  # browser()
  data <-
    eulerr::euler(combinations = combinations, ...) |>
    plot(quantities = show_quantities) |>
    purrr::pluck("data")


  tibble::as_tibble(data$ellipses, rownames = "Variables") |>
    ggplot2::ggplot() +
    ggforce::geom_ellipse(
      mapping = ggplot2::aes(
        x0 = h, y0 = k, a = a, b = b, angle = 0, fill = Variables
      ),
      alpha = 0.5,
      linewidth = 1.5
    ) +
    ggplot2::geom_text(
      data = {
        data$centers |>
          dplyr::mutate(
            label = labels |> purrr::map2(quantities, ~ {
              if (!is.na(.x) && !is.na(.y) && show_labels) {
                paste0(.x, "\n", sprintf(.y, fmt = "%.2g"))
              } else if (!is.na(.x) && show_labels) {
                .x
              } else if (!is.na(.y)) {
                .y
              } else {
                ""
              }
            })
          )
      },
      mapping = ggplot2::aes(x = x, y = y, label = label),
      size = 8
    ) +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_hue()
}

#' Easily plot euler diagrams
#'
#' @param data data
#' @param x name of main variable
#' @param y name of secondary variables
#' @param z grouping variable
#' @param seed seed
#'
#' @returns patchwork object
#' @export
#'
#' @examples
#' data.frame(
#'   A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
#'   B = sample(c("A", "C"), 50, TRUE),
#'   C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE),
#'   D = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
#' ) |> plot_euler("A", c("B", "C"), "D", seed = 4)
#' mtcars |> plot_euler("vs", "am", seed = 1)
plot_euler <- function(data, pri, sec, ter = NULL, seed = 2103) {
  set.seed(seed = seed)
  if (!is.null(ter)) {
    ds <- split(data, data[ter])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.x){
    .x[c(pri, sec)] |>
      as.data.frame() |>
      na.omit() |>
      plot_euler_single()
  })

  # names(out)
  wrap_plot_list(out,title=glue::glue("Grouped by {get_label(data,ter)}"))
  # patchwork::wrap_plots(out, guides = "collect")
}

#' Easily plot single euler diagrams
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' data.frame(
#'   A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
#'   B = sample(c("A", "C"), 50, TRUE),
#'   C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE),
#'   D = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
#' ) |> plot_euler_single()
#' mtcars[c("vs", "am")] |> plot_euler_single()
plot_euler_single <- function(data) {
  # if (any("categorical" %in% data_type(data))){
  #   shape <- "ellipse"
  # } else {
  #   shape <- "circle"
  # }

  data |>
    ggeulerr(shape = "circle") +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      # axis.text.y = element_blank(),
      # axis.title.y = element_blank(),
      text = ggplot2::element_text(size = 20),
      axis.text = ggplot2::element_blank(),
      # plot.title = element_blank(),
      # panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_blank()
    )
}


########
#### Current file: /Users/au301842/FreesearchR/R//plot_hbar.R
########

#' Nice horizontal stacked bars (Grotta bars)
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_hbars(pri = "carb", sec = "cyl")
#' mtcars |> plot_hbars(pri = "carb", sec = NULL)
plot_hbars <- function(data, pri, sec, ter = NULL) {
  out <- vertical_stacked_bars(data = data, score = pri, group = sec, strata = ter)

  out
}


#' Vertical stacked bar plot wrapper
#'
#' @param data data.frame
#' @param score outcome variable
#' @param group grouping variable
#' @param strata stratifying variable
#' @param t.size text size
#'
#' @return ggplot2 object
#' @export
#'
vertical_stacked_bars <- function(data,
                                  score = "full_score",
                                  group = "pase_0_q",
                                  strata = NULL,
                                  t.size = 10,
                                  l.color = "black",
                                  l.size = .5,
                                  draw.lines = TRUE) {
  if (is.null(group)) {
    df.table <- data[c(score, group, strata)] |>
      dplyr::mutate("All" = 1) |>
      table()
    group <- "All"
    draw.lines <- FALSE
  } else {
    df.table <- data[c(score, group, strata)] |>
      table()
  }

  p <- df.table |>
    rankinPlot::grottaBar(
      scoreName = score,
      groupName = group,
      textColor = c("black", "white"),
      strataName = strata,
      textCut = 6,
      textSize = 20,
      printNumbers = "none",
      lineSize = l.size,
      returnData = TRUE
    )

  colors <- viridisLite::viridis(nrow(df.table))
  contrast_cut <-
    sum(contrast_text(colors, threshold = .3) == "white")

  score_label <- data |> get_label(var = score)
  group_label <- data |> get_label(var = group)

  p |>
    (\(.x){
      .x$plot +
        ggplot2::geom_text(
          data = .x$rectData[which(.x$rectData$n >
                                     0), ],
          size = t.size,
          fontface = "plain",
          ggplot2::aes(
            x = group,
            y = p_prev + 0.49 * p,
            color = as.numeric(score) > contrast_cut,
            # label = paste0(sprintf("%2.0f", 100 * p),"%"),
            label = sprintf("%2.0f", 100 * p)
          )
        ) +
        ggplot2::labs(fill = score_label) +
        ggplot2::scale_fill_manual(values = rev(colors)) +
        ggplot2::theme(
          legend.position = "bottom",
          axis.title = ggplot2::element_text(),
        ) +
        ggplot2::xlab(group_label) +
        ggplot2::ylab(NULL)
      # viridis::scale_fill_viridis(discrete = TRUE, direction = -1, option = "D")
    })()
}


########
#### Current file: /Users/au301842/FreesearchR/R//plot_ridge.R
########

#' Plot nice ridge plot
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |>
#'   default_parsing() |>
#'   plot_ridge(x = "mpg", y = "cyl")
#' mtcars |> plot_ridge(x = "mpg", y = "cyl", z = "gear")
plot_ridge <- function(data, x, y, z = NULL, ...) {
  if (!is.null(z)) {
    ds <- split(data, data[z])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.ds){
    ggplot2::ggplot(.ds, ggplot2::aes(x = !!dplyr::sym(x), y = !!dplyr::sym(y), fill = !!dplyr::sym(y))) +
      ggridges::geom_density_ridges() +
      ggridges::theme_ridges() +
      ggplot2::theme(legend.position = "none") |> rempsyc:::theme_apa()
  })

  patchwork::wrap_plots(out)
}


########
#### Current file: /Users/au301842/FreesearchR/R//plot_sankey.R
########

#' Readying data for sankey plot
#'
#' @name data-plots
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' ds <- data.frame(g = sample(LETTERS[1:2], 100, TRUE), first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)), last = sample(c(letters[1:4], NA), 100, TRUE, prob = c(rep(.23, 4), .08)))
#' ds |> sankey_ready("first", "last")
#' ds |> sankey_ready("first", "last", numbers = "percentage")
#' data.frame(
#'   g = sample(LETTERS[1:2], 100, TRUE),
#'   first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)),
#'   last = sample(c(TRUE, FALSE, FALSE), 100, TRUE)
#' ) |>
#'   sankey_ready("first", "last")
sankey_ready <- function(data, pri, sec, numbers = "count", ...) {
  ## TODO: Ensure ordering x and y

  ## Ensure all are factors
  data[c(pri, sec)] <- data[c(pri, sec)] |>
    dplyr::mutate(dplyr::across(!dplyr::where(is.factor), forcats::as_factor))

  out <- dplyr::count(data, !!dplyr::sym(pri), !!dplyr::sym(sec), .drop = FALSE)

  out <- out |>
    dplyr::group_by(!!dplyr::sym(pri)) |>
    dplyr::mutate(gx.sum = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::group_by(!!dplyr::sym(sec)) |>
    dplyr::mutate(gy.sum = sum(n)) |>
    dplyr::ungroup()

  if (numbers == "count") {
    out <- out |> dplyr::mutate(
      lx = factor(paste0(!!dplyr::sym(pri), "\n(n=", gx.sum, ")")),
      ly = factor(paste0(!!dplyr::sym(sec), "\n(n=", gy.sum, ")"))
    )
  } else if (numbers == "percentage") {
    out <- out |> dplyr::mutate(
      lx = factor(paste0(!!dplyr::sym(pri), "\n(", round((gx.sum / sum(n)) * 100, 1), "%)")),
      ly = factor(paste0(!!dplyr::sym(sec), "\n(", round((gy.sum / sum(n)) * 100, 1), "%)"))
    )
  }

  if (is.factor(data[[pri]])) {
    index <- match(levels(data[[pri]]), str_remove_last(levels(out$lx), "\n"))
    out$lx <- factor(out$lx, levels = levels(out$lx)[index])
  }

  if (is.factor(data[[sec]])) {
    index <- match(levels(data[[sec]]), str_remove_last(levels(out$ly), "\n"))
    out$ly <- factor(out$ly, levels = levels(out$ly)[index])
  }

  out
}

str_remove_last <- function(data, pattern = "\n") {
  strsplit(data, split = pattern) |>
    lapply(\(.x)paste(unlist(.x[[-length(.x)]]), collapse = pattern)) |>
    unlist()
}

#' Beautiful sankey plot with option to split by a tertiary group
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' ds <- data.frame(g = sample(LETTERS[1:2], 100, TRUE), first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)), last = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)))
#' ds |> plot_sankey("first", "last")
#' ds |> plot_sankey("first", "last", color.group = "sec")
#' ds |> plot_sankey("first", "last", ter = "g", color.group = "sec")
#' mtcars |>
#'   default_parsing() |>
#'   plot_sankey("cyl", "gear", "am", color.group = "pri")
#' ## In this case, the last plot as the secondary variable in wrong order
#' ## Dont know why...
#' mtcars |>
#'   default_parsing() |>
#'   plot_sankey("cyl", "gear", "vs", color.group = "pri")
plot_sankey <- function(data, pri, sec, ter = NULL, color.group = "pri", colors = NULL) {
  if (!is.null(ter)) {
    ds <- split(data, data[ter])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.ds){
    plot_sankey_single(.ds, pri = pri, sec = sec, color.group = color.group, colors = colors)
  })

  patchwork::wrap_plots(out)
}

#' Beautiful sankey plot
#'
#' @param color.group set group to colour by. "x" or "y".
#' @param colors optinally specify colors. Give NA color, color for each level
#' in primary group and color for each level in secondary group.
#' @param ... passed to sankey_ready()
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' ds <- data.frame(g = sample(LETTERS[1:2], 100, TRUE), first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)), last = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)))
#' ds |> plot_sankey_single("first", "last")
#' ds |> plot_sankey_single("first", "last", color.group = "sec")
#' data.frame(
#'   g = sample(LETTERS[1:2], 100, TRUE),
#'   first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)),
#'   last = sample(c(TRUE, FALSE, FALSE), 100, TRUE)
#' ) |>
#'   plot_sankey_single("first", "last", color.group = "pri")
#' mtcars |>
#'   default_parsing() |>
#' plot_sankey_single("cyl", "vs", color.group = "pri")
plot_sankey_single <- function(data, pri, sec, color.group = c("pri", "sec"), colors = NULL, ...) {
  color.group <- match.arg(color.group)

  data_orig <- data
  data[c(pri, sec)] <- data[c(pri, sec)] |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), forcats::fct_drop))

  # browser()

  data <- data |> sankey_ready(pri = pri, sec = sec, ...)

  na.color <- "#2986cc"
  box.color <- "#1E4B66"

  if (is.null(colors)) {
    if (color.group == "sec") {
      main.colors <- viridisLite::viridis(n = length(levels(data_orig[[sec]])))
      ## Only keep colors for included levels
      main.colors <- main.colors[match(levels(data[[sec]]), levels(data_orig[[sec]]))]

      secondary.colors <- rep(na.color, length(levels(data[[pri]])))
      label.colors <- Reduce(c, lapply(list(secondary.colors, rev(main.colors)), contrast_text))
    } else {
      main.colors <- viridisLite::viridis(n = length(levels(data_orig[[pri]])))
      ## Only keep colors for included levels
      main.colors <- main.colors[match(levels(data[[pri]]), levels(data_orig[[pri]]))]

      secondary.colors <- rep(na.color, length(levels(data[[sec]])))
      label.colors <- Reduce(c, lapply(list(rev(main.colors), secondary.colors), contrast_text))
    }
    colors <- c(na.color, main.colors, secondary.colors)
  } else {
    label.colors <- contrast_text(colors)
  }

  group_labels <- c(get_label(data, pri), get_label(data, sec)) |>
    sapply(line_break) |>
    unname()

  p <- ggplot2::ggplot(data, ggplot2::aes(y = n, axis1 = lx, axis2 = ly))

  if (color.group == "sec") {
    p <- p +
      ggalluvial::geom_alluvium(
        ggplot2::aes(
          fill = !!dplyr::sym(sec) # ,
          ## Including will print strings when levels are empty
          # color = !!dplyr::sym(sec)
        ),
        width = 1 / 16,
        alpha = .8,
        knot.pos = 0.4,
        curve_type = "sigmoid"
      ) + ggalluvial::geom_stratum(ggplot2::aes(fill = !!dplyr::sym(sec)),
        size = 2,
        width = 1 / 3.4
      )
  } else {
    p <- p +
      ggalluvial::geom_alluvium(
        ggplot2::aes(
          fill = !!dplyr::sym(pri) # ,
          # color = !!dplyr::sym(pri)
        ),
        width = 1 / 16,
        alpha = .8,
        knot.pos = 0.4,
        curve_type = "sigmoid"
      ) + ggalluvial::geom_stratum(ggplot2::aes(fill = !!dplyr::sym(pri)),
        size = 2,
        width = 1 / 3.4
      )
  }

  ## Will fail to use stat="stratum" if library is not loaded.
  library(ggalluvial)
  p +
    ggplot2::geom_text(
      stat = "stratum",
      ggplot2::aes(label = after_stat(stratum)),
      colour = label.colors,
      size = 8,
      lineheight = 1
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:2,
      labels = group_labels
    ) +
    ggplot2::scale_fill_manual(values = colors[-1], na.value = colors[1]) +
    # ggplot2::scale_color_manual(values = main.colors) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      # axis.text.y = element_blank(),
      # axis.title.y = element_blank(),
      axis.text.x = ggplot2::element_text(size = 20),
      # text = element_text(size = 5),
      # plot.title = element_blank(),
      # panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_blank()
    )
}


########
#### Current file: /Users/au301842/FreesearchR/R//plot_scatter.R
########

#' Beautiful violin plot
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_scatter(pri = "mpg", sec = "wt")
plot_scatter <- function(data, pri, sec, ter = NULL) {
  if (is.null(ter)) {
    rempsyc::nice_scatter(
      data = data,
      predictor = sec,
      response = pri,
      xtitle = get_label(data, var = sec),
      ytitle = get_label(data, var = pri)
    )
  } else {
    rempsyc::nice_scatter(
      data = data,
      predictor = sec,
      response = pri,
      group = ter,
      xtitle = get_label(data, var = sec),
      ytitle = get_label(data, var = pri)
    )
  }
}


########
#### Current file: /Users/au301842/FreesearchR/R//plot_violin.R
########

#' Beatiful violin plot
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_violin(pri = "mpg", sec = "cyl", ter = "gear")
plot_violin <- function(data, pri, sec, ter = NULL) {
  if (!is.null(ter)) {
    ds <- split(data, data[ter])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.ds){
    rempsyc::nice_violin(
      data = .ds,
      group = sec,
      response = pri,
      xtitle = get_label(data, var = sec),
      ytitle = get_label(data, var = pri)
    )
  })

  wrap_plot_list(out,title=glue::glue("Grouped by {get_label(data,ter)}"))
  # patchwork::wrap_plots(out,guides = "collect")
}


########
#### Current file: /Users/au301842/FreesearchR/R//plot-download-module.R
########

plot_download_ui <- regression_ui <- function(id, ...) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyWidgets::noUiSliderInput(
      inputId = ns("plot_height"),
      label = "Plot height (mm)",
      min = 50,
      max = 300,
      value = 100,
      step = 1,
      format = shinyWidgets::wNumbFormat(decimals = 0),
      color = datamods:::get_primary_color()
    ),
    shinyWidgets::noUiSliderInput(
      inputId = ns("plot_width"),
      label = "Plot width (mm)",
      min = 50,
      max = 300,
      value = 100,
      step = 1,
      format = shinyWidgets::wNumbFormat(decimals = 0),
      color = datamods:::get_primary_color()
    ),
    shiny::selectInput(
      inputId = ns("plot_type"),
      label = "File format",
      choices = list(
        "png",
        "tiff",
        "eps",
        "pdf",
        "jpeg",
        "svg"
      )
    ),
    shiny::br(),
    # Button
    shiny::downloadButton(
      outputId = ns("download_plot"),
      label = "Download plot",
      icon = shiny::icon("download")
    )
  )
}

plot_download_server <- function(id,
                                 data,
                                 file_name = "reg_plot",
                                 ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # ns <- session$ns



      output$download_plot <- shiny::downloadHandler(
        filename = paste0(file_name, ".", input$plot_type),
        content = function(file) {
          shiny::withProgress(message = "Saving the plot. Hold on for a moment..", {
            ggplot2::ggsave(
              filename = file,
              plot = data,
              width = input$plot_width,
              height = input$plot_height,
              dpi = 300,
              units = "mm", scale = 2
            )
          })
        }
      )
    }
  )
}


########
#### Current file: /Users/au301842/FreesearchR/R//redcap_read_shiny_module.R
########

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


########
#### Current file: /Users/au301842/FreesearchR/R//regression_model.R
########

#' Create a regression model programatically
#'
#' @param data data set
#' @param fun Name of function as character vector or function to use for model creation.
#' @param vars character vector of variables to include
#' @param outcome.str Name of outcome variable. Character vector.
#' @param auto.mode Make assumptions on function dependent on outcome data format. Overwrites other arguments.
#' @param formula.str Formula as string. Passed through 'glue::glue'. If given, 'outcome.str' and 'vars' are ignored. Optional.
#' @param args.list List of arguments passed to 'fun' with 'do.call'.
#' @param ... ignored for now
#'
#' @importFrom stats as.formula
#'
#' @return object of standard class for fun
#' @export
#' @rdname regression_model
#'
#' @examples
#' gtsummary::trial |>
#'   regression_model(outcome.str = "age")
#' gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "age",
#'     auto.mode = FALSE,
#'     fun = "stats::lm",
#'     formula.str = "{outcome.str}~.",
#'     args.list = NULL
#'   )
#' gtsummary::trial |>
#'   default_parsing() |>
#'   regression_model(
#'     outcome.str = "trt",
#'     auto.mode = FALSE,
#'     fun = "stats::glm",
#'     args.list = list(family = binomial(link = "logit"))
#'   )
#' m <- mtcars |>
#'   default_parsing() |>
#'   regression_model(
#'     outcome.str = "mpg",
#'     auto.mode = FALSE,
#'     fun = "stats::lm",
#'     formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
#'     args.list = NULL,
#'     vars = c("mpg", "cyl")
#'   )
#' broom::tidy(m)
regression_model <- function(data,
                             outcome.str = NULL,
                             auto.mode = FALSE,
                             formula.str = NULL,
                             args.list = NULL,
                             fun = NULL,
                             vars = NULL,
                             ...) {
  if (!is.null(formula.str)) {
    if (formula.str == "") {
      formula.str <- NULL
    }
  }

  ## This will handle if outcome is not in data for nicer shiny behavior
  if (isTRUE(!outcome.str %in% names(data))) {
    outcome.str <- names(data)[1]
    print("Outcome variable is not in data, first column is used")
  }

  if (!is.null(formula.str)) {
    formula.glue <- glue::glue(formula.str)
    outcome.str <- NULL
  } else {
    assertthat::assert_that(outcome.str %in% names(data),
      msg = "Outcome variable is not present in the provided dataset"
    )
    formula.glue <- glue::glue("{outcome.str}~{paste(vars,collapse='+')}")
  }

  if (is.null(vars)) {
    vars <- names(data)[!names(data) %in% outcome.str]
  } else if (!is.null(outcome.str)) {
    if (outcome.str %in% vars) {
      vars <- vars[!vars %in% outcome.str]
    }
    data <- data |> dplyr::select(dplyr::all_of(c(vars, outcome.str)))
  }

  # Formatting character variables as factor
  # Improvement should add a missing vector to format as NA
  data <- data |>
    purrr::map(\(.x){
      if (is.character(.x)) {
        suppressWarnings(REDCapCAST::as_factor(.x))
      } else {
        .x
      }
    }) |>
    dplyr::bind_cols(.name_repair = "unique_quiet")

  if (is.null(fun)) auto.mode <- TRUE

  if (isTRUE(auto.mode)) {
    if (is.numeric(data[[outcome.str]])) {
      fun <- "stats::lm"
    } else if (is.factor(data[[outcome.str]])) {
      if (length(levels(data[[outcome.str]])) == 2) {
        fun <- "stats::glm"
        args.list <- list(family = stats::binomial(link = "logit"))
      } else if (length(levels(data[[outcome.str]])) > 2) {
        fun <- "MASS::polr"
        args.list <- list(
          Hess = TRUE,
          method = "logistic"
        )
      } else {
        stop("The provided output variable only has one level")
      }
    } else {
      stop("Output variable should be either numeric or factor for auto.mode")
    }
  }

  assertthat::assert_that("character" %in% class(fun),
    msg = "Please provide the function as a character vector."
  )

  out <- do.call(
    getfun(fun),
    c(
      list(
        data = data,
        formula = as.formula(formula.glue)
      ),
      args.list
    )
  )

  # out <- REDCapCAST::set_attr(out,label = fun,attr = "fun.call")

  # Recreating the call
  # out$call <-  match.call(definition=eval(parse(text=fun)), call(fun, data = 'data',formula = as.formula(formula.str),args.list))

  return(out)
}

#' Create a regression model programatically
#'
#' @param data data set
#' @param fun Name of function as character vector or function to use for model creation.
#' @param vars character vector of variables to include
#' @param outcome.str Name of outcome variable. Character vector.
#' @param args.list List of arguments passed to 'fun' with 'do.call'.
#' @param ... ignored for now
#'
#' @importFrom stats as.formula
#' @rdname regression_model
#'
#' @return object of standard class for fun
#' @export
#'
#' @examples
#' \dontrun{
#' gtsummary::trial |>
#'   regression_model_uv(outcome.str = "age")
#' gtsummary::trial |>
#'   regression_model_uv(
#'     outcome.str = "age",
#'     fun = "stats::lm",
#'     args.list = NULL
#'   )
#' m <- gtsummary::trial |> regression_model_uv(
#'   outcome.str = "trt",
#'   fun = "stats::glm",
#'   args.list = list(family = stats::binomial(link = "logit"))
#' )
#' lapply(m, broom::tidy) |> dplyr::bind_rows()
#' }
regression_model_uv <- function(data,
                                outcome.str,
                                args.list = NULL,
                                fun = NULL,
                                vars = NULL,
                                ...) {
  ## This will handle if outcome is not in data for nicer shiny behavior
  if (!outcome.str %in% names(data)) {
    outcome.str <- names(data)[1]
    print("outcome is not in data, first column is used")
  }

  if (!is.null(vars)) {
    data <- data |>
      dplyr::select(dplyr::all_of(
        unique(c(outcome.str, vars))
      ))
  }

  if (is.null(args.list)) {
    args.list <- list()
  }

  if (is.null(fun)) {
    if (is.numeric(data[[outcome.str]])) {
      fun <- "stats::lm"
    } else if (is.factor(data[[outcome.str]])) {
      if (length(levels(data[[outcome.str]])) == 2) {
        fun <- "stats::glm"
        args.list <- list(family = stats::binomial(link = "logit"))
      } else if (length(levels(data[[outcome.str]])) > 2) {
        fun <- "MASS::polr"
        args.list <- list(
          Hess = TRUE,
          method = "logistic"
        )
      } else {
        stop("The provided output variable only has one level")
      }
    } else {
      stop("Output variable should be either numeric or factor for auto.mode")
    }
  }

  assertthat::assert_that("character" %in% class(fun),
    msg = "Please provide the function as a character vector."
  )

  out <- names(data)[!names(data) %in% outcome.str] |>
    purrr::map(\(.var){
      do.call(
        regression_model,
        c(
          list(
            data = data[match(c(outcome.str, .var), names(data))],
            outcome.str = outcome.str
          ),
          args.list
        )
      )
    })

  return(out)
}


### HELPERS

#' Data type assessment.
#'
#' @description
#' These are more overall than the native typeof. This is used to assess a more
#' meaningful "clinical" data type.
#'
#' @param data vector or data.frame. if data frame, each column is evaluated.
#'
#' @returns outcome type
#' @export
#'
#' @examples
#' mtcars |>
#'   default_parsing() |>
#'   lapply(data_type)
#' mtcars |>
#'   default_parsing() |>
#'   data_type()
#' c(1, 2) |> data_type()
#' 1 |> data_type()
#' c(rep(NA, 10)) |> data_type()
#' sample(1:100, 50) |> data_type()
#' factor(letters[1:20]) |> data_type()
#' as.Date(1:20) |> data_type()
data_type <- function(data) {
  if (is.data.frame(data)) {
    sapply(data, data_type)
  } else {
    cl_d <- class(data)
    l_unique <- length(unique(na.omit(data)))
    if (all(is.na(data))) {
      out <- "empty"
    } else if (l_unique < 2) {
      out <- "monotone"
    } else if (any(c("factor", "logical") %in% cl_d) | l_unique == 2) {
      if (identical("logical", cl_d) | l_unique == 2) {
        out <- "dichotomous"
      } else {
        # if (is.ordered(data)) {
        #   out <- "ordinal"
        # } else {
          out <- "categorical"
        # }
      }
    } else if (identical(cl_d, "character")) {
      out <- "text"
    } else if (any(c("hms", "Date", "POSIXct", "POSIXt") %in% cl_d)) {
      out <- "datetime"
    } else if (l_unique > 2) {
      ## Previously had all thinkable classes
      ## Now just assumes the class has not been defined above
      ## any(c("numeric", "integer", "hms", "Date", "timediff") %in% cl_d) &
      out <- "continuous"
    } else {
      out <- "unknown"
    }

    out
  }
}

#' Recognised data types from data_type
#'
#' @returns vector
#' @export
#'
#' @examples
#' data_types()
data_types <- function() {
  list(
    "empty" = list(descr="Variable of all NAs",classes="Any class"),
    "monotone" = list(descr="Variable with only one unique value",classes="Any class"),
    "dichotomous" = list(descr="Variable with only two unique values",classes="Any class"),
    "categorical"= list(descr="Factor variable",classes="factor (ordered or unordered)"),
    "text"= list(descr="Character variable",classes="character"),
    "datetime"= list(descr="Variable of time, date or datetime values",classes="hms, Date, POSIXct and POSIXt"),
    "continuous"= list(descr="Numeric variable",classes="numeric, integer or double"),
    "unknown"= list(descr="Anything not falling within the previous",classes="Any other class")
    )
}


#' Implemented functions
#'
#' @description
#' Library of supported functions. The list name and "descr" element should be
#' unique for each element on list.
#'
#'
#' @returns list
#' @export
#'
#' @examples
#' supported_functions()
supported_functions <- function() {
  list(
    lm = list(
      descr = "Linear regression model",
      design = "cross-sectional",
      out.type = "continuous",
      fun = "stats::lm",
      args.list = NULL,
      formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
      table.fun = "gtsummary::tbl_regression",
      table.args.list = list(exponentiate = FALSE)
    ),
    glm = list(
      descr = "Logistic regression model",
      design = "cross-sectional",
      out.type = "dichotomous",
      fun = "stats::glm",
      args.list = list(family = "binomial"),
      formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
      table.fun = "gtsummary::tbl_regression",
      table.args.list = list()
    ),
    polr = list(
      descr = "Ordinal logistic regression model",
      design = "cross-sectional",
      out.type = c("categorical"),
      fun = "MASS::polr",
      args.list = list(
        Hess = TRUE,
        method = "logistic"
      ),
      formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
      table.fun = "gtsummary::tbl_regression",
      table.args.list = list()
    )
  )
}


#' Get possible regression models
#'
#' @param data data
#'
#' @returns character vector
#' @export
#'
#' @examples
#' mtcars |>
#'   default_parsing() |>
#'   dplyr::pull("cyl") |>
#'   possible_functions(design = "cross-sectional")
#'
#' mtcars |>
#'   default_parsing() |>
#'   dplyr::select("cyl") |>
#'   possible_functions(design = "cross-sectional")
possible_functions <- function(data, design = c("cross-sectional")) {
  #
  # data <- if (is.reactive(data)) data() else data
  if (is.data.frame(data)) {
    data <- data[[1]]
  }

  design <- match.arg(design)
  type <- data_type(data)

  design_ls <- supported_functions() |>
    lapply(\(.x){
      if (design %in% .x$design) {
        .x
      }
    })

  if (type == "unknown") {
    out <- type
  } else {
    out <- design_ls |>
      lapply(\(.x){
        if (type %in% .x$out.type) {
          .x$descr
        }
      }) |>
      unlist()
  }
  unname(out)
}


#' Get the function options based on the selected function description
#'
#' @param data vector
#'
#' @returns list
#' @export
#'
#' @examples
#' mtcars |>
#'   default_parsing() |>
#'   dplyr::pull(mpg) |>
#'   possible_functions(design = "cross-sectional") |>
#'   (\(.x){
#'     .x[[1]]
#'   })() |>
#'   get_fun_options()
get_fun_options <- function(data) {
  descrs <- supported_functions() |>
    lapply(\(.x){
      .x$descr
    }) |>
    unlist()
  supported_functions() |>
    (\(.x){
      .x[match(data, descrs)]
    })()
}


#' Wrapper to create regression model based on supported models
#'
#' @description
#' Output is a concatenated list of model information and model
#'
#'
#' @param data data
#' @param outcome.str name of outcome variable
#' @param fun.descr Description of chosen function matching description in
#' "supported_functions()"
#' @param fun name of custom function. Default is NULL.
#' @param formula.str custom formula glue string. Default is NULL.
#' @param args.list custom character string to be converted using
#' argsstring2list() or list of arguments. Default is NULL.
#' @param ... ignored
#'
#' @returns list
#' @export
#' @rdname regression_model
#'
#' @examples
#' \dontrun{
#' gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "age",
#'     fun = "stats::lm",
#'     formula.str = "{outcome.str}~.",
#'     args.list = NULL
#'   )
#' ls <- regression_model_list(data = default_parsing(mtcars), outcome.str = "cyl", fun.descr = "Ordinal logistic regression model")
#' summary(ls$model)
#' ls <- regression_model_list(data = default_parsing(mtcars), outcome.str = "mpg", fun.descr = "Linear regression model")
#'
#' ls <- regression_model_list(data = default_parsing(gtsummary::trial), outcome.str = "trt", fun.descr = "Logistic regression model")
#' tbl <- gtsummary::tbl_regression(ls$model, exponentiate = TRUE)
#' m <- gtsummary::trial |>
#'   default_parsing() |>
#'   regression_model(
#'     outcome.str = "trt",
#'     fun = "stats::glm",
#'     formula.str = "{outcome.str}~.",
#'     args.list = list(family = "binomial")
#'   )
#' tbl2 <- gtsummary::tbl_regression(m, exponentiate = TRUE)
#' broom::tidy(ls$model)
#' broom::tidy(m)
#' }
regression_model_list <- function(data,
                                  outcome.str,
                                  fun.descr,
                                  fun = NULL,
                                  formula.str = NULL,
                                  args.list = NULL,
                                  vars = NULL,
                                  ...) {
  options <- get_fun_options(fun.descr) |>
    (\(.x){
      .x[[1]]
    })()

  ## Custom, specific fun, args and formula options

  if (is.null(formula.str)) {
    formula.str.c <- options$formula.str
  } else {
    formula.str.c <- formula.str
  }

  if (is.null(fun)) {
    fun.c <- options$fun
  } else {
    fun.c <- fun
  }

  if (is.null(args.list)) {
    args.list.c <- options$args.list
  } else {
    args.list.c <- args.list
  }

  if (is.character(args.list.c)) args.list.c <- argsstring2list(args.list.c)

  ## Handling vars to print code

  if (is.null(vars)) {
    vars <- names(data)[!names(data) %in% outcome.str]
  } else {
    if (outcome.str %in% vars) {
      vars <- vars[!vars %in% outcome.str]
    }
  }

  parameters <- list(
    data = data,
    fun = fun.c,
    formula.str = glue::glue(formula.str.c),
    args.list = args.list.c
  )

  model <- do.call(
    regression_model,
    parameters
  )

  parameters_code <- Filter(
    length,
    modifyList(parameters, list(
      data = as.symbol("df"),
      formula.str = as.character(glue::glue(formula.str.c)),
      outcome.str = NULL
      # args.list = NULL,
    ))
  )

  ## The easiest solution was to simple paste as a string
  ## The rlang::call2 or rlang::expr functions would probably work as well
  # code <- glue::glue("FreesearchR::regression_model({parameters_print}, args.list=list({list2str(args.list.c)}))", .null = "NULL")
  code <- rlang::call2("regression_model", !!!parameters_code, .ns = "FreesearchR")

  list(
    options = options,
    model = model,
    code = expression_string(code)
  )
}

list2str <- function(data) {
  out <- purrr::imap(data, \(.x, .i){
    if (is.logical(.x)) {
      arg <- .x
    } else {
      arg <- glue::glue("'{.x}'")
    }
    glue::glue("{.i} = {arg}")
  }) |>
    unlist() |>
    paste(collapse = (", "))

  if (out == "") {
    return(NULL)
  } else {
    out
  }
}


#' @returns list
#' @export
#' @rdname regression_model
#'
#' @examples
#' \dontrun{
#' gtsummary::trial |>
#'   regression_model_uv(
#'     outcome.str = "trt",
#'     fun = "stats::glm",
#'     args.list = list(family = stats::binomial(link = "logit"))
#'   ) |>
#'   lapply(broom::tidy) |>
#'   dplyr::bind_rows()
#' ms <- regression_model_uv_list(data = default_parsing(mtcars), outcome.str = "mpg", fun.descr = "Linear regression model")
#' ms$code
#' ls <- regression_model_uv_list(data = default_parsing(mtcars), outcome.str = "am", fun.descr = "Logistic regression model")
#' ls$code
#' lapply(ms$model, broom::tidy) |> dplyr::bind_rows()
#' }
regression_model_uv_list <- function(data,
                                     outcome.str,
                                     fun.descr,
                                     fun = NULL,
                                     formula.str = NULL,
                                     args.list = NULL,
                                     vars = NULL,
                                     ...) {
  options <- get_fun_options(fun.descr) |>
    (\(.x){
      .x[[1]]
    })()

  ## Custom, specific fun, args and formula options

  if (is.null(formula.str)) {
    formula.str.c <- options$formula.str
  } else {
    formula.str.c <- formula.str
  }

  if (is.null(fun)) {
    fun.c <- options$fun
  } else {
    fun.c <- fun
  }

  if (is.null(args.list)) {
    args.list.c <- options$args.list
  } else {
    args.list.c <- args.list
  }

  if (is.character(args.list.c)) args.list.c <- argsstring2list(args.list.c)

  ## Handling vars to print code

  if (is.null(vars)) {
    vars <- names(data)[!names(data) %in% outcome.str]
  } else {
    if (outcome.str %in% vars) {
      vars <- vars[!vars %in% outcome.str]
    }
  }

  # assertthat::assert_that("character" %in% class(fun),
  #   msg = "Please provide the function as a character vector."
  # )

  # model <- do.call(
  #   regression_model,
  #   c(
  #     list(data = data),
  #     list(outcome.str = outcome.str),
  #     list(fun = fun.c),
  #     list(formula.str = formula.str.c),
  #     args.list.c
  #   )
  # )

  model <- vars |>
    lapply(\(.var){
      parameters <-
        list(
          fun = fun.c,
          data = data[c(outcome.str, .var)],
          formula.str = as.character(glue::glue(gsub("vars", ".var", formula.str.c))),
          args.list = args.list.c
        )

      out <- do.call(
        regression_model,
        parameters
      )

      ## This is the very long version
      ## Handles deeply nested glue string
      # code <- glue::glue("FreesearchR::regression_model(data=df,{list2str(modifyList(parameters,list(data=NULL,args.list=list2str(args.list.c))))})")
      code <- rlang::call2("regression_model", !!!modifyList(parameters, list(data = as.symbol("df"), args.list = args.list.c)), .ns = "FreesearchR")
      REDCapCAST::set_attr(out, code, "code")
    })

  code <- model |>
    lapply(\(.x)REDCapCAST::get_attr(.x, "code")) |>
    lapply(expression_string) |>
    pipe_string(collapse = ",\n") |>
    (\(.x){
      paste0("list(\n", .x, ")")
    })()


  list(
    options = options,
    model = model,
    code = code
  )
}


# regression_model(mtcars, fun = "stats::lm", formula.str = "mpg~cyl")


########
#### Current file: /Users/au301842/FreesearchR/R//regression_plot.R
########

#' Regression coef plot from gtsummary. Slightly modified to pass on arguments
#'
#' @param x (`tbl_regression`, `tbl_uvregression`)\cr
#'   A 'tbl_regression' or 'tbl_uvregression' object
#' @param plot_ref (scalar `logical`)\cr
#'   plot reference values
#' @param remove_header_rows (scalar `logical`)\cr
#'   logical indicating whether to remove header rows
#'   for categorical variables. Default is `TRUE`
#' @param remove_reference_rows (scalar `logical`)\cr
#'   logical indicating whether to remove reference rows
#'   for categorical variables. Default is `FALSE`.
#' @param ... arguments passed to `ggstats::ggcoef_plot(...)`
#'
#' @returns ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' mod <- lm(mpg ~ ., default_parsing(mtcars))
#' p <- mod |>
#'   gtsummary::tbl_regression() |>
#'   plot(colour = "variable")
#' }
#'
plot.tbl_regression <- function(x,
                                plot_ref = TRUE,
                                remove_header_rows = TRUE,
                                remove_reference_rows = FALSE,
                                ...) {
  # check_dots_empty()
  gtsummary:::check_pkg_installed("ggstats")
  gtsummary:::check_not_missing(x)
  # gtsummary:::check_scalar_logical(remove_header_rows)
  # gtsummary:::check_scalar_logical(remove_reference_rows)

  df_coefs <- x$table_body

  if (isTRUE(remove_header_rows)) {
    df_coefs <- df_coefs |> dplyr::filter(!header_row %in% TRUE)
  }
  if (isTRUE(remove_reference_rows)) {
    df_coefs <- df_coefs |> dplyr::filter(!reference_row %in% TRUE)
  }

  # Removes redundant label
  df_coefs$label[df_coefs$row_type == "label"] <- ""
  # browser()
  # Add estimate value to reference level
  if (plot_ref == TRUE) {
    df_coefs[df_coefs$var_type %in% c("categorical", "dichotomous") & df_coefs$reference_row & !is.na(df_coefs$reference_row), "estimate"] <- if (x$inputs$exponentiate) 1 else 0
  }

  p <- df_coefs |>
    ggstats::ggcoef_plot(exponentiate = x$inputs$exponentiate, ...)

  if (x$inputs$exponentiate) {
    p <- symmetrical_scale_x_log10(p)
  }
  p
}


#' Wrapper to pivot gtsummary table data to long for plotting
#'
#' @param list a custom regression models list
#' @param model.names names of models to include
#'
#' @returns list
#' @export
#'
merge_long <- function(list, model.names) {
  l_subset <- list$tables[model.names]

  l_merged <- l_subset |> tbl_merge()

  df_body <- l_merged$table_body

  sel_list <- lapply(seq_along(l_subset), \(.i){
    endsWith(names(df_body), paste0("_", .i))
  }) |>
    setNames(names(l_subset))

  common <- !Reduce(`|`, sel_list)

  df_body_long <- sel_list |>
    purrr::imap(\(.l, .i){
      d <- dplyr::bind_cols(
        df_body[common],
        df_body[.l],
        model = .i
      )
      setNames(d, gsub("_[0-9]{,}$", "", names(d)))
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(model = REDCapCAST::as_factor(model))

  l_merged$table_body <- df_body_long

  l_merged$inputs$exponentiate <- !identical(class(list$models$Multivariable$model), "lm")

  l_merged
}


#' Easily round log scale limits for nice plots
#'
#' @param data data
#' @param fun rounding function (floor/ceiling)
#' @param ... ignored
#'
#' @returns numeric vector
#' @export
#'
#' @examples
#' limit_log(-.1, floor)
#' limit_log(.1, ceiling)
#' limit_log(-2.1, ceiling)
#' limit_log(2.1, ceiling)
limit_log <- function(data, fun, ...) {
  fun(10^-floor(data) * 10^data) / 10^-floor(data)
}

#' Create summetric log ticks
#'
#' @param data numeric vector
#'
#' @returns numeric vector
#' @export
#'
#' @examples
#' c(sample(seq(.1, 1, .1), 3), sample(1:10, 3)) |> create_log_tics()
create_log_tics <- function(data) {
  sort(round(unique(c(1 / data, data, 1)), 2))
}

#' Ensure symmetrical plot around 1 on a logarithmic x scale for ratio plots
#'
#' @param plot ggplot2 plot
#' @param breaks breaks used and mirrored
#' @param ... ignored
#'
#' @returns ggplot2 object
#' @export
#'
symmetrical_scale_x_log10 <- function(plot, breaks = c(1, 2, 3, 5, 10), ...) {
  rx <- ggplot2::layer_scales(plot)$x$get_limits()

  x_min <- floor(10 * rx[1]) / 10
  x_max <- ceiling(10 * rx[2]) / 10

  rx_min <- limit_log(rx[1], floor)
  rx_max <- limit_log(rx[2], ceiling)

  max_abs_x <- max(abs(c(x_min, x_max)))

  ticks <- log10(breaks) + (ceiling(max_abs_x) - 1)

  plot + ggplot2::scale_x_log10(limits = c(rx_min, rx_max), breaks = create_log_tics(10^ticks[ticks <= max_abs_x]))
}


########
#### Current file: /Users/au301842/FreesearchR/R//regression_table.R
########

#' Create table of regression model
#'
#' @param x regression model
#' @param args.list list of arguments passed to 'fun'.
#' @param fun function to use for table creation. Default is "gtsummary::tbl_regression".
#' @param ... passed to methods
#'
#' @return object of standard class for fun
#' @export
#' @name regression_table
#'
#' @examples
#' \dontrun{
#' tbl <- gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "stage",
#'     fun = "MASS::polr"
#'   ) |>
#'   regression_table(args.list = list("exponentiate" = TRUE))
#' gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "age",
#'     fun = "stats::lm",
#'     formula.str = "{outcome.str}~.",
#'     args.list = NULL
#'   ) |>
#'   regression_table() |>
#'   plot()
#' gtsummary::trial |>
#'   regression_model(
#'     outcome.str = "trt",
#'     fun = "stats::glm",
#'     args.list = list(family = binomial(link = "logit"))
#'   ) |>
#'   regression_table()
#' gtsummary::trial |>
#'   regression_model_uv(
#'     outcome.str = "trt",
#'     fun = "stats::glm",
#'     args.list = list(family = stats::binomial(link = "logit"))
#'   ) |>
#'   regression_table()
#' gtsummary::trial |>
#'   regression_model_uv(
#'     outcome.str = "stage",
#'     args.list = list(family = stats::binomial(link = "logit"))
#'   ) |>
#'   regression_table()
#' mtcars|>
#'  regression_model(
#'     outcome.str = "mpg",
#'     args.list = NULL)
#'   ) |>
#'   regression_table()
#'
#'
#' list(
#'   "Univariable" = regression_model_uv,
#'   "Multivariable" = regression_model
#' ) |>
#'   lapply(\(.fun){
#'     do.call(
#'       .fun,
#'       c(
#'         list(data = gtsummary::trial),
#'         list(outcome.str = "stage")
#'       )
#'     )
#'   }) |>
#'   purrr::map(regression_table) |>
#'   tbl_merge()
#' }
#' regression_table <- function(x, ...) {
#'   UseMethod("regression_table")
#' }
#'
#' #' @rdname regression_table
#' #' @export
#' regression_table.list <- function(x, ...) {
#'   x |>
#'     purrr::map(\(.m){
#'       regression_table(x = .m, ...) |>
#'         gtsummary::add_n()
#'     }) |>
#'     gtsummary::tbl_stack()
#' }
#'
#' #' @rdname regression_table
#' #' @export
#' regression_table.default <- function(x, ..., args.list = NULL, fun = "gtsummary::tbl_regression") {
#'   # Stripping custom class
#'   class(x) <- class(x)[class(x) != "freesearchr_model"]
#'
#'   if (any(c(length(class(x)) != 1, class(x) != "lm"))) {
#'     if (!"exponentiate" %in% names(args.list)) {
#'       args.list <- c(args.list, list(exponentiate = TRUE))
#'     }
#'   }
#'
#'   out <- do.call(getfun(fun), c(list(x = x), args.list))
#'   out |>
#'     gtsummary::add_glance_source_note() # |>
#'   # gtsummary::bold_p()
#' }
regression_table <- function(x, ...) {
  args <- list(...)

  if ("list" %in% class(x)) {
    x |>
      purrr::map(\(.m){
        regression_table_create(x = .m, args.list = args) |>
          gtsummary::add_n()
      }) |>
      gtsummary::tbl_stack()
  } else {
    regression_table_create(x, args.list = args)
  }
}

#' Create regression summary table
#'
#' @param x (list of) regression model
#' @param ... ignored for now
#' @param args.list args.list for the summary function
#' @param fun table summary function. Default is "gtsummary::tbl_regression"
#' @param theme summary table theme
#'
#' @returns gtsummary list object
#' @export
#'
regression_table_create <- function(x, ..., args.list = NULL, fun = "gtsummary::tbl_regression", theme = c("jama", "lancet", "nejm", "qjecon")) {
  # Stripping custom class
  class(x) <- class(x)[class(x) != "freesearchr_model"]

  theme <- match.arg(theme)

  if (any(c(length(class(x)) != 1, class(x) != "lm"))) {
    if (!"exponentiate" %in% names(args.list)) {
      args.list <- c(args.list, list(exponentiate = TRUE, p.values = TRUE))
    }
  }

  gtsummary::theme_gtsummary_journal(journal = theme)
  if (inherits(x, "polr")) {
    # browser()
    out <- do.call(getfun(fun), c(list(x = x), args.list))
    # out <- do.call(getfun(fun), c(list(x = x, tidy_fun = list(residual_type = "normal")), args.list))
    # out <- do.call(what = getfun(fun),
    #                args = c(
    #                  list(
    #                    x = x,
    #                    tidy_fun = list(
    #                      conf.int = TRUE,
    #                      conf.level = 0.95,
    #                      residual_type = "normal")),
    #                  args.list)
    # )
  } else {
    out <- do.call(getfun(fun), c(list(x = x), args.list))
  }

  out
}


#' A substitue to gtsummary::tbl_merge, that will use list names for the tab
#' spanner names.
#'
#' @param data gtsummary list object
#'
#' @return gt summary list object
#' @export
#'
tbl_merge <- function(data) {
  if (is.null(names(data))) {
    data |> gtsummary::tbl_merge()
  } else {
    data |> gtsummary::tbl_merge(tab_spanner = names(data))
  }
}

# as_kable(tbl) |> write_lines(file=here::here("inst/apps/data_analysis_modules/www/_table1.md"))
# as_kable_extra(tbl)|> write_lines(file=here::here("inst/apps/data_analysis_modules/www/table1.md"))


########
#### Current file: /Users/au301842/FreesearchR/R//regression-module.R
########

### On rewriting this module
###
### This module (and the plotting module) should be rewritten to allow for
### dynamically defining variable-selection for model evaluation.
### The principle of having a library of supported functions is fine, but should
### be expanded.
###
###

# list(
#   lm = list(
#     descr = "Linear regression model",
#     design = "cross-sectional",
#     parameters=list(
#       fun = "stats::lm",
#       args.list = NULL
#     ),
#     variables = list(
#       outcome.str = list(
#         fun = "columnSelectInput",
#         multiple = FALSE,
#         label = "Select the dependent/outcome variable."
#       )
#     ),
#     out.type = "continuous",
#     formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
#     table.fun = "gtsummary::tbl_regression",
#     table.args.list = list(exponentiate = FALSE)
#   ))
#
#   Regarding the regression model, it really should be the design selection,
#   that holds the input selection information, as this is what is deciding
#   the number and type of primary inputs.
#
#   Cross-sectional: outcome
#   MMRM: outcome, random effect (id, time)
#   Survival: time, status, strata(?)
#
#



regression_ui <- function(id, ...) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # title = "",
    bslib::nav_panel(
      title = "Regression table",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          shiny::uiOutput(outputId = ns("data_info"), inline = TRUE),
          bslib::accordion(
            open = "acc_reg",
            multiple = FALSE,
            bslib::accordion_panel(
              value = "acc_reg",
              title = "Regression",
              icon = bsicons::bs_icon("calculator"),
              shiny::uiOutput(outputId = ns("outcome_var")),
              # shiny::selectInput(
              #   inputId = "design",
              #   label = "Study design",
              #   selected = "no",
              #   inline = TRUE,
              #   choices = list(
              #     "Cross-sectional" = "cross-sectional"
              #   )
              # ),
              shiny::uiOutput(outputId = ns("regression_type")),
              shiny::radioButtons(
                inputId = ns("all"),
                label = "Specify covariables",
                inline = TRUE, selected = 2,
                choiceNames = c(
                  "Yes",
                  "No"
                ),
                choiceValues = c(1, 2)
              ),
              shiny::conditionalPanel(
                condition = "input.all==1",
                shiny::uiOutput(outputId = ns("regression_vars")),
                shiny::helpText("If none are selected, all are included."),
                shiny::tags$br(),
                ns = ns
              ),
              bslib::input_task_button(
                id = ns("load"),
                label = "Analyse",
                icon = bsicons::bs_icon("pencil"),
                label_busy = "Working...",
                icon_busy = fontawesome::fa_i("arrows-rotate",
                  class = "fa-spin",
                  "aria-hidden" = "true"
                ),
                type = "secondary",
                auto_reset = TRUE
              ),
              shiny::helpText("Press 'Analyse' to create the regression model and after changing parameters."),
              shiny::tags$br(),
              shiny::radioButtons(
                inputId = ns("add_regression_p"),
                label = "Show p-value",
                inline = TRUE,
                selected = "yes",
                choices = list(
                  "Yes" = "yes",
                  "No" = "no"
                )
              ),
              # shiny::tags$br(),
              # shiny::radioButtons(
              #   inputId = ns("tbl_theme"),
              #   label = "Show p-value",
              #   inline = TRUE,
              #   selected = "jama",
              #   choices = list(
              #     "JAMA" = "jama",
              #     "Lancet" = "lancet",
              #     "NEJM" = "nejm"
              #   )
              # ),
              shiny::tags$br()
            )
          )
        ),
        gt::gt_output(outputId = ns("table2"))
      )
    ),
    bslib::nav_panel(
      title = "Coefficient plot",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::accordion(
            open = "acc_reg",
            multiple = FALSE,
            do.call(
              bslib::accordion_panel,
              c(
                list(
                  value = "acc_plot",
                  title = "Coefficient plot",
                  icon = bsicons::bs_icon("bar-chart-steps"),
                  shiny::tags$br(),
                  shiny::uiOutput(outputId = ns("plot_model"))
                ),
                # plot_download_ui(ns("reg_plot_download"))
                shiny::tagList(
                  shinyWidgets::noUiSliderInput(
                    inputId = ns("plot_height"),
                    label = "Plot height (mm)",
                    min = 50,
                    max = 300,
                    value = 100,
                    step = 1,
                    format = shinyWidgets::wNumbFormat(decimals = 0),
                    color = datamods:::get_primary_color()
                  ),
                  shinyWidgets::noUiSliderInput(
                    inputId = ns("plot_width"),
                    label = "Plot width (mm)",
                    min = 50,
                    max = 300,
                    value = 100,
                    step = 1,
                    format = shinyWidgets::wNumbFormat(decimals = 0),
                    color = datamods:::get_primary_color()
                  ),
                  shiny::selectInput(
                    inputId = ns("plot_type"),
                    label = "File format",
                    choices = list(
                      "png",
                      "tiff",
                      "eps",
                      "pdf",
                      "jpeg",
                      "svg"
                    )
                  ),
                  shiny::br(),
                  # Button
                  shiny::downloadButton(
                    outputId = ns("download_plot"),
                    label = "Download plot",
                    icon = shiny::icon("download")
                  )
                )
              )
            )
          )
        ),
        shiny::plotOutput(outputId = ns("regression_plot"), height = "80vh")
      )
    ),
    bslib::nav_panel(
      title = "Model checks",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          bslib::accordion(
            open = "acc_reg",
            multiple = FALSE,
            bslib::accordion_panel(
              value = "acc_checks",
              title = "Checks",
              icon = bsicons::bs_icon("clipboard-check"),
              shiny::uiOutput(outputId = ns("plot_checks"))
            )
          )
        ),
        shiny::plotOutput(outputId = ns("check"), height = "90vh")
      )
    )
  )
}


regression_server <- function(id,
                              data,
                              ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      rv <- shiny::reactiveValues(
        data = NULL,
        plot = NULL,
        check = NULL,
        list = list()
      )

      data_r <- shiny::reactive({
        if (shiny::is.reactive(data)) {
          data()
        } else {
          data
        }
      })

      output$data_info <- shiny::renderUI({
        shiny::req(regression_vars())
        shiny::req(data_r())
        data_description(data_r()[regression_vars()])
      })

      ##############################################################################
      #########
      #########  Input fields
      #########
      ##############################################################################

      ## Keep these "old" selection options as a simple alternative to the modification pane


      output$regression_vars <- shiny::renderUI({
        columnSelectInput(
          inputId = ns("regression_vars"),
          selected = NULL,
          label = "Covariables to include",
          data = data_r(),
          multiple = TRUE
        )
      })

      output$outcome_var <- shiny::renderUI({
        columnSelectInput(
          inputId = ns("outcome_var"),
          selected = NULL,
          label = "Select outcome variable",
          data = data_r(),
          multiple = FALSE
        )
      })

      output$regression_type <- shiny::renderUI({
        shiny::req(input$outcome_var)
        shiny::selectizeInput(
          inputId = ns("regression_type"),
          label = "Choose regression analysis",
          ## The below ifelse statement handles the case of loading a new dataset
          choices = possible_functions(
            data = dplyr::select(
              data_r(),
              ifelse(input$outcome_var %in% names(data_r()),
                input$outcome_var,
                names(data_r())[1]
              )
            ), design = "cross-sectional"
          ),
          multiple = FALSE
        )
      })

      output$factor_vars <- shiny::renderUI({
        shiny::selectizeInput(
          inputId = ns("factor_vars"),
          selected = colnames(data_r())[sapply(data_r(), is.factor)],
          label = "Covariables to format as categorical",
          choices = colnames(data_r()),
          multiple = TRUE
        )
      })

      ## Collected regression variables
      regression_vars <- shiny::reactive({
        if (is.null(input$regression_vars)) {
          out <- colnames(data_r())
        } else {
          out <- unique(c(input$regression_vars, input$outcome_var))
        }
        return(out)
      })

      output$strat_var <- shiny::renderUI({
        columnSelectInput(
          inputId = ns("strat_var"),
          selected = "none",
          label = "Select variable to stratify baseline",
          data = data_r(),
          col_subset = c(
            "none",
            names(data_r())[unlist(lapply(data_r(), data_type)) %in% c("dichotomous", "categorical", "ordinal")]
          )
        )
      })


      output$plot_model <- shiny::renderUI({
        shiny::req(rv$list$regression$tables)
        shiny::selectInput(
          inputId = ns("plot_model"),
          selected = 1,
          label = "Select models to plot",
          choices = names(rv$list$regression$tables),
          multiple = TRUE
        )
      })

      ##############################################################################
      #########
      #########  Regression models
      #########
      ##############################################################################

      shiny::observeEvent(
        input$load,
        {
          shiny::req(input$outcome_var)

          rv$list$regression$models <- NULL

          tryCatch(
            {
              ## Which models to create should be decided by input
              ## Could also include
              ##   imputed or
              ##   minimally adjusted
              model_lists <- list(
                "Univariable" = "regression_model_uv_list",
                "Multivariable" = "regression_model_list"
              ) |>
                lapply(\(.fun){
                  parameters <- list(
                    data = data_r()[regression_vars()],
                    outcome.str = input$outcome_var,
                    fun.descr = input$regression_type
                  )

                  do.call(
                    .fun,
                    parameters
                  )
                })

              rv$list$regression$params <- get_fun_options(input$regression_type) |>
                (\(.x){
                  .x[[1]]
                })()

              rv$list$regression$models <- model_lists
            },
            error = function(err) {
              showNotification(paste0("Creating regression models failed with the following error: ", err), type = "err")
            }
          )
        }
      )



      shiny::observeEvent(
        list(
          data_r(),
          regression_vars()
        ),
        {
          rv$list$regression$tables <- NULL
        }
      )

      ##############################################################################
      #########
      #########  Regression table
      #########
      ##############################################################################

      ### Creating the regression table
      shiny::observeEvent(
        input$load,
        {
          shiny::req(rv$list$regression$models)
          ## To avoid plotting old models on fail/error
          rv$list$regression$tables <- NULL

          # browser()
          tryCatch(
            {
              parameters <- list(
                p.values = input$add_regression_p == "no"
              )

              out <- lapply(rv$list$regression$models, \(.x){
                .x$model
              }) |>
                purrr::map(\(.x){
                  do.call(
                    regression_table,
                    append_list(.x, parameters, "x")
                  )
                })

              rv$list$regression$models |>
                purrr::imap(\(.x, .i){
                  rv$list$regression$models[[.i]][["code_table"]] <- paste(
                    .x$code,
                    expression_string(rlang::call2(.fn = "regression_table", !!!parameters, .ns = "FreesearchR"), assign.str = NULL),
                    sep = "|>\n"
                  )
                })

              rv$list$regression$tables <- out
              rv$list$input <- input
            },
            warning = function(warn) {
              showNotification(paste0(warn), type = "warning")
            },
            error = function(err) {
              showNotification(paste0("Creating a regression table failed with the following error: ", err), type = "err")
            }
          )
        }
      )

      ## Consider creating merged table with theming and then passing object
      ## to render.

      output$table2 <- gt::render_gt({
        ## Print checks if a regression table is present
        if (!is.null(rv$list$regression$tables)) {
          # gtsummary::theme_gtsummary_journal(journal = "jama")
          merged <- rv$list$regression$tables |>
            tbl_merge()

          if (input$add_regression_p == "no") {
            merged <- merged |>
              gtsummary::modify_column_hide(column = dplyr::starts_with("p.value"))
          }

          out <- merged |>
            gtsummary::as_gt() |>
            gt::tab_header(gt::md(glue::glue("**Table 2: {rv$list$regression$params$descr}**")))

          # rv$list$regression$table_merged <- out

          out
        } else {
          return(NULL)
        }
      })

      ##############################################################################
      #########
      #########  Coefficients plot
      #########
      ##############################################################################

      shiny::observeEvent(list(
        input$plot_model,
        rv$list$regression
      ), {
        shiny::req(input$plot_model)

        tryCatch(
          {
            p <- merge_long(
              rv$list$regression,
              sort_by(
                input$plot_model,
                c("Univariable", "Minimal", "Multivariable"),
                na.rm = TRUE
              )
            ) |>
              (\(.x){
                if (length(input$plot_model) > 1) {
                  plot.tbl_regression(
                    x = .x,
                    colour = "model",
                    dodged = TRUE
                  ) +
                    ggplot2::theme(legend.position = "bottom") +
                    ggplot2::guides(color = ggplot2::guide_legend(reverse = TRUE))
                } else {
                  plot.tbl_regression(
                    x = .x,
                    colour = "variable"
                  ) +
                    ggplot2::theme(legend.position = "none")
                }
              })()

            rv$plot <- p +
              ggplot2::scale_y_discrete(labels = scales::label_wrap(15)) +
              gg_theme_shiny()
          },
          error = function(err) {
            showNotification(paste0(err), type = "err")
          }
        )
      })


      output$regression_plot <- shiny::renderPlot(
        {
          shiny::req(input$plot_model)

          rv$plot
        },
        alt = "Regression coefficient plot"
      )

      # plot_download_server(
      #   id = ns("reg_plot_download"),
      #   data = shiny::reactive(rv$plot)
      # )

      output$download_plot <- shiny::downloadHandler(
        filename = paste0("regression_plot.", input$plot_type),
        content = function(file) {
          shiny::withProgress(message = "Saving the plot. Hold on for a moment..", {
            ggplot2::ggsave(
              filename = file,
              plot = rv$plot,
              width = input$plot_width,
              height = input$plot_height,
              dpi = 300,
              units = "mm", scale = 2
            )
          })
        }
      )

      ##############################################################################
      #########
      #########  Model checks
      #########
      ##############################################################################

      shiny::observeEvent(
        list(
          rv$list$regression$models
        ),
        {
          shiny::req(rv$list$regression$models)
          tryCatch(
            {
              rv$check <- lapply(rv$list$regression$models, \(.x){
                .x$model
              }) |>
                purrr::pluck("Multivariable") |>
                performance::check_model()
            },
            # warning = function(warn) {
            #   showNotification(paste0(warn), type = "warning")
            # },
            error = function(err) {
              showNotification(paste0("Running model assumptions checks failed with the following error: ", err), type = "err")
            }
          )
        }
      )

      rv$check_plot <- shiny::reactive(plot(rv$check))

      output$plot_checks <- shiny::renderUI({
        shiny::req(rv$list$regression$models)
        shiny::req(rv$check_plot)

        ## Implement correct plotting
        names <- sapply(rv$check_plot(), \(.i){
          # .i$labels$title
          get_ggplot_label(.i, "title")
        })

        vectorSelectInput(
          inputId = ns("plot_checks"),
          selected = 1,
          label = "Select checks to plot",
          choices = names,
          multiple = TRUE
        )
      })

      output$check <- shiny::renderPlot(
        {
          shiny::req(rv$check_plot)
          shiny::req(input$plot_checks)

          ## Print checks if a regression table is present
          if (!is.null(rv$list$regression$tables)) {
            p <- rv$check_plot() +
              # patchwork::wrap_plots() +
              patchwork::plot_annotation(title = "Multivariable regression model checks")


            layout <- sapply(seq_len(length(p)), \(.x){
              patchwork::area(.x, 1)
            })

            p_list <- p + patchwork::plot_layout(design = Reduce(c, layout))

            index <- match(
              input$plot_checks,
              sapply(rv$check_plot(), \(.i){
                get_ggplot_label(.i, "title")
              })
            )

            ls <- list()

            for (i in index) {
              p <- p_list[[i]] +
                ggplot2::theme(
                  axis.text = ggplot2::element_text(size = 10),
                  axis.title = ggplot2::element_text(size = 12),
                  legend.text = ggplot2::element_text(size = 12),
                  plot.subtitle = ggplot2::element_text(size = 12),
                  plot.title = ggplot2::element_text(size = 18)
                )
              ls <- c(ls, list(p))
            }
            # browser()
            tryCatch(
              {
                out <- patchwork::wrap_plots(ls, ncol = if (length(ls) == 1) 1 else 2)
              },
              error = function(err) {
                showNotification(err, type = "err")
              }
            )

            out
          } else {
            return(NULL)
          }
        },
        alt = "Assumptions testing of the multivariable regression model"
      )

      ##############################################################################
      #########
      #########  Output
      #########
      ##############################################################################

      return(shiny::reactive({
        rv$list
      }))
    }
  )
}


########
#### Current file: /Users/au301842/FreesearchR/R//report.R
########

#' Split vector by an index and embed addition
#'
#' @param data vector
#' @param index split index
#' @param add addition
#'
#' @return vector
#' @export
#'
index_embed <- function(data, index, add = NULL) {
  start <- seq_len(index)
  end <- seq_along(data)[-start]
  c(
    data[start],
    add,
    data[end]
  )
}

#' Specify format arguments to include in qmd header/frontmatter
#'
#' @param data vector
#' @param fileformat format to include
#'
#' @return vector
#' @export
#'
specify_qmd_format <- function(data, fileformat = c("docx", "odt", "pdf", "all")) {
  fileformat <- match.arg(fileformat)
  args_list <- default_format_arguments() |> purrr::imap(format_writer)

  if (fileformat == "all") {
    out <- data |> index_embed(index = 4, add = Reduce(c, args_list))
  } else {
    out <- data |> index_embed(index = 4, add = args_list[[fileformat]])
  }
  out
}

#' Merges list of named arguments for qmd header generation
#'
#' @param data vector
#' @param name name
#'
#' @return vector
#' @export
#'
format_writer <- function(data, name) {
  if (data == "default") {
    glue::glue("  {name}: {data}")
  } else {
    warning("Not implemented")
  }
}

#' Defaults qmd formats
#'
#' @return list
#' @export
#'
default_format_arguments <- function() {
  list(
    docx = list("default"),
    odt = list("default"),
    pdf = list("default")
  )
}

#' Wrapper to modify quarto file to render specific formats
#'
#' @param file filename
#' @param format desired output
#'
#' @return none
#' @export
#'
modify_qmd <- function(file, format) {
  readLines(file) |>
    specify_qmd_format(fileformat = "all") |>
    writeLines(paste0(tools::file_path_sans_ext(file), "_format.", tools::file_ext(file)))
}



########
#### Current file: /Users/au301842/FreesearchR/R//syntax_highlight.R
########

## Inpiration:
##
## https://stackoverflow.com/questions/47445260/how-to-enable-syntax-highlighting-in-r-shiny-app-with-htmloutput

prismCodeBlock <- function(code) {
  tagList(
    HTML(html_code_wrap(code)),
    tags$script("Prism.highlightAll()")
  )
}

prismDependencies <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
  tags$link(rel = "stylesheet", type = "text/css",
            href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
)

prismRDependency <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-r.min.js")
)

html_code_wrap <- function(string,lang="r"){
  glue::glue("<pre><code class='language-{lang}'>{string}
  </code></pre>")
}


########
#### Current file: /Users/au301842/FreesearchR/R//theme.R
########

#' Custom theme based on unity
#'
#' @param ... everything passed on to bslib::bs_theme()
#'
#' @returns theme list
#' @export
custom_theme <- function(...,
                         version = 5,
                         primary = FreesearchR_colors("primary"),
                         secondary = FreesearchR_colors("secondary"),
                         bootswatch = "united",
                         base_font = bslib::font_google("Montserrat"),
                         heading_font = bslib::font_google("Public Sans", wght = "700"),
                         code_font = bslib::font_google("Open Sans"),
                         success = FreesearchR_colors("success"),
                         info = FreesearchR_colors("info"),
                         warning = FreesearchR_colors("warning"),
                         danger = FreesearchR_colors("danger")
                         # fg = "#000",
                         # bg="#fff",
                         # base_font = bslib::font_google("Alice"),
                         # heading_font = bslib::font_google("Jost", wght = "800"),
                         # heading_font = bslib::font_google("Noto Serif"),
                         # heading_font = bslib::font_google("Alice"),
) {
  bslib::bs_theme(
    ...,
    "navbar-bg" = primary,
    version = version,
    primary = primary,
    secondary = secondary,
    bootswatch = bootswatch,
    base_font = base_font,
    heading_font = heading_font,
    code_font = code_font,
    success=success,
    info=info,
    warning=warning,
    danger=danger
  )
}

FreesearchR_colors <- function(choose = NULL) {
  out <- c(
    primary = "#1E4A8F",
    secondary = "#FF6F61",
    success = "#00C896",
    warning = "#FFB100",
    danger = "#CC2E25",
    extra = "#8A4FFF",
    info = "#11A0EC",
    bg = "#FFFFFF",
    dark = "#2D2D42",
    fg = "#000000"
  )
  if (!is.null(choose)) {
    unname(out[choose])
  } else {
    out
  }
}

#' Use the FreesearchR colors
#'
#' @param n number of colors
#'
#' @returns character vector
#' @export
#'
#' @examples
#' FreesearchR_palette(n=7)
FreesearchR_palette <- function(n){
  rep_len(FreesearchR_colors(),n)
}



#' GGplot default theme for plotting in Shiny
#'
#' @param data ggplot object
#'
#' @returns ggplot object
#' @export
#'
gg_theme_shiny <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 18),
    axis.text = ggplot2::element_text(size = 14),
    strip.text = ggplot2::element_text(size = 14),
    legend.title = ggplot2::element_text(size = 18),
    legend.text = ggplot2::element_text(size = 14),
    plot.title = ggplot2::element_text(size = 24),
    plot.subtitle = ggplot2::element_text(size = 18)
  )
}


#' GGplot default theme for plotting export objects
#'
#' @param data ggplot object
#'
#' @returns ggplot object
#' @export
#'
gg_theme_export <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 18),
    axis.text.x = ggplot2::element_text(size = 14),
    legend.title = ggplot2::element_text(size = 18),
    legend.text = ggplot2::element_text(size = 14),
    plot.title = ggplot2::element_text(size = 24)
  )
}


########
#### Current file: /Users/au301842/FreesearchR/R//ui_elements.R
########

#' FreesearchR UI elements list
#'
#' @param selection specify element to output
#'
#' @returns Shinu UI elements
#' @export
#'
ui_elements <- function(selection) {
  out <- list(
    ##############################################################################
    #########
    #########  Home panel
    #########
    ##############################################################################
    "home" = bslib::nav_panel(
      title = "FreesearchR",
      # title = shiny::div(htmltools::img(src="FreesearchR-logo-white-nobg-h80.png")),
      icon = shiny::icon("house"),
      shiny::fluidRow(
        ## On building the dev-version for shinyapps.io, the dev_banner() is redefined
        ## Default just output "NULL"
        ## This could probably be achieved more legantly, but this works.
        dev_banner(),
        shiny::column(width = 2),
        shiny::column(
          width = 8,
          shiny::markdown(readLines("www/intro.md")),
          shiny::column(width = 2)
        )
      )
    ),
    ##############################################################################
    #########
    #########  Import panel
    #########
    ##############################################################################
    "import" = bslib::nav_panel(
      title = "Get started",
      icon = shiny::icon("play"),
      value = "nav_import",
      shiny::fluidRow(
        shiny::column(width = 2),
        shiny::column(
          width = 8,
          shiny::h4("Choose your data source"),
          shiny::br(),
          # shiny::uiOutput(outputId = "source"),
          shinyWidgets::radioGroupButtons(
            inputId = "source",
            selected = "file",
            choices = c(
              "File upload" = "file",
              "REDCap server export" = "redcap",
              "Local or sample data" = "env"
            ),
            size = "lg"
          ),
          shiny::tags$script('document.querySelector("#source div").style.width = "100%"'),
          shiny::helpText("Upload a file from your device, get data directly from REDCap or select a sample data set for testing from the app."),
          shiny::br(),
          shiny::br(),
          shiny::conditionalPanel(
            condition = "input.source=='file'",
            import_file_ui(
              id = "file_import",
              layout_params = "dropdown",
              # title = "Choose a datafile to upload",
              file_extensions = c(".csv", ".tsv", ".txt", ".xls", ".xlsx", ".rds", ".ods", ".dta")
            )
          ),
          shiny::conditionalPanel(
            condition = "input.source=='redcap'",
            shinyWidgets::alert(
              id = "redcap-warning",
              status = "info",
              shiny::tags$h2(shiny::markdown("Careful with sensitive data")),
              shiny::tags$p("The", shiny::tags$i(shiny::tags$b("FreesearchR")), "app only stores data for analyses, but please only use with sensitive data when running locally.", "", shiny::tags$a("Read more here", href = "https://agdamsbo.github.io/FreesearchR/#run-locally-on-your-own-machine"), "."),
              dismissible = TRUE
            ),
            m_redcap_readUI(
              id = "redcap_import",
              title = ""
            )
          ),
          shiny::conditionalPanel(
            condition = "input.source=='env'",
            import_globalenv_ui(id = "env", title = NULL)
          ),
          # shiny::conditionalPanel(
          #   condition = "input.source=='redcap'",
          #   DT::DTOutput(outputId = "redcap_prev")
          # ),
          shiny::conditionalPanel(
            condition = "output.data_loaded == true",
            shiny::br(),
            shiny::actionButton(
              inputId = "modal_initial_view",
              label = "Quick overview",
              width = "100%",
              icon = shiny::icon("binoculars"),
              disabled = FALSE
            ),
            shiny::br(),
            shiny::br(),
            shiny::h5("Select variables for final import"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::p("Exclude incomplete variables:"),
                shiny::br(),
                shinyWidgets::noUiSliderInput(
                  inputId = "complete_cutoff",
                  label = NULL,
                  update_on = "end",
                  min = 0,
                  max = 100,
                  step = 5,
                  value = 30,
                  format = shinyWidgets::wNumbFormat(decimals = 0),
                  color = datamods:::get_primary_color()
                ),
                shiny::helpText("Only include variables missing less observations than the specified percentage."),
                shiny::br()
              ),
              shiny::column(
                width = 6,
                shiny::p("Manual selection:"),
                shiny::br(),
                shiny::uiOutput(outputId = "import_var"),
                shiny::br()
              )
            ),
            shiny::uiOutput(outputId = "data_info_import", inline = TRUE),
            shiny::br(),
            shiny::br(),
            shiny::actionButton(
              inputId = "act_start",
              label = "Start",
              width = "100%",
              icon = shiny::icon("play"),
              disabled = TRUE
            ),
            shiny::helpText('After importing, hit "Start" or navigate to the desired tab.'),
            shiny::br(),
            shiny::br()
          ),
          shiny::column(width = 2)
        ),
        shiny::br(),
        shiny::br()
      )
    ),
    ##############################################################################
    #########
    #########  Data overview panel
    #########
    ##############################################################################
    "prepare" = bslib::nav_menu(
      title = "Prepare",
      icon = shiny::icon("pen-to-square"),
      value = "nav_prepare",
      bslib::nav_panel(
        title = "Overview",
        icon = shiny::icon("eye"),
        value = "nav_prepare_overview",
        tags$h3("Overview and filtering"),
        fluidRow(
          shiny::column(
            width = 9,
            shiny::uiOutput(outputId = "data_info", inline = TRUE),
            shiny::tags$p(
              "Below is a short summary table, on the right you can click to visualise data classes or browse data and create different data filters."
            )
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(
              inputId = "modal_visual_overview",
              label = "Visual overview",
              width = "100%",
              disabled = TRUE
            ),
            shiny::br(),
            shiny::br(),
            shiny::actionButton(
              inputId = "modal_browse",
              label = "Browse data",
              width = "100%",
              disabled = TRUE
            ),
            shiny::br(),
            shiny::br()
          )
        ),
        fluidRow(
          shiny::column(
            width = 9,
            data_summary_ui(id = "data_summary"),
            shiny::br(),
            shiny::br(),
            shiny::br(),
            shiny::br(),
            shiny::br()
          ),
          shiny::column(
            width = 3,
            shiny::tags$h6("Filter data types"),
            shiny::uiOutput(
              outputId = "column_filter"
            ),
            shiny::helpText("Read more on how ", tags$a(
              "data types",
              href = "https://agdamsbo.github.io/FreesearchR/articles/data-types.html",
              target = "_blank",
              rel = "noopener noreferrer"
            ), " are defined."),
            shiny::br(),
            shiny::br(),
            shiny::tags$h6("Filter observations"),
            shiny::tags$p("Filter on observation level"),
            IDEAFilter::IDEAFilter_ui("data_filter"),
            shiny::br(),
            shiny::br()
          )
        ),
        shiny::br(),
        shiny::br(),
        shiny::br()
      ),
      bslib::nav_panel(
        title = "Modify",
        icon = shiny::icon("file-pen"),
        tags$h3("Subset, rename and convert variables"),
        fluidRow(
          shiny::column(
            width = 9,
            shiny::tags$p(
              shiny::markdown("Below, are several options for simple data manipulation like update variables by renaming, creating new labels (for nicer tables in the report) and changing variable classes (numeric, factor/categorical etc.)."),
              shiny::markdown("There are more advanced options to modify factor/categorical variables as well as create new factor from a continous variable or new variables with *R* code. At the bottom you can restore the original data."),
              shiny::markdown("Please note that data modifications are applied before any filtering.")
            )
          )
        ),
        update_variables_ui("modal_variables"),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::tags$h4("Advanced data manipulation"),
        shiny::tags$p("Below options allow more advanced varaible manipulations."),
        shiny::tags$br(),
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = "modal_update",
              label = "Reorder factor levels",
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::helpText("Reorder the levels of factor/categorical variables."),
            shiny::tags$br(),
            shiny::tags$br()
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = "modal_cut",
              label = "New factor",
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::helpText("Create factor/categorical variable from a continous variable (number/date/time)."),
            shiny::tags$br(),
            shiny::tags$br()
          ),
          shiny::column(
            width = 4,
            shiny::actionButton(
              inputId = "modal_column",
              label = "New variable",
              width = "100%"
            ),
            shiny::tags$br(),
            shiny::helpText(shiny::markdown("Create a new variable/column based on an *R*-expression.")),
            shiny::tags$br(),
            shiny::tags$br()
          )
        ),
        tags$h4("Compare modified data to original"),
        shiny::tags$br(),
        shiny::tags$p(
          "Raw print of the original vs the modified data."
        ),
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::tags$b("Original data:"),
            shiny::verbatimTextOutput("original_str")
          ),
          shiny::column(
            width = 6,
            shiny::tags$b("Modified data:"),
            shiny::verbatimTextOutput("modified_str")
          )
        ),
        shiny::tags$br(),
        shiny::actionButton(
          inputId = "data_reset",
          label = "Restore original data",
          width = "100%"
        ),
        shiny::tags$br(),
        shiny::helpText("Reset to original imported dataset. Careful! There is no un-doing."),
        shiny::tags$br()
      )
      # )
    ),
    ##############################################################################
    #########
    #########  Descriptive analyses panel
    #########
    ##############################################################################
    "describe" =
      bslib::nav_menu(
        title = "Evaluate",
        icon = shiny::icon("magnifying-glass-chart"),
        value = "nav_describe",
        # id = "navdescribe",
        # bslib::navset_bar(
        #   title = "",
        bslib::nav_panel(
          title = "Characteristics",
          icon = bsicons::bs_icon("table"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              shiny::uiOutput(outputId = "data_info_nochar", inline = TRUE),
              bslib::accordion(
                open = "acc_chars",
                multiple = FALSE,
                bslib::accordion_panel(
                  open = TRUE,
                  value = "acc_chars",
                  title = "Settings",
                  icon = bsicons::bs_icon("table"),
                  shiny::uiOutput("strat_var"),
                  shiny::helpText("Only factor/categorical variables are available for stratification. Go back to the 'Prepare' tab to reclass a variable if it's not on the list."),
                  shiny::conditionalPanel(
                    condition = "input.strat_var!='none'",
                    shiny::radioButtons(
                      inputId = "add_p",
                      label = "Compare strata?",
                      selected = "no",
                      inline = TRUE,
                      choices = list(
                        "No" = "no",
                        "Yes" = "yes"
                      )
                    ),
                    shiny::helpText("Option to perform statistical comparisons between strata in baseline table.")
                  ),
                  shiny::br(),
                  shiny::br(),
                  shiny::actionButton(
                    inputId = "act_eval",
                    label = "Evaluate",
                    width = "100%",
                    icon = shiny::icon("calculator"),
                    disabled = TRUE
                  )
                )
              )
            ),
            gt::gt_output(outputId = "table1")
          )
        ),
        bslib::nav_panel(
          title = "Correlations",
          icon = bsicons::bs_icon("bounding-box"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              shiny::uiOutput(outputId = "data_info_nochar", inline = TRUE),
              bslib::accordion(
                open = "acc_chars",
                multiple = FALSE,
                bslib::accordion_panel(
                  value = "acc_cor",
                  title = "Correlations",
                  icon = bsicons::bs_icon("bounding-box"),
                  shiny::uiOutput("outcome_var_cor"),
                  shiny::helpText("To avoid evaluating the correlation of the outcome variable, this can be excluded from the plot or select 'none'."),
                  shiny::br(),
                  shinyWidgets::noUiSliderInput(
                    inputId = "cor_cutoff",
                    label = "Correlation cut-off",
                    min = 0,
                    max = 1,
                    step = .01,
                    value = .8,
                    format = shinyWidgets::wNumbFormat(decimals = 2),
                    color = datamods:::get_primary_color()
                  ),
                  shiny::helpText("Set the cut-off for considered 'highly correlated'.")
                )
              )
            ),
            data_correlations_ui(id = "correlations", height = 600)
          )
        ),
        bslib::nav_panel(
          title = "Missings",
          icon = bsicons::bs_icon("x-circle"),
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              bslib::accordion(
                open = "acc_chars",
                multiple = FALSE,
                bslib::accordion_panel(
                  vlaue = "acc_mis",
                  title = "Missings",
                  icon = bsicons::bs_icon("x-circle"),
                  shiny::uiOutput("missings_var"),
                  shiny::helpText("To consider if data is missing by random, choose the outcome/dependent variable, if it has any missings to evaluate if there is a significant difference across other variables depending on missing data or not.")
                )
              )
            ),
            data_missings_ui(id = "missingness")
          )
        )
      ),
    ##############################################################################
    #########
    #########  Download panel
    #########
    ##############################################################################
    "visuals" = do.call(
      bslib::nav_panel,
      c(
        list(
          title = "Visuals",
          icon = shiny::icon("chart-line"),
          value = "nav_visuals"
        ),
        data_visuals_ui("visuals")
      )
      # do.call(
      #   bslib::navset_bar,
      #     data_visuals_ui("visuals")#,
      # c(

      # )
      # )
    ),
    ##############################################################################
    #########
    #########  Regression analyses panel
    #########
    ##############################################################################
    "analyze" =
      bslib::nav_panel(
        title = "Regression",
        icon = shiny::icon("calculator"),
        value = "nav_analyses",
        do.call(
          bslib::navset_card_tab,
          regression_ui("regression")
        )
      ),
    ##############################################################################
    #########
    #########  Download panel
    #########
    ##############################################################################
    "download" =
      bslib::nav_panel(
        title = "Download",
        icon = shiny::icon("download"),
        value = "nav_download",
        shiny::fluidRow(
          shiny::column(width = 2),
          shiny::column(
            width = 8,
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::h4("Report"),
                shiny::helpText("Choose your favourite output file format for further work, and download, when the analyses are done."),
                shiny::br(),
                shiny::br(),
                shiny::selectInput(
                  inputId = "output_type",
                  label = "Output format",
                  selected = NULL,
                  choices = list(
                    "MS Word" = "docx",
                    "LibreOffice" = "odt"
                    # ,
                    # "PDF" = "pdf",
                    # "All the above" = "all"
                  )
                ),
                shiny::br(),
                # Button
                shiny::downloadButton(
                  outputId = "report",
                  label = "Download report",
                  icon = shiny::icon("download")
                )
                # shiny::helpText("If choosing to output to MS Word, please note, that when opening the document, two errors will pop-up. Choose to repair and choose not to update references. The issue is being worked on. You can always choose LibreOffice instead."),
              ),
              shiny::column(
                width = 6,
                shiny::h4("Data"),
                shiny::helpText("Choose your favourite output data format to download the modified data."),
                shiny::br(),
                shiny::br(),
                shiny::selectInput(
                  inputId = "data_type",
                  label = "Data format",
                  selected = NULL,
                  choices = list(
                    "R" = "rds",
                    "stata" = "dta",
                    "CSV" = "csv"
                  )
                ),
                shiny::helpText("No metadata is saved when exporting to csv."),
                shiny::br(),
                shiny::br(),
                # Button
                shiny::downloadButton(
                  outputId = "data_modified",
                  label = "Download data",
                  icon = shiny::icon("download")
                )
              )
            ),
            shiny::br(),
            shiny::br(),
            shiny::h4("Code snippets"),
            shiny::tags$p("Below are the code bits used to create the final data set and the main analyses."),
            shiny::tags$p("This can be used as a starting point for learning to code and for reproducibility."),
            shiny::tagList(
              lapply(
                paste0("code_", c(
                  "import", "format", "data", "variables", "filter", "table1", "univariable", "multivariable"
                )),
                \(.x)shiny::htmlOutput(outputId = .x)
              )
            ),
            shiny::tags$br(),
            shiny::br()
          ),
          shiny::column(width = 2)
        )
      ),
    ##############################################################################
    #########
    #########  Feedback link
    #########
    ##############################################################################
    "feedback" = bslib::nav_item(
      # shiny::img(shiny::icon("book")),
      shiny::tags$a(
        href = "https://redcap.au.dk/surveys/?s=JPCLPTXYDKFA8DA8",
        "Feedback", shiny::icon("arrow-up-right-from-square"),
        target = "_blank",
        rel = "noopener noreferrer"
      )
    ),
    ##############################################################################
    #########
    #########  Documentation link
    #########
    ##############################################################################
    "docs" = bslib::nav_item(
      # shiny::img(shiny::icon("book")),
      shiny::tags$a(
        href = "https://agdamsbo.github.io/FreesearchR/",
        "Docs", shiny::icon("arrow-up-right-from-square"),
        target = "_blank",
        rel = "noopener noreferrer"
      )
    )
    #   bslib::nav_panel(
    #   title = "Documentation",
    #   # shiny::tags$iframe("www/docs.html", height=600, width=535),
    #   shiny::htmlOutput("docs_file"),
    #   shiny::br()
    # )
  )
if (!is.null(selection)){
  out[[selection]]
} else {
  out
}

  }


# ls <- list("home"=1:4,
#            "test"=1:4)
#


########
#### Current file: /Users/au301842/FreesearchR/R//update-factor-ext.R
########


## Works, but not implemented
##
## These edits mainly allows for


#' @title Module to Reorder the Levels of a Factor Variable
#'
#' @description
#' This module contain an interface to reorder the levels of a factor variable.
#'
#'
#' @param id Module ID.
#'
#' @return A [shiny::reactive()] function returning the data.
#' @export
#'
#' @importFrom shiny NS fluidRow tagList column actionButton
#' @importFrom shinyWidgets virtualSelectInput prettyCheckbox
#' @importFrom toastui datagridOutput
#' @importFrom htmltools tags
#'
#' @name update-factor
#'
update_factor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      ".tui-grid-row-header-draggable span {width: 3px !important; height: 3px !important;}"
    ),
    fluidRow(
      column(
        width = 6,
        shinyWidgets::virtualSelectInput(
          inputId = ns("variable"),
          label = i18n("Factor variable to reorder:"),
          choices = NULL,
          width = "100%",
          zIndex = 50
        )
      ),
      column(
        width = 3,
        class = "d-flex align-items-end",
        actionButton(
          inputId = ns("sort_levels"),
          label = tagList(
            phosphoricons::ph("sort-ascending"),
            datamods:::i18n("Sort by levels")
          ),
          class = "btn-outline-primary mb-3",
          width = "100%"
        )
      ),
      column(
        width = 3,
        class = "d-flex align-items-end",
        actionButton(
          inputId = ns("sort_occurrences"),
          label = tagList(
            phosphoricons::ph("sort-ascending"),
            datamods:::i18n("Sort by count")
          ),
          class = "btn-outline-primary mb-3",
          width = "100%"
        )
      )
    ),
    toastui::datagridOutput(ns("grid")),
    tags$div(
      class = "float-end",
      shinyWidgets::prettyCheckbox(
        inputId = ns("new_var"),
        label = datamods:::i18n("Create a new variable (otherwise replaces the one selected)"),
        value = FALSE,
        status = "primary",
        outline = TRUE,
        inline = TRUE
      ),
      actionButton(
        inputId = ns("create"),
        label = tagList(phosphoricons::ph("arrow-clockwise"), datamods:::i18n("Update factor variable")),
        class = "btn-outline-primary"
      )
    ),
    tags$div(class = "clearfix")
  )
}


#' @param data_r A [shiny::reactive()] function returning a `data.frame`.
#'
#' @export
#'
#' @importFrom shiny moduleServer observeEvent reactive reactiveValues req bindEvent isTruthy updateActionButton
#' @importFrom shinyWidgets updateVirtualSelect
#' @importFrom toastui renderDatagrid datagrid grid_columns grid_colorbar
#'
#' @rdname update-factor
update_factor_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {

      rv <- reactiveValues(data = NULL, data_grid = NULL)

      bindEvent(observe({
        data <- data_r()
        rv$data <- data
        vars_factor <- vapply(data, is.factor, logical(1))
        vars_factor <- names(vars_factor)[vars_factor]
        updateVirtualSelect(
          inputId = "variable",
          choices = vars_factor,
          selected = if (isTruthy(input$variable)) input$variable else vars_factor[1]
        )
      }), data_r(), input$hidden)

      observeEvent(input$variable, {
        data <- req(data_r())
        variable <- req(input$variable)
        grid <- as.data.frame(table(data[[variable]]))
        rv$data_grid <- grid
      })

      observeEvent(input$sort_levels, {
        if (input$sort_levels %% 2 == 1) {
          decreasing <- FALSE
          label <- tagList(
            phosphoricons::ph("sort-descending"),
            "Sort Levels"
          )
        } else {
          decreasing <- TRUE
          label <- tagList(
            phosphoricons::ph("sort-ascending"),
            "Sort Levels"
          )
        }
        updateActionButton(inputId = "sort_levels", label = as.character(label))
        rv$data_grid <- rv$data_grid[order(rv$data_grid[[1]], decreasing = decreasing), ]
      })

      observeEvent(input$sort_occurrences, {
        if (input$sort_occurrences %% 2 == 1) {
          decreasing <- FALSE
          label <- tagList(
            phosphoricons::ph("sort-descending"),
            datamods:::i18n("Sort count")
          )
        } else {
          decreasing <- TRUE
          label <- tagList(
            phosphoricons::ph("sort-ascending"),
            datamods:::i18n("Sort count")
          )
        }
        updateActionButton(inputId = "sort_occurrences", label = as.character(label))
        rv$data_grid <- rv$data_grid[order(rv$data_grid[[2]], decreasing = decreasing), ]
      })


      output$grid <- renderDatagrid({
        req(rv$data_grid)
        gridTheme <- getOption("datagrid.theme")
        if (length(gridTheme) < 1) {
          datamods:::apply_grid_theme()
        }
        on.exit(toastui::reset_grid_theme())
        data <- rv$data_grid
        data <- add_var_toset(data, "Var1", "New label")

        grid <- datagrid(
          data = data,
          draggable = TRUE,
          sortable = FALSE,
          data_as_input = TRUE
        )
        grid <- grid_columns(
          grid,
          columns = c("Var1", "Var1_toset", "Freq"),
          header = c(datamods:::i18n("Levels"), "New label", datamods:::i18n("Count"))
        )
        grid <- grid_colorbar(
          grid,
          column = "Freq",
          label_outside = TRUE,
          label_width = "30px",
          background = "#D8DEE9",
          bar_bg = datamods:::get_primary_color(),
          from = c(0, max(rv$data_grid$Freq) + 1)
        )
        grid <- toastui::grid_style_column(
          grid = grid,
          column = "Var1_toset",
          fontStyle = "italic"
        )
        grid <- toastui::grid_editor(
          grid = grid,
          column = "Var1_toset",
          type = "text"
        )
        grid
      })

      data_updated_r <- reactive({
        data <- req(data_r())
        variable <- req(input$variable)
        grid <- req(input$grid_data)
        name_var <- if (isTRUE(input$new_var)) {
          paste0(variable, "_updated")
        } else {
          variable
        }
        data[[name_var]] <- factor(
          as.character(data[[variable]]),
          levels = grid[["Var1"]]
        )
        data[[name_var]] <- factor(
          data[[variable]],
          labels = ifelse(grid[["Var1_toset"]]=="New label",grid[["Var1"]],grid[["Var1_toset"]])
        )
        data
      })

      data_returned_r <- observeEvent(input$create, {
        rv$data <- data_updated_r()
      })
      return(reactive(rv$data))
    }
  )
}



#' @inheritParams shiny::modalDialog
#' @export
#'
#' @importFrom shiny showModal modalDialog textInput
#' @importFrom htmltools tagList
#'
#' @rdname update-factor
modal_update_factor <- function(id,
                                title = i18n("Update levels of a factor"),
                                easyClose = TRUE,
                                size = "l",
                                footer = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    update_factor_ui(id),
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
#' @rdname update-factor
winbox_update_factor <- function(id,
                                 title = i18n("Update levels of a factor"),
                                 options = shinyWidgets::wbOptions(),
                                 controls = shinyWidgets::wbControls()) {
  ns <- NS(id)
  WinBox(
    title = title,
    ui = tagList(
      update_factor_ui(id),
      tags$div(
        style = "display: none;",
        textInput(inputId = ns("hidden"), label = NULL, value = genId())
      )
    ),
    options = modifyList(
      shinyWidgets::wbOptions(height = "615px", modal = TRUE),
      options
    ),
    controls = controls,
    auto_height = FALSE
  )
}



########
#### Current file: /Users/au301842/FreesearchR/R//update-variables-ext.R
########

#' Select, rename and convert variables
#'
#' @param id Module id. See [shiny::moduleServer()].
#' @param title Module's title, if `TRUE` use the default title,
#'  use \code{NULL} for no title or a `shiny.tag` for a custom one.
#'
#' @return A [shiny::reactive()] function returning the updated data.
#' @export
#'
#' @name update-variables
#'
update_variables_ui <- function(id, title = "") {
  ns <- NS(id)
  if (isTRUE(title)) {
    title <- htmltools::tags$h4(
      i18n("Update & select variables"),
      class = "datamods-title"
    )
  }
  htmltools::tags$div(
    class = "datamods-update",
    shinyWidgets::html_dependency_pretty(),
    title,
    htmltools::tags$div(
      style = "min-height: 25px;",
      htmltools::tags$div(
        shiny::uiOutput(outputId = ns("data_info"), inline = TRUE),
        shiny::tagAppendAttributes(
          shinyWidgets::dropMenu(
            placement = "bottom-end",
            shiny::actionButton(
              inputId = ns("settings"),
              label = phosphoricons::ph("gear"),
              class = "pull-right float-right"
            ),
            shinyWidgets::textInputIcon(
              inputId = ns("format"),
              label = i18n("Date format:"),
              value = "%Y-%m-%d",
              icon = list(phosphoricons::ph("clock"))
            ),
            shinyWidgets::textInputIcon(
              inputId = ns("origin"),
              label = i18n("Date to use as origin to convert date/datetime:"),
              value = "1970-01-01",
              icon = list(phosphoricons::ph("calendar"))
            ),
            shinyWidgets::textInputIcon(
              inputId = ns("dec"),
              label = i18n("Decimal separator:"),
              value = ".",
              icon = list("0.00")
            )
          ),
          style = "display: inline;"
        )
      ),
      htmltools::tags$br(),
      toastui::datagridOutput(outputId = ns("table"))
    ),
    htmltools::tags$br(),
    htmltools::tags$div(
      id = ns("update-placeholder"),
      shinyWidgets::alert(
        id = ns("update-result"),
        status = "info",
        phosphoricons::ph("info"),
        paste(
          "Select variables to keep (if none selected, all are kept), rename",
          "variables and labels, and convert variable type/class in the table",
          "above. Apply changes by clicking the button below."
        )
      )
    ),
    shiny::actionButton(
      inputId = ns("validate"),
      label = htmltools::tagList(
        phosphoricons::ph("arrow-circle-right", title = datamods::i18n("Apply changes")),
        datamods::i18n("Apply changes")
      ),
      width = "100%"
    )
  )
}

#' @export
#'
#' @param id Module's ID
#' @param data a \code{data.frame} or a \code{reactive} function returning a \code{data.frame}.
#' @param height Height for the table.
#' @param return_data_on_init Return initial data when module is called.
#' @param try_silent logical: should the report of error messages be suppressed?
#'
#' @rdname update-variables
#'
update_variables_server <- function(id,
                                    data,
                                    height = NULL,
                                    return_data_on_init = FALSE,
                                    try_silent = FALSE) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns
      updated_data <- shiny::reactiveValues(x = NULL)

      data_r <- shiny::reactive({
        if (shiny::is.reactive(data)) {
          data()
        } else {
          data
        }
      })

      output$data_info <- shiny::renderUI({
        shiny::req(data_r())
        data_description(data_r())
        # sprintf(i18n("Data has %s observations and %s variables."), nrow(data), ncol(data))
      })

      variables_r <- shiny::reactive({
        shiny::validate(
          shiny::need(data(), i18n("No data to display."))
        )
        data <- data_r()
        if (isTRUE(return_data_on_init)) {
          updated_data$x <- data
        } else {
          updated_data$x <- NULL
        }
        summary_vars(data)
      })

      output$table <- toastui::renderDatagrid({
        shiny::req(variables_r())

        variables <- variables_r()

        update_variables_datagrid(
          variables,
          height = height,
          selectionId = ns("row_selected"),
          buttonId = "validate"
        )
      })

      shiny::observeEvent(input$validate,
        {
          updated_data$list_rename <- NULL
          updated_data$list_select <- NULL
          updated_data$list_mutate <- NULL
          updated_data$list_relabel <- NULL
          # shiny::req(updated_data$x)
          data <- data_r()
          new_selections <- input$row_selected
          if (length(new_selections) < 1) {
            new_selections <- seq_along(data)
          }

          data_inputs <- data.table::as.data.table(input$table_data)
          data.table::setorderv(data_inputs, "rowKey")

          old_names <- data_inputs$name
          new_names <- data_inputs$name_toset
          new_names[new_names == "New name"] <- NA
          new_names[is.na(new_names)] <- old_names[is.na(new_names)]
          new_names[new_names == ""] <- old_names[new_names == ""]

          # browser()

          old_label <- data_inputs$label
          new_label <- data_inputs$label_toset

          new_label[new_label == "New label"] <- old_label[new_label == "New label"]

          ## Later, "" will be interpreted as NA/empty and removed
          new_label[is.na(new_label) | new_label %in% c('""',"''"," ")] <- ""

          # new_label[is.na(new_label)] <- old_label[is.na(new_label)]
          new_label <- setNames(new_label, new_names)

          new_classes <- data_inputs$class_toset
          new_classes[new_classes == "Select"] <- NA

          data_sv <- variables_r()
          vars_to_change <- get_vars_to_convert(data_sv, setNames(as.list(new_classes), old_names))

          res_update <- try(
            {
              # convert
              if (nrow(vars_to_change) > 0) {
                data <- convert_to(
                  data = data,
                  variable = vars_to_change$name,
                  new_class = vars_to_change$class_to_set,
                  origin = input$origin,
                  format = input$format,
                  dec = input$dec
                )
              }
              list_mutate <- attr(data, "code_03_convert")

              # rename
              list_rename <- setNames(
                as.list(old_names),
                unlist(new_names, use.names = FALSE)
              )
              list_rename <- list_rename[names(list_rename) != unlist(list_rename, use.names = FALSE)]
              names(data) <- unlist(new_names, use.names = FALSE)

              # relabel
              list_relabel <- as.list(new_label)
              data <- set_column_label(data, list_relabel)

              # select
              list_select <- setdiff(names(data), names(data)[new_selections])
              data <- data[, new_selections, drop = FALSE]
            },
            silent = try_silent
          )

          if (inherits(res_update, "try-error")) {
            datamods:::insert_error(selector = "update")
          } else {
            datamods:::insert_alert(
              selector = ns("update"),
              status = "success",
              tags$b(phosphoricons::ph("check"), datamods::i18n("Data successfully updated!"))
            )
            updated_data$x <- data
            updated_data$list_rename <- list_rename
            updated_data$list_select <- list_select
            updated_data$list_mutate <- list_mutate
            updated_data$list_relabel <- list_relabel
          }
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )

      # shiny::observeEvent(input$close,
      #                                         {
      return(shiny::reactive({
        shiny::req(updated_data$x)
        # browser()
        data <- updated_data$x
        code <- list()
        if (!is.null(data) && shiny::isTruthy(updated_data$list_mutate) && length(updated_data$list_mutate) > 0) {
          code <- c(code, list(rlang::call2("mutate", !!!updated_data$list_mutate,.ns="dplyr")))
        }
        if (!is.null(data) && shiny::isTruthy(updated_data$list_rename) && length(updated_data$list_rename) > 0) {
          code <- c(code, list(rlang::call2("rename", !!!updated_data$list_rename,.ns="dplyr")))
        }
        if (!is.null(data) && shiny::isTruthy(updated_data$list_select) && length(updated_data$list_select) > 0) {
          code <- c(code, list(rlang::expr(dplyr::select(-dplyr::any_of(c(!!!updated_data$list_select))))))
        }
        if (!is.null(data) && shiny::isTruthy(updated_data$list_relabel) && length(updated_data$list_relabel) > 0) {
          code <- c(code,list(rlang::call2("set_column_label",label=updated_data$list_relabel,.ns="FreesearchR")))
        }
        if (length(code) > 0) {
          attr(data, "code") <- Reduce(
            f = function(x, y) rlang::expr(!!x %>% !!y),
            x = code
          )
        }
        return(data)
      }))
      # })

      # shiny::reactive({
      #   data <- updated_data$x
      #   code <- list()
      #   if (!is.null(data) && shiny::isTruthy(updated_data$list_mutate) && length(updated_data$list_mutate) > 0) {
      #     code <- c(code, list(rlang::call2("mutate", !!!updated_data$list_mutate)))
      #   }
      #   if (!is.null(data) && shiny::isTruthy(updated_data$list_rename) && length(updated_data$list_rename) > 0) {
      #     code <- c(code, list(rlang::call2("rename", !!!updated_data$list_rename)))
      #   }
      #   if (!is.null(data) && shiny::isTruthy(updated_data$list_select) && length(updated_data$list_select) > 0) {
      #     code <- c(code, list(rlang::expr(select(-any_of(c(!!!updated_data$list_select))))))
      #   }
      #   if (!is.null(data) && shiny::isTruthy(updated_data$list_relabel) && length(updated_data$list_relabel) > 0) {
      #     code <- c(code, list(rlang::call2("purrr::map2(list_relabel,
      #                                                  function(.data,.label){
      #                                                    REDCapCAST::set_attr(.data,.label,attr = 'label')
      #                                                  }) |> dplyr::bind_cols(.name_repair = 'unique_quiet')")))
      #   }
      #   if (length(code) > 0) {
      #     attr(data, "code") <- Reduce(
      #       f = function(x, y) rlang::expr(!!x %>% !!y),
      #       x = code
      #     )
      #   }
      #   updated_data$return_data <- data
      # })

      # shiny::observeEvent(input$close,
      #                     {
      #                       shiny::req(input$close)
      #                       return(shiny::reactive({
      #                         data <- updated_data$return_data
      #                         return(data)
      #                         }))
      #                     })
    }
  )
}


modal_update_variables <- function(id,
                                   title = "Select, rename and reclass variables",
                                   easyClose = TRUE,
                                   size = "xl",
                                   footer = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    update_variables_ui(id),
    # tags$div(
    #   style = "display: none;",
    #   textInput(inputId = ns("hidden"), label = NULL, value = datamods:::genId())
    # ),
    easyClose = easyClose,
    size = size,
    footer = footer
  ))
}



# utils -------------------------------------------------------------------


#' Get variables classes from a \code{data.frame}
#'
#' @param data a \code{data.frame}
#'
#' @return a \code{character} vector as same length as number of variables
#' @noRd
#'
#' @examples
#'
#' get_classes(mtcars)
get_classes <- function(data) {
  classes <- lapply(
    X = data,
    FUN = function(x) {
      paste(class(x), collapse = ", ")
    }
  )
  unlist(classes, use.names = FALSE)
}


#' Get count of unique values in variables of \code{data.frame}
#'
#' @param data a \code{data.frame}
#'
#' @return a \code{numeric} vector as same length as number of variables
#' @noRd
#'
#'
#' @examples
#' get_n_unique(mtcars)
get_n_unique <- function(data) {
  u <- lapply(data, FUN = function(x) {
    if (is.atomic(x)) {
      data.table::uniqueN(x)
    } else {
      NA_integer_
    }
  })
  unlist(u, use.names = FALSE)
}



#' Add padding 0 to a vector
#'
#' @param x a \code{vector}
#'
#' @return a \code{character} vector
#' @noRd
#'
#' @examples
#'
#' pad0(1:10)
#' pad0(c(1, 15, 150, NA))
pad0 <- function(x) {
  NAs <- which(is.na(x))
  x <- formatC(x, width = max(nchar(as.character(x)), na.rm = TRUE), flag = "0")
  x[NAs] <- NA
  x
}

#' Variables summary
#'
#' @param data a \code{data.frame}
#'
#' @return a \code{data.frame}
#' @noRd
#'
#' @examples
#'
#' summary_vars(iris)
#' summary_vars(mtcars)
summary_vars <- function(data) {
  data <- as.data.frame(data)
  datsum <- dplyr::tibble(
    name = names(data),
    label = lapply(data, \(.x) REDCapCAST::get_attr(.x, "label")) |> unlist(),
    class = get_classes(data),
    n_missing = unname(colSums(is.na(data))),
    p_complete = 1 - n_missing / nrow(data),
    n_unique = get_n_unique(data)
  )

  datsum
}

add_var_toset <- function(data, var_name, default = "") {
  datanames <- names(data)
  datanames <- append(
    x = datanames,
    values = paste0(var_name, "_toset"),
    after = which(datanames == var_name)
  )
  data[[paste0(var_name, "_toset")]] <- default
  data[, datanames]
}

#' Modified from the datamods pacakge
#'
#' @param data data
#'
#' @param height height
#' @param selectionId selectionId
#' @param buttonId buttonId
#'
#' @examples
#' mtcars |>
#'   summary_vars() |>
#'   update_variables_datagrid()
#'
update_variables_datagrid <- function(data, height = NULL, selectionId = NULL, buttonId = NULL) {
  # browser()
  data <- add_var_toset(data, "name", "New name")
  data <- add_var_toset(data, "class", "Select")
  data <- add_var_toset(data, "label", "New label")

  gridTheme <- getOption("datagrid.theme")
  if (length(gridTheme) < 1) {
    datamods:::apply_grid_theme()
  }
  on.exit(toastui::reset_grid_theme())

  col.names <- names(data)

  std_names <- c(
    "name", "name_toset", "label", "label_toset", "class", "class_toset", "n_missing", "p_complete", "n_unique"
  ) |>
    setNames(c(
      "Name", "New name", "Label", "New label", "Class", "New class", "Missing", "Complete", "Unique"
    ))

  headers <- lapply(col.names, \(.x){
    if (.x %in% std_names) {
      names(std_names)[match(.x, std_names)]
    } else {
      .x
    }
  }) |> unlist()

  grid <- toastui::datagrid(
    data = data,
    theme = "default",
    colwidths = NULL
  )
  grid <- toastui::grid_columns(
    grid = grid,
    columns = col.names,
    header = headers,
    minWidth = 100
  )

  grid <- toastui::grid_format(
    grid = grid,
    "p_complete",
    formatter = toastui::JS("function(obj) {return (obj.value*100).toFixed(0) + '%';}")
  )
  grid <- toastui::grid_style_column(
    grid = grid,
    column = "name_toset",
    fontStyle = "italic"
  )
  grid <- toastui::grid_style_column(
    grid = grid,
    column = "label_toset",
    fontStyle = "italic"
  )
  grid <- toastui::grid_style_column(
    grid = grid,
    column = "class_toset",
    fontStyle = "italic"
  )

  grid <- toastui::grid_filters(
    grid = grid,
    column = "name",
    # columns = unname(std_names[std_names!="vals"]),
    showApplyBtn = FALSE,
    showClearBtn = TRUE,
    type = "text"
  )

  # grid <- toastui::grid_columns(
  #   grid = grid,
  #   columns = "name_toset",
  #   editor = list(type = "text"),
  #   validation = toastui::validateOpts()
  # )
  #
  # grid <- toastui::grid_columns(
  #   grid = grid,
  #   columns = "label_toset",
  #   editor = list(type = "text"),
  #   validation = toastui::validateOpts()
  # )
  #
  # grid <- toastui::grid_columns(
  #   grid = grid,
  #   columns = "class_toset",
  #   editor = list(
  #     type = "radio",
  #     options = list(
  #       instantApply = TRUE,
  #       listItems = lapply(
  #         X = c("Select", "character", "factor", "numeric", "integer", "date", "datetime", "hms"),
  #         FUN = function(x) {
  #           list(text = x, value = x)
  #         }
  #       )
  #     )
  #   ),
  #   validation = toastui::validateOpts()
  # )

  grid <- toastui::grid_editor(
    grid = grid,
    column = "name_toset",
    type = "text"
  )
  grid <- toastui::grid_editor(
    grid = grid,
    column = "label_toset",
    type = "text"
  )
  grid <- toastui::grid_editor(
    grid = grid,
    column = "class_toset",
    type = "select",
    choices = c("Select", "character", "factor", "numeric", "integer", "date", "datetime", "hms")
  )
  grid <- toastui::grid_editor_opts(
    grid = grid,
    editingEvent = "click",
    actionButtonId = NULL,
    session = NULL
  )
  grid <- toastui::grid_selection_row(
    grid = grid,
    inputId = selectionId,
    type = "checkbox",
    return = "index"
  )

  return(grid)
}



#' Convert a variable to specific new class
#'
#' @param data A \code{data.frame}
#' @param variable Name of the variable to convert
#' @param new_class Class to set
#' @param ... Other arguments passed on to methods.
#'
#' @return A \code{data.frame}
#' @noRd
#'
#' @importFrom utils type.convert
#' @importFrom rlang sym expr
#'
#' @examples
#' dat <- data.frame(
#'   v1 = month.name,
#'   v2 = month.abb,
#'   v3 = 1:12,
#'   v4 = as.numeric(Sys.Date() + 0:11),
#'   v5 = as.character(Sys.Date() + 0:11),
#'   v6 = as.factor(c("a", "a", "b", "a", "b", "a", "a", "b", "a", "b", "b", "a")),
#'   v7 = as.character(11:22),
#'   stringsAsFactors = FALSE
#' )
#'
#' str(dat)
#'
#' str(convert_to(dat, "v3", "character"))
#' str(convert_to(dat, "v6", "character"))
#' str(convert_to(dat, "v7", "numeric"))
#' str(convert_to(dat, "v4", "date", origin = "1970-01-01"))
#' str(convert_to(dat, "v5", "date"))
#'
#' str(convert_to(dat, c("v1", "v3"), c("factor", "character")))
#'
#' str(convert_to(dat, c("v1", "v3", "v4"), c("factor", "character", "date"), origin = "1970-01-01"))
#'
convert_to <- function(data,
                       variable,
                       new_class = c("character", "factor", "numeric", "integer", "date", "datetime", "hms"),
                       ...) {
  new_class <- match.arg(new_class, several.ok = TRUE)
  stopifnot(length(new_class) == length(variable))
  args <- list(...)
  args$format <- clean_sep(args$format)
  if (length(variable) > 1) {
    for (i in seq_along(variable)) {
      data <- convert_to(data, variable[i], new_class[i], ...)
    }
    return(data)
  }
  if (identical(new_class, "character")) {
    data[[variable]] <- as.character(x = data[[variable]], ...)
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.character(!!sym(variable)))), variable)
    )
  } else if (identical(new_class, "factor")) {
    data[[variable]] <- REDCapCAST::as_factor(x = data[[variable]])
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(REDCapCAST::as_factor(!!sym(variable)))), variable)
    )
  } else if (identical(new_class, "numeric")) {
    data[[variable]] <- as.numeric(data[[variable]], ...)
    # This is the original, that would convert to character and then to numeric
    # resulting in all NAs, setting as.is = FALSE would result in a numeric
    # vector in order of appearance. Now it is acting like integer conversion
    # data[[variable]] <- as.numeric(type.convert(data[[variable]], as.is = TRUE, ...))
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.numeric(!!sym(variable)))), variable)
    )
  } else if (identical(new_class, "integer")) {
    data[[variable]] <- as.integer(x = data[[variable]], ...)
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.integer(!!sym(variable)))), variable)
    )
  } else if (identical(new_class, "date")) {
    data[[variable]] <- as.Date(x = clean_date(data[[variable]]), ...)
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.Date(clean_date(!!sym(variable)), origin = !!args$origin, format = clean_sep(!!args$format)))), variable)
    )
  } else if (identical(new_class, "datetime")) {
    data[[variable]] <- as.POSIXct(x = data[[variable]], ...)
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.POSIXct(!!sym(variable)))), variable)
    )
  } else if (identical(new_class, "hms")) {
    data[[variable]] <- hms::as_hms(x = data[[variable]])
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(hms::as_hms(!!sym(variable)))), variable)
    )
  }
  return(data)
}








#' Get variable(s) to convert
#'
#' @param vars Output of [summary_vars()]
#' @param classes_input List of inputs containing new classes
#'
#' @return a `data.table`.
#' @noRd
#'
#' @importFrom data.table data.table as.data.table
#'
#' @examples
#' # 2 variables to convert
#' new_classes <- list(
#'   "Sepal.Length" = "numeric",
#'   "Sepal.Width" = "numeric",
#'   "Petal.Length" = "character",
#'   "Petal.Width" = "numeric",
#'   "Species" = "character"
#' )
#' get_vars_to_convert(summary_vars(iris), new_classes)
#'
#'
#' # No changes
#' new_classes <- list(
#'   "Sepal.Length" = "numeric",
#'   "Sepal.Width" = "numeric",
#'   "Petal.Length" = "numeric",
#'   "Petal.Width" = "numeric",
#'   "Species" = "factor"
#' )
#' get_vars_to_convert(summary_vars(iris), new_classes)
#'
#' # Not set = NA or ""
#' new_classes <- list(
#'   "Sepal.Length" = NA,
#'   "Sepal.Width" = NA,
#'   "Petal.Length" = NA,
#'   "Petal.Width" = NA,
#'   "Species" = NA
#' )
#' get_vars_to_convert(summary_vars(iris), new_classes)
#'
#' # Set for one var
#' new_classes <- list(
#'   "Sepal.Length" = "",
#'   "Sepal.Width" = "",
#'   "Petal.Length" = "",
#'   "Petal.Width" = "",
#'   "Species" = "character"
#' )
#' get_vars_to_convert(summary_vars(iris), new_classes)
#'
#' new_classes <- list(
#'   "mpg" = "character",
#'   "cyl" = "numeric",
#'   "disp" = "character",
#'   "hp" = "numeric",
#'   "drat" = "character",
#'   "wt" = "character",
#'   "qsec" = "numeric",
#'   "vs" = "character",
#'   "am" = "numeric",
#'   "gear" = "character",
#'   "carb" = "integer"
#' )
#' get_vars_to_convert(summary_vars(mtcars), new_classes)
get_vars_to_convert <- function(vars, classes_input) {
  vars <- data.table::as.data.table(vars)
  classes_input <- data.table::data.table(
    name = names(classes_input),
    class_to_set = unlist(classes_input, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  classes_input <- classes_input[!is.na(class_to_set) & class_to_set != ""]
  classes_df <- merge(x = vars, y = classes_input, by = "name")
  classes_df <- classes_df[!is.na(class_to_set)]
  classes_df[class != class_to_set]
}


#' gsub wrapper for piping with default values for separator substituting
#'
#' @param data character vector
#' @param old.sep old separator
#' @param new.sep new separator
#'
#' @returns character vector
#' @export
#'
clean_sep <- function(data, old.sep = "[-.,/]", new.sep = "-") {
  gsub(old.sep, new.sep, data)
}

#' Attempts at applying uniform date format
#'
#' @param data character string vector of possible dates
#'
#' @returns character string
#' @export
#'
clean_date <- function(data) {
  data |>
    clean_sep() |>
    sapply(\(.x){
      if (is.na(.x)) {
        .x
      } else {
        strsplit(.x, "-") |>
          unlist() |>
          lapply(\(.y){
            if (nchar(.y) == 1) paste0("0", .y) else .y
          }) |>
          paste(collapse = "-")
      }
    }) |>
    unname()
}


########
#### Current file: /Users/au301842/FreesearchR/R//visual_summary.R
########

#' Data correlations evaluation module
#'
#' @param id Module id
#'
#' @name data-missings
#' @returns Shiny ui module
#' @export
visual_summary_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::plotOutput(outputId = ns("visual_plot"), height = "70vh")
  )
}

visual_summary_server <- function(id,
                                  data_r=shiny::reactive(NULL),
                                  ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # ns <- session$ns
      rv <- shiny::reactiveValues(data = NULL)

      shiny::bindEvent(shiny::observe({
        data <- data_r()
        rv$data <- data
        # vars_num <- vapply(data, \(.x){
        #   is.numeric(.x) || is_datetime(.x)
        # }, logical(1))
        # vars_num <- names(vars_num)[vars_num]
        # shinyWidgets::updateVirtualSelect(
        #   inputId = "variable",
        #   choices = vars_num,
        #   selected = if (isTruthy(input$variable)) input$variable else vars_num[1]
        # )
      }), data_r(), input$hidden)

      # datar <- if (is.reactive(data)) data else reactive(data)


      # apexcharter::renderApexchart({
      #   missings_apex_plot(datar(), ...)
      # })
      output$visual_plot <- shiny::renderPlot(expr = {
        visual_summary(data = rv$data,...)
      })
    }
  )
}

visual_summary_demo_app <- function() {
  ui <- shiny::fluidPage(
    shiny::actionButton(
      inputId = "modal_missings",
      label = "Visual summary",
      width = "100%",
      disabled = FALSE
    )
  )
  server <- function(input, output, session) {
    data_demo <- mtcars
    data_demo[sample(1:32, 10), "cyl"] <- NA
    data_demo[sample(1:32, 8), "vs"] <- NA

    visual_summary_server(id = "data", data = shiny::reactive(data_demo))

    observeEvent(input$modal_missings, {
      tryCatch(
        {
          modal_visual_summary(id = "data")
        },
        error = function(err) {
          showNotification(paste0("We encountered the following error browsing your data: ", err), type = "err")
        }
      )
    })
  }
  shiny::shinyApp(ui, server)
}

visual_summary_demo_app()


modal_visual_summary <- function(id,
                                 title = "Visual overview of data classes and missing observations",
                                 easyClose = TRUE,
                                 size = "xl",
                                 footer = NULL,
                                 ...) {
  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    visual_summary_ui(id = id),
    easyClose = easyClose,
    size = size,
    footer = footer
  ))
}


## Slow with many observations...

#' Plot missings and class with apexcharter
#'
#' @param data data frame
#'
#' @returns An [apexchart()] `htmlwidget` object.
#' @export
#'
#' @examples
#' data_demo <- mtcars
#' data_demo[2:4, "cyl"] <- NA
#' rbind(data_demo, data_demo, data_demo, data_demo) |> missings_apex_plot()
#' data_demo |> missings_apex_plot()
#' mtcars |> missings_apex_plot(animation = TRUE)
#' # dplyr::storms |> missings_apex_plot()
#' visdat::vis_dat(dplyr::storms)
missings_apex_plot <- function(data, animation = FALSE, ...) {
  l <- data_summary_gather(data, ...)

  df_plot <- l$data

  out <- apexcharter::apex(
    data = df_plot,
    type = "heatmap",
    mapping = apexcharter::aes(x = variable, y = rows, fill = valueType_num),
    ...
  ) |>
    apexcharter::ax_stroke(width = NULL) |>
    apexcharter::ax_plotOptions(
      heatmap = apexcharter::heatmap_opts(
        radius = 0,
        enableShades = FALSE,
        colorScale = list(
          ranges = l$labels
        ),
        useFillColorAsStroke = TRUE
      )
    ) |>
    apexcharter::ax_dataLabels(enabled = FALSE) |>
    apexcharter::ax_tooltip(
      enabled = FALSE,
      intersect = FALSE
    )

  if (!isTRUE(animation)) {
    out <- out |>
      apexcharter::ax_chart(animations = list(enabled = FALSE))
  }

  out
}



#' Ggplot2 data summary visualisation based on visdat::vis_dat.
#'
#' @param data data
#' @param ... optional arguments passed to data_summary_gather()
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' data_demo <- mtcars
#' data_demo[sample(1:32, 10), "cyl"] <- NA
#' data_demo[sample(1:32, 8), "vs"] <- NA
#' visual_summary(data_demo)
#' visual_summary(data_demo, palette.fun = scales::hue_pal())
#' visual_summary(dplyr::storms)
#' visual_summary(dplyr::storms, summary.fun = data_type)
visual_summary <- function(data, legend.title = "Data class", ...) {
  l <- data_summary_gather(data, ...)

  df <- l$data

  df$valueType <- factor(df$valueType, levels = names(l$colors))
  df$variable <- factor(df$variable, levels = unique_short(names(data)))

  ggplot2::ggplot(data = df, ggplot2::aes(x = variable, y = rows)) +
    ggplot2::geom_raster(ggplot2::aes(fill = valueType)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 45,
      vjust = 1, hjust = 1
    )) +
    ggplot2::scale_fill_manual(values = l$colors) +
    ggplot2::labs(x = "", y = "Observations") +
    ggplot2::scale_y_reverse() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::guides(colour = "none") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title)) +
    # change the limits etc.
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Type")) +
    # add info about the axes
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0)) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 18),
      plot.title = ggplot2::element_blank()
    )
}

#' Data summary for printing visual summary
#'
#' @param data data.frame
#' @param fun summary function. Default is "class"
#' @param palette.fun optionally use specific palette functions. First argument
#' has to be the length.
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' mtcars |> data_summary_gather()
data_summary_gather <- function(data, summary.fun = class, palette.fun = viridisLite::viridis) {
  df_plot <- setNames(data, unique_short(names(data))) |>
    purrr::map_df(\(x){
      ifelse(is.na(x),
        yes = NA,
        no = glue::glue_collapse(summary.fun(x),
          sep = "\n"
        )
      )
    }) |>
    dplyr::mutate(rows = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = -rows,
      names_to = "variable", values_to = "valueType", values_transform = list(valueType = as.character)
    ) |>
    dplyr::arrange(rows, variable, valueType)


  df_plot$valueType_num <- df_plot$valueType |>
    forcats::as_factor() |>
    as.numeric()

  df_plot$valueType[is.na(df_plot$valueType)] <- "NA"
  df_plot$valueType_num[is.na(df_plot$valueType_num)] <- max(df_plot$valueType_num, na.rm = TRUE) + 1

  labels <- setNames(unique(df_plot$valueType_num), unique(df_plot$valueType)) |> sort()

  if (any(df_plot$valueType == "NA")) {
    colors <- setNames(c(palette.fun(length(labels) - 1), "#999999"), names(labels))
  } else {
    colors <- setNames(palette.fun(length(labels)), names(labels))
  }


  label_list <- labels |>
    purrr::imap(\(.x, .i){
      list(
        from = .x,
        to = .x,
        color = colors[[.i]],
        name = .i
      )
    }) |>
    setNames(NULL)

  list(data = df_plot, colors = colors, labels = label_list)
}



#' Create unique short names of character vector items based on index
#'
#' @description
#' The function will prefer original names, and only append index to long
#' strings.
#'
#'
#' @param data character vector
#' @param max maximum final name length
#'
#' @returns character vector
#' @export
#'
#' @examples
#' c("kahdleidnsallskdj", "hej") |> unique_short()
unique_short <- function(data, max = 15) {
  purrr::imap(data, \(.x, .i){
    if (nchar(.x) > max) {
      glue::glue("{substr(.x,1,(max-(nchar(.i)+1)))}_{.i}")
    } else {
      .x
    }
  }) |> unlist()
}


########
#### Current file: /Users/au301842/FreesearchR/R//wide2long.R
########

#' Alternative pivoting method for easily pivoting based on name pattern
#'
#' @description
#' This function requires and assumes a systematic naming of variables.
#' For now only supports one level pivoting. Adding more levels would require
#' an added "ignore" string pattern or similarly. Example 2.
#'
#'
#' @param data data
#' @param pattern pattern(s) to match. Character vector of length 1 or more.
#' @param type type of match. can be one of "prefix","infix" or "suffix".
#' @param id.col ID column. Will fill ID for all. Column name or numeric index.
#' Default is "1", first column.
#' @param instance.name
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' data.frame(
#'   1:20, sample(70:80, 20, TRUE),
#'   sample(70:100, 20, TRUE),
#'   sample(70:100, 20, TRUE),
#'   sample(170:200, 20, TRUE)
#' ) |>
#'   setNames(c("id", "age", "weight_0", "weight_1", "height_1")) |>
#'   wide2long(pattern = c("_0", "_1"), type = "suffix")
#' data.frame(
#'   1:20, sample(70:80, 20, TRUE),
#'   sample(70:100, 20, TRUE),
#'   sample(70:100, 20, TRUE),
#'   sample(170:200, 20, TRUE)
#' ) |>
#'   setNames(c("id", "age", "weight_0", "weight_a_1", "height_b_1")) |>
#'   wide2long(pattern = c("_0", "_1"), type = "suffix")
#' # Optional filling of missing values by last observation carried forward
#' # Needed for mmrm analyses
#' long_missings |>
#'   # Fills record ID assuming none are missing
#'   tidyr::fill(record_id) |>
#'   # Grouping by ID for the last step
#'   dplyr::group_by(record_id) |>
#'   # Filling missing data by ID
#'   tidyr::fill(names(long_missings)[!names(long_missings) %in% new_names]) |>
#'   # Remove grouping
#'   dplyr::ungroup()
wide2long <- function(
    data,
    pattern,
    type = c("prefix", "infix", "suffix"),
    id.col = 1,
    instance.name = "instance") {
  type <- match.arg(type)

  ## Give the unique suffix names to use for identifying repeated measures
  # suffixes <- c("_0", "_1")

  ## If no ID column is present, one is added
  if (id.col == "none" | is.null(id.col)) {
    data <- stats::setNames(
      data.frame(seq_len(nrow(data)), data),
      make.names(c("id", names(data)), unique = TRUE)
    )
    id.col <- 1
  }
# browser()
  ## Relevant columns are determined based on suffixes
  cols <- names(data)[grepl_fix(names(data), pattern = pattern, type = type)]

  ## New colnames are created by removing suffixes
  new_names <- unique(gsub(paste(pattern, collapse = "|"), "", cols))

  out <- split(data, seq_len(nrow(data))) |> # Splits dataset by row
    # Starts data modifications for each subject
    lapply(\(.x){
      ## Pivots data with repeated measures as determined by the defined suffixes
      long_ls <- split.default(
        # Subset only repeated data
        .x[cols],
        # ... and split by meassure
        gsub(paste(new_names, collapse = "|"), "", cols)
      ) |>
        # Sort data by order of given suffixes to ensure chronology
        sort_by(pattern) |>
        # New colnames are applied
        lapply(\(.y){
          setNames(
            .y,
            gsub(paste(pattern, collapse = "|"), "", names(.y))
          )
        })

      # Subsets non-pivotted data (this is assumed to belong to same )
      single <- .x[-match(cols, names(.x))]

      # Extends with empty rows to get same dimensions as long data
      single[(nrow(single) + 1):length(long_ls), ] <- NA

      # Fills ID col
      single[id.col] <- single[1, id.col]

      # Everything is merged together
      merged <- dplyr::bind_cols(
        single,
        # Instance names are defined as suffixes without leading non-characters
        REDCapCAST::as_factor(data.frame(gsub(
          "^[^[:alnum:]]+", "",
          names(long_ls)
        ))),
        dplyr::bind_rows(long_ls)
      )

      # Ensure unique new names based on supplied
      colnames(merged) <- make.names(
        c(
          names(single),
          instance.name,
          names(merged)[(NCOL(single) + 2):NCOL(merged)]
        ),
        unique = TRUE
      )

      merged
    }) |> dplyr::bind_rows()

  rownames(out) <- NULL

  out
}


#' Matches pattern to vector based on match type
#'
#' @param data vector
#' @param pattern pattern(s) to match. Character vector of length 1 or more.
#' @param type type of match. can be one of "prefix","infix" or "suffix".
#'
#' @returns logical vector
#' @export
#'
#' @examples
#' c("id", "age", "weight_0", "weight_1") |> grepl_fix(pattern = c("_0", "_1"), type = "suffix")
grepl_fix <- function(data, pattern, type = c("prefix", "infix", "suffix")) {
  type <- match.arg(type)

  if (type == "prefix") {
    grepl(paste0("^(", paste(pattern, collapse = "|"), ")*"), data)
  } else if (type == "suffix") {
    grepl(paste0("*(", paste(pattern, collapse = "|"), ")$"), data)
  } else if (type == "infix") {
    grepl(paste0("*(", paste(pattern, collapse = "|"), ")*"), data)
  }
}


########
#### Current file: /Users/au301842/FreesearchR/dev/header_include.R
########

header_include <- function(){
  shiny::tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src="scripts.js"))
}


########
#### Current file: /Users/au301842/FreesearchR/dev/dev_banner.R
########

dev_banner <- function(){
    NULL
      }


########
#### Current file: /Users/au301842/FreesearchR/app/ui.R
########

# ns <- NS(id)




# Initial attempt at creating light and dark versions
light <- custom_theme()
dark <- custom_theme(
  bg = "#000",
  fg = "#fff"
)

# Fonts to consider:
# https://webdesignerdepot.com/17-open-source-fonts-youll-actually-love/

ui <- bslib::page_fixed(
  ## Code formatting dependencies
  prismDependencies,
  prismRDependency,
  ## Version dependent header
  header_include(),
  ## This adds the actual favicon
  ## png and ico versions are kept for compatibility
  shiny::tags$head(tags$link(rel = "shortcut icon", href = "favicon.svg")),
  title = "FreesearchR",
  theme = light,
  shiny::useBusyIndicators(),
  shinyjs::useShinyjs(),
  shiny::div(
    id = "loading_page",
    # shiny::h1("Loading the FreesearchR app..."),
    shinybusy::add_busy_spinner(position = "full-page")
  ),
  shinyjs::hidden(
    shiny::div(
      id = "main_content",
      bslib::page_navbar(
        id = "main_panel",
        ui_elements("home"),
        ui_elements("import"),
        ui_elements("prepare"),
        ui_elements("describe"),
        ui_elements("visuals"),
        ui_elements("analyze"),
        ui_elements("download"),
        bslib::nav_spacer(),
        # ui_elements$feedback,
        # ui_elements$docs,
        fillable = FALSE,
        footer = shiny::tags$footer(
          style = "background-color: #14131326; padding: 4px; text-align: center; bottom: 0; width: 100%;",
          shiny::p(
            style = "margin: 1",
            "Data is only stored for analyses and deleted when the app is closed.", shiny::markdown("Consider [running ***FreesearchR*** locally](https://agdamsbo.github.io/FreesearchR/#run-locally-on-your-own-machine) if working with sensitive data.")
          ),
          shiny::p(
            style = "margin: 1; color: #888;",
            shiny::tags$a("Documentation", href = "https://agdamsbo.github.io/FreesearchR/", target = "_blank", rel = "noopener noreferrer"), " | ", hosted_version(), " | ", shiny::tags$a("License: AGPLv3", href = "https://github.com/agdamsbo/FreesearchR/blob/main/LICENSE.md", target = "_blank", rel = "noopener noreferrer"), " | ", shiny::tags$a("Source", href = "https://github.com/agdamsbo/FreesearchR/", target = "_blank", rel = "noopener noreferrer"), " | ", shiny::tags$a("Share feedback", href = "https://redcap.au.dk/surveys/?s=JPCLPTXYDKFA8DA8", target = "_blank", rel = "noopener noreferrer")
          ),
        )
      )
    )
  )
)


########
#### Current file: /Users/au301842/FreesearchR/app/server.R
########

data(mtcars)

# trial <- gtsummary::trial
# starwars <- dplyr::starwars
#
# mtcars_na <- rbind(mtcars,NA,NA)

# thematic::thematic_shiny()

load_data <- function() {
  Sys.sleep(1)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}

# is_local = is.na(Sys.getenv('SHINY_SERVER_VERSION', NA))

server <- function(input, output, session) {
  ## Listing files in www in session start to keep when ending and removing
  ## everything else.
  files.to.keep <- list.files("www/")

  load_data()

  ##############################################################################
  #########
  #########  Night mode (just very popular, not really needed)
  #########
  ##############################################################################

  # observeEvent(input$dark_mode,{
  #   session$setCurrentTheme(
  #   if (isTRUE(input$dark_mode)) dark else light
  # )})

  # observe({
  #   if(input$dark_mode==TRUE)
  #     session$setCurrentTheme(bs_theme_update(theme = custom_theme(version = 5)))
  #   if(input$dark_mode==FALSE)
  #     session$setCurrentTheme(bs_theme_update(theme = custom_theme(version = 5, bg = "#000",fg="#fff")))
  # })


  ##############################################################################
  #########
  #########  Setting reactive values
  #########
  ##############################################################################

  rv <- shiny::reactiveValues(
    list = list(),
    regression = NULL,
    missings = NULL,
    ds = NULL,
    local_temp = NULL,
    ready = NULL,
    test = "no",
    data_original = NULL,
    data_temp = NULL,
    data = NULL,
    data_variables = NULL,
    data_filtered = NULL,
    models = NULL,
    code = list()
  )

  ##############################################################################
  #########
  #########  Data import section
  #########
  ##############################################################################

  data_file <- import_file_server(
    id = "file_import",
    show_data_in = "popup",
    trigger_return = "change",
    return_class = "data.frame"
  )

  shiny::observeEvent(data_file$data(), {
    shiny::req(data_file$data())
    rv$data_temp <- data_file$data()
    rv$code <- modifyList(x = rv$code, list(import = data_file$code()))
  })

  from_redcap <- m_redcap_readServer(
    id = "redcap_import"
  )

  shiny::observeEvent(from_redcap$data(), {
    rv$data_temp <- from_redcap$data()
    rv$code <- modifyList(x = rv$code, list(import = from_redcap$code()))
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
    rv$code <- modifyList(x = rv$code, list(import = from_env$name()))
  })

  visual_summary_server(
    id = "initial_summary",
    data_r = shiny::reactive({
      shiny::req(rv$data_temp)
      default_parsing(rv$data_temp)
    }),
    palette.fun = FreesearchR_palette
  )

  observeEvent(input$modal_initial_view, {
    tryCatch(
      {
        modal_visual_summary(
          id = "initial_summary",
          footer = NULL,
          size = "xl"
        )
      },
      error = function(err) {
        showNotification(paste0("We encountered the following error showing missingness: ", err), type = "err")
      }
    )
  })

  output$import_var <- shiny::renderUI({
    shiny::req(rv$data_temp)

    preselect <- names(rv$data_temp)[sapply(rv$data_temp, missing_fraction) <= (input$complete_cutoff / 100)]

    shinyWidgets::virtualSelectInput(
      inputId = "import_var",
      label = "Select variables to include",
      selected = preselect,
      choices = names(rv$data_temp),
      updateOn = "change",
      multiple = TRUE,
      search = TRUE,
      showValueAsTags = TRUE
    )
  })

  output$data_loaded <- shiny::reactive({
    !is.null(rv$data_temp)
  })

  shiny::observeEvent(input$source, {
    rv$data_temp <- NULL
  })

  shiny::outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  shiny::observeEvent(
    eventExpr = list(
      input$import_var,
      input$complete_cutoff,
      rv$data_temp
    ),
    handlerExpr = {
      shiny::req(rv$data_temp)
      shiny::req(input$import_var)
      # browser()
      temp_data <- rv$data_temp
      if (all(input$import_var %in% names(temp_data))) {
        temp_data <- temp_data |> dplyr::select(input$import_var)
      }

      rv$data_original <- temp_data |>
        default_parsing()

      rv$code$import <- rv$code$import |>
        expression_string(assign.str = "df <-")

      rv$code$format <- list(
        "df",
        rlang::expr(dplyr::select(dplyr::all_of(!!input$import_var))),
        rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
      ) |>
        lapply(expression_string) |>
        pipe_string() |>
        expression_string(assign.str = "df <-")

      rv$code$filter <- NULL
      rv$code$modify <- NULL
    }, ignoreNULL = FALSE
  )

  output$data_info_import <- shiny::renderUI({
    shiny::req(rv$data_original)
    data_description(rv$data_original)
  })

  ## Activating action buttons on data imported
  shiny::observeEvent(rv$data_original, {
    if (is.null(rv$data_original) | NROW(rv$data_original) == 0) {
      shiny::updateActionButton(inputId = "act_start", disabled = TRUE)
      shiny::updateActionButton(inputId = "modal_browse", disabled = TRUE)
      shiny::updateActionButton(inputId = "modal_visual_overview", disabled = TRUE)
      shiny::updateActionButton(inputId = "act_eval", disabled = TRUE)

      bslib::nav_hide(id = "main_panel",
                        target = "nav_visuals")
    } else {
      shiny::updateActionButton(inputId = "act_start", disabled = FALSE)
      shiny::updateActionButton(inputId = "modal_browse", disabled = FALSE)
      shiny::updateActionButton(inputId = "modal_visual_overview", disabled = FALSE)
      shiny::updateActionButton(inputId = "act_eval", disabled = FALSE)

      bslib::nav_show(id = "main_panel",
                      target = "nav_visuals")


    }
  })

  ##############################################################################
  #########
  #########  Data modification section
  #########
  ##############################################################################

  shiny::observeEvent(
    eventExpr = list(
      rv$data_original
    ),
    handlerExpr = {
      shiny::req(rv$data_original)

      rv$data <- rv$data_original
    }
  )

  ## For now this solution work, but I would prefer to solve this with the above
  shiny::observeEvent(input$reset_confirm,
    {
      if (isTRUE(input$reset_confirm)) {
        shiny::req(rv$data_original)
        rv$data <- rv$data_original
        rv$code$filter <- NULL
        rv$code$variables <- NULL
        rv$code$modify <- NULL
      }
    },
    ignoreNULL = TRUE
  )


  shiny::observeEvent(input$data_reset, {
    shinyWidgets::ask_confirmation(
      cancelOnDismiss = TRUE,
      inputId = "reset_confirm",
      title = "Please confirm data reset?",
      type = "warning"
    )
  })

  #########
  #########  Modifications
  #########

  ## Using modified version of the datamods::cut_variable_server function
  ## Further modifications are needed to have cut/bin options based on class of variable
  ## Could be defined server-side

  output$data_info <- shiny::renderUI({
    shiny::req(data_filter())
    data_description(data_filter(), "The filtered data")
  })

  #########  Create factor

  shiny::observeEvent(
    input$modal_cut,
    modal_cut_variable("modal_cut", title = "Create new factor")
  )

  data_modal_cut <- cut_variable_server(
    id = "modal_cut",
    data_r = shiny::reactive(rv$data)
  )

  shiny::observeEvent(data_modal_cut(), {
    rv$data <- data_modal_cut()
    rv$code$modify[[length(rv$code$modify) + 1]] <- attr(rv$data, "code")
  })

  #########  Modify factor

  shiny::observeEvent(
    input$modal_update,
    datamods::modal_update_factor(id = "modal_update", title = "Reorder factor levels")
  )

  data_modal_update <- datamods::update_factor_server(
    id = "modal_update",
    data_r = reactive(rv$data)
  )

  shiny::observeEvent(data_modal_update(), {
    shiny::removeModal()
    rv$data <- data_modal_update()
    rv$code$modify[[length(rv$code$modify) + 1]] <- attr(rv$data, "code")
  })

  #########  Create column

  shiny::observeEvent(
    input$modal_column,
    modal_create_column(
      id = "modal_column",
      footer = shiny::markdown("This window is aimed at advanced users and require some *R*-experience!"),
      title = "Create new variables"
    )
  )
  data_modal_r <- create_column_server(
    id = "modal_column",
    data_r = reactive(rv$data)
  )
  shiny::observeEvent(
    data_modal_r(),
    {
      rv$data <- data_modal_r()
      rv$code$modify[[length(rv$code$modify) + 1]] <- attr(rv$data, "code")
    }
  )

  #########  Subset, rename, reclass

  updated_data <- update_variables_server(
    id = "modal_variables",
    data = shiny::reactive(rv$data),
    return_data_on_init = FALSE
  )

  shiny::observeEvent(updated_data(), {
    rv$data <- updated_data()
    rv$code$modify[[length(rv$code$modify) + 1]] <- attr(rv$data, "code")
  })

  ### Column filter
  ### Completely implemented, but it takes a little considering where in the
  ### data flow to implement, as it will act destructively on previous
  ### manipulations

  output$column_filter <- shiny::renderUI({
    shiny::req(rv$data)
    # c("dichotomous", "ordinal", "categorical", "datatime", "continuous")
    shinyWidgets::virtualSelectInput(
      inputId = "column_filter",
      label = "Select data types to include",
      selected = unique(data_type(rv$data)),
      choices = unique(data_type(rv$data)),
      updateOn = "change",
      multiple = TRUE,
      search = FALSE,
      showValueAsTags = TRUE
    )
  })

  shiny::observe({
    # shiny::req(input$column_filter)
    out <- data_type_filter(rv$data, input$column_filter)
    rv$data_variables <- out
    if (!is.null(input$column_filter)) {
      rv$code$variables <- attr(out, "code")
    }
    # rv$code$modify[[length(rv$code$modify) + 1]] <- attr(rv$data, "code")
  })


  #########  Data filter
  # IDEAFilter has the least cluttered UI, but might have a License issue
  # Consider using shinyDataFilter, though not on CRAN
  data_filter <- IDEAFilter::IDEAFilter("data_filter",
    data = shiny::reactive(rv$data_variables),
    verbose = TRUE
  )

  shiny::observeEvent(
    list(
      shiny::reactive(rv$data_variables),
      shiny::reactive(rv$data_original),
      data_filter(),
      # regression_vars(),
      input$complete_cutoff
    ),
    {
      ###  Save filtered data
      rv$data_filtered <- data_filter()

      ###  Save filtered data
      ###  without empty factor levels
      rv$list$data <- data_filter() |>
        REDCapCAST::fct_drop() |>
        (\(.x){
          .x[!sapply(.x, is.character)]
        })()

      ## This looks messy!! But it works as intended for now

      out <- gsub(
        "filter", "dplyr::filter",
        gsub(
          "\\s{2,}", " ",
          paste0(
            capture.output(attr(rv$data_filtered, "code")),
            collapse = " "
          )
        )
      )

      out <- strsplit(out, "%>%") |>
        unlist() |>
        (\(.x){
          paste(c("df <- df", .x[-1], "REDCapCAST::fct_drop()"),
            collapse = "|> \n "
          )
        })()

      rv$code <- append_list(data = out, list = rv$code, index = "filter")
    }
  )

  #########  Data preview

  ###  Overview

  data_summary_server(
    id = "data_summary",
    data = shiny::reactive({
      rv$data_filtered
    }),
    color.main = "#2A004E",
    color.sec = "#C62300",
    pagination = 10
  )

  observeEvent(input$modal_browse, {
    tryCatch(
      {
        show_data(REDCapCAST::fct_drop(rv$data_filtered), title = "Uploaded data overview", type = "modal")
      },
      error = function(err) {
        showNotification(paste0("We encountered the following error browsing your data: ", err), type = "err")
      }
    )
  })

  visual_summary_server(
    id = "visual_overview",
    data_r = shiny::reactive({
      shiny::req(rv$data_filtered)
      REDCapCAST::fct_drop(rv$data_filtered)
    }),
    palette.fun = FreesearchR_palette
  )

  observeEvent(input$modal_visual_overview, {
    tryCatch(
      {
        modal_visual_summary(
          id = "visual_overview",
          footer = "Here is an overview of how your data is interpreted, and where data is missing. Use this information to consider if data is missing at random or if some observations are missing systematically wich may be caused by an observation bias.",
          size = "xl"
        )
      },
      error = function(err) {
        showNotification(paste0("We encountered the following error showing missingness: ", err), type = "err")
      }
    )
  })

  output$original_str <- renderPrint({
    str(rv$data_original)
  })

  output$modified_str <- renderPrint({
    str(as.data.frame(rv$data_filtered) |>
      REDCapCAST::set_attr(
        label = NULL,
        attr = "code"
      ))
  })

  ## Evaluation table/plots reset on data change
  ## This does not work (!?)
  shiny::observeEvent(
    list(
      rv$data_filtered
    ),
    {
      shiny::req(rv$data_filtered)

      rv$list$table1 <- NULL
    }
  )


  ##############################################################################
  #########
  #########  Code export
  #########
  ##############################################################################

  ## This really should be collapsed to only one call, but I'll leave it for now
  ## as a working example of dynamically defining outputs and rendering.

  # output$code_import <- shiny::renderPrint({
  #   shiny::req(rv$code$import)
  #   cat(c("#Data import\n", rv$code$import))
  # })

  output$code_import <- shiny::renderUI({
    shiny::req(rv$code$import)
    prismCodeBlock(paste0("#Data import\n", rv$code$import))
  })

  output$code_format <- shiny::renderUI({
    shiny::req(rv$code$format)
    prismCodeBlock(paste0("#Data import formatting\n", rv$code$format))
  })

  output$code_data <- shiny::renderUI({
    shiny::req(rv$code$modify)
    # browser()
    ## This will create three lines for each modification
    # ls <- rv$code$modify
    ## This will remove all non-unique entries
    # ls <- rv$code$modify |> unique()
    ## This will only remove all non-repeating entries
    ls <- rv$code$modify[!is_identical_to_previous(rv$code$modify)]

    out <- ls |>
      lapply(expression_string) |>
      pipe_string() |>
      expression_string(assign.str = "df <- df |>\n")

    prismCodeBlock(paste0("#Data modifications\n", out))
  })

  output$code_variables <- shiny::renderUI({
    shiny::req(rv$code$variables)
    out <- expression_string(rv$code$variables, assign.str = "df <- df |>\n")
    prismCodeBlock(paste0("#Variables filter\n", out))
  })

  output$code_filter <- shiny::renderUI({
    shiny::req(rv$code$filter)
    prismCodeBlock(paste0("#Data filter\n", rv$code$filter))
  })

  output$code_table1 <- shiny::renderUI({
    shiny::req(rv$code$table1)
    prismCodeBlock(paste0("#Data characteristics table\n", rv$code$table1))
  })


  ## Just a note to self
  ## This is a very rewarding couple of lines marking new insights to dynamically rendering code
  shiny::observe({
    shiny::req(rv$regression)
    rv$regression()$regression$models |> purrr::imap(\(.x, .i){
      output[[paste0("code_", tolower(.i))]] <- shiny::renderUI({
        prismCodeBlock(paste0(paste("#", .i, "regression model\n"), .x$code_table))
      })
    })
  })


  ##############################################################################
  #########
  #########  Data analyses Inputs
  #########
  ##############################################################################

  output$strat_var <- shiny::renderUI({
    columnSelectInput(
      inputId = "strat_var",
      selected = "none",
      label = "Select variable to stratify baseline",
      data = shiny::reactive(rv$data_filtered)(),
      col_subset = c(
        "none",
        names(rv$data_filtered)[unlist(lapply(rv$data_filtered, data_type)) %in% c("dichotomous", "categorical", "ordinal")]
      )
    )
  })

  ##############################################################################
  #########
  #########  Descriptive evaluations
  #########
  ##############################################################################


  output$data_info_nochar <- shiny::renderUI({
    shiny::req(rv$list$data)
    data_description(rv$list$data, data_text = "The dataset without text variables")
  })

  shiny::observeEvent(
    list(
      input$act_eval
    ),
    {
      shiny::req(input$strat_var)
      shiny::req(rv$list$data)

      parameters <- list(
        by.var = input$strat_var,
        add.p = input$add_p == "yes",
        add.overall = TRUE
      )

      shiny::withProgress(message = "Creating the table. Hold on for a moment..", {
        rv$list$table1 <- rlang::exec(create_baseline, !!!append_list(rv$list$data, parameters, "data"))
      })

      rv$code$table1 <- glue::glue("FreesearchR::create_baseline(df,{list2str(parameters)})")
    }
  )

  output$table1 <- gt::render_gt({
    if (!is.null(rv$list$table1)) {
      rv$list$table1 |>
        gtsummary::as_gt() |>
        gt::tab_header(gt::md("**Table 1: Baseline Characteristics**"))
    } else {
      return(NULL)
    }
  })

  output$outcome_var_cor <- shiny::renderUI({
    columnSelectInput(
      inputId = "outcome_var_cor",
      selected = "none",
      data = rv$list$data,
      label = "Select outcome variable",
      col_subset = c(
        "none",
        colnames(rv$list$data)
      ),
      multiple = FALSE
    )
  })

  data_correlations_server(
    id = "correlations",
    data = shiny::reactive({
      shiny::req(rv$list$data)
      out <- rv$list$data
      if (!is.null(input$outcome_var_cor) && input$outcome_var_cor != "none") {
        out <- out[!names(out) %in% input$outcome_var_cor]
      }
      out
    }),
    cutoff = shiny::reactive(input$cor_cutoff)
  )

  output$missings_var <- shiny::renderUI({
    columnSelectInput(
      inputId = "missings_var",
      label = "Select variable to stratify analysis",
      data = shiny::reactive({
        shiny::req(rv$data_filtered)
        rv$data_filtered[apply(rv$data_filtered, 2, anyNA)]
      })()
    )
  })

  rv$missings <- data_missings_server(
    id = "missingness",
    data = shiny::reactive(rv$data_filtered),
    variable = shiny::reactive(input$missings_var)
  )


  ##############################################################################
  #########
  #########  Data visuals
  #########
  ##############################################################################

  pl <- data_visuals_server("visuals", data = shiny::reactive(rv$list$data))

  ##############################################################################
  #########
  #########  Regression model analyses
  #########
  ##############################################################################

  rv$regression <- regression_server("regression", data = shiny::reactive(rv$list$data))

  ##############################################################################
  #########
  #########  Page navigation
  #########
  ##############################################################################

  shiny::observeEvent(input$act_start, {
    bslib::nav_select(id = "main_panel", selected = "nav_prepare_overview")
  })

  ##############################################################################
  #########
  #########  Reactivity
  #########
  ##############################################################################

  output$uploaded <- shiny::reactive({
    if (is.null(rv$ds)) {
      "no"
    } else {
      "yes"
    }
  })

  shiny::outputOptions(output, "uploaded", suspendWhenHidden = FALSE)

  output$ready <- shiny::reactive({
    if (is.null(rv$ready)) {
      "no"
    } else {
      "yes"
    }
  })

  shiny::outputOptions(output, "ready", suspendWhenHidden = FALSE)

  ##############################################################################
  #########
  #########  Downloads
  #########
  ##############################################################################

  # Could be rendered with other tables or should show progress
  # Investigate quarto render problems
  # On temp file handling: https://github.com/quarto-dev/quarto-cli/issues/3992
  output$report <- downloadHandler(
    filename = shiny::reactive({
      paste0("report.", input$output_type)
    }),
    content = function(file, type = input$output_type) {
      ## Notification is not progressing
      ## Presumably due to missing
      # Simplified for .rmd output attempt
      format <- ifelse(type == "docx", "word_document", "odt_document")

      rv$list$regression <- rv$regression()
      rv$list$missings <- rv$missings()

      shiny::withProgress(message = "Generating the report. Hold on for a moment..", {
        tryCatch(
          {
            rv$list |>
              write_rmd(
                params.args = list(
                  regression.p = rv$list$regression$input$add_regression_p
                ),
                output_format = format,
                input = file.path(getwd(), "www/report.rmd")
              )
          },
          error = function(err) {
            showNotification(paste0("We encountered the following error creating your report: ", err), type = "err")
          }
        )
      })
      file.rename(paste0("www/report.", type), file)
    }
  )

  output$data_modified <- downloadHandler(
    filename = shiny::reactive({
      paste0("modified_data.", input$data_type)
    }),
    content = function(file, type = input$data_type) {
      if (type == "rds") {
        readr::write_rds(rv$list$data, file = file)
      } else if (type == "dta") {
        haven::write_dta(as.data.frame(rv$list$data), path = file)
      } else if (type == "csv") {
        readr::write_csv(rv$list$data, file = file)
      }
    }
  )

  ##############################################################################
  #########
  #########  Clearing the session on end
  #########
  ##############################################################################

  session$onSessionEnded(function() {
    cat("Session Ended\n")
    files <- list.files("www/")
    lapply(files[!files %in% files.to.keep], \(.x){
      unlink(paste0("www/", .x), recursive = FALSE)
      print(paste(.x, "deleted"))
    })
  })
}


########
#### Current file: /Users/au301842/FreesearchR/app/launch.R
########

shinyApp(ui, server)
