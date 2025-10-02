
#' @title Import data from an Environment
#'
#' @description Let the user select a dataset from its own environment or from a package's environment.
#' Modified from datamods
#'
#' @param id Module's ID.
#' @param globalenv Search for data in Global environment.
#' @param packages Name of packages in which to search data.
#' @param title Module's title, if `TRUE` use the default title,
#'  use `NULL` for no title or a `shiny.tag` for a custom one.
#'
#' @export
#'
#' @name import-globalenv
#'
import_globalenv_ui <- function(id,
                                globalenv = TRUE,
                                packages = datamods::get_data_packages(),
                                title = TRUE) {

  ns <- NS(id)

  choices <- list()
  if (isTRUE(globalenv)) {
    choices <- append(choices, "Global Environment")
  }
  if (!is.null(packages)) {
    choices <- append(choices, list(Packages = as.character(packages)))
  }

  if (isTRUE(globalenv)) {
    selected <- "Global Environment"
  } else {
    selected <- packages[1]
  }

  if (isTRUE(title)) {
    title <- tags$h4(
      i18n$t("Import a dataset from an environment"),
      class = "datamods-title"
    )
  }

  tags$div(
    class = "datamods-import",
    datamods:::html_dependency_datamods(),
    title,
    shinyWidgets::pickerInput(
      inputId = ns("env"),
      label = i18n$t("Select a data source:"),
      choices = choices,
      selected = selected,
      width = "100%",
      options = list(
        "title" = i18n$t("Select source"),
        "live-search" = TRUE,
        "size" = 10
      )
    ),
    shinyWidgets::pickerInput(
      inputId = ns("data"),
      label = i18n$t("Select a dataset:"),
      # selected = character(0),
      choices = NULL,
      # options = list(title = i18n$t("List of datasets...")),
      width = "100%"
    ),

    tags$div(
      id = ns("import-placeholder"),
      shinyWidgets::alert(
        id = ns("import-result"),
        status = "info",
        tags$b(i18n$t("No data selected!")),
        i18n$t("Use a datasat from your environment or from the environment of a package."),
        dismissible = TRUE
      )
    ),
    uiOutput(
      outputId = ns("container_valid_btn"),
      style = "margin-top: 20px;"
    )
  )
}



#' @param btn_show_data Display or not a button to display data in a modal window if import is successful.
#' @param show_data_in Where to display data: in a `"popup"` or in a `"modal"` window.
#' @param trigger_return When to update selected data:
#'  `"button"` (when user click on button) or
#'  `"change"` (each time user select a dataset in the list).
#' @param return_class Class of returned data: `data.frame`, `data.table`, `tbl_df` (tibble) or `raw`.
#' @param reset A `reactive` function that when triggered resets the data.
#'
#' @export
#'
#' @importFrom shiny moduleServer reactiveValues observeEvent reactive removeUI is.reactive icon actionLink isTruthy
#' @importFrom htmltools tags tagList
#' @importFrom shinyWidgets updatePickerInput
#'
#' @rdname import-globalenv
import_globalenv_server <- function(id,
                                    btn_show_data = TRUE,
                                    show_data_in = c("popup", "modal"),
                                    trigger_return = c("button", "change"),
                                    return_class = c("data.frame", "data.table", "tbl_df", "raw"),
                                    reset = reactive(NULL)) {

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

    output$container_valid_btn <- renderUI({
      if (identical(trigger_return, "button")) {
        button_import()
      }
    })

    observeEvent(input$env, {
      if (identical(input$env, "Global Environment")) {
        choices <- datamods:::search_obj("data.frame")
      } else {
        choices <- datamods:::list_pkg_data(input$env)
      }
      if (is.null(choices)) {
        choices <- i18n$t("No dataset here...")
        choicesOpt <- list(disabled = TRUE)
      } else {
        choicesOpt <- list(
          subtext = datamods:::get_dimensions(choices)
        )
      }
      temporary_rv$package <- attr(choices, "package")
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "data",
        selected = character(0),
        choices = choices,
        choicesOpt = choicesOpt,
        options = list(title = i18n$t("List of datasets..."))
      )
    })

    observe(
      shinyWidgets::alert(
        id = "import-result",
        status = "info",
        tags$b(i18n$t("No data selected!")),
        i18n$t("Use a datasat from your environment or from the environment of a package."),
        dismissible = TRUE
      )
    )


    observeEvent(input$trigger, {
      if (identical(trigger_return, "change")) {
        datamods:::hideUI(selector = paste0("#", ns("container_valid_btn")))
      }
    })



    observeEvent(input$data, {
      if (!isTruthy(input$data)) {
        datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
        datamods:::insert_alert(
          selector = ns("import"),
          status = "info",
          tags$b(i18n$t("No data selected!")),
          i18n$t("Use a dataset from your environment or from the environment of a package.")
        )
      } else {
        name_df <- input$data

        if (!is.null(temporary_rv$package)) {
          attr(name_df, "package") <- temporary_rv$package
        }

        imported <- try(get_env_data(name_df), silent = TRUE)

        if (inherits(imported, "try-error") || NROW(imported) < 1) {
          datamods:::toggle_widget(inputId = "confirm", enable = FALSE)
          datamods:::insert_error(mssg = i18n$t(attr(imported, "condition")$message))
          temporary_rv$status <- "error"
          temporary_rv$data <- NULL
          temporary_rv$name <- NULL
        } else {
          datamods:::toggle_widget(inputId = "confirm", enable = TRUE)
          datamods:::insert_alert(
            selector = ns("import"),
            status = "success",
            datamods:::make_success_alert(
              imported,
              trigger_return = trigger_return,
              btn_show_data = btn_show_data
            )
          )
          pkg <- attr(name_df, "package")
          if (!is.null(pkg)) {
            name <- paste(pkg, input$data, sep = "::")
          } else {
            name <- input$data
          }
          name <- trimws(sub("\\(([^\\)]+)\\)", "", name))
          temporary_rv$status <- "success"
          temporary_rv$data <- imported
          temporary_rv$name <- name
        }
      }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)


    observeEvent(input$see_data, {
      show_data(temporary_rv$data, title = i18n$t("Imported data"), type = show_data_in)
    })

    observeEvent(input$confirm, {
      imported_rv$data <- temporary_rv$data
      imported_rv$name <- temporary_rv$name
    })


    if (identical(trigger_return, "button")) {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(imported_rv$name),
        data = reactive(datamods:::as_out(imported_rv$data, return_class))
      ))
    } else {
      return(list(
        status = reactive(temporary_rv$status),
        name = reactive(temporary_rv$name),
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


#' Get packages containing datasets
#'
#' @return a character vector of packages names
#' @export
#'
#' @importFrom utils data
#'
#' @examples
#' if (interactive()) {
#'
#'   get_data_packages()
#'
#' }
get_data_packages <- function() {
  suppressWarnings({
    pkgs <- data(package = .packages(all.available = TRUE))
  })
  unique(pkgs$results[, 1])
}


#' List dataset contained in a package
#'
#' @param pkg Name of the package, must be installed.
#'
#' @return a \code{character} vector or \code{NULL}.
#' @export
#'
#' @importFrom utils data
#'
#' @examples
#'
#' list_pkg_data("ggplot2")
list_pkg_data <- function(pkg) {
  if (isTRUE(requireNamespace(pkg, quietly = TRUE))) {
    list_data <- data(package = pkg, envir = environment())$results[, "Item"]
    list_data <- sort(list_data)
    attr(list_data, "package") <- pkg
    if (length(list_data) < 1) {
      NULL
    } else {
      unname(list_data)
    }
  } else {
    NULL
  }
}

#' @importFrom utils data
get_env_data <- function(obj, env = globalenv()) {
  pkg <- attr(obj, "package")
  re <- regexpr(pattern = "\\(([^\\)]+)\\)", text = obj)
  obj_ <- substr(x = obj, start = re + 1, stop = re + attr(re, "match.length") - 2)
  obj <- gsub(pattern = "\\s.*", replacement = "", x = obj)
  if (obj %in% ls(name = env)) {
    get(x = obj, envir = env)
  } else if (!is.null(pkg) && !identical(pkg, "")) {
    res <- suppressWarnings(try(
      get(utils::data(list = obj, package = pkg, envir = environment())), silent = TRUE
    ))
    if (!inherits(res, "try-error"))
      return(res)
    data(list = obj_, package = pkg, envir = environment())
    get(obj, envir = environment())
  } else {
    NULL
  }
}


get_dimensions <- function(objs) {
  if (is.null(objs))
    return(NULL)
  dataframes_dims <- Map(
    f = function(name, pkg) {
      attr(name, "package") <- pkg
      tmp <- suppressWarnings(get_env_data(name))
      if (is.data.frame(tmp)) {
        sprintf("%d obs. of  %d variables", nrow(tmp), ncol(tmp))
      } else {
        i18n$t("Not a data.frame")
      }
    },
    name = objs,
    pkg = if (!is.null(attr(objs, "package"))) {
      attr(objs, "package")
    } else {
      character(1)
    }
  )
  unlist(dataframes_dims)
}
