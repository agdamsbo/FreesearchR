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
          label = i18n$t("Factor variable to reorder:"),
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
            i18n$t("Sort by levels")
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
            i18n$t("Sort by count")
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
        label = i18n$t("Create a new variable (otherwise replaces the one selected)"),
        value = FALSE,
        status = "primary",
        outline = TRUE,
        inline = TRUE
      ),
      actionButton(
        inputId = ns("create"),
        label = tagList(phosphoricons::ph("arrow-clockwise"), i18n$t("Update factor variable")),
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
            i18n$t("Sort count")
          )
        } else {
          decreasing <- TRUE
          label <- tagList(
            phosphoricons::ph("sort-ascending"),
            i18n$t("Sort count")
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
          header = c(i18n$t("Levels"), "New label", i18n$t("Count"))
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

        parameters <- list(
          variable = variable,
          new_variable = isTRUE(input$new_var) | any(grid[["Var1_toset"]] == "New label"),
          new_levels = as.character(grid[["Var1"]]),
          new_labels = as.character(grid[["Var1_toset"]]),
          ignore = "New label"
        )

        data <- tryCatch(
          {
            rlang::exec(
              factor_new_levels_labels,
              !!!modifyList(parameters,
                val = list(data = data)
              )
            )
          },
          error = function(err) {
            showNotification(paste("We encountered the following error creating the new factor:", err), type = "err")
          }
        )

        # browser()
        code <- rlang::call2(
          "factor_new_levels_labels",
          !!!parameters,
          .ns = "FreesearchR"
        )
        attr(data, "code") <- code

        data
      })

      data_returned_r <- observeEvent(input$create, {
        rv$data <- data_updated_r()
      })
      return(reactive(rv$data))
    }
  )
}

#' Simple function to apply new levels and/or labels to factor
#'
#' @param variable factor variable
#' @param new_level new levels, same length as original
#' @param new_label new labels, same length as original
#' @param ignore character string to ignore in new labels
#'
#' @returns factor
#' @export
#'
#' @examples
#' data_n <- mtcars
#' data_n$cyl <- factor(data_n$cyl)
#' factor_new_levels_labels(data_n, "cyl", new_labels = c("four", "New label", "New label"))
factor_new_levels_labels <- function(
    data,
    variable,
    new_variable = TRUE,
    new_levels = NULL,
    new_labels = NULL,
    ignore = "New label") {
  if (!is.factor(data[[variable]])) {
    return(data)
  }

  if (is.null(new_levels)) {
    new_levels <- levels(data[[variable]])
  }

  if (is.null(new_labels)) {
    new_labels <- labels(data[[variable]])
  }

  with_level <- factor(
    as.character(data[[variable]]),
    levels = new_levels
  )
  with_label <- factor(
    with_level,
    labels = ifelse(new_labels == "New label", new_levels, new_labels)
  )

  # browser()

  if (isTRUE(new_variable)) {
    append_column(
      data = data,
      column = with_label,
      name = unique_names(new = paste0(variable, "_updated"), existing = names(data))
    )
  } else {
    data[[variable]] <- new_variable
    data
  }
}



#' @inheritParams shiny::modalDialog
#' @export
#'
#' @importFrom shiny showModal modalDialog textInput
#' @importFrom htmltools tagList
#'
#' @rdname update-factor
modal_update_factor <- function(id,
                                title = i18n$t("Update levels of a factor"),
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
                                 title = i18n$t("Update levels of a factor"),
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


#' Make unique variable names
#'
#' Helper function to create new variable names that are unique
#' given a set of existing names (in a data set, for example).
#' If a variable name already exists, a number will be appended.
#'
#' @param new a vector of proposed new variable names
#' @param existing a vector of existing variable names
#' @return a vector of unique new variable names
#' @examples
#' unique_names(c("var_x", "var_y", "var_x"), c("var_x", "var_z"))
#'
#' @export
unique_names <- function(new, existing = character()) {
  new_names <- make.unique(c(existing, new), sep = "_")

  new_names[-seq_along(existing)]
}
