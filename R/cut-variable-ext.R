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
        # shiny::uiOutput(outputId = ns("variable"))
        shinyWidgets::virtualSelectInput(
          inputId = ns("variable"),
          label = i18n$t("Variable to cut:"),
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
          label = i18n$t("Number of breaks:"),
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
          label = i18n$t("Close intervals on the right"),
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("include_lowest"),
          label = i18n$t("Include lowest value"),
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
      label = tagList(phosphoricons::ph("scissors"), i18n$t("Create factor variable")),
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
          is.numeric(.x) || is_datetime(.x) || (is.factor(.x) && length(levels(.x)) > 2)
        }, logical(1))
        vars_num <- names(vars_num)[vars_num]

        # shiny::renderUI(
        # columnSelectInput(
        #   inputId = "variable",
        #   data = data,
        #   label = i18n$t("Variable to cut:"),
        #   width = "100%",
        #   choices = vars_num,
        #   selected = if (isTruthy(input$variable)) input$variable else vars_num[1]
        # ))

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

        if (is_datetime(data[[variable]]) || is.factor(data[[variable]])) {
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
          label = i18n$t("Fixed breaks:"),
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

        if (any(c("hms", "POSIXct") %in% class(data[[variable]]))) {
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
        } else if ("factor" %in% class(data[[variable]])) {
          choices <- c(
            choices,
            "top" # ,
            # "bottom",
          )
        } else {
          choices <- c(
            choices,
            "fixed",
            "quantile" # ,
            # "sd",
            # "equal",
            # "pretty",
            # "kmeans",
            # "hclust",
            # "bclust",
            # "fisher",
            # "jenks",
            # "headtails" # ,
            # "maximum",
            # "box"
          )
        }

        choices <- unique(choices)

        shinyWidgets::virtualSelectInput(
          inputId = session$ns("method"),
          label = i18n$t("Method:"),
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
        } else if (input$method %in% c(
          "top",
          "bottom"
        )) {
          # This allows factor simplification to get the top or bottom count
          f <- cut_var(data[[variable]], breaks = input$n_breaks)
          list(var = f, brks = input$n_breaks, type = input$method)
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


        if (input$method %in% c(
          "day",
          "weekday",
          "week",
          "month",
          "month_only",
          "quarter",
          "year",
          "hour"
        )
        ) {
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

        if ("type" %in% names(breaks_r())) {
          parameters <- modifyList(parameters, list(type = breaks_r()$type))
        }

        new_variable <- tryCatch(
          {
            rlang::exec(cut_var, !!!parameters)
          },
          error = function(err) {
            showNotification(paste("We encountered the following error creating the new factor:", err), type = "err")
          }
        )

        data <- append_column(data, column = new_variable, name = paste0(variable, "_cut"), index = "right")

        code <- rlang::call2(
          "append_column",
          !!!list(
            column = rlang::call2("cut_var",
              !!!modifyList(parameters, list(x = as.symbol(paste0("data$", variable)))),
              .ns = "FreesearchR"
            ),
            name = paste0(variable, "_cut"), index = "right"
          ),
          .ns = "FreesearchR"
        )
        attr(data, "code") <- code

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
                               title = i18n$t("Convert Numeric to Factor"),
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
plot_histogram <- function(data, column = NULL, bins = 30, breaks = NULL, color = "#112466") {
  if (is.vector(data)) {
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
