

########
#### Current file: /Users/au301842/freesearcheR/inst/apps/data_analysis_modules/functions.R 
########



########
#### Current file: R//baseline_table.R 
########














baseline_table <- function(data, fun.args = NULL, fun = gtsummary::tbl_summary, vars = NULL) {
  if (!is.null(vars)) {
    data <- data |> dplyr::select(dplyr::all_of(vars))
  }

  out <- do.call(fun, c(list(data = data), fun.args))
  return(out)
}



########
#### Current file: R//cut-variable-dates.R 
########

library(datamods)
library(toastui)
library(phosphoricons)
library(rlang)
library(shiny)


# old_deprecated_cut.hms <- function(x, breaks = "hour", ...) {
#   # For now, this function will allways try to cut to hours
#   # This limits time cutting to only do hour-binning, no matter the
#
#   breaks_o <- breaks
#
#   if (identical(breaks, "hour")) {
#     # splitter <- match(
#     #   num,
#     #   levels(factor(num))
#     # )
#     breaks <- hms::as_hms(paste0(1:23, ":00:00"))
#   }
#
#   # if (identical(breaks, "daynight")) {
#   #   # splitter <- num %in% 8:20 + 1
#   #   breaks <- hms::as_hms(c("08:00:00","20:00:00"))
#   # }
#
#   if (length(breaks) != 1) {
#     if ("hms" %in% class(breaks)) {
#       splitter <- seq_along(breaks) |>
#         purrr::map(\(.x){
#           # browser()
#           out <- x %in% x[x >= breaks[.x] & x < breaks[.x + 1]]
#           if (.x == length(breaks)) {
#             out[match(breaks[length(breaks)], x)] <- TRUE
#           }
#           ifelse(out, .x, 0)
#         }) |>
#         dplyr::bind_cols(.name_repair = "unique_quiet") |>
#         rowSums()
#       splitter[splitter == 0] <- NA
#     } else {
#       breaks <- "hour"
#     }
#   }
#
#   if (is.numeric(breaks)) {
#     breaks_n <- quantile(x, probs = seq(0, 1, 1 / breaks))
#     ## Use lapply or similar to go through levels two at a time
#     splitter <- seq(breaks) |>
#       purrr::map(\(.x){
#         # browser()
#         out <- x %in% x[x >= breaks_n[.x] & x < breaks_n[.x + 1]]
#         if (.x == breaks) {
#           out[match(breaks_n[length(breaks_n)], x)] <- TRUE
#         }
#         ifelse(out, .x, 0)
#       }) |>
#       dplyr::bind_cols(.name_repair = "unique_quiet") |>
#       rowSums()
#   }
#
#   # browser()
#
#   num <- strsplit(as.character(x), ":") |>
#     lapply(\(.x).x[[1]]) |>
#     unlist() |>
#     as.numeric()
#
#   # browser()
#   labs <- split(x, splitter) |>
#     purrr::imap(\(.x, .i){
#       # if (identical(breaks_o, "daynight") && .i == 1) {
#       #   h <- hms::as_hms(hms::hms(hours = 24) - abs(.x - hms::hms(hours = 8)))
#       #
#       #   paste0("[", .x[match(sort(h)[1], h)], ",", .x[match(sort(h)[length(h)], h)], "]")
#       # } else {
#       .x <- sort(.x)
#       paste0("[", .x[1], ",", .x[length(.x)], "]")
#       # }
#     }) |>
#     unlist()
#
#   structure(match(splitter, names(labs)), levels = labs, class = "factor")
# }



















cut.hms <- function(x, breaks, ...) {
  if (hms::is_hms(breaks)) {
    breaks <- lubridate::as_datetime(breaks, tz = "UTC")
  }
  x <- lubridate::as_datetime(x, tz = "UTC")
  out <- cut.POSIXt(x, breaks = breaks, ...)
  attr(out, which = "brks") <- hms::as_hms(lubridate::as_datetime(attr(out, which = "brks")))
  attr(out, which = "levels") <- as.character(hms::as_hms(lubridate::as_datetime(attr(out, which = "levels"))))
  out
}








cut.POSIXt <- function(x, breaks, right = FALSE, include.lowest = TRUE, start.on.monday=TRUE, ...) {
  breaks_o <- breaks
  # browser()
  if (is.numeric(breaks)) {
    breaks <- quantile(
      x,
      probs = seq(0, 1, 1 / breaks),
      right = right,
      include.lowest = include.lowest,
      na.rm=TRUE
    )
  }

  if(identical(breaks,"weekday")){
    days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
              "Sunday")
    if (!start.on.monday){
      days <- days[c(7,1:6)]
    }
    out <- factor(weekdays(x),levels=days) |> forcats::fct_drop()
  } else if (identical(breaks,"month_only")){
    ms <- paste0("1970-",1:12,"-01") |> as.Date() |> months()

    out <- factor(months(x),levels=ms) |> forcats::fct_drop()
  } else {
  ## Doesn't really work very well for breaks other than the special character cases as right border is excluded
  out <- base::cut.POSIXt(x, breaks=breaks,right=right,...) |> forcats::fct_drop()
  # browser()
}
  l <- levels(out)
  if (is.numeric(breaks_o)) {
    l <- breaks
  } else if (is.character(breaks) && length(breaks) == 1 && !(identical(breaks,"weekday") | identical(breaks,"month_only"))) {
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



cut.POSIXct <- cut.POSIXt







cut.Date <- function(x,breaks,start.on.monday=TRUE,...){
  if(identical(breaks,"weekday")){
    days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
              "Sunday")
    if (!start.on.monday){
      days <- days[c(7,1:6)]
    }
    out <- factor(weekdays(x),levels=days) |> forcats::fct_drop()
  } else if (identical(breaks,"month_only")){
    ms <- paste0("1970-",1:12,"-01") |> as.Date() |> months()

    out <- factor(months(x),levels=ms) |> forcats::fct_drop()
  } else {
    ## Doesn't really work very well for breaks other than the special character cases as right border is excluded
    out <- base::cut.Date(x, breaks=breaks,...) |> forcats::fct_drop()
    # browser()
  }
  out
}















is_any_class <- function(data, class.vec) {
  any(class(data) %in% class.vec)
}










is_datetime <- function(data) {
  is_any_class(data, class.vec = c("hms", "Date", "POSIXct", "POSIXt"))
}


















cut_variable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      column(
        width = 3,
        virtualSelectInput(
          inputId = ns("variable"),
          label = i18n("Variable to cut:"),
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
          label = i18n("Number of breaks:"),
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
          label = i18n("Close intervals on the right"),
          value = TRUE
        ),
        checkboxInput(
          inputId = ns("include_lowest"),
          label = i18n("Include lowest value"),
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
    datagridOutput2(outputId = ns("count")),
    actionButton(
      inputId = ns("create"),
      label = tagList(ph("scissors"), i18n("Create factor variable")),
      class = "btn-outline-primary float-end"
    ),
    tags$div(class = "clearfix")
  )
}












cut_variable_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(data = NULL)

      bindEvent(observe({
        data <- data_r()
        rv$data <- data
        vars_num <- vapply(data, \(.x){
          is.numeric(.x) || is_datetime(.x)
        }, logical(1))
        vars_num <- names(vars_num)[vars_num]
        updateVirtualSelect(
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
          brks <- cut(data[[variable]],
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


        noUiSliderInput(
          inputId = session$ns("fixed_brks"),
          label = i18n("Fixed breaks:"),
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
          # "quantile"
          )

        if ("hms" %in% class(data[[variable]])) {
          choices <- c(choices, "hour")
        } else if (any(c("POSIXt","Date") %in% class(data[[variable]]))) {
          choices <- c(
            choices,
            "day",
            "weekday",
            "week",
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

        shinyWidgets::virtualSelectInput(
          inputId = session$ns("method"),
          label = i18n("Method:"),
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
          if (any(c("hms", "POSIXt") %in% class(data[[variable]]))) {
            cut.POSIXct <- cut.POSIXt
            f <- cut(data[[variable]], breaks = input$fixed_brks)
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
            cut.POSIXct <- cut.POSIXt
            f <- cut(data[[variable]], breaks = input$n_breaks)
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
          cut.POSIXct <- cut.POSIXt
          f <- cut(data[[variable]], breaks = input$method)
          list(var = f, brks = levels(f))
        } else if (input$method %in% c("hour")) {
          # To enable datetime cutting
          cut.POSIXct <- cut.POSIXt
          f <- cut(data[[variable]], breaks = "hour")
          list(var = f, brks = levels(f))
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
      })


      data_cutted_r <- reactive({
        data <- req(data_r())
        variable <- req(input$variable)
        data[[paste0(variable, "_cut")]] <- cut(
          x = data[[variable]],
          breaks = if (input$method %in% c("day", "weekday", "week", "month", "month_only", "quarter", "year", "hour")) input$method else breaks_r()$brks,
          include.lowest = input$include_lowest,
          right = input$right
        )
        code <- call2(
          "mutate",
          !!!set_names(
            list(
              expr(cut(
                !!!syms(list(x = variable)),
                !!!list(breaks = breaks_r()$brks, include.lowest = input$include_lowest, right = input$right)
              ))
            ),
            paste0(variable, "_cut")
          )
        )
        attr(data, "code") <- Reduce(
          f = function(x, y) expr(!!x %>% !!y),
          x = c(attr(data, "code"), code)
        )
        data
      })

      output$count <- renderDatagrid2({
        data <- req(data_cutted_r())
        variable <- req(input$variable)
        count_data <- as.data.frame(
          table(
            breaks = data[[paste0(variable, "_cut")]],
            useNA = "ifany"
          ),
          responseName = "count"
        )
        gridTheme <- getOption("datagrid.theme")
        if (length(gridTheme) < 1) {
          datamods:::apply_grid_theme()
        }
        on.exit(toastui::reset_grid_theme())
        grid <- datagrid(
          data = count_data,
          colwidths = "guess",
          theme = "default",
          bodyHeight = "auto"
        )
        grid <- toastui::grid_columns(grid, className = "font-monospace")
        grid_colorbar(
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










modal_cut_variable <- function(id,
                               title = i18n("Convert Numeric to Factor"),
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








winbox_cut_variable <- function(id,
                                title = i18n("Convert Numeric to Factor"),
                                options = shinyWidgets::wbOptions(),
                                controls = shinyWidgets::wbControls()) {
  ns <- NS(id)
  WinBox(
    title = title,
    ui = tagList(
      cut_variable_ui(id),
      tags$div(
        style = "display: none;",
        textInput(inputId = ns("hidden"), label = NULL, value = genId())
      )
    ),
    options = modifyList(
      shinyWidgets::wbOptions(height = "750px", modal = TRUE),
      options
    ),
    controls = controls,
    auto_height = FALSE
  )
}



plot_histogram <- function(data, column, bins = 30, breaks = NULL, color = "#112466") {
  x <- data[[column]]
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
#### Current file: R//data-summary.R 
########

data_summary_ui <- function(id) {
  ns <- NS(id)

  toastui::datagridOutput(outputId = "tbl_summary")
}


data_summary_server <- function(id,
                                data) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      data_r <- shiny::reactive({
        if (shiny::is.reactive(data)) {
          data()
        } else {
          data
        }
      })

      output$tbl_summary <- shiny::reactive({
        toastui::renderDatagrid(
        data_r() |>
          overview_vars() |>
          create_overview_datagrid() |>
          add_sparkline(
            column = "vals"
          )
      )
      })
    }
  )
}
















add_sparkline <- function(grid, column = "vals", color.main = "#2a8484", color.sec = "#84EF84") {
  out <- toastui::grid_sparkline(
    grid = grid,
    column = column,
    renderer = function(data) {
      data_cl <- class(data)
      if (identical(data_cl, "factor")) {
        type <- "column"
        s <- summary(data)
        ds <- data.frame(x = names(s), y = s)
        horizontal <- FALSE
      } else if (any(c("numeric", "integer") %in% data_cl)) {
        if (length(unique(data)) == length(data)) {
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










overview_vars <- function(data) {
  data <- as.data.frame(data)

  dplyr::tibble(
    class = get_classes(data),
    name = names(data),
    n_missing = unname(colSums(is.na(data))),
    p_complete = 1 - n_missing / nrow(data),
    n_unique = get_n_unique(data),
    vals = as.list(data)
  )
}












create_overview_datagrid <- function(data) {
  # browser()
  gridTheme <- getOption("datagrid.theme")
  if (length(gridTheme) < 1) {
    datamods:::apply_grid_theme()
  }
  on.exit(toastui::reset_grid_theme())

  col.names <- names(data)

  std_names <- c(
    "Name" = "name",
    "Class" = "class",
    "Missing" = "n_missing",
    "Complete" = "p_complete",
    "Unique" = "n_unique",
    "Plot" = "vals"
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
    colwidths = "auto"
  )

  grid <- toastui::grid_columns(
    grid = grid,
    columns = col.names,
    header = headers,
    resizable = TRUE,
    width = 80
  )

  grid <- add_class_icon(
    grid = grid,
    column = "class"
  )

  # grid <- toastui::grid_format(
  #   grid = grid,
  #   "p_complete",
  #   formatter = toastui::JS("function(obj) {return (obj.value*100).toFixed(0) + '%';}")
  # )

  return(grid)
}










add_class_icon <- function(grid, column = "class") {
  out <- toastui::grid_format(
    grid = grid,
    column = column,
    formatter = function(value) {
      lapply(
        X = value,
        FUN = function(x) {
          if (identical(x, "numeric")) {
            shiny::icon("chart-line")
          } else if (identical(x, "factor")) {
            shiny::icon("chart-column")
          } else if (identical(x, "integer")) {
            shiny::icon("arrow-down-1-9")
          } else if (identical(x, "character")) {
            shiny::icon("arrow-down-a-z")
          } else if (any(c("Date", "POSIXct", "POSIXt") %in% x)) {
            shiny::icon("calendar-days")
          } else if ("hms" %in% x) {
            shiny::icon("clock")
          } else {
            shiny::icon("table")
          }
        }
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


########
#### Current file: R//file-import-module.R 
########








m_datafileUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(
      inputId = ns("file"),
      label = "Upload a file",
      multiple = FALSE,
      accept = c(
        ".csv",
        ".xlsx",
        ".xls",
        ".dta",
        ".ods",
        ".rds"
      )
    ),
    shiny::h4("Parameter specifications"),
    shiny::helpText(shiny::em("Select the desired variables and press 'Submit'")),
    shiny::uiOutput(ns("include_vars")),
    DT::DTOutput(ns("data_input")),
    shiny::actionButton(ns("submit"), "Submit")
  )
}

m_datafileServer <- function(id, output.format = "df") {
  shiny::moduleServer(id, function(input, output, session, ...) {
    ns <- shiny::NS(id)
    ds <- shiny::reactive({
      REDCapCAST::read_input(input$file$datapath) |> REDCapCAST::parse_data()
    })

    output$include_vars <- shiny::renderUI({
      shiny::req(input$file)
      shiny::selectizeInput(
        inputId = ns("include_vars"),
        selected = NULL,
        label = "Covariables to include",
        choices = colnames(ds()),
        multiple = TRUE
      )
    })

    base_vars <- shiny::reactive({
      if (is.null(input$include_vars)) {
        out <- colnames(ds())
      } else {
        out <- input$include_vars
      }
      out
    })

    output$data_input <-
      DT::renderDT({
        shiny::req(input$file)
        ds()[base_vars()]
      })

    shiny::eventReactive(input$submit, {
      # shiny::req(input$file)

      data <- shiny::isolate({
        ds()[base_vars()]
      })

      file_export(data,
        output.format = output.format,
        tools::file_path_sans_ext(input$file$name)
      )
    })
  })
}





file_app <- function() {
  ui <- shiny::fluidPage(
    m_datafileUI("data"),
    # DT::DTOutput(outputId = "redcap_prev")
    toastui::datagridOutput2(outputId = "redcap_prev")
  )
  server <- function(input, output, session) {
    m_datafileServer("data", output.format = "list")
  }
  shiny::shinyApp(ui, server)
}

file_app()

tdm_data_upload <- teal::teal_data_module(
  ui <- function(id) {
    shiny::fluidPage(
      m_datafileUI(id)
    )
  },
  server = function(id) {
    m_datafileServer(id, output.format = "teal")
  }
)

tdm_data_read <- teal::teal_data_module(
  ui <- function(id) {
    shiny::fluidPage(
      m_redcap_readUI(id = "redcap")
    )
  },
  server = function(id) {
    moduleServer(
      id,
      function(input, output, session) {
        ns <- session$ns

        m_redcap_readServer(id = "redcap", output.format = "teal")
      }
    )
  }
)


########
#### Current file: R//helpers.R 
########












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









write_quarto <- function(data, ...) {
  # Exports data to temporary location
  #
  # I assume this is more secure than putting it in the www folder and deleting
  # on session end
  temp <- tempfile(fileext = ".rds")
  readr::write_rds(data, file = temp)

  ## Specifying a output path will make the rendering fail
  ## Ref: https://github.com/quarto-dev/quarto-cli/discussions/4041
  ## Outputs to the same as the .qmd file
  quarto::quarto_render(
    execute_params = list(data.file = temp),
    ...
  )
}











read_input <- function(file, consider.na = c("NA", '""', "")) {
  ext <- tools::file_ext(file)

  if (ext == "csv") {
    df <- readr::read_csv(file = file, na = consider.na)
  } else if (ext %in% c("xls", "xlsx")) {
    df <- openxlsx2::read_xlsx(file = file, na.strings = consider.na)
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











argsstring2list <- function(string) {
  eval(parse(text = paste0("list(", string, ")")))
}









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














default_parsing <- function(data) {
  name_labels <- lapply(data,\(.x) REDCapCAST::get_attr(.x,attr = "label"))

  out <- data |>
    REDCapCAST::parse_data() |>
    REDCapCAST::as_factor() |>
    REDCapCAST::numchar2fct()

  purrr::map2(out,name_labels,\(.x,.l){
    if (!(is.na(.l) | .l=="")) {
      REDCapCAST::set_attr(.x, .l, attr = "label")
    } else {
      attr(x = .x, which = "label") <- NULL
      .x
    }
    # REDCapCAST::set_attr(data = .x, label = .l,attr = "label", overwrite = FALSE)
  }) |> dplyr::bind_cols()
}











remove_na_attr <- function(data,attr="label"){
  out <- data |> lapply(\(.x){
    ls <- REDCapCAST::get_attr(data = .x,attr = attr)
    if (is.na(ls) | ls == ""){
      attr(x = .x, which = attr) <- NULL
    }
    .x
  })

  dplyr::bind_cols(out)
}


########
#### Current file: R//redcap_read_shiny_module.R 
########










m_redcap_readUI <- function(id, include_title = TRUE) {
  ns <- shiny::NS(id)

  server_ui <- shiny::tagList(
    # width = 6,
    shiny::tags$h4("REDCap server information"),
    shiny::textInput(
      inputId = ns("uri"),
      label = "URI/Address",
      value = "https://redcap.your.institution/api/"
    ),
    shiny::textInput(
      inputId = ns("api"),
      label = "API token",
      value = ""
    )
  )


  params_ui <-
    shiny::tagList(
      # width = 6,
      shiny::tags$h4("Data import parameters"),
      shiny::helpText("Options here will show, when API and uri are typed"),
      shiny::uiOutput(outputId = ns("fields")),
      shinyWidgets::switchInput(
        inputId = "do_filter",
        label = "Apply filter?",
        value = FALSE,
        inline = FALSE,
        onLabel = "YES",
        offLabel = "NO"
      ),
      # shiny::radioButtons(
      #   inputId = "do_filter",
      #   label = "Filter export?",
      #   selected = "no",
      #   inline = TRUE,
      #   choices = list(
      #     "No" = "no",
      #     "Yes" = "yes"
      #   )
      # ),
      shiny::conditionalPanel(
        condition = "input.do_filter",
        shiny::uiOutput(outputId = ns("arms")),
        shiny::textInput(
          inputId = ns("filter"),
          label = "Optional filter logic (e.g., ⁠[gender] = 'female')"
        )
      )
    )


  shiny::fluidPage(
    if (include_title) shiny::tags$h3("Import data from REDCap"),
    bslib::layout_columns(
      server_ui,
      params_ui,
      col_widths = bslib::breakpoints(
        sm = c(12, 12),
        md = c(12, 12)
      )
    ),
    shiny::column(
      width = 12,
      # shiny::actionButton(inputId = ns("import"), label = "Import"),
      bslib::input_task_button(
        id = ns("import"),
        label = "Import",
        icon = shiny::icon("download", lib = "glyphicon"),
        label_busy = "Just a minute...",
        icon_busy = fontawesome::fa_i("arrows-rotate",
          class = "fa-spin",
          "aria-hidden" = "true"
        ),
        type = "primary",
        auto_reset = TRUE
      ),
      shiny::helpText("Press 'Import' after having specified API token and URI to export data from the REDCap server. A preview will show below the DataDictionary."),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      DT::DTOutput(outputId = ns("table"))
      # toastui::datagridOutput2(outputId = ns("table"))
    )
    # toastui::datagridOutput2(outputId = ns("table")),
    # toastui::datagridOutput2(outputId = ns("data")),
    # shiny::actionButton(inputId = ns("submit"), label = "Submit"),
    # DT::DTOutput(outputId = ns("data_prev"))
  )
}







m_redcap_readServer <- function(id, output.format = c("df", "teal", "list")) {
  output.format <- match.arg(output.format)

  module <- function(input, output, session) {
    # ns <- shiny::NS(id)
    ns <- session$ns

    # data_list <- shiny::reactiveValues(
    #   dict = NULL,
    #   stat = NULL,
    #   arms = NULL,
    #   data = NULL,
    #   name = NULL
    # )

    dd <- shiny::reactive({
      shiny::req(input$api)
      shiny::req(input$uri)


      REDCapR::redcap_metadata_read(
        redcap_uri = input$uri,
        token = input$api
      )$data
    })

    # dd <- shiny::reactive({
    #   shiny::req(input$api)
    #   shiny::req(input$uri)
    #
    #
    #   out <- REDCapR::redcap_metadata_read(
    #     redcap_uri = input$uri,
    #     token = input$api
    #   )
    #
    #   data_list$dict <- out$data
    #   data_list$stat <- out$success
    #
    #   out$data
    # })

    arms <- shiny::reactive({
      shiny::req(input$api)
      shiny::req(input$uri)

      REDCapR::redcap_event_read(
        redcap_uri = input$uri,
        token = input$api
      )$data

      # data_list$arms <- out
      # out
    })

    output$fields <- shiny::renderUI({
      shinyWidgets::virtualSelectInput(
        inputId = ns("fields"),
        label = "Select fields/variables to import:",
        choices = dd() |>
          dplyr::select(field_name, form_name) |>
          (\(.x){
            split(.x$field_name, .x$form_name)
          })() # |>
        # stats::setNames(instr()[["data"]][[2]])
        ,
        updateOn = "close",
        multiple = TRUE,
        search = TRUE,
        showValueAsTags = TRUE
      )
    })

    output$arms <- shiny::renderUI({
      shiny::selectizeInput(
        # inputId = "arms",
        inputId = ns("arms"),
        selected = NULL,
        label = "Filter by events/arms",
        choices = arms()[[3]],
        multiple = TRUE
      )
    })

    output$table <- DT::renderDT(
      {
        shiny::req(input$api)
        shiny::req(input$uri)
        # shiny::req(data_list$dict)
        # dd()[["data"]][c(1,2,4,5,6,8)]
        # browser()
        data.df <- dd()[, c(1, 2, 4, 5, 6, 8)]
        DT::datatable(data.df,
          caption = "Subset of data dictionary"
        )
      },
      server = TRUE
    )

    # Messes up the overlay of other objects. JS thing?
    # output$table <- toastui::renderDatagrid2(
    #   {
    #     shiny::req(input$api)
    #     shiny::req(input$uri)
    #     # shiny::req(data_list$dict)
    #     # dd()[["data"]][c(1,2,4,5,6,8)]
    #     # browser()
    #     toastui::datagrid(dd()[,c(1, 2, 4, 5, 6, 8)]
    #     )
    #   }
    # )

    name <- shiny::reactive({
      shiny::req(input$api)
      REDCapR::redcap_project_info_read(
        redcap_uri = input$uri,
        token = input$api
      )$data$project_title
    })

    shiny::eventReactive(input$import, {
      shiny::req(input$api)
      shiny::req(input$fields)
      record_id <- dd()[[1]][1]

      redcap_data <- REDCapCAST::read_redcap_tables(
        uri = input$uri,
        token = input$api,
        fields = unique(c(record_id, input$fields)),
        # forms = input$instruments,
        events = input$arms,
        raw_or_label = "both",
        filter_logic = input$filter
      ) |>
        REDCapCAST::redcap_wider() |>
        dplyr::select(-dplyr::ends_with("_complete")) |>
        dplyr::select(-dplyr::any_of(record_id)) |>
        REDCapCAST::suffix2label()

      out_object <- file_export(redcap_data,
        output.format = output.format,
        filename = name()
      )

      if (output.format == "list") {
        out <- list(
          data = shiny::reactive(redcap_data),
          meta = dd(),
          name = name(),
          filter = input$filter
        )
      } else {
        out <- out_object
      }

      return(out)
    })
  }

  shiny::moduleServer(
    id = id,
    module = module
  )
}




tdm_redcap_read <- teal::teal_data_module(
  ui <- function(id) {
    shiny::fluidPage(
      m_redcap_readUI(id)
    )
  },
  server = function(id) {
    m_redcap_readServer(id, output.format = "teal")
  }
)










redcap_app <- function() {
  ui <- shiny::fluidPage(
    m_redcap_readUI("data"),
    # DT::DTOutput(outputId = "redcap_prev")
    toastui::datagridOutput2(outputId = "redcap_prev"),
    shiny::fluidRow(
      shiny::column(
        8,
        # verbatimTextOutput("data_filter_code"),
        DT::DTOutput("data_summary")
      ),
      shiny::column(4, IDEAFilter::IDEAFilter_ui("data_filter"))
    )
  )
  server <- function(input, output, session) {
    data_val <- shiny::reactiveValues(data = NULL)

    ds <- m_redcap_readServer("data", output.format = "df")
    # output$redcap_prev <- DT::renderDT(
    #   {
    #     DT::datatable(purrr::pluck(ds(), "data")(),
    #       caption = "Observations"
    #     )
    #   },
    #   server = TRUE
    # )

    # shiny::reactive({
    #   data_val$data <- purrr::pluck(ds(), "data")()
    # })

    output$redcap_prev <- toastui::renderDatagrid2({
      # toastui::datagrid(purrr::pluck(ds(), "data")())
      # toastui::datagrid(data_val$data)
      toastui::datagrid(ds())
    })

    filtered_data <- IDEAFilter::IDEAFilter("data_filter",
      data = ds,
      verbose = FALSE
    )

    # filtered_data <- shiny::reactive({
    #   IDEAFilter::IDEAFilter("data_filter",
    #                          data = purrr::pluck(ds(), "data")(),
    #                          verbose = FALSE)
    # })

    # output$data_filter_code <- renderPrint({
    #   cat(gsub(
    #     "%>%", "%>% \n ",
    #     gsub(
    #       "\\s{2,}", " ",
    #       paste0(
    #         capture.output(attr(filtered_data(), "code")),
    #         collapse = " "
    #       )
    #     )
    #   ))
    # })

    output$data_summary <- DT::renderDataTable(
      {
        filtered_data()
      },
      options = list(
        scrollX = TRUE,
        pageLength = 5
      )
    )
  }
  shiny::shinyApp(ui, server)
}


########
#### Current file: R//regression_model.R 
########


































regression_model <- function(data,
                             outcome.str,
                             auto.mode = TRUE,
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

  if (!is.null(formula.str)) {
    formula.str <- glue::glue(formula.str)
  } else {
    assertthat::assert_that(outcome.str %in% names(data),
      msg = "Outcome variable is not present in the provided dataset"
    )
    formula.str <- glue::glue("{outcome.str}~.")

    if (!is.null(vars)) {
      if (outcome.str %in% vars) {
        vars <- vars[vars %in% outcome.str]
      }
      data <- data |> dplyr::select(dplyr::all_of(c(vars, outcome.str)))
    }
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

  if (auto.mode) {
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
      list(data = data),
      list(formula = as.formula(formula.str)),
      args.list
    )
  )

  # Recreating the call
  # out$call <-  match.call(definition=eval(parse(text=fun)), call(fun, data = 'data',formula = as.formula(formula.str),args.list))

  return(out)
}































regression_model_uv <- function(data,
                                outcome.str,
                                args.list = NULL,
                                fun = NULL,
                                vars = NULL,
                                ...) {
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
          list(data = data[match(c(outcome.str, .var), names(data))]),
          list(outcome.str = outcome.str),
          list(args.list = args.list)
        )
      )
    })

  return(out)
}



########
#### Current file: R//regression_table.R 
########


































































































regression_table <- function(x, ...) {
  if ("list" %in% class(x)){
    x |>
      purrr::map(\(.m){
        regression_table_create(x = .m, ...) |>
          gtsummary::add_n()
      }) |>
      gtsummary::tbl_stack()
  } else {
    regression_table_create(x,...)
  }
}

regression_table_create <- function(x, ..., args.list = NULL, fun = "gtsummary::tbl_regression") {
  # Stripping custom class
  class(x) <- class(x)[class(x) != "freesearcher_model"]

  if (any(c(length(class(x)) != 1, class(x) != "lm"))) {
    if (!"exponentiate" %in% names(args.list)) {
      args.list <- c(args.list, list(exponentiate = TRUE))
    }
  }

  out <- do.call(getfun(fun), c(list(x = x), args.list))
  out |>
    gtsummary::add_glance_source_note() # |>
  # gtsummary::bold_p()
}










tbl_merge <- function(data) {
  if (is.null(names(data))) {
    data |> gtsummary::tbl_merge()
  } else {
    data |> gtsummary::tbl_merge(tab_spanner = names(data))
  }
}


########
#### Current file: R//report.R 
########










index_embed <- function(data, index, add = NULL) {
  start <- seq_len(index)
  end <- seq_along(data)[-start]
  c(
    data[start],
    add,
    data[end]
  )
}









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









format_writer <- function(data, name) {
  if (data == "default") {
    glue::glue("  {name}: {data}")
  } else {
    warning("Not implemented")
  }
}






default_format_arguments <- function() {
  list(
    docx = list("default"),
    odt = list("default"),
    pdf = list("default")
  )
}









modify_qmd <- function(file, format) {
  readLines(file) |>
    specify_qmd_format(fileformat = "all") |>
    writeLines(paste0(tools::file_path_sans_ext(file), "_format.", tools::file_ext(file)))
}


########
#### Current file: R//shiny_freesearcheR.R 
########

















shiny_freesearcheR <- function(...) {
  appDir <- system.file("apps", "data_analysis_modules", package = "freesearcheR")
  if (appDir == "") {
    stop("Could not find the app directory. Try re-installing `freesearcheR`.", call. = FALSE)
  }

  a <- shiny::runApp(appDir = paste0(appDir,"/app.R"), ...)
  return(invisible(a))
}


########
#### Current file: R//sparkline_h_minimal.R 
########

# dependencies
library(apexcharter)
library(toastui)

spark_data <- mtcars |>
  (\(.x){
    dplyr::tibble(
      name = names(.x),
      vals = as.list(.x)
    )
  })()

ui <- fluidPage(
  toastui::datagridOutput("tbl")
)

server <- function(input, output) {
  output$tbl <- toastui::renderDatagrid(
    spark_data |>
      toastui::datagrid() |>
      toastui::grid_sparkline(
        column = "vals",
        renderer = function(data) {
          apex(data.frame(x = 1, y = data), aes(x, y), type = "box") |>
            ax_chart(sparkline = list(enabled = TRUE)) |>
            ax_plotOptions(
              bar = bar_opts(horizontal=TRUE)
            )
        }
      )
  )
}

shinyApp(ui = ui, server = server)


########
#### Current file: R//theme.R 
########







custom_theme <- function(...,
                         version = 5,
                         primary = "#1E4A8F",
                         secondary = "#FF6F61",
                         # success = "#1E4A8F",
                         # info = ,
                         # warning = ,
                         # danger = ,
                         # fg = "#000",
                         # bg="#fff",
                         bootswatch = "united",
                         base_font = bslib::font_google("Montserrat"),
                         # base_font = bslib::font_google("Alice"),
                         # heading_font = bslib::font_google("Jost", wght = "800"),
                         # heading_font = bslib::font_google("Noto Serif"),
                         # heading_font = bslib::font_google("Alice"),
                         heading_font = bslib::font_google("Public Sans",wght = "700"),
                         code_font = bslib::font_google("Open Sans")){
  bslib::bs_theme(
    ...,
    version = version,
    primary = primary,
    secondary = secondary,
    bootswatch = bootswatch,
    base_font = base_font,
    heading_font = heading_font,
    code_font = code_font
  )
}


########
#### Current file: R//update-variables-ext.R 
########

library(data.table)
library(rlang)














update_variables_ui <- function(id, title = TRUE) {
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
        datamods::i18n(paste(
          "Select, rename and convert variables in table above,",
          "then apply changes by clicking button below."
        ))
      )
    ),
    shiny::actionButton(
      inputId = ns("validate"),
      label = htmltools::tagList(
        phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
        datamods::i18n("Apply changes")
      ),
      width = "100%"
    )
  )
}











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
        data <- data_r()
        sprintf(i18n("Data has %s observations and %s variables."), nrow(data), ncol(data))
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
        # browser()
        variables <- variables_r()

        # variables <- variables |>
        #   dplyr::mutate(vals=as.list(dplyr::as_tibble(data_r())))

        # variables <- variables |>
        #   dplyr::mutate(n_id=seq_len(nrow(variables)))

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
          data <- data_r()
          new_selections <- input$row_selected
          if (length(new_selections) < 1) {
            new_selections <- seq_along(data)
          }
          # browser()
          data_inputs <- data.table::as.data.table(input$table_data)
          data.table::setorderv(data_inputs, "rowKey")

          old_names <- data_inputs$name
          new_names <- data_inputs$name_toset
          new_names[new_names == "New name"] <- NA
          new_names[is.na(new_names)] <- old_names[is.na(new_names)]
          new_names[new_names == ""] <- old_names[new_names == ""]

          old_label <- data_inputs$label
          new_label <- data_inputs$label_toset
          new_label[new_label == "New label"] <- ""
          new_label[is.na(new_label)] <- old_label[is.na(new_label)]
          new_label[new_label == ""] <- old_label[new_label == ""]

          new_classes <- data_inputs$class_toset
          new_classes[new_classes == "Select"] <- NA

          # browser()
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
              data <- purrr::map2(
                data, list_relabel,
                \(.data, .label){
                  if (!(is.na(.label) | .label == "")) {
                    REDCapCAST::set_attr(.data, .label, attr = "label")
                  } else {
                    attr(x = .data, which = "label") <- NULL
                    .data
                  }
                }
              ) |> dplyr::bind_cols(.name_repair = "unique_quiet")

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

      return(shiny::reactive({
        data <- updated_data$x
        code <- list()
        if (!is.null(data) && shiny::isTruthy(updated_data$list_mutate) && length(updated_data$list_mutate) > 0) {
          code <- c(code, list(rlang::call2("mutate", !!!updated_data$list_mutate)))
        }
        if (!is.null(data) && shiny::isTruthy(updated_data$list_rename) && length(updated_data$list_rename) > 0) {
          code <- c(code, list(rlang::call2("rename", !!!updated_data$list_rename)))
        }
        if (!is.null(data) && shiny::isTruthy(updated_data$list_select) && length(updated_data$list_select) > 0) {
          code <- c(code, list(rlang::expr(select(-any_of(c(!!!updated_data$list_select))))))
        }
        if (!is.null(data) && shiny::isTruthy(updated_data$list_relabel) && length(updated_data$list_relabel) > 0) {
          code <- c(code, list(rlang::call2("purrr::map2(list_relabel,
                                                       function(.data,.label){
                                                         REDCapCAST::set_attr(.data,.label,attr = 'label')
                                                       }) |> dplyr::bind_cols(.name_repair = 'unique_quiet')")))
        }
        if (length(code) > 0) {
          attr(data, "code") <- Reduce(
            f = function(x, y) rlang::expr(!!x %>% !!y),
            x = code
          )
        }
        return(data)
      }))
    }
  )
}






# utils -------------------------------------------------------------------












get_classes <- function(data) {
  classes <- lapply(
    X = data,
    FUN = function(x) {
      paste(class(x), collapse = ", ")
    }
  )
  unlist(classes, use.names = FALSE)
}












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














pad0 <- function(x) {
  NAs <- which(is.na(x))
  x <- formatC(x, width = max(nchar(as.character(x)), na.rm = TRUE), flag = "0")
  x[NAs] <- NA
  x
}












summary_vars <- function(data) {
  data <- as.data.frame(data)
  datsum <- dplyr::tibble(
    name = names(data),
    label = lapply(data, \(.x) REDCapCAST::get_attr(.x, "label")) |> unlist(),
    class = get_classes(data),
    # n_missing = unname(colSums(is.na(data))),
    # p_complete = 1 - n_missing / nrow(data),
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

  # grid <- toastui::grid_format(
  #   grid = grid,
  #   "p_complete",
  #   formatter = toastui::JS("function(obj) {return (obj.value*100).toFixed(0) + '%';}")
  # )
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
    choices = c("Select new class", "character", "factor", "numeric", "integer", "date", "datetime", "hms")
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








































convert_to <- function(data,
                       variable,
                       new_class = c("character", "factor", "numeric", "integer", "date", "datetime", "hms"),
                       ...) {
  new_class <- match.arg(new_class, several.ok = TRUE)
  stopifnot(length(new_class) == length(variable))
  args <- list(...)
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
    data[[variable]] <- as.factor(x = data[[variable]])
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.factor(!!sym(variable)))), variable)
    )
  } else if (identical(new_class, "numeric")) {
    data[[variable]] <- as.numeric(type.convert(data[[variable]], as.is = TRUE, ...))
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
    data[[variable]] <- as.Date(x = data[[variable]], ...)
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.Date(!!sym(variable), origin = !!args$origin))), variable)
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




########
#### Current file: /Users/au301842/freesearcheR/inst/apps/data_analysis_modules/ui.R 
########

# ns <- NS(id)

ui_elements <- list(
  ##############################################################################
  #########
  #########  Home panel
  #########
  ##############################################################################
  "home" = bslib::nav_panel(
    title = "freesearcheR",
    shiny::markdown(readLines("www/intro.md")),
    icon = shiny::icon("home")
  ),
  ##############################################################################
  #########
  #########  Import panel
  #########
  ##############################################################################
  "import" = bslib::nav_panel(
    title = "Import",
    shiny::tagList(
      shiny::h4("Choose your data source"),
      # shiny::conditionalPanel(
      #   condition = "output.has_input=='yes'",
      #   # Input: Select a file ----
      #   shiny::helpText("Analyses are performed on provided data")
      # ),
      # shiny::conditionalPanel(
      #   condition = "output.has_input=='no'",
      # Input: Select a file ----
      shinyWidgets::radioGroupButtons(
        inputId = "source",
        selected = "env",
        # label = "Choice: ",
        choices = c(
          "File upload" = "file",
          "REDCap server" = "redcap",
          "Local data" = "env"
        ),
        # checkIcon = list(
        #   yes = icon("square-check"),
        #   no = icon("square")
        # ),
        width = "100%"
      ),
      shiny::conditionalPanel(
        condition = "input.source=='file'",
        datamods::import_file_ui("file_import",
          title = "Choose a datafile to upload",
          file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat", ".sav", ".ods", ".dta")
        )
      ),
      shiny::conditionalPanel(
        condition = "input.source=='redcap'",
        m_redcap_readUI("redcap_import")
      ),
      shiny::conditionalPanel(
        condition = "input.source=='env'",
        import_globalenv_ui(id = "env", title = NULL)
      ),
      shiny::conditionalPanel(
        condition = "input.source=='redcap'",
        DT::DTOutput(outputId = "redcap_prev")
      ),
      shiny::br(),
      shiny::actionButton(
        inputId = "act_start",
        label = "Start",
        width = "100%",
        icon = shiny::icon("play")
      ),
      shiny::helpText('After importing, hit "Start" or navigate to the desired tab.'),
      shiny::br(),
      shiny::br()
    )
  ),
  ##############################################################################
  #########
  #########  Data overview panel
  #########
  ##############################################################################
  "overview" =
  # bslib::nav_panel_hidden(
    bslib::nav_panel(
      # value = "overview",
      title = "Data",
      bslib::navset_bar(
        fillable = TRUE,
        bslib::nav_panel(
          title = "Summary & filter",
          tags$h3("Data summary and filtering"),
          fluidRow(
            shiny::column(
              width = 9,
              shiny::tags$p(
              "Below is a short summary table of the provided data.
              On the right hand side you have the option to create filters.
              At the bottom you'll find a raw overview of the original vs the modified data.")
            )
          ),
          fluidRow(
            # column(
            #   width = 3,
            #   shiny::uiOutput("filter_vars"),
            #   shiny::conditionalPanel(
            #     condition = "(typeof input.filter_vars !== 'undefined' && input.filter_vars.length > 0)",
            #     datamods::filter_data_ui("filtering", max_height = "500px")
            #   )
            # ),
            # column(
            #   width = 9,
            #   DT::DTOutput(outputId = "filtered_table"),
            #   tags$b("Code dplyr:"),
            #   verbatimTextOutput(outputId = "filtered_code")
            # ),
            shiny::column(
              width = 9,
              toastui::datagridOutput(outputId = "tbl_overview"),
              # data_summary_ui(id = "data_summary"),
              shiny::tags$b("Reproducible code:"),
              shiny::verbatimTextOutput(outputId = "filtered_code")
            ),
            shiny::column(
              width = 3,
              IDEAFilter::IDEAFilter_ui("data_filter") # ,
              # shiny::actionButton("save_filter", "Apply the filter")
            )
          ),
          fluidRow(
            column(
              width = 6,
              tags$b("Original data:"),
              # verbatimTextOutput("original"),
              verbatimTextOutput("original_str")
            ),
            column(
              width = 6,
              tags$b("Modified data:"),
              # verbatimTextOutput("modified"),
              verbatimTextOutput("modified_str")
            )
          )
        ),
        # bslib::nav_panel(
        #   title = "Overview",
        #   DT::DTOutput(outputId = "table")
        # ),
        bslib::nav_panel(
          title = "Modify",
          tags$h3("Subset, rename and convert variables"),
          fluidRow(
            shiny::column(
              width = 9,
              shiny::tags$p("Below, you can subset the data (by not selecting the variables to exclude on applying changes), rename variables, set new labels (for nicer tables in the analysis report) and change variable classes.
                            Italic text can be edited/changed.
                            On the right, you can create and modify factor/categorical variables as well as resetting the data to the originally imported data.")
            )
          ),
          fluidRow(
            shiny::column(
              width = 9,
              update_variables_ui("vars_update")
            ),
            shiny::column(
              width = 3,
              shiny::actionButton("modal_cut", "Create factor variable"),
              shiny::tags$br(),
              shiny::helpText("Create factor/categorical variable from an other value."),
              shiny::tags$br(),
              shiny::tags$br(),
              shiny::actionButton("modal_update", "Reorder factor levels"),
              shiny::tags$br(),
              shiny::helpText("Reorder the levels of factor/categorical variables."),
              shiny::tags$br(),
              shiny::tags$br(),
              shiny::actionButton("data_reset", "Restore original data"),
              shiny::tags$br(),
              shiny::helpText("Reset to original imported dataset. Careful! There is no un-doing."),
              shiny::tags$br() # ,
              # shiny::tags$br(),
              # shiny::tags$br(),
              # IDEAFilter::IDEAFilter_ui("data_filter") # ,
              # shiny::actionButton("save_filter", "Apply the filter")
            )
            # datamods::update_variables_ui("vars_update")
          )
        ),
        bslib::nav_panel(
          title = "Browser",
          tags$h3("Browse the provided data"),
          shiny::tags$p(
                "Below is a data table with all the modified data provided to browse and understand data."
                ),
          shinyWidgets::html_dependency_winbox(),
          # fluidRow(
          # column(
          #   width = 3,
          #   shiny::uiOutput("filter_vars"),
          #   shiny::conditionalPanel(
          #     condition = "(typeof input.filter_vars !== 'undefined' && input.filter_vars.length > 0)",
          #     datamods::filter_data_ui("filtering", max_height = "500px")
          #   )
          # ),
          # column(
          #   width = 9,
          #   DT::DTOutput(outputId = "filtered_table"),
          #   tags$b("Code dplyr:"),
          #   verbatimTextOutput(outputId = "filtered_code")
          # ),
          # shiny::column(
          #   width = 8,
          toastui::datagridOutput(outputId = "table_mod") # ,
          # shiny::tags$b("Reproducible code:"),
          # shiny::verbatimTextOutput(outputId = "filtered_code")
          #   ),
          #   shiny::column(
          #     width = 4,
          #     shiny::actionButton("modal_cut", "Create factor from a variable"),
          #     shiny::tags$br(),
          #     shiny::tags$br(),
          #     shiny::actionButton("modal_update", "Reorder factor levels")#,
          #     # shiny::tags$br(),
          #     # shiny::tags$br(),
          #     # IDEAFilter::IDEAFilter_ui("data_filter") # ,
          #     # shiny::actionButton("save_filter", "Apply the filter")
          #   )
          # )
        )


        # column(
        #   8,
        #   shiny::verbatimTextOutput("filtered_code"),
        #   DT::DTOutput("filtered_table")
        # ),
        # column(4, IDEAFilter::IDEAFilter_ui("data_filter"))
      )
    ),
  ##############################################################################
  #########
  #########  Data analyses panel
  #########
  ##############################################################################
  "analyze" =
  # bslib::nav_panel_hidden(
    bslib::nav_panel(
      # value = "analyze",
      title = "Analyses",
      bslib::navset_bar(
        title = "",
        # bslib::layout_sidebar(
        #   fillable = TRUE,
        sidebar = bslib::sidebar(
          shiny::helpText(em("Please specify relevant settings for your data, and press 'Analyse'")),
          shiny::uiOutput("outcome_var"),
          shiny::radioButtons(
            inputId = "all",
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
            shiny::uiOutput("include_vars")
          ),
          shiny::uiOutput("strat_var"),
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
          shiny::radioButtons(
            inputId = "specify_factors",
            label = "Specify categorical variables?",
            selected = "no",
            inline = TRUE,
            choices = list(
              "Yes" = "yes",
              "No" = "no"
            )
          ),
          shiny::conditionalPanel(
            condition = "input.specify_factors=='yes'",
            shiny::uiOutput("factor_vars")
          ),
          bslib::input_task_button(
            id = "load",
            label = "Analyse",
            icon = shiny::icon("pencil", lib = "glyphicon"),
            label_busy = "Working...",
            icon_busy = fontawesome::fa_i("arrows-rotate",
              class = "fa-spin",
              "aria-hidden" = "true"
            ),
            type = "secondary",
            auto_reset = TRUE
          ),
          shiny::helpText("If you change the parameters, press 'Analyse' again to update the tables"),
          # shiny::conditionalPanel(
          #   condition = "output.ready=='yes'",
          shiny::tags$hr(),
          shiny::h4("Download results"),
          shiny::helpText("Choose your favourite output file format for further work, and download, when the analyses are done."),
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
          ),
          shiny::helpText("If choosing to output to MS Word, please note, that when opening the document, two errors will pop-up. Choose to repair and choose not to update references. The issue is being worked on. You can always choose LibreOffice instead."),
          shiny::tags$hr(),
          shiny::h4("Download data"),
          shiny::helpText("Choose your favourite output data format to download the modified data."),
          shiny::selectInput(
            inputId = "data_type",
            label = "Data format",
            selected = NULL,
            choices = list(
              "R" = "rds",
              "stata" = "dta"
            )
          ),
          shiny::br(),
          # Button
          shiny::downloadButton(
            outputId = "data_modified",
            label = "Download data",
            icon = shiny::icon("download")
          )
        ),
        bslib::nav_panel(
          title = "Baseline characteristics",
          gt::gt_output(outputId = "table1")
        ),
        bslib::nav_panel(
          title = "Regression table",
          gt::gt_output(outputId = "table2")
        ),
        bslib::nav_panel(
          title = "Regression checks",
          shiny::plotOutput(outputId = "check")
        )
      )
    ),
  ##############################################################################
  #########
  #########  Documentation panel
  #########
  ##############################################################################
  "docs" = bslib::nav_panel(
    title = "Documentation",
    # shiny::tags$iframe("www/docs.html", height=600, width=535),
    shiny::htmlOutput("docs_file"),
    shiny::br()
  )
)

# Initial attempt at creating light and dark versions
light <- custom_theme()
dark <- custom_theme(
  bg = "#000",
  fg = "#fff"
)

# Fonts to consider:
# https://webdesignerdepot.com/17-open-source-fonts-youll-actually-love/

ui <- bslib::page_fixed(
  shiny::tags$style(
    type = "text/css",
    # add the name of the tab you want to use as title in data-value
    shiny::HTML(
      ".container-fluid > .nav > li >
                        a[data-value='freesearcheR'] {font-size: 28px}"
    )
  ),
  title = "freesearcheR",
  theme = light,
  shiny::useBusyIndicators(),
  bslib::page_navbar(
    # title = "freesearcheR",
    id = "main_panel",
    # header = shiny::tags$header(shiny::p("Data is only stored temporarily for analysis and deleted immediately afterwards.")),
    ui_elements$home,
    ui_elements$import,
    ui_elements$overview,
    ui_elements$analyze,
    ui_elements$docs,
    # bslib::nav_spacer(),
    # bslib::nav_item(shinyWidgets::circleButton(inputId = "mode", icon = icon("moon"),status = "primary")),
    fillable = FALSE,
    footer = shiny::tags$footer(
      style = "background-color: #14131326; padding: 4px; text-align: center; bottom: 0; width: 100%;",
      shiny::p(
        style = "margin: 1",
        "Data is only stored for analyses and deleted immediately afterwards."
      ),
      shiny::p(
        style = "margin: 1; color: #888;",
        "Andreas G Damsbo | AGPLv3 license | ", shiny::tags$a("Source on Github", href = "https://github.com/agdamsbo/freesearcheR/", target = "_blank", rel = "noopener noreferrer")
      ),
    )
  )
)


########
#### Current file: /Users/au301842/freesearcheR/inst/apps/data_analysis_modules/server.R 
########

library(readr)
library(MASS)
library(stats)
library(gtsummary)
library(gt)
library(openxlsx2)
library(haven)
library(readODS)
require(shiny)
library(bslib)
library(assertthat)
library(dplyr)
library(quarto)
library(here)
library(broom)
library(broom.helpers)
# library(REDCapCAST)
library(easystats)
library(patchwork)
library(DHARMa)
library(apexcharter)
library(toastui)
library(datamods)
library(data.table)
library(IDEAFilter)
library(shinyWidgets)
library(DT)
# library(freesearcheR)

# source("functions.R")



# light <- custom_theme()
#
# dark <- custom_theme(bg = "#000",fg="#fff")


server <- function(input, output, session) {
  ## Listing files in www in session start to keep when ending and removing
  ## everything else.
  files.to.keep <- list.files("www/")

  output$docs_file <- shiny::renderUI({
    # shiny::includeHTML("www/docs.html")
    shiny::HTML(readLines("www/docs.html"))
  })

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
    ds = NULL,
    local_temp = NULL,
    ready = NULL,
    test = "no",
    data_original = NULL,
    data = NULL,
    data_filtered = NULL
  )

  ##############################################################################
  #########
  #########  Data import section
  #########
  ##############################################################################

  data_file <- datamods::import_file_server(
    id = "file_import",
    show_data_in = "popup",
    trigger_return = "change",
    return_class = "data.frame",
    read_fns = list(
      ods = function(file) {
        readODS::read_ods(path = file)
      },
      dta = function(file) {
        haven::read_dta(file = file)
      }
    )
  )

  shiny::observeEvent(data_file$data(), {
    shiny::req(data_file$data())
    rv$data_original <- data_file$data()
  })

  data_redcap <- m_redcap_readServer(
    id = "redcap_import",
    output.format = "list"
  )

  shiny::observeEvent(data_redcap(), {
    rv$data_original <- purrr::pluck(data_redcap(), "data")()
  })

  output$redcap_prev <- DT::renderDT(
    {
      DT::datatable(head(purrr::pluck(data_redcap(), "data")(), 5),
        caption = "First 5 observations"
      )
    },
    server = TRUE
  )

  from_env <- import_globalenv_server(
    id = "env",
    trigger_return = "change",
    btn_show_data = FALSE,
    reset = reactive(input$hidden)
  )

  shiny::observeEvent(from_env$data(), {
    shiny::req(from_env$data())
    rv$data_original <- from_env$data()
  })


  ##############################################################################
  #########
  #########  Data modification section
  #########
  ##############################################################################

  shiny::observeEvent(rv$data_original, rv$data <- rv$data_original |> default_parsing())
  shiny::observeEvent(input$data_reset, rv$data <- rv$data_original |> default_parsing())

  #########  Overview

  output$tbl_overview <- toastui::renderDatagrid(
    data_filter() |>
      overview_vars() |>
      create_overview_datagrid()|>
      add_sparkline(
        column = "vals",
        color.main = "#2A004E",
        color.sec = "#C62300"
      )
  )

  # data_summary_server(id = "data_summary",
  #                     data = data_filter())

  #########  Modifications


  ## Using modified version of the datamods::cut_variable_server function
  ## Further modifications are needed to have cut/bin options based on class of variable
  ## Could be defined server-side
  shiny::observeEvent(input$modal_cut, modal_cut_variable("modal_cut"))
  data_modal_cut <- cut_variable_server(
    id = "modal_cut",
    data_r = shiny::reactive(rv$data)
  )
  shiny::observeEvent(data_modal_cut(), rv$data <- data_modal_cut())


  shiny::observeEvent(input$modal_update, datamods::modal_update_factor("modal_update"))
  data_modal_update <- datamods::update_factor_server(
    id = "modal_update",
    data_r = reactive(rv$data)
  )
  shiny::observeEvent(data_modal_update(), {
    shiny::removeModal()
    rv$data <- data_modal_update()
  })



  # Show result
  output$table_mod <- toastui::renderDatagrid({
    shiny::req(rv$data)
    # data <- rv$data
    toastui::datagrid(
      # data = rv$data # ,
      data = data_filter()
      # bordered = TRUE,
      # compact = TRUE,
      # striped = TRUE
    )
  })

  output$code <- renderPrint({
    attr(rv$data, "code")
  })

  # updated_data <- datamods::update_variables_server(
    updated_data <- update_variables_server(
    id = "vars_update",
    data = reactive(rv$data),
    return_data_on_init = FALSE
  )

  output$original_str <- renderPrint({
    str(rv$data_original)
  })

  output$modified_str <- renderPrint({
    str(rv$data)
  })

  shiny::observeEvent(updated_data(), {
    rv$data <- updated_data()
  })

  # IDEAFilter has the least cluttered UI, but might have a License issue
  data_filter <- IDEAFilter::IDEAFilter("data_filter", data = reactive(rv$data), verbose = TRUE)

  # shiny::observeEvent(data_filter(), {
  #   rv$data_filtered <- data_filter()
  # })

  output$filtered_code <- shiny::renderPrint({
    cat(gsub(
      "%>%", "|> \n ",
      gsub(
        "\\s{2,}", " ",
        gsub(
          "reactive(rv$data)", "data",
          paste0(
            capture.output(attr(data_filter(), "code")),
            collapse = " "
          )
        )
      )
    ))
  })



  ##############################################################################
  #########
  #########  Data analyses section
  #########
  ##############################################################################

  ## Keep these "old" selection options as a simple alternative to the modification pane

  output$include_vars <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "include_vars",
      selected = NULL,
      label = "Covariables to include",
      choices = colnames(data_filter()),
      multiple = TRUE
    )
  })

  output$outcome_var <- shiny::renderUI({
    shiny::selectInput(
      inputId = "outcome_var",
      selected = NULL,
      label = "Select outcome variable",
      choices = colnames(data_filter()),
      multiple = FALSE
    )
  })


  output$factor_vars <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "factor_vars",
      selected = colnames(data_filter())[sapply(data_filter(), is.factor)],
      label = "Covariables to format as categorical",
      choices = colnames(data_filter()),
      multiple = TRUE
    )
  })

  base_vars <- shiny::reactive({
    if (is.null(input$include_vars)) {
      out <- colnames(data_filter())
    } else {
      out <- unique(c(input$include_vars, input$outcome_var))
    }
    return(out)
  })

  output$strat_var <- shiny::renderUI({
    shiny::selectInput(
      inputId = "strat_var",
      selected = "none",
      label = "Select variable to stratify baseline",
      choices = c("none", colnames(data_filter()[base_vars()])),
      multiple = FALSE
    )
  })

  ## Have a look at column filters at some point
  ## There should be a way to use the filtering the filter data for further analyses
  ## Disabled for now, as the JS is apparently not isolated
  # output$data_table <-
  #   DT::renderDT(
  #     {
  #       DT::datatable(ds()[base_vars()])
  #     },
  #     server = FALSE
  #   )
  #
  # output$data.classes <- gt::render_gt({
  #   shiny::req(input$file)
  #   data.frame(matrix(sapply(ds(), \(.x){
  #     class(.x)[1]
  #   }), nrow = 1)) |>
  #     stats::setNames(names(ds())) |>
  #     gt::gt()
  # })

  shiny::observeEvent(
    {
      input$load
    },
    {
      shiny::req(input$outcome_var)
      # browser()
      # Assumes all character variables can be formatted as factors
      # data <- data_filter$filtered() |>
      tryCatch(
        {
          data <- data_filter() |>
            dplyr::mutate(dplyr::across(dplyr::where(is.character), as.factor)) |>
            REDCapCAST::fct_drop.data.frame() |>
            factorize(vars = input$factor_vars) |>
            remove_na_attr()

          if (input$strat_var == "none") {
            by.var <- NULL
          } else {
            by.var <- input$strat_var
          }

          data <- data[base_vars()]

          # model <- data |>
          #   regression_model(
          #     outcome.str = input$outcome_var,
          #     auto.mode = input$regression_auto == 1,
          #     formula.str = input$regression_formula,
          #     fun = input$regression_fun,
          #     args.list = eval(parse(text = paste0("list(", input$regression_args, ")")))
          #   )

          models <- list(
            "Univariable" = regression_model_uv,
            "Multivariable" = regression_model
          ) |>
            lapply(\(.fun){
              do.call(
                .fun,
                c(
                  list(data = data),
                  list(outcome.str = input$outcome_var),
                  list(formula.str = input$regression_formula),
                  list(fun = input$regression_fun),
                  list(args.list = eval(parse(text = paste0("list(", input$regression_args, ")"))))
                )
              )
            })

          rv$list$data <- data



          rv$list$check <- purrr::pluck(models, "Multivariable") |>
            performance::check_model()

          rv$list$table1 <- data |>
            baseline_table(
              fun.args =
                list(
                  by = by.var
                )
            ) |>
            (\(.x){
              if (!is.null(by.var)) {
                .x |> gtsummary::add_overall()
              } else {
                .x
              }
            })() |>
            (\(.x){
              if (input$add_p == "yes") {
                .x |>
                  gtsummary::add_p() |>
                  gtsummary::bold_p()
              } else {
                .x
              }
            })()

          rv$list$table2 <- models |>
            purrr::map(regression_table) |>
            tbl_merge()


          rv$list$input <- input


          # rv$list <- list(
          #   data = data,
          #   check = check,
          #   table1 = data |>
          #     baseline_table(
          #       fun.args =
          #         list(
          #           by = by.var
          #         )
          #     ) |>
          #     (\(.x){
          #       if (!is.null(by.var)) {
          #         .x |> gtsummary::add_overall()
          #       } else {
          #         .x
          #       }
          #     })() |>
          #     (\(.x){
          #       if (input$add_p == "yes") {
          #         .x |>
          #           gtsummary::add_p() |>
          #           gtsummary::bold_p()
          #       } else {
          #         .x
          #       }
          #     })(),
          #   table2 = models |>
          #     purrr::map(regression_table) |>
          #     tbl_merge(),
          #   input = input
          # )

          output$table1 <- gt::render_gt(
            rv$list$table1 |>
              gtsummary::as_gt()
          )

          output$table2 <- gt::render_gt(
            rv$list$table2 |>
              gtsummary::as_gt()
          )

          output$check <- shiny::renderPlot({
            p <- plot(rv$list$check) +
              patchwork::plot_annotation(title = "Multivariable regression model checks")
            p
            # Generate checks in one column
            # layout <- sapply(seq_len(length(p)), \(.x){
            #   patchwork::area(.x, 1)
            # })
            #
            # p + patchwork::plot_layout(design = Reduce(c, layout))

            # patchwork::wrap_plots(ncol=1) +
            # patchwork::plot_annotation(title = 'Multivariable regression model checks')
          })
        },
        warning = function(warn) {
          showNotification(paste0(warn), type = "warning")
        },
        error = function(err) {
          showNotification(paste0("There was the following error. Inspect your data and adjust settings. Error: ", err), type = "err")
        }
      )
      rv$ready <- "ready"
    }
  )


  shiny::conditionalPanel(
    condition = "output.uploaded == 'yes'",
  )

  # observeEvent(input$act_start, {
  #   nav_show(id = "overview",target = "Import"
  #   )
  # })

  ##############################################################################
  #########
  #########  Page navigation
  #########
  ##############################################################################

  shiny::observeEvent(input$act_start, {
    bslib::nav_select(id = "main_panel", selected = "Modifications")
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

  # Reimplement from environment at later time
  # output$has_input <- shiny::reactive({
  #   if (rv$input) {
  #     "yes"
  #   } else {
  #     "no"
  #   }
  # })

  # shiny::outputOptions(output, "has_input", suspendWhenHidden = FALSE)

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
      shiny::withProgress(message = "Generating the report. Hold on for a moment..", {
        rv$list |>
          write_quarto(
            output_format = type,
            input = file.path(getwd(), "www/report.qmd")
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
      if (type == "rds"){
        readr::write_rds(rv$list$data,file = file)
      } else {
        haven::write_dta(as.data.frame(rv$list$data),path = file)
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
#### Current file: /Users/au301842/freesearcheR/inst/apps/data_analysis_modules/launch.R 
########

shinyApp(ui, server)
