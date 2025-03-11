

########
#### Current file: /Users/au301842/freesearcheR/inst/apps/freesearcheR/functions.R 
########



########
#### Current file: R//app_version.R 
########

app_version <- function()'250311_1338'


########
#### Current file: R//baseline_table.R 
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
  if (!is.null(vars)) {
    data <- data |> dplyr::select(dplyr::all_of(vars))
  }

  out <- do.call(fun, c(list(data = data), fun.args))
  return(out)
}



########
#### Current file: R//contrast_text.R 
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
#' @importFrom grDevices col2rgb
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
#### Current file: R//correlations-module.R 
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
        out
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
        psych::pairs.panels(rv$data())
      })
    }
  )
}

correlation_pairs <- function(data, threshold = .8) {
  data <- data[!sapply(data, is.character)]
  data <- data |> dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.numeric))
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
    paste(paste(data[-length(data)], collapse = ", "), data[length(data)], collapse = glue::glue(" {and.str} "))
  }
}


cor_app <- function() {
  ui <- shiny::fluidPage(
    shiny::sliderInput(
      inputId = "cor_cutoff",
      label = "Correlation cut-off",
      min = 0,
      max = 1,
      step = .1,
      value = .7,
      ticks = FALSE
    ),
    data_correlations_ui("data", height = 600)
  )
  server <- function(input, output, session) {
    data_correlations_server("data", data = shiny::reactive(mtcars), cutoff = shiny::reactive(input$cor_cutoff))
  }
  shiny::shinyApp(ui, server)
}

cor_app()


########
#### Current file: R//custom_SelectInput.R 
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
#'
#' @return a \code{\link[shiny]{selectizeInput}} dropdown element
#'
#' @importFrom shiny selectizeInput
#' @export
#'
columnSelectInput <- function(inputId, label, data, selected = "", ...,
                              col_subset = NULL, placeholder = "", onInitialize, none_label="No variable selected") {
  datar <- if (is.reactive(data)) data else reactive(data)
  col_subsetr <- if (is.reactive(col_subset)) col_subset else reactive(col_subset)

  labels <- Map(function(col) {
    json <- sprintf(
      IDEAFilter:::strip_leading_ws('
    {
      "name": "%s",
      "label": "%s",
      "datatype": "%s"
    }'),
      col,
      attr(datar()[[col]], "label") %||% "",
      IDEAFilter:::get_dataFilter_class(datar()[[col]])
    )
  }, col = names(datar()))

  if (!"none" %in% names(datar())){
    labels <- c("none"=list(sprintf('\n    {\n      \"name\": \"none\",\n      \"label\": \"%s\",\n      \"datatype\": \"\"\n    }',none_label)),labels)
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
                      '<span style=\"opacity: 0.3;\"><code style=\"color: black;\"> ' +
                        item.data.datatype +
                      '</code></span>' +
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

#' Extended cutting function
#'
#' @param x an object inheriting from class "hms"
#' @param ... passed on
#'
#' @rdname cut
#'
#' @return factor
#' @export
#'
#' @examples
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut(2)
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut("min")
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut(breaks = "hour")
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut(breaks = hms::as_hms(c("01:00:00", "03:01:20", "9:20:20")))
#' d_t <- readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA))
#' f <- d_t |> cut(2)
#' readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA)) |> cut(breaks = lubridate::as_datetime(c(hms::as_hms(levels(f)), hms::as_hms(max(d_t, na.rm = TRUE) + 1))), right = FALSE)
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

#' @rdname cut
#' @param x an object inheriting from class "POSIXt" or "Date"
#'
#' @examples
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut(2)
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut(breaks="weekday")
#' readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut(breaks="month_only")
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

#' @rdname cut
#' @param x an object inheriting from class "POSIXct"
cut.POSIXct <- cut.POSIXt

#' @rdname cut
#' @param x an object inheriting from class "POSIXct"
#'
#' @examples
#' as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut(2)
#' as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut(breaks="weekday")
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



#' @inheritParams shiny::modalDialog
#' @export
#'
#' @importFrom shiny showModal modalDialog textInput
#' @importFrom htmltools tagList
#'
#' @rdname cut-variable
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


#' @inheritParams shinyWidgets::WinBox
#' @export
#'
#' @importFrom shinyWidgets WinBox wbOptions wbControls
#' @importFrom htmltools tagList
#' @rdname cut-variable
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


#' @importFrom graphics abline axis hist par plot.new plot.window
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
#### Current file: R//data_plots.R 
########

# source(here::here("functions.R"))

#' Data correlations evaluation module
#'
#' @param id Module id. (Use 'ns("id")')
#'
#' @name data-correlations
#' @returns Shiny ui module
#' @export
#'
data_visuals_ui <- function(id, tab_title = "Plots", ...) {
  ns <- shiny::NS(id)

  # bslib::navset_bar(
  list(

    # Sidebar with a slider input
    sidebar = bslib::sidebar(
      bslib::accordion(
        multiple = FALSE,
        bslib::accordion_panel(
          title = "Creating plot",
          icon = bsicons::bs_icon("graph-up"),
          shiny::uiOutput(outputId = ns("primary")),
          shiny::uiOutput(outputId = ns("type")),
          shiny::uiOutput(outputId = ns("secondary")),
          shiny::uiOutput(outputId = ns("tertiary"))
        ),
        bslib::accordion_panel(
          title = "Advanced",
          icon = bsicons::bs_icon("gear")
        ),
        bslib::accordion_panel(
          title = "Download",
          icon = bsicons::bs_icon("download"),
          shinyWidgets::noUiSliderInput(
            inputId = ns("height"),
            label = "Plot height (mm)",
            min = 50,
            max = 300,
            value = 100,
            step = 1,
            format = shinyWidgets::wNumbFormat(decimals = 0),
            color = datamods:::get_primary_color()
          ),
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
      )
    ),
    bslib::nav_panel(
      title = tab_title,
      shiny::plotOutput(ns("plot"))
    )
  )
}


#'
#' @param data data
#' @param ... ignored
#'
#' @name data-correlations
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
        plot = NULL
      )

      output$primary <- shiny::renderUI({
        columnSelectInput(
          inputId = ns("primary"),
          data = data,
          placeholder = "Select variable",
          label = "Response variable",
          multiple = FALSE
        )
      })


      output$type <- shiny::renderUI({
        shiny::req(input$primary)
        # browser()

        if (!input$primary %in% names(data())) {
          plot_data <- data()[1]
        } else {
          plot_data <- data()[input$primary]
        }

        plots <- possible_plots(
          data = plot_data
        )

        shiny::selectizeInput(
          inputId = ns("type"),
          selected = NULL,
          label = shiny::h4("Plot type"),
          choices = plots,
          multiple = FALSE
        )
      })

      rv$plot.params <- shiny::reactive({
        get_plot_options(input$type)
      })

      output$secondary <- shiny::renderUI({
        shiny::req(input$type)
        # browser()

        columnSelectInput(
          inputId = ns("secondary"),
          data = data,
          placeholder = "Select variable",
          label = "Secondary/group variable",
          multiple = FALSE,
          col_subset = c(
            purrr::pluck(rv$plot.params(), 1)[["secondary.extra"]],
            all_but(
              colnames(subset_types(
                data(),
                purrr::pluck(rv$plot.params(), 1)[["secondary.type"]]
              )),
              input$primary
            )
          ),
          none_label = "No variable"
        )
      })

      output$tertiary <- shiny::renderUI({
        shiny::req(input$type)
        columnSelectInput(
          inputId = ns("tertiary"),
          data = data,
          placeholder = "Select variable",
          label = "Strata variable",
          multiple = FALSE,
          col_subset = c(
            "none",
            all_but(
              colnames(subset_types(
                data(),
                purrr::pluck(rv$plot.params(), 1)[["tertiary.type"]]
              )),
              input$primary,
              input$secondary
            )
          ),
          none_label = "No stratification"
        )
      })

      rv$plot <- shiny::reactive({
        shiny::req(input$primary)
        shiny::req(input$type)
        shiny::req(input$secondary)
        shiny::req(input$tertiary)
        create_plot(
          data = data(),
          type = names(rv$plot.params()),
          x = input$primary,
          y = input$secondary,
          z = input$tertiary
        )
      })

      output$plot <- shiny::renderPlot({
        rv$plot()
      })

      output$download_plot <- shiny::downloadHandler(
        filename = shiny::reactive({
          paste0("plot.", input$plot_type)
        }),
        content = function(file) {
          shiny::withProgress(message = "Drawing the plot. Hold on for a moment..", {
            ggplot2::ggsave(
              filename = file,
              plot = rv$plot(),
              width = input$width,
              height = input$height,
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
#' default_parsing(mtcars) |> subset_types(c("dichotomous", "ordinal"))
#' #' default_parsing(mtcars) |> subset_types("factor",class)
subset_types <- function(data, types, type.fun = outcome_type) {
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
      descr = "Stacked horizontal bars",
      note = "A classical way of visualising the distribution of an ordinal scale like the modified Ranking Scale and known as Grotta bars",
      primary.type = c("dichotomous", "ordinal"),
      secondary.type = c("dichotomous", "ordinal"),
      tertiary.type = c("dichotomous", "ordinal"),
      secondary.extra = "none"
    ),
    plot_violin = list(
      descr = "Violin plot",
      note = "A modern alternative to the classic boxplot to visualise data distribution",
      primary.type = c("continuous", "dichotomous", "ordinal"),
      secondary.type = c("dichotomous", "ordinal"),
      tertiary.type = c("dichotomous", "ordinal"),
      secondary.extra = "none"
    ),
    # plot_ridge = list(
    #   descr = "Ridge plot",
    #   note = "An alternative option to visualise data distribution",
    #   primary.type = "continuous",
    #   secondary.type = c("dichotomous", "ordinal"),
    #   tertiary.type = c("dichotomous", "ordinal"),
    #   secondary.extra = NULL
    # ),
    plot_sankey = list(
      descr = "Sankey plot",
      note = "A way of visualising change between groups",
      primary.type = c("dichotomous", "ordinal"),
      secondary.type = c("dichotomous", "ordinal"),
      tertiary.type = c("dichotomous", "ordinal"),
      secondary.extra = NULL
    ),
    plot_scatter = list(
      descr = "Scatter plot",
      note = "A classic way of showing the association between to variables",
      primary.type = "continuous",
      secondary.type = c("continuous", "ordinal"),
      tertiary.type = c("dichotomous", "ordinal"),
      secondary.extra = NULL
    )
  )
}

#' Title
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
  if (is.data.frame(data)) {
    data <- data[[1]]
  }

  type <- outcome_type(data)

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
#' @param x primary variable
#' @param y secondary variable
#' @param z tertiary variable
#' @param type plot type (derived from possible_plots() and matches custom function)
#' @param ... ignored for now
#'
#' @name data-plots
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' create_plot(mtcars, "plot_violin", "mpg", "cyl")
create_plot <- function(data, type, x, y, z = NULL, ...) {
  if (!y %in% names(data)) {
    y <- NULL
  }

  if (!z %in% names(data)) {
    z <- NULL
  }

  do.call(
    type,
    list(data, x, y, z, ...)
  )
}


#' Nice horizontal stacked bars (Grotta bars)
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_hbars(x = "carb", y = "cyl")
#' mtcars |> plot_hbars(x = "carb", y = NULL)
plot_hbars <- function(data, x, y, z = NULL) {
  out <- vertical_stacked_bars(data = data, score = x, group = y, strata = z)

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

  score_label <- ifelse(is.na(REDCapCAST::get_attr(data$score, "label")), score, REDCapCAST::get_attr(data$score, "label"))
  group_label <- ifelse(is.na(REDCapCAST::get_attr(data$group, "label")), group, REDCapCAST::get_attr(data$group, "label"))


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
#' 1:10 |> get_label()
get_label <- function(data, var = NULL) {
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


#' Beatiful violin plot
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_violin(x = "mpg", y = "cyl", z = "gear")
plot_violin <- function(data, x, y, z = NULL) {
  if (!is.null(z)) {
    ds <- split(data, data[z])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.ds){
    rempsyc::nice_violin(
      data = .ds,
      group = y,
      response = x, xtitle = get_label(data, var = y), ytitle = get_label(data, var = x)
    )
  })

  patchwork::wrap_plots(out)
}


#' Beautiful violin plot
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_scatter(x = "mpg", y = "wt")
plot_scatter <- function(data, x, y, z = NULL) {
  if (is.null(z)) {
    rempsyc::nice_scatter(
      data = data,
      predictor = y,
      response = x, xtitle = get_label(data, var = y), ytitle = get_label(data, var = x)
    )
  } else {
    rempsyc::nice_scatter(
      data = data,
      predictor = y,
      response = x,
      group = z, xtitle = get_label(data, var = y), ytitle = get_label(data, var = x)
    )
  }
}

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
sankey_ready <- function(data, x, y, z = NULL, numbers = "count") {
  ## TODO: Ensure ordering x and y

  if (is.null(z)) {
    out <- dplyr::count(data, !!dplyr::sym(x), !!dplyr::sym(y))
  } else {
    out <- dplyr::count(data, !!dplyr::sym(x), !!dplyr::sym(y), !!dplyr::sym(z))
  }
  out <- out |>
    dplyr::group_by(!!dplyr::sym(x)) |>
    dplyr::mutate(gx.sum = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::group_by(!!dplyr::sym(y)) |>
    dplyr::mutate(gy.sum = sum(n)) |>
    dplyr::ungroup()

  if (numbers == "count") {
    out <- out |> dplyr::mutate(
      lx = factor(paste0(!!dplyr::sym(x), "\n(n=", gx.sum, ")")),
      ly = factor(paste0(!!dplyr::sym(y), "\n(n=", gy.sum, ")"))
    )
  } else if (numbers == "percentage") {
    out <- out |> dplyr::mutate(
      lx = factor(paste0(!!dplyr::sym(x), "\n(", round((gx.sum / sum(n)) * 100, 1), "%)")),
      ly = factor(paste0(!!dplyr::sym(y), "\n(", round((gy.sum / sum(n)) * 100, 1), "%)"))
    )
  }

  if (is.factor(data[[x]])) {
    index <- match(levels(data[[x]]), str_remove_last(levels(out$lx), "\n"))
    out$lx <- factor(out$lx, levels = levels(out$lx)[index])
  }

  if (is.factor(data[[y]])) {
    index <- match(levels(data[[y]]), str_remove_last(levels(out$ly), "\n"))
    out$ly <- factor(out$ly, levels = levels(out$ly)[index])
  }

  out
}

str_remove_last <- function(data, pattern = "\n") {
  strsplit(data, split = pattern) |>
    lapply(\(.x)paste(unlist(.x[[-length(.x)]]), collapse = pattern)) |>
    unlist()
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
#' paste(sample(letters[1:10], 100, TRUE), collapse = "") |> line_break(fixed=TRUE)
line_break <- function(data, lineLength = 20, fixed = FALSE) {
  if (isTRUE(force)) {
    gsub(paste0("(.{1,", lineLength, "})(\\s|[[:alnum:]])"), "\\1\n", data)
  } else {
    paste(strwrap(data, lineLength), collapse = "\n")
  }
  ## https://stackoverflow.com/a/29847221
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
#' ds |> plot_sankey("first", "last", color.group = "y")
#' ds |> plot_sankey("first", "last", z = "g", color.group = "y")
plot_sankey <- function(data, x, y, z = NULL, color.group = "x", colors = NULL) {
  if (!is.null(z)) {
    ds <- split(data, data[z])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.ds){
    plot_sankey_single(.ds, x = x, y = y, color.group = color.group, colors = colors)
  })

  patchwork::wrap_plots(out)
}

default_theme <- function() {
  theme_void()
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
#' ds |> plot_sankey_single("first", "last", color.group = "y")
plot_sankey_single <- function(data, x, y, color.group = c("x","y"), colors = NULL, ...) {
  color.group <- match.arg(color.group)
  data <- data |> sankey_ready(x = x, y = y, ...)
  # browser()
  library(ggalluvial)

  na.color <- "#2986cc"
  box.color <- "#1E4B66"

  if (is.null(colors)) {
    if (color.group == "y") {
      main.colors <- viridisLite::viridis(n = length(levels(data[[y]])))
      secondary.colors <- rep(na.color, length(levels(data[[x]])))
      label.colors <- Reduce(c, lapply(list(secondary.colors, rev(main.colors)), contrast_text))
    } else {
      main.colors <- viridisLite::viridis(n = length(levels(data[[x]])))
      secondary.colors <- rep(na.color, length(levels(data[[y]])))
      label.colors <- Reduce(c, lapply(list(rev(main.colors), secondary.colors), contrast_text))
    }
    colors <- c(na.color, main.colors, secondary.colors)
  } else {
    label.colors <- contrast_text(colors)
  }

  group_labels <- c(get_label(data, x), get_label(data, y)) |>
    sapply(line_break) |>
    unname()

  p <- ggplot2::ggplot(data, ggplot2::aes(y = n, axis1 = lx, axis2 = ly))

  if (color.group == "y") {
    p <- p +
      ggalluvial::geom_alluvium(
        ggplot2::aes(fill = !!dplyr::sym(y), color = !!dplyr::sym(y)),
        width = 1 / 16,
        alpha = .8,
        knot.pos = 0.4,
        curve_type = "sigmoid"
      ) + ggalluvial::geom_stratum(ggplot2::aes(fill = !!dplyr::sym(y)),
        size = 2,
        width = 1 / 3.4
      )
  } else {
    p <- p +
      ggalluvial::geom_alluvium(
        ggplot2::aes(fill = !!dplyr::sym(x), color = !!dplyr::sym(x)),
        width = 1 / 16,
        alpha = .8,
        knot.pos = 0.4,
        curve_type = "sigmoid"
      ) + ggalluvial::geom_stratum(ggplot2::aes(fill = !!dplyr::sym(x)),
        size = 2,
        width = 1 / 3.4
      )
  }

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
    ggplot2::scale_color_manual(values = main.colors) +
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
#### Current file: R//data-summary.R 
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
#' @param ... arguments passed to toastui::datagrid
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

      # data_r <- shiny::reactive({
      #   if (shiny::is.reactive(data)) {
      #     data()
      #   } else {
      #     data
      #   }
      # })

      output$tbl_summary <-
        toastui::renderDatagrid(
          {
            shiny::req(data())
            data() |>
            overview_vars() |>
            create_overview_datagrid() |>
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
      if (identical(data_cl, "factor")) {
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
    class = get_classes(data),
    type = get_classes(data),
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
    colwidths = "fit"
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
    columns = "class",
    header = " ",
    align = "center",sortable = FALSE,
    width = 40
  )

  grid <- add_class_icon(
    grid = grid,
    column = "class"
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
add_class_icon <- function(grid, column = "class") {
  out <- toastui::grid_format(
    grid = grid,
    column = column,
    formatter = function(value) {
      lapply(
        X = value,
        FUN = function(x) {
          if (identical(x, "numeric")) {
            shiny::icon("calculator")
          } else if (identical(x, "factor")) {
            shiny::icon("chart-simple")
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

#' Shiny UI module to load a data file
#'
#' @param id id
#'
#' @return shiny UI
#' @export
#'
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

# tdm_data_upload <- teal::teal_data_module(
#   ui <- function(id) {
#     shiny::fluidPage(
#       m_datafileUI(id)
#     )
#   },
#   server = function(id) {
#     m_datafileServer(id, output.format = "teal")
#   }
# )
#
# tdm_data_read <- teal::teal_data_module(
#   ui <- function(id) {
#     shiny::fluidPage(
#       m_redcap_readUI(id = "redcap")
#     )
#   },
#   server = function(id) {
#     moduleServer(
#       id,
#       function(input, output, session) {
#         ns <- session$ns
#
#         m_redcap_readServer(id = "redcap", output.format = "teal")
#       }
#     )
#   }
# )


########
#### Current file: R//helpers.R 
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
write_quarto <- function(data,...) {
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

write_rmd <- function(data,...) {
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
    params = list(data.file = "web_data.rds"),
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

#' Remove NA labels
#'
#' @param data data
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' ds <- mtcars |> lapply(\(.x) REDCapCAST::set_attr(.x,label=NA,attr = "label"))
#' ds |> remove_na_attr() |> str()
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

#' Removes columns with completenes below cutoff
#'
#' @param data data frame
#' @param cutoff numeric
#'
#' @returns data frame
#' @export
#'
#' @examples
#'data.frame(a=1:10,b=NA, c=c(2,NA)) |> remove_empty_cols(cutoff=.5)
remove_empty_cols <- function(data,cutoff=.7){
  filter <- apply(X = data,MARGIN = 2,FUN = \(.x){
    sum(as.numeric(!is.na(.x)))/length(.x)
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
#'
#' @examples
#' ls_d <- list(test=c(1:20))
#' ls_d <- list()
#' data.frame(letters[1:20],1:20) |> append_list(ls_d,"letters")
#' letters[1:20]|> append_list(ls_d,"letters")
append_list <- function(data,list,index){
  ## This will overwrite and not warn
  ## Not very safe, but convenient to append code to list
  if (index %in% names(list)){
    list[[index]] <- data
    out <- list
  } else {
    out <- setNames(c(list,list(data)),c(names(list),index))
  }
  out
}


########
#### Current file: R//import-file-ext.R 
########


#' @title Import data from a file
#'
#' @description Let user upload a file and import data
#'
#' @inheritParams import-globalenv
#' @param preview_data Show or not a preview of the data under the file input.
#' @param file_extensions File extensions accepted by [shiny::fileInput()], can also be MIME type.
#' @param layout_params How to display import parameters : in a dropdown button or inline below file input.
#'
#' @template module-import
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


#' @inheritParams import_globalenv_server
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
#' @importFrom datamods split_char
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


########
#### Current file: R//redcap_read_shiny_module.R 
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
m_redcap_readUI <- function(id, include_title = TRUE) {
  ns <- shiny::NS(id)

  server_ui <- shiny::tagList(
    # width = 6,
    shiny::tags$h4("REDCap server"),
    shiny::textInput(
      inputId = ns("uri"),
      label = "Web address",
      value = "https://redcap.your.institution/"
    ),
    shiny::helpText("Format should be either 'https://redcap.your.institution/' or 'https://your.institution/redcap/'"),
    shiny::textInput(
      inputId = ns("api"),
      label = "API token",
      value = ""
    ),
    shiny::helpText("The token is a string of 32 numbers and letters."),
    shiny::actionButton(
      inputId = ns("data_connect"),
      label = "Connect",
      icon = shiny::icon("link", lib = "glyphicon"),
      # width = NULL,
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


  params_ui <-
    shiny::tagList(
      # width = 6,
      shiny::tags$h4("Data import parameters"),
      shiny::helpText("Options here will show, when API and uri are typed"),
      shiny::uiOutput(outputId = ns("fields")),
      shiny::uiOutput(outputId = ns("data_type")),
      shiny::uiOutput(outputId = ns("fill")),
      shinyWidgets::switchInput(
        inputId = "do_filter",
        label = "Apply filter?",
        value = FALSE,
        inline = FALSE,
        onLabel = "YES",
        offLabel = "NO"
      ),
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
      ## TODO: Use busy indicator like on download to have button activate/deactivate
      shiny::actionButton(
        inputId = ns("data_import"),
        label = "Import",
        icon = shiny::icon("download", lib = "glyphicon"),
        width = "100%",
        disabled = TRUE
      ),
      # bslib::input_task_button(
      #   id = ns("data_import"),
      #   label = "Import",
      #   icon = shiny::icon("download", lib = "glyphicon"),
      #   label_busy = "Just a minute...",
      #   icon_busy = fontawesome::fa_i("arrows-rotate",
      #     class = "fa-spin",
      #     "aria-hidden" = "true"
      #   ),
      #   type = "primary",
      #   auto_reset = TRUE#,state="busy"
      # ),
      shiny::br(),
      shiny::br(),
      shiny::helpText("Press 'Import' to get data from the REDCap server. Check the preview below before proceeding."),
      shiny::br(),
      shiny::br()
    )
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
      imported = NULL
    )

    shiny::observeEvent(list(input$api, input$uri), {
      uri <- paste0(ifelse(endsWith(input$uri, "/"), input$uri, paste0(input$uri, "/")), "api/")

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
            imported <- try(rlang::exec(REDCapR::redcap_metadata_read, !!!parameters), silent = TRUE)

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
                include_data_alert(see_data_text = "Click to see data dictionary",
                  dataIdName = "see_data",
                  extra = tags$p(tags$b(phosphoricons::ph("check", weight = "bold"), "Connected to server!"), tags$p(paste0(data_rv$info$project_title, " loaded."))),
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

    shiny::observeEvent(input$see_data, {
      datamods::show_data(
        purrr::pluck(data_rv$dd_list, "data"),
        title = "Data dictionary",
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
        label = "Select variables to import:",
        choices = purrr::pluck(data_rv$dd_list, "data") |>
          dplyr::select(field_name, form_name) |>
          (\(.x){
            split(.x$field_name, REDCapCAST::as_factor(.x$form_name))
          })(),
        updateOn = "change",
        multiple = TRUE,
        search = TRUE,
        showValueAsTags = TRUE
      )
    })

    output$data_type <- shiny::renderUI({
      shiny::req(data_rv$info)
      if (isTRUE(data_rv$info$has_repeating_instruments_or_events)) {
        vectorSelectInput(
          inputId = ns("data_type"),
          label = "Select the data format to import",
          choices = c(
            "Wide data (One row for each subject)" = "wide",
            "Long data for project with repeating instruments (default REDCap)" = "long"
          ),
          selected = "wide",
          multiple = FALSE
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
          selected = "yes",
          multiple = FALSE
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
      vectorSelectInput(
        inputId = ns("arms"),
        selected = NULL,
        label = "Filter by events/arms",
        choices = stats::setNames(arms()[[3]], arms()[[1]]),
        multiple = TRUE
      )
    })

    shiny::observeEvent(input$data_import, {
      shiny::req(input$fields)
      record_id <- purrr::pluck(data_rv$dd_list, "data")[[1]][1]


      parameters <- list(
        uri = data_rv$uri,
        token = input$api,
        fields = unique(c(record_id, input$fields)),
        events = input$arms,
        raw_or_label = "both",
        filter_logic = input$filter,
        split_forms = if (input$data_type == "long") "none" else "all"
      )

      shiny::withProgress(message = "Downloading REDCap data. Hold on for a moment..", {
        imported <- try(rlang::exec(REDCapCAST::read_redcap_tables, !!!parameters), silent = TRUE)
      })
      code <- rlang::call2(REDCapCAST::read_redcap_tables, !!!parameters)


      if (inherits(imported, "try-error") || NROW(imported) < 1) {
        data_rv$data_status <- "error"
        data_rv$data_list <- NULL
      } else {
        data_rv$data_status <- "success"

        ## The data management below should be separated to allow for changing
        ## "wide"/"long" without re-importing data
        if (input$data_type != "long") {
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

        data_rv$data <- out |>
          dplyr::select(-dplyr::ends_with("_complete")) |>
          # dplyr::select(-dplyr::any_of(record_id)) |>
          REDCapCAST::suffix2label()
      }
    })

    # shiny::observe({
    #   shiny::req(data_rv$imported)
    #
    #   imported <- data_rv$imported
    #
    #
    # })

    return(shiny::reactive(data_rv$data))
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
    m_redcap_readUI("data"),
    DT::DTOutput("data_summary")
  )
  server <- function(input, output, session) {
    data_val <- shiny::reactiveValues(data = NULL)


    data_val$data <- m_redcap_readServer(id = "data")

    output$data_summary <- DT::renderDataTable(
      {
        shiny::req(data_val$data)
        data_val$data()
      },
      options = list(
        scrollX = TRUE,
        pageLength = 5
      ),
    )
  }
  shiny::shinyApp(ui, server)
}


########
#### Current file: R//redcap.R 
########




########
#### Current file: R//regression_model.R 
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
#'   broom::tidy(m)
regression_model <- function(data,
                             outcome.str,
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
  if (!outcome.str %in% names(data)){
    outcome.str <- names(data)[1]
    print("outcome is not in data, first column is used")
  }

  if (is.null(vars)) {
    vars <- names(data)[!names(data) %in% outcome.str]
  } else {
    if (outcome.str %in% vars) {
      vars <- vars[!vars %in% outcome.str]
    }
    data <- data |> dplyr::select(dplyr::all_of(c(vars, outcome.str)))
  }

  if (!is.null(formula.str)) {
    formula.glue <- glue::glue(formula.str)
  } else {
    assertthat::assert_that(outcome.str %in% names(data),
      msg = "Outcome variable is not present in the provided dataset"
    )
    formula.glue <- glue::glue("{outcome.str}~{paste(vars,collapse='+')}")
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

  # browser()
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
#' lapply(m,broom::tidy) |> dplyr::bind_rows()
#' }
regression_model_uv <- function(data,
                                outcome.str,
                                args.list = NULL,
                                fun = NULL,
                                vars = NULL,
                                ...) {

  ## This will handle if outcome is not in data for nicer shiny behavior
  if (!outcome.str %in% names(data)){
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

#' Outcome data type assessment
#'
#' @param data data
#'
#' @returns outcome type
#' @export
#'
#' @examples
#' mtcars |>
#'   default_parsing() |>
#'   lapply(outcome_type)
outcome_type <- function(data) {
  cl_d <- class(data)
  if (any(c("numeric", "integer") %in% cl_d)) {
    out <- "continuous"
  } else if (identical("factor", cl_d)) {
    if (length(levels(data)) == 2) {
      out <- "dichotomous"
    } else if (length(levels(data)) > 2) {
      out <- "ordinal"
    }
  } else {
    out <- "unknown"
  }

  out
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
      args.list = list(family = stats::binomial(link = "logit")),
      formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
      table.fun = "gtsummary::tbl_regression",
      table.args.list = list()
    ),
    polr = list(
      descr = "Ordinal logistic regression model",
      design = "cross-sectional",
      out.type = "ordinal",
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
  # browser()
  if (is.data.frame(data)) {
    data <- data[[1]]
  }

  design <- match.arg(design)
  type <- outcome_type(data)

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
#'
#' ls <- regression_model_list(data = default_parsing(gtsummary::trial), outcome.str = "trt", fun.descr = "Logistic regression model")
#' tbl <- gtsummary::tbl_regression(ls$model, exponentiate = TRUE)
#' m <- gtsummary::trial |>
#'   default_parsing() |>
#'   regression_model(
#'     outcome.str = "trt",
#'     fun = "stats::glm",
#'     formula.str = "{outcome.str}~.",
#'     args.list = list(family = stats::binomial(link = "logit"))
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

  model <- do.call(
    regression_model,
    list(
      data = data,
      outcome.str = outcome.str,
      fun = fun.c,
      formula.str = formula.str.c,
      args.list = args.list.c
    )
  )

  code <- glue::glue(
    "{fun.c}({paste(Filter(length,list(glue::glue(formula.str.c),'data = data',list2str(args.list.c))),collapse=', ')})"
  )

  list(
    options = options,
    model = model,
    code = code
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
#' gtsummary::trial |> regression_model_uv(
#'   outcome.str = "trt",
#'   fun = "stats::glm",
#'   args.list = list(family = stats::binomial(link = "logit"))
#' ) |> lapply(broom::tidy) |> dplyr::bind_rows()
#' ms <- regression_model_uv_list(data = default_parsing(mtcars), outcome.str = "mpg", fun.descr = "Linear regression model")
#' lapply(ms$model,broom::tidy) |> dplyr::bind_rows()
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
      do.call(
        regression_model,
        list(
          data = data[c(outcome.str, .var)],
          outcome.str = outcome.str,
          fun = fun.c,
          formula.str = formula.str.c,
          args.list = args.list.c
        )
      )
    })


  vars <- "."

  code_raw <- glue::glue(
    "{fun.c}({paste(Filter(length,list(glue::glue(formula.str.c),'data = .d',list2str(args.list.c))),collapse=', ')})"
  )

  code <- glue::glue("lapply(data,function(.d){code_raw})")

  list(
    options = options,
    model = model,
    code = code
  )
}


########
#### Current file: R//regression_plot.R 
########

#' Regression coef plot from gtsummary. Slightly modified to pass on arguments
#'
#' @param x (`tbl_regression`, `tbl_uvregression`)\cr
#'   A 'tbl_regression' or 'tbl_uvregression' object
##  #' @param remove_header_rows (scalar `logical`)\cr
##  #'   logical indicating whether to remove header rows
##  #'   for categorical variables. Default is `TRUE`
##  #' @param remove_reference_rows (scalar `logical`)\cr
##  #'   logical indicating whether to remove reference rows
##  #'   for categorical variables. Default is `FALSE`.
#' @param ... arguments passed to `ggstats::ggcoef_plot(...)`
#'
#' @returns ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' mod <- lm(mpg ~ ., mtcars)
#' p <- mod |>
#'   gtsummary::tbl_regression() |>
#'   plot(colour = "variable")
#' }
#'
plot.tbl_regression <- function(x,
                                # remove_header_rows = TRUE,
                                # remove_reference_rows = FALSE,
                                ...) {
  # check_dots_empty()
  gtsummary:::check_pkg_installed("ggstats")
  gtsummary:::check_not_missing(x)
  # gtsummary:::check_scalar_logical(remove_header_rows)
  # gtsummary:::check_scalar_logical(remove_reference_rows)

  df_coefs <- x$table_body
  # if (isTRUE(remove_header_rows)) {
  #   df_coefs <- df_coefs |> dplyr::filter(!.data$header_row %in% TRUE)
  # }
  # if (isTRUE(remove_reference_rows)) {
  #   df_coefs <- df_coefs |> dplyr::filter(!.data$reference_row %in% TRUE)
  # }

  # browser()

  df_coefs$label[df_coefs$row_type == "label"] <- ""

  df_coefs %>%
    ggstats::ggcoef_plot(exponentiate = x$inputs$exponentiate, ...)
}


# default_parsing(mtcars) |> lapply(class)
#
# purrr::imap(mtcars,\(.x,.i){
#   if (.i %in% c("vs","am","gear","carb")){
#     as.factor(.x)
#   } else .x
#   }) |> dplyr::bind_cols()
#
#


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
    dplyr::bind_rows() |> dplyr::mutate(model=as_factor(model))

  l_merged$table_body <- df_body_long

  l_merged$inputs$exponentiate <- !identical(class(list$models$Multivariable$model), "lm")

  l_merged
}


########
#### Current file: R//regression_table.R 
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
#'   regression_table() |> plot()
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
#'   }
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
#'   class(x) <- class(x)[class(x) != "freesearcher_model"]
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
      args.list <- c(args.list, list(exponentiate = TRUE, p.values = TRUE))
    }
  }

  out <- do.call(getfun(fun), c(list(x = x), args.list))
  out |>
    gtsummary::add_glance_source_note() # |>
  # gtsummary::bold_p()
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
#### Current file: R//report.R 
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
#### Current file: R//selectInputIcon.R 
########

#' @title Create a select input control with icon(s)
#'
#' @description Extend form controls by adding text or icons before,
#'  after, or on both sides of a classic `selectInput`.
#'
#' @inheritParams shiny::selectInput
#' @inheritParams textInputIcon
#'
#' @return A numeric input control that can be added to a UI definition.
#' @seealso See [updateNumericInputIcon()] to update server-side, and [textInputIcon()] for using text value.
#' @export
#'
#' @importFrom shiny restoreInput
#' @importFrom htmltools tags validateCssUnit css
#'
#' @example examples/numericInputIcon.R
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
        class = "form-control numeric-input-icon",
        shiny:::selectOptions(choices, selected, inputId, selectize)
      ),
      shinyWidgets:::markup_input_group(icon, "right", theme_func = shiny::getCurrentTheme)
    ),
    shinyWidgets:::html_dependency_input_icons()
  )
}


########
#### Current file: R//shiny_freesearcheR.R 
########

#' Launch the freesearcheR tool locally
#'
#' @description
#' All data.frames in the global environment will be accessible through the app.
#'
#'
#' @param ... arguments passed on to `shiny::runApp()`
#'
#' @return shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' shiny_freesearcheR(launch.browser = TRUE)
#' }
shiny_freesearcheR <- function(...) {
  appDir <- system.file("apps", "freesearcheR", package = "freesearcheR")
  if (appDir == "") {
    stop("Could not find the app directory. Try re-installing `freesearcheR`.", call. = FALSE)
  }

  a <- shiny::runApp(appDir = paste0(appDir,"/app.R"), ...)
  return(invisible(a))
}


#' Easily launch the freesearcheR app
#'
#' @param ... passed on to `shiny::runApp()`
#'
#' @returns shiny app
#' @export
#'
launch_freesearcheR <- function(...){
  shiny_freesearcheR(...)
}


########
#### Current file: R//theme.R 
########

#' Custom theme based on unity
#'
#' @param ... everything passed on to bslib::bs_theme()
#'
#' @returns theme list
#' @export
custom_theme <- function(...,
                         version = 5,
                         primary = "#1E4A8F",
                         secondary = "#FF6F61",
                         bootswatch = "united",
                         base_font = bslib::font_google("Montserrat"),
                         heading_font = bslib::font_google("Public Sans",wght = "700"),
                         code_font = bslib::font_google("Open Sans")
                         # success = "#1E4A8F",
                         # info = ,
                         # warning = ,
                         # danger = ,
                         # fg = "#000",
                         # bg="#fff",
                         # base_font = bslib::font_google("Alice"),
                         # heading_font = bslib::font_google("Jost", wght = "800"),
                         # heading_font = bslib::font_google("Noto Serif"),
                         # heading_font = bslib::font_google("Alice"),
                         ){
  bslib::bs_theme(
    ...,
    "navbar-bg" = primary,
    version = version,
    primary = primary,
    secondary = secondary,
    bootswatch = bootswatch,
    base_font = base_font,
    heading_font = heading_font,
    code_font = code_font
  )
}


#' GGplot default theme for plotting in Shiny
#'
#' @param data ggplot object
#'
#' @returns ggplot object
#' @export
#'
gg_theme_shiny <- function(){
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 18),
      axis.text = ggplot2::element_text(size = 14),
      strip.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 24),
      plot.subtitle = ggplot2::element_text(size = 18),
      legend.position = "none"
    )
}


#' GGplot default theme for plotting export objects
#'
#' @param data ggplot object
#'
#' @returns ggplot object
#' @export
#'
gg_theme_export <- function(){
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 18),
      axis.text.x = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 24)
    )
}


########
#### Current file: R//update-factor-ext.R 
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
        virtualSelectInput(
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
            ph("sort-ascending"),
            i18n("Sort by levels")
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
            ph("sort-ascending"),
            i18n("Sort by count")
          ),
          class = "btn-outline-primary mb-3",
          width = "100%"
        )
      )
    ),
    datagridOutput(ns("grid")),
    tags$div(
      class = "float-end",
      prettyCheckbox(
        inputId = ns("new_var"),
        label = i18n("Create a new variable (otherwise replaces the one selected)"),
        value = FALSE,
        status = "primary",
        outline = TRUE,
        inline = TRUE
      ),
      actionButton(
        inputId = ns("create"),
        label = tagList(ph("arrow-clockwise"), i18n("Update factor variable")),
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
            ph("sort-descending"),
            "Sort Levels"
          )
        } else {
          decreasing <- TRUE
          label <- tagList(
            ph("sort-ascending"),
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
            ph("sort-descending"),
            i18n("Sort count")
          )
        } else {
          decreasing <- TRUE
          label <- tagList(
            ph("sort-ascending"),
            i18n("Sort count")
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
          header = c(i18n("Levels"), "New label", i18n("Count"))
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
#' @rdname create-column
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
#### Current file: R//update-variables-ext.R 
########

library(data.table)
library(rlang)


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
        phosphoricons::ph("arrow-circle-right", title = i18n("Apply changes")),
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


modal_update_variables <- function(id,
                                title = "Select, rename and reclass variables",
                                easyClose = TRUE,
                                size = "xl",
                                footer = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    update_variables_ui(id),
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = datamods:::genId())
    ),
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
    data[[variable]] <- as.Date(x = clean_date(data[[variable]]), ...)
    attr(data, "code_03_convert") <- c(
      attr(data, "code_03_convert"),
      setNames(list(expr(as.Date(clean_date(!!sym(variable)), origin = !!args$origin, format=clean_sep(!!args$format)))), variable)
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
clean_sep <- function(data,old.sep="[-.,/]",new.sep="-"){
  gsub(old.sep,new.sep,data)
}

#' Attempts at applying uniform date format
#'
#' @param data character string vector of possible dates
#'
#' @returns character string
#' @export
#'
clean_date <- function(data){
  data |>
    clean_sep() |>
    sapply(\(.x){
      if (is.na(.x)){
        .x
      } else {
        strsplit(.x,"-") |>
          unlist()|>
          lapply(\(.y){
            if (nchar(.y)==1) paste0("0",.y) else .y
          }) |> paste(collapse="-")
      }
    }) |>
    unname()
}


########
#### Current file: R//wide2long.R 
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
#### Current file: /Users/au301842/freesearcheR/inst/apps/freesearcheR/ui.R 
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
    shiny::fluidRow(
      shiny::column(width = 2),
      shiny::column(
        width = 8,
        shiny::markdown(readLines("www/intro.md")),
        shiny::column(width = 2)
      )
    ),
    icon = shiny::icon("home")
  ),
  ##############################################################################
  #########
  #########  Import panel
  #########
  ##############################################################################
  "import" = bslib::nav_panel(
    title = "Import",
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
            id = "file_import",
            layout_params = "dropdown",
            title = "Choose a datafile to upload",
            file_extensions = c(".csv", ".tsv", ".txt", ".xls", ".xlsx", ".rds", ".sas7bdat", ".ods", ".dta")
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
        shiny::br(),
        shiny::h5("Exclude in-complete variables"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::br(),
            shiny::br(),
            shiny::p("Filter incomplete variables, by setting a completeness threshold:"),
            shiny::br()
          ),
          shiny::column(
            width = 6,
            shinyWidgets::noUiSliderInput(
              inputId = "complete_cutoff",
              label = NULL,
              min = 0,
              max = 100,
              step = 5,
              value = 70,
              format = shinyWidgets::wNumbFormat(decimals = 0),
              color = datamods:::get_primary_color()
            ),
            shiny::helpText("Include variables with completeness above the specified percentage.")
          )
        ),
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
        shiny::br(),
        shiny::column(width = 2)
      )
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
          title = "Overview",
          tags$h3("Overview and filtering"),
          fluidRow(
            shiny::column(
              width = 9,
              shiny::tags$p(
                "Below is a short summary table of the provided data.
              On the right hand side you have the option to create filters.
              At the bottom you'll find a raw overview of the original vs the modified data."
              )
            )
          ),
          fluidRow(
            shiny::column(
              width = 9,
              data_summary_ui(id = "data_summary")
            ),
            shiny::column(
              width = 3,
              IDEAFilter::IDEAFilter_ui("data_filter"),
              shiny::tags$br()
            )
          )
        ),
        bslib::nav_panel(
          title = "Browse",
          tags$h3("Browse the provided data"),
          shiny::tags$p(
            "Below is a table with all the modified data provided to browse and understand data."
          ),
          shinyWidgets::html_dependency_winbox(),
          fluidRow(
            toastui::datagridOutput(outputId = "table_mod")
          ),
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::tags$br(),
          shiny::tags$br()
        ),
        bslib::nav_panel(
          title = "Modify",
          tags$h3("Subset, rename and convert variables"),
          fluidRow(
            shiny::column(
              width = 9,
              shiny::tags$p(shiny::markdown("Below, you can subset the data (select variables to include on clicking 'Apply changes'), rename variables, set new labels (for nicer tables in the report) and change variable classes (numeric, factor/categorical etc.).
                            Italic text can be edited/changed.
                            On the right, you can create and modify factor/categorical variables as well as create new variables with *R* code."))
            )
          ),
          shiny::tags$br(),
          shiny::tags$br(),
          fluidRow(
            shiny::column(
              width = 2
            ),
            shiny::column(
              width = 8,
              fluidRow(
                shiny::column(
                  width = 6,
                  tags$h4("Update variables"),
                  shiny::tags$br(),
                  shiny::actionButton(
                    inputId = "modal_variables",
                    label = "Subset, rename and change class/type",
                    width = "100%"
                  ),
                  shiny::tags$br(),
                  shiny::helpText("Subset variables, rename variables and labels, and apply new class to variables"),
                  shiny::tags$br(),
                  shiny::tags$br(),
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
                  width = 6,
                  tags$h4("Create new variables"),
                  shiny::tags$br(),
                  shiny::actionButton(
                    inputId = "modal_cut",
                    label = "Create factor variable",
                    width = "100%"
                  ),
                  shiny::tags$br(),
                  shiny::helpText("Create factor/categorical variable from an other value."),
                  shiny::tags$br(),
                  shiny::tags$br(),
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
              tags$h4("Restore"),
              shiny::actionButton(
                inputId = "data_reset",
                label = "Restore original data",
                width = "100%"
              ),
              shiny::tags$br(),
              shiny::helpText("Reset to original imported dataset. Careful! There is no un-doing.")
            ),
            shiny::column(
              width = 2
            )
          ),
          shiny::tags$br(),
          shiny::tags$br(),
          tags$h4("Restore"),
          shiny::tags$br(),
          shiny::tags$p(
            "Below, you'll find a raw overview of the original vs the modified data."
          ),
          shiny::tags$br(),
          shiny::tags$br(),
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
        )
      )
    ),
  ##############################################################################
  #########
  #########  Descriptive analyses panel
  #########
  ##############################################################################
  "describe" =
    bslib::nav_panel(
      title = "Evaluate",
      id = "navdescribe",
      bslib::navset_bar(
        title = "",
        sidebar = bslib::sidebar(
          bslib::accordion(
            open = "acc_chars",
            multiple = FALSE,
            bslib::accordion_panel(
              value = "acc_chars",
              title = "Characteristics",
              icon = bsicons::bs_icon("table"),
              shiny::uiOutput("strat_var"),
              shiny::helpText("Only factor/categorical variables are available for stratification. Go back to the 'Data' tab to reclass a variable if it's not on the list."),
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
              )
            ),
            bslib::accordion_panel(
              vlaue = "acc_cor",
              title = "Correlations",
              icon = bsicons::bs_icon("table"),
              shiny::uiOutput("outcome_var_cor"),
              shiny::helpText("This variable will be excluded from the correlation plot."),
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
              )
            )
          )
        ),
        bslib::nav_panel(
          title = "Baseline characteristics",
          gt::gt_output(outputId = "table1")
        ),
        bslib::nav_panel(
          title = "Variable correlations",
          data_correlations_ui(id = "correlations", height = 600)
        )
      )
    ),
  ##############################################################################
  #########
  #########  Download panel
  #########
  ##############################################################################
  "visuals" = bslib::nav_panel(
    title = "Visuals",
    id = "navvisuals",
    do.call(
      bslib::navset_bar,
      c(
        data_visuals_ui("visuals"),
        shiny::tagList(
          bslib::nav_spacer(),
          bslib::nav_panel(
            title = "Notes",
            shiny::fluidRow(
              shiny::column(width = 2),
              shiny::column(
                width = 8,
                shiny::markdown(readLines("www/notes_visuals.md")),
                shiny::column(width = 2)
              )
            )
          )
        )
      )
    )
  ),
  ##############################################################################
  #########
  #########  Regression analyses panel
  #########
  ##############################################################################
  "analyze" =
    bslib::nav_panel(
      title = "Regression",
      id = "navanalyses",
      bslib::navset_bar(
        title = "",
        # bslib::layout_sidebar(
        #   fillable = TRUE,
        sidebar = bslib::sidebar(
          bslib::accordion(
            open = "acc_reg",
            multiple = FALSE,
            bslib::accordion_panel(
              value = "acc_reg",
              title = "Regression",
              icon = bsicons::bs_icon("calculator"),
              shiny::uiOutput("outcome_var"),
              # shiny::selectInput(
              #   inputId = "design",
              #   label = "Study design",
              #   selected = "no",
              #   inline = TRUE,
              #   choices = list(
              #     "Cross-sectional" = "cross-sectional"
              #   )
              # ),
              shiny::uiOutput("regression_type"),
              shiny::radioButtons(
                inputId = "add_regression_p",
                label = "Add p-value",
                inline = TRUE,
                selected = "yes",
                choices = list(
                  "Yes" = "yes",
                  "No" = "no"
                )
              ),
              bslib::input_task_button(
                id = "load",
                label = "Analyse",
                # icon = shiny::icon("pencil", lib = "glyphicon"),
                icon = bsicons::bs_icon("pencil"),
                label_busy = "Working...",
                icon_busy = fontawesome::fa_i("arrows-rotate",
                  class = "fa-spin",
                  "aria-hidden" = "true"
                ),
                type = "secondary",
                auto_reset = TRUE
              ),
              shiny::helpText("Press 'Analyse' again after changing parameters."),
              shiny::tags$br(),
              shiny::uiOutput("plot_model")
            ),
            bslib::accordion_panel(
              value = "acc_advanced",
              title = "Advanced",
              icon = bsicons::bs_icon("gear"),
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
              )
            )
          ),
          # shiny::helpText(em("Please specify relevant settings for your data, and press 'Analyse'")),
          # shiny::radioButtons(
          #   inputId = "specify_factors",
          #   label = "Specify categorical variables?",
          #   selected = "no",
          #   inline = TRUE,
          #   choices = list(
          #     "Yes" = "yes",
          #     "No" = "no"
          #   )
          # ),
          # shiny::conditionalPanel(
          #   condition = "input.specify_factors=='yes'",
          #   shiny::uiOutput("factor_vars")
          # ),
          # shiny::conditionalPanel(
          #   condition = "output.ready=='yes'",
          # shiny::tags$hr(),
        ),
        bslib::nav_panel(
          title = "Regression table",
          gt::gt_output(outputId = "table2")
        ),
        bslib::nav_panel(
          title = "Coefficient plot",
          shiny::plotOutput(outputId = "regression_plot")
        ),
        bslib::nav_panel(
          title = "Model checks",
          shiny::plotOutput(outputId = "check")
          # shiny::uiOutput(outputId = "check_1")
        )
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
      id = "navdownload",
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
          shiny::tags$b("Code snippets:"),
          shiny::verbatimTextOutput(outputId = "code_import"),
          shiny::verbatimTextOutput(outputId = "code_data"),
          shiny::verbatimTextOutput(outputId = "code_filter"),
          shiny::tags$br(),
          shiny::br(),
          shiny::column(width = 2)
        )
      )
    ),
  ##############################################################################
  #########
  #########  Documentation panel
  #########
  ##############################################################################
  "docs" = bslib::nav_item(
    # shiny::img(shiny::icon("book")),
    shiny::tags$a(
      href = "https://agdamsbo.github.io/freesearcheR/",
      "Docs (external)",
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
# Initial attempt at creating light and dark versions
light <- custom_theme()
dark <- custom_theme(
  bg = "#000",
  fg = "#fff"
)

# Fonts to consider:
# https://webdesignerdepot.com/17-open-source-fonts-youll-actually-love/

ui <- bslib::page_fixed(
  shiny::tags$head(includeHTML(("www/umami-app.html"))),
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
    id = "main_panel",
    ui_elements$home,
    ui_elements$import,
    ui_elements$overview,
    ui_elements$describe,
    ui_elements$visuals,
    ui_elements$analyze,
    ui_elements$download,
    bslib::nav_spacer(),
    ui_elements$docs,
    fillable = FALSE,
    footer = shiny::tags$footer(
      style = "background-color: #14131326; padding: 4px; text-align: center; bottom: 0; width: 100%;",
      shiny::p(
        style = "margin: 1",
        "Data is only stored for analyses and deleted immediately afterwards."
      ),
      shiny::p(
        style = "margin: 1; color: #888;",
        "AG Damsbo | v", app_version(), " | AGPLv3 license | ", shiny::tags$a("Source on Github", href = "https://github.com/agdamsbo/freesearcheR/", target = "_blank", rel = "noopener noreferrer")
      ),
    )
  )
)


########
#### Current file: /Users/au301842/freesearcheR/inst/apps/freesearcheR/server.R 
########

library(readr)
library(MASS)
library(stats)
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
library(esquisse)
library(patchwork)
library(DHARMa)
library(apexcharter)
library(toastui)
library(datamods)
library(data.table)
library(IDEAFilter)
library(shinyWidgets)
library(DT)
library(gtsummary)
# library(freesearcheR)

# source("functions.R")

data(mtcars)
trial <- gtsummary::trial |> default_parsing()

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
    data_filtered = NULL,
    models = NULL,
    code = list()
  )

  ##############################################################################
  #########
  #########  Data import section
  #########
  ##############################################################################

  consider.na <- c("NA", "\"\"", "", "\'\'", "na")

  data_file <- import_file_server(
    id = "file_import",
    show_data_in = "popup",
    trigger_return = "change",
    return_class = "data.frame",
    read_fns = list(
      ods = function(file, which, skip, na) {
        readODS::read_ods(
          path = file,
          # Sheet and skip not implemented for .ods in the original implementation
          sheet = which,
          skip = skip,
          na = na
        )
      },
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
      xls = function(file, which, skip, na) {
        openxlsx2::read_xlsx(
          file = file,
          sheet = which,
          skip_empty_rows = TRUE,
          start_row = skip - 1,
          na.strings = na
          )
      },
      xlsx = function(file, which, skip, na) {
        openxlsx2::read_xlsx(
          file = file,
          sheet = sheet,
          skip_empty_rows = TRUE,
          start_row = skip - 1,
          na.strings = na)
      },
      rds = function(file) {
        readr::read_rds(
          file = file,
          name_repair = "unique_quiet")
      }
    )
  )

  shiny::observeEvent(data_file$data(), {
    shiny::req(data_file$data())
    rv$data_original <- data_file$data()
    rv$code <- append_list(data = data_file$code(), list = rv$code, index = "import")
  })

  data_redcap <- m_redcap_readServer(
    id = "redcap_import"#,
    # output.format = "list"
  )

  shiny::observeEvent(data_redcap(), {
    # rv$data_original <- purrr::pluck(data_redcap(), "data")()
    rv$data_original <- data_redcap()
  })

  output$redcap_prev <- DT::renderDT(
    {
      DT::datatable(head(data_redcap(), 5),
      # DT::datatable(head(purrr::pluck(data_redcap(), "data")(), 5),
        caption = "First 5 observations"
      )
    },
    server = TRUE
  )

  from_env <- datamods::import_globalenv_server(
    id = "env",
    trigger_return = "change",
    btn_show_data = FALSE,
    reset = reactive(input$hidden)
  )

  shiny::observeEvent(from_env$data(), {
    shiny::req(from_env$data())
    rv$data_original <- from_env$data()
    # rv$code <- append_list(data = from_env$code(),list = rv$code,index = "import")
  })

  shiny::observeEvent(rv$data_original, {
    if (is.null(rv$data_original) | NROW(rv$data_original) == 0) {
      shiny::updateActionButton(inputId = "act_start", disabled = TRUE)
    } else {
      shiny::updateActionButton(inputId = "act_start", disabled = FALSE)
    }
  })

  ##############################################################################
  #########
  #########  Data modification section
  #########
  ##############################################################################

  shiny::observeEvent(
    eventExpr = list(
      rv$data_original,
      input$complete_cutoff
    ),
    handlerExpr = {
      shiny::req(rv$data_original)

      rv$data <- rv$data_original |>
        # janitor::clean_names() |>
        default_parsing() |>
        remove_empty_cols(
          cutoff = input$complete_cutoff / 100
        )
    }
  )

  ## For now this solution work, but I would prefer to solve this with the above
  shiny::observeEvent(input$reset_confirm, {
    if (isTRUE(input$reset_confirm)) {
      shiny::req(rv$data_original)
      rv$data <- rv$data_original |>
        default_parsing() |>
        remove_empty_cols(
          cutoff = input$complete_cutoff / 100
        )
    }
  }, ignoreNULL = TRUE)


  shiny::observeEvent(input$data_reset, {
    shinyWidgets::ask_confirmation(
      cancelOnDismiss = TRUE,
      inputId = "reset_confirm",
      title = "Please confirm data reset?",
      type = "warning"
    )
  })

  # shiny::observeEvent(input$reset_confirm, {
  #   rv$data <- rv$data_original |> default_parsing()
  # })

  #########  Overview

  data_summary_server(
    id = "data_summary",
    data = shiny::reactive({
      rv$data_filtered
    }),
    color.main = "#2A004E",
    color.sec = "#C62300",
    pagination = 20
  )

  #########
  #########  Modifications
  #########

  ## Using modified version of the datamods::cut_variable_server function
  ## Further modifications are needed to have cut/bin options based on class of variable
  ## Could be defined server-side

  shiny::observeEvent(
    input$modal_variables,
    modal_update_variables("modal_variables",title = "Modify factor levels")
  )


  #########  Create factor

  shiny::observeEvent(
    input$modal_cut,
    modal_cut_variable("modal_cut",title = "Modify factor levels")
  )

  data_modal_cut <- cut_variable_server(
    id = "modal_cut",
    data_r = shiny::reactive(rv$data)
  )

  shiny::observeEvent(data_modal_cut(), rv$data <- data_modal_cut())

  #########  Modify factor

  shiny::observeEvent(
    input$modal_update,
    datamods::modal_update_factor(id = "modal_update")
  )

  data_modal_update <- datamods::update_factor_server(
    id = "modal_update",
    data_r = reactive(rv$data)
  )

  shiny::observeEvent(data_modal_update(), {
    shiny::removeModal()
    rv$data <- data_modal_update()
  })

  #########  Create column

  shiny::observeEvent(
    input$modal_column,
    datamods::modal_create_column(id = "modal_column",footer = "This is only for advanced users!")
  )
  data_modal_r <- datamods::create_column_server(
    id = "modal_column",
    data_r = reactive(rv$data)
  )
  shiny::observeEvent(
    data_modal_r(),
    {
      rv$data <- data_modal_r()
    }
  )

  #########  Show result
  tryCatch(
    {
      output$table_mod <- toastui::renderDatagrid({
        shiny::req(rv$data)
        # data <- rv$data
        toastui::datagrid(
          # data = rv$data # ,
          data = data_filter(),
          pagination = 10
          # bordered = TRUE,
          # compact = TRUE,
          # striped = TRUE
        )
      })
    },
    warning = function(warn) {
      showNotification(paste0(warn), type = "warning")
    },
    error = function(err) {
      showNotification(paste0(err), type = "err")
    }
  )

  output$code <- renderPrint({
    attr(rv$data, "code")
  })

  # updated_data <- datamods::update_variables_server(
  updated_data <- update_variables_server(
    id = "modal_variables",
    data = reactive(rv$data),
    return_data_on_init = FALSE
  )

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

  shiny::observeEvent(updated_data(), {
    rv$data <- updated_data()
  })

  # IDEAFilter has the least cluttered UI, but might have a License issue
  data_filter <- IDEAFilter::IDEAFilter("data_filter", data = reactive(rv$data), verbose = TRUE)

  shiny::observeEvent(
    list(
      shiny::reactive(rv$data),
      shiny::reactive(rv$data_original),
      data_filter(),
      regression_vars(),
      input$complete_cutoff
    ),
    {
      rv$data_filtered <- data_filter()

      rv$list$data <- data_filter() |>
        REDCapCAST::fct_drop()
    }
  )

  shiny::observeEvent(
    list(
      shiny::reactive(rv$data),
      shiny::reactive(rv$data_original),
      data_filter(),
      shiny::reactive(rv$data_filtered)
    ),
    {
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
          paste(c("data", .x[-1]), collapse = "|> \n ")
        })()

      rv$code <- append_list(data = out, list = rv$code, index = "filter")
    }
  )

  # output$filtered_code <- shiny::renderPrint({
  #   out <- gsub(
  #     "filter", "dplyr::filter",
  #     gsub(
  #       "\\s{2,}", " ",
  #       paste0(
  #         capture.output(attr(rv$data_filtered, "code")),
  #         collapse = " "
  #       )
  #     )
  #   )
  #
  #   out <- strsplit(out, "%>%") |>
  #     unlist() |>
  #     (\(.x){
  #       paste(c("data", .x[-1]), collapse = "|> \n ")
  #     })()
  #
  #   cat(out)
  # })

  output$code_import <- shiny::renderPrint({
    cat(rv$code$import)
    })

  output$code_data <- shiny::renderPrint({
    attr(rv$data, "code")
  })

  output$code_filter <- shiny::renderPrint({
    cat(rv$code$filter)
  })

  ##############################################################################
  #########
  #########  Data analyses Inputs
  #########
  ##############################################################################

  ## Keep these "old" selection options as a simple alternative to the modification pane

  output$include_vars <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "include_vars",
      selected = NULL,
      label = "Covariables to include",
      choices = colnames(rv$data_filtered),
      multiple = TRUE
    )
  })

  output$outcome_var <- shiny::renderUI({
    shiny::selectInput(
      inputId = "outcome_var",
      selected = NULL,
      label = "Select outcome variable",
      choices = colnames(rv$data_filtered),
      multiple = FALSE
    )
  })

  output$regression_type <- shiny::renderUI({
    shiny::req(input$outcome_var)
    shiny::selectizeInput(
      inputId = "regression_type",
      label = "Choose regression analysis",
      ## The below ifelse statement handles the case of loading a new dataset
      choices = possible_functions(
        data = dplyr::select(
          rv$data_filtered,
          ifelse(input$outcome_var %in% names(rv$data_filtered),
            input$outcome_var,
            names(rv$data_filtered)[1]
          )
        ), design = "cross-sectional"
      ),
      multiple = FALSE
    )
  })

  output$factor_vars <- shiny::renderUI({
    shiny::selectizeInput(
      inputId = "factor_vars",
      selected = colnames(rv$data_filtered)[sapply(rv$data_filtered, is.factor)],
      label = "Covariables to format as categorical",
      choices = colnames(rv$data_filtered),
      multiple = TRUE
    )
  })

  ## Collected regression variables
  regression_vars <- shiny::reactive({
    if (is.null(input$include_vars)) {
      out <- colnames(rv$data_filtered)
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
      choices = c(
        "none",
        rv$data_filtered |>
          (\(.x){
            lapply(.x, \(.c){
              if (identical("factor", class(.c))) {
                .c
              }
            }) |>
              dplyr::bind_cols()
          })() |>
          colnames()
      ),
      multiple = FALSE
    )
  })


  output$plot_model <- shiny::renderUI({
    shiny::req(rv$list$regression$tables)
    shiny::selectInput(
      inputId = "plot_model",
      selected = "none",
      label = "Select models to plot",
      choices = names(rv$list$regression$tables),
      multiple = TRUE
    )
  })


  ##############################################################################
  #########
  #########  Descriptive evaluations
  #########
  ##############################################################################

  shiny::observeEvent(
    # ignoreInit = TRUE,
    list(
      shiny::reactive(rv$list$data),
      shiny::reactive(rv$data),
      shiny::reactive(rv$data_original),
      data_filter(),
      input$strat_var,
      input$include_vars,
      input$add_p,
      input$complete_cutoff
    ),
    {
      shiny::req(input$strat_var)
      shiny::req(rv$list$data)

      if (input$strat_var == "none" | !input$strat_var %in% names(rv$list$data)) {
        by.var <- NULL
      } else {
        by.var <- input$strat_var
      }

      rv$list$table1 <-
        rv$list$data |>
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
          if (input$add_p == "yes" & !is.null(by.var)) {
            .x |>
              gtsummary::add_p() |>
              gtsummary::bold_p()
          } else {
            .x
          }
        })()

      # gtsummary::as_kable(rv$list$table1) |>
      #   readr::write_lines(file="./www/_table1.md")
    }
  )

  output$outcome_var_cor <- shiny::renderUI({
    shiny::selectInput(
      inputId = "outcome_var_cor",
      selected = NULL,
      label = "Select outcome variable",
      choices = c(
        colnames(rv$list$data)
        # ,"none"
      ),
      multiple = FALSE
    )
  })

  output$table1 <- gt::render_gt({
    shiny::req(rv$list$table1)

    rv$list$table1 |>
      gtsummary::as_gt() |>
      gt::tab_header(gt::md("**Table 1: Baseline Characteristics**"))
  })

  data_correlations_server(
    id = "correlations",
    data = shiny::reactive({
      shiny::req(rv$list$data)
      out <- dplyr::select(rv$list$data, -!!input$outcome_var_cor)
      #  input$outcome_var_cor=="none"){
      #   out <- rv$list$data
      # }
      out
    }),
    cutoff = shiny::reactive(input$cor_cutoff)
  )

  ##############################################################################
  #########
  #########  Data visuals
  #########
  ##############################################################################

  pl <- data_visuals_server("visuals", data = shiny::reactive(rv$data))

  ##############################################################################
  #########
  #########  Regression model analyses
  #########
  ##############################################################################

  shiny::observeEvent(
    input$load,
    {
      shiny::req(input$outcome_var)
      # browser()
      # Assumes all character variables can be formatted as factors
      # data <- data_filter$filtered() |>
      tryCatch(
        {
          ## Which models to create should be decided by input
          ## Could also include
          ##   imputed or
          ##   minimally adjusted
          model_lists <- list(
            "Univariable" = regression_model_uv_list,
            "Multivariable" = regression_model_list
          ) |>
            lapply(\(.fun){
              ls <- do.call(
                .fun,
                c(
                  list(data = rv$list$data|>
                         (\(.x){
                           .x[regression_vars()]
                         })()),
                  list(outcome.str = input$outcome_var),
                  list(fun.descr = input$regression_type)
                )
              )
            })

          # browser()

          rv$list$regression$params <- get_fun_options(input$regression_type) |>
            (\(.x){
              .x[[1]]
            })()

          rv$list$regression$models <- model_lists

          # names(rv$list$regression)

          # rv$models <- lapply(model_lists, \(.x){
          #   .x$model
          # })
        },
        warning = function(warn) {
          showNotification(paste0(warn), type = "warning")
        },
        error = function(err) {
          showNotification(paste0("Creating regression models failed with the following error: ", err), type = "err")
        }
      )
    }
  )

  shiny::observeEvent(
    ignoreInit = TRUE,
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
        warning = function(warn) {
          showNotification(paste0(warn), type = "warning")
        },
        error = function(err) {
          showNotification(paste0("Running model assumptions checks failed with the following error: ", err), type = "err")
        }
      )
    }
  )

  output$check <- shiny::renderPlot(
    {
      shiny::req(rv$check)
      # browser()
      # p <- plot(rv$check) +
      #   patchwork::plot_annotation(title = "Multivariable regression model checks")

      p <- plot(rv$check) +
        patchwork::plot_annotation(title = "Multivariable regression model checks")

      for (i in seq_len(length(p))) {
        p[[i]] <- p[[i]] + gg_theme_shiny()
      }

      p

      # p + patchwork::plot_layout(ncol = 1, design = ggplot2::waiver())

      # Generate checks in one column
      # layout <- sapply(seq_len(length(p)), \(.x){
      #   patchwork::area(.x, 1)
      # })
      #
      # p + patchwork::plot_layout(design = Reduce(c, layout))

      # patchwork::wrap_plots(ncol=1) +
      # patchwork::plot_annotation(title = 'Multivariable regression model checks')
    },
    height = 600,
    alt = "Assumptions testing of the multivariable regression model"
  )


  shiny::observeEvent(
    input$load,
    {
      shiny::req(rv$list$regression$models)
      tryCatch(
        {
          out <- lapply(rv$list$regression$models, \(.x){
            .x$model
          }) |>
            purrr::map(regression_table)

          if (input$add_regression_p == "no") {
            out <- out |>
              lapply(\(.x){
                .x |>
                  gtsummary::modify_column_hide(
                    column = "p.value"
                  )
              })
          }

          rv$list$regression$tables <- out

          # rv$list$regression$table <- out |>
          #   tbl_merge()

          # gtsummary::as_kable(rv$list$regression$table) |>
          #   readr::write_lines(file="./www/_regression_table.md")

          rv$list$input <- input
        },
        warning = function(warn) {
          showNotification(paste0(warn), type = "warning")
        },
        error = function(err) {
          showNotification(paste0("Creating a regression table failed with the following error: ", err), type = "err")
        }
      )
      rv$ready <- "ready"
    }
  )

  output$table2 <- gt::render_gt({
    shiny::req(rv$list$regression$tables)
    rv$list$regression$tables |>
      tbl_merge() |>
      gtsummary::as_gt() |>
      gt::tab_header(gt::md(glue::glue("**Table 2: {rv$list$regression$params$descr}**")))
  })

  output$regression_plot <- shiny::renderPlot(
    {
      # shiny::req(rv$list$regression$plot)
      shiny::req(input$plot_model)

      out <- merge_long(rv$list$regression, input$plot_model) |>
        plot.tbl_regression(
          colour = "variable",
          facet_col = "model"
        )

      out +
        ggplot2::scale_y_discrete(labels = scales::label_wrap(15)) +
        gg_theme_shiny()

      # rv$list$regression$tables$Multivariable |>
      #   plot(colour = "variable") +
      #   ggplot2::scale_y_discrete(labels = scales::label_wrap(15)) +
      #   gg_theme_shiny()
    },
    height = 500,
    alt = "Regression coefficient plot"
  )

  shiny::conditionalPanel(
    condition = "output.uploaded == 'yes'",
  )

  ##############################################################################
  #########
  #########  Page navigation
  #########
  ##############################################################################

  shiny::observeEvent(input$act_start, {
    bslib::nav_select(id = "main_panel", selected = "Data")
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
      # shiny::req(rv$list$regression)
      ## Notification is not progressing
      ## Presumably due to missing

      # Simplified for .rmd output attempt
      format <- ifelse(type == "docx", "word_document", "odt_document")

      shiny::withProgress(message = "Generating the report. Hold on for a moment..", {
        rv$list |>
          write_rmd(
            output_format = format,
            input = file.path(getwd(), "www/report.rmd")
          )

        # write_quarto(
        #   output_format = type,
        #   input = file.path(getwd(), "www/report.qmd")
        # )
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
#### Current file: /Users/au301842/freesearcheR/inst/apps/freesearcheR/launch.R 
########

shinyApp(ui, server)
