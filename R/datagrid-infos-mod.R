
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
          i18n$t("Unique:"), length(unique(x))
        ),
        tags$div(
          i18n$t("Missing:"), sum(is.na(x))
        ),
        tags$div(
          style = htmltools::css(whiteSpace = "normal", wordBreak = "break-all"),
          i18n$t("Most Common:"), gsub(
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
      class(x)
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
          i18n$t("Missing:"), fmt_p(missing, total)
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
          i18n$t("Min:"), round(min(x, na.rm = TRUE), 2)
        ),
        tags$div(
          i18n$t("Mean:"), round(mean(x, na.rm = TRUE), 2)
        ),
        tags$div(
          i18n$t("Median:"), round(median(x, na.rm = TRUE), 2)
        ),
        tags$div(
          i18n$t("Max:"), round(max(x, na.rm = TRUE), 2)
        ),
        tags$div(
          i18n$t("Missing:"), sum(is.na(x))
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
          i18n$t("Min:"), min(x, na.rm = TRUE)
        ),
        tags$div(
          i18n$t("Max:"), max(x, na.rm = TRUE)
        ),
        tags$div(
          i18n$t("Missing:"), sum(is.na(x))
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
          i18n$t("Min:"), min(x, na.rm = TRUE)
        ),
        tags$div(
          i18n$t("Max:"), max(x, na.rm = TRUE)
        ),
        tags$div(
          i18n$t("Missing:"), sum(is.na(x))
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
          i18n$t("Unique:"), length(unique(x))
        ),
        tags$div(
          i18n$t("Missing:"), sum(is.na(x))
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
        } else if (inherits(values, c("factor","logical"))) {
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
