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
columnSelectInput <- function(inputId,
                              label,
                              data,
                              selected = "",
                              ...,
                              col_subset = NULL,
                              placeholder = "",
                              onInitialize,
                              none_label = "No variable selected",
                              maxItems = NULL) {
  datar <- if (is.reactive(data))
    data
  else
    reactive(data)
  col_subsetr <- if (is.reactive(col_subset))
    col_subset
  else
    reactive(col_subset)

  labels <- Map(function(col) {
    json <- sprintf(
      IDEAFilter:::strip_leading_ws(
        '
    {
      "name": "%s",
      "label": "%s",
      "dataclass": "%s",
      "datatype": "%s"
    }'
      ),
      col,
      attr(datar()[[col]], "label") %||% "",
      IDEAFilter:::get_dataFilter_class(datar()[[col]]),
      data_type(datar()[[col]])
    )
  }, col = names(datar()))

  if (!"none" %in% names(datar())) {
    labels <- c("none" = list(
      sprintf(
        '\n    {\n      \"name\": \"none\",\n      \"label\": \"%s\",\n      \"dataclass\": \"\",\n      \"datatype\": \"\"\n    }',
        none_label
      )
    ), labels)
    choices <- setNames(names(labels), labels)
    choices <- choices[match(if (length(col_subsetr()) == 0 ||
                                 isTRUE(col_subsetr() == ""))
      names(datar())
      else
        col_subsetr(), choices)]
  } else {
    choices <- setNames(names(datar()), labels)
    choices <- choices[match(if (length(col_subsetr()) == 0 ||
                                 isTRUE(col_subsetr() == ""))
      choices
      else
        col_subsetr(), choices)]
  }

  shiny::selectizeInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    ...,
    options = c(list(
      render = I(
        "{
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
      }"
      )
    ), if (!is.null(maxItems))
      list(maxItems = maxItems))
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
#'   shinyApp(
#'     ui = fluidPage(
#'       shiny::uiOutput("select"),
#'       tableOutput("data")
#'     ),
#'     server = function(input, output) {
#'       output$select <- shiny::renderUI({
#'         vectorSelectInput(
#'           inputId = "variable", label = "Variable:",
#'           data = c(
#'             "Cylinders" = "cyl",
#'             "Transmission" = "am",
#'             "Gears" = "gear"
#'           )
#'         )
#'       })
#'
#'       output$data <- renderTable(
#'         {
#'           mtcars[, c("mpg", input$variable), drop = FALSE]
#'         },
#'         rownames = TRUE
#'       )
#'     }
#'   )
#' }
vectorSelectInput <- function(inputId,
                              label,
                              choices,
                              selected = "",
                              ...,
                              placeholder = "",
                              onInitialize) {
  datar <- if (shiny::is.reactive(choices))
    data
  else
    shiny::reactive(choices)

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
    options = c(list(
      render = I(
        "{
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
      }"
      )
    ))
  )
}


#' A selectizeInput customized for named vectors of color names supported by
#' \code{\link{generate_colors}}
#'
#' @param inputId passed to \code{\link[shiny]{selectizeInput}}
#' @param label passed to \code{\link[shiny]{selectizeInput}}
#' @param choices A named \code{vector} from which fields should be populated
#' @param selected default selection
#' @param previews number of preview colors. Default is 4.
#' @param ... passed to \code{\link[shiny]{selectizeInput}}
#' @param placeholder passed to \code{\link[shiny]{selectizeInput}} options
#' @param onInitialize passed to \code{\link[shiny]{selectizeInput}} options
#'
#' @returns a \code{\link[shiny]{selectizeInput}} dropdown element
#' @export
#'
#' @examples
#' if (shiny::interactive()) {
#'top_palettes <- c(
#'"Perceptual (blue-yellow)"   = "viridis",
#'"Perceptual (fire)"          = "plasma",
#'"Colour-blind friendly"      = "Okabe-Ito",
#'"Qualitative (bold)"         = "Dark 2",
#'"Qualitative (paired)"       = "Paired",
#'"Sequential (blues)"         = "Blues",
#'"Diverging (red-blue)"       = "RdBu",
#'"Tableau style"              = "Tableau 10",
#'"Pastel"                     = "Pastel 1",
#'"Rainbow"                    = "rainbow"
#')
#'   shinyApp(
#'     ui = fluidPage(
#'     titlePanel("Color Palette Select Test"),
#'     colorSelectInput(
#'       inputId  = "palette",
#'       label    = "Color palette",
#'       choices  = top_palettes,
#'       selected = "viridis"
#'     ),
#'     verbatimTextOutput("selected")
#'     ),
#'     server = function(input, output, session) {
#'     output$selected <- renderPrint(input$palette)
#'     }
#'   )
#' }
colorSelectInput <- function(inputId,
                             label,
                             choices,
                             selected = NULL,
                             previews = 4,
                             ...,
                             placeholder = "") {
  vals <- if (shiny::is.reactive(choices)) {
    choices()
  } else{
    choices
  }

  swatch_html <- function(palette_name) {
    colors <- tryCatch(
      suppressMessages(generate_colors(previews, palette_name)),
      error = function(e)
        rep("#cccccc", 3)
    )
    # Strip alpha channel to ensure valid 6-digit CSS hex
    colors <- substr(colors, 1, 7)
    paste0(
      sprintf(
        "<span style='display:inline-block;width:12px;height:12px;background:%s;border-radius:2px;margin-right:1px;'></span>",
        colors
      ),
      collapse = ""
    )
  }

  labels <- sprintf(
    '{"name": "%s", "label": "%s", "swatch": "%s"}',
    vals,
    names(vals) %||% "",
    vapply(vals, swatch_html, character(1))
  )

  choices_new <- stats::setNames(vals, labels)

  if (is.null(selected) || selected == "") {
    selected <- vals[[1]]
  }

  shiny::selectizeInput(
    inputId  = inputId,
    label    = label,
    choices  = choices_new,
    selected = selected,
    ...,
    options = list(
      render = I(
        "{
    option: function(item, escape) {
      item.data = JSON.parse(item.label);
      return '<div style=\"padding:3px 12px\">' +
               '<div><strong>' + escape(item.data.name) + '</strong></div>' +
               (item.data.label != '' ? '<div><small>' + escape(item.data.label) + '</small></div>' : '') +
               '<div style=\"margin-top:4px\">' + item.data.swatch + '</div>' +
             '</div>';
    },
    item: function(item, escape) {
      item.data = JSON.parse(item.label);
      return '<div style=\"display:flex;align-items:center;gap:6px\">' +
               '<span>' + escape(item.data.name) + '</span>' +
               item.data.swatch +
             '</div>';
    }
  }"
      ),
      onInitialize = I(
        "function() {
    var self = this;
    self.$control_input.prop('readonly', true);
    self.$control_input.css('cursor', 'default');
    self.$control.css('cursor', 'pointer');
  }"
      )
    )
  )
}
