string_split_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      # shiny::textOutput(outputId = ns("no_splits")),
      column(
        width = 4,
        shiny::uiOutput(outputId = ns("variable"))
      ),
      column(
        width = 4,
        shiny::uiOutput(outputId = ns("delim"))
      ),
      column(
        width = 4,
        shiny::uiOutput(outputId = ns("direction"))
      ) # ,
      # column(
      #   width = 3,
      #   actionButton(
      #     inputId = ns("split"),
      #     label = tagList(phosphoricons::ph("scissors"), i18n$t("Split the variable")),
      #     class = "btn-outline-primary float-end"
      #   )
      # )
    ),
    shiny::fluidRow(
      column(
        width = 3,
        shiny::h4(i18n$t("Original data")),
        shiny::tableOutput(outputId = ns("orig_data"))
        # This doesn't render...
        # toastui::datagridOutput2(outputId = ns("orig_data_2"))
      ),
      column(width = 1),
      column(
        width = 8,
        shiny::h4(i18n$t("Preview of result")),
        shiny::tableOutput(outputId = ns("new_data"))
      )
    ),
    actionButton(
      inputId = ns("create"),
      label = tagList(phosphoricons::ph("pencil"), i18n$t("Apply split")),
      class = "btn-outline-primary float-end"
    ),
    tags$div(class = "clearfix")
  )
}



string_split_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id,
    function(input, output, session) {
      rv <- reactiveValues(data = NULL, temp = NULL, out=NULL)

      ns <- session$ns

      # output$no_splits <- shiny::renderText({
      #   req({
      #     data_r()
      #   })
      #
      #   if (any(is_splittable(data_r()))) {
      #     i18n$t("No character variables with accepted delimiters detected.")
      #   }
      # })

      shiny::observe({
        req(data_r())

        # if (any(is_splittable(data_r()))) {
        data <- data_r()
        rv$data <- data

        vars_num <- vapply(data, \(.x){
          is_splittable(.x)
        }, logical(1))
        vars_num <- names(vars_num)[vars_num]

        output$variable <- shiny::renderUI(
          columnSelectInput(
            inputId = ns("variable"),
            data = data,
            label = i18n$t("Variable to split:"),
            width = "100%",
            col_subset = vars_num,
            selected = if (isTruthy(input$variable)) input$variable else vars_num[1]
          )
        )
        # }
        # shinyWidgets::updateVirtualSelect(
        #   inputId = "variable",
        #   choices = vars_num,
        #   selected = if (isTruthy(input$variable)) input$variable else vars_num[1]
        # )
      })

      output$delim <- shiny::renderUI({
        req(rv$data)
        req(input$variable)
        # browser()

        # req({
        #   any(apply(data_r(),2,is_splittable))
        # })
        # if (any(is_splittable(data_r()))) {
        data <- rv$data |>
          dplyr::select(tidyselect::all_of(input$variable))

        delimiters <- Reduce(c, unique(sapply(data[[1]], detect_delimiter)))

        # shiny::textInput(inputId = ns("delim"), label = i18n$t("Text or character to split string by"))
        shiny::selectInput(
          inputId = ns("delim"), label = i18n$t("Select delimiter"),
          choices = setNames(
            delimiters,
            glue::glue("'{delimiters}'")
          ), selected = 1
        )
        # }
      })


      output$direction <- shiny::renderUI({
        # req({
        #   rv$data
        # })

        # if (any(is_splittable(data_r()))) {
        vectorSelectInput(
          inputId = ns("direction"),
          label = i18n$t("Direction:"),
          choices = setNames(
            c(
              "wide",
              "long"
            ),
            c(
              i18n$t("Split string to multiple columns. Keep number of rows."),
              i18n$t("Split string to multiple observations (rows) in the same column. Also ads id and instance columns")
            )
          ),
          selected = "wide",
          width = "100%"
        )
        # }
      })

      observeEvent(
        list(
          input$variable,
          input$delim,
          input$direction
        ),
        {
          req(rv$data)
          req(input$variable)
          req(input$delim)
          req(input$direction)

          data <- rv$data |>
            dplyr::select(tidyselect::all_of(input$variable))
          # browser()
          rv$temp <- separate_string(
            data = data,
            col = input$variable,
            delim = input$delim,
            direction = input$direction
          )
        }
      )

      # shiny::observeEvent(input$split, {
      #   show_data(rv$temp, title = i18n$t("Browse data preview"), type = "modal")
      # })

      ## Toastui would not render the original data, so the solution was to go
      ## with native table rendering, which works, but doesn't please the eye

      output$orig_data <- shiny::renderTable({
        req(data_r())
        req(input$variable)
        data <- data_r() |>
          dplyr::select(tidyselect::all_of(input$variable))
        # browser()
        head(data, 10)
      })

      # output$orig_data_2 <- toastui::renderDatagrid2({
      #   req(data_r())
      #   req(input$variable)
      #   data <- data_r() |>
      #     dplyr::select(tidyselect::all_of(input$variable))
      #   # browser()
      #   toastui::datagrid(head(data, 10))
      # })



      output$new_data <- shiny::renderTable({
        shiny::req(rv$temp)
        data <- rv$temp
        head(data, 10)
        # toastui::datagrid(
        #   data = head(data, 100),
        #   colwidths = "guess",
        #   theme = "default",
        #   bodyHeight = "auto", pagination = 10
        # )
      })

      data_split_r <- reactive({
        req(rv$temp)

        data <- rv$data

        parameters <- list(
          col = input$variable,
          delim = input$delim,
          direction = input$direction
        )

        out <- tryCatch({
          rlang::exec(separate_string, !!!modifyList(
            parameters,
            list(
              data = data
            )
          ))
        })
        # browser()

        # separate_string(
        #   data = data,
        #
        # )

        code <- rlang::call2(
          "separate_string",
          !!!parameters,
          .ns = "FreesearchR"
        )
        attr(out, "code") <- code
      out})

      data_returned_r <- observeEvent(input$create, {
        rv$out <- data_split_r()
      })

      return(reactive(rv$out))
    }
  )
}


modal_string_split <- function(id,
                               title = i18n$t("Split character string"),
                               easyClose = TRUE,
                               size = "xl",
                               footer = NULL) {
  ns <- NS(id)
  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    string_split_ui(id),
    tags$div(
      style = "display: none;",
      textInput(inputId = ns("hidden"), label = NULL, value = datamods:::genId())
    ),
    easyClose = easyClose,
    size = size,
    footer = footer
  ))
}


### Helpers

#' Separate string wide or long
#'
#' @param data data
#' @param col column
#' @param delim delimiter
#' @param direction target direction
#'
#' @returns data.frame
#' @export
#'
separate_string <- function(data, col, delim, direction = c("wide", "long")) {
  direction <- match.arg(direction)

  if (direction == "long") {
    out <- data |>
      dplyr::mutate(id_str_split = dplyr::row_number()) |>
      dplyr::group_by_at("id_str_split") |>
      tidyr::separate_longer_delim(cols = tidyselect::all_of(col), delim = delim) |>
      dplyr::mutate(instance_str_split = dplyr::row_number()) |>
      # add_instance(by="id")
      dplyr::ungroup() |>
      dplyr::mutate(dplyr::across(tidyselect::matches(col), trimws))
  } else if (direction == "wide") {
    ## Experiment of wide to long

    out <- data |>
      tidyr::separate_wider_delim(
        cols = tidyselect::all_of(col),
        delim = delim,
        names_sep = "_",
        too_few = "align_start"
      ) |>
      dplyr::mutate(dplyr::across(tidyselect::starts_with(col), trimws))
  }

  out
}



#' Detect delimiters in string based on allowed delimiters
#'
#' @description
#' Accepts any repeat of delimiters and includes surrounding whitespace
#'
#'
#' @param text character vector
#' @param delimiters allowed delimiters
#'
#' @returns character vector
#' @export
#'
#' @examples
#' sapply(c("Walk - run", "Sel__Re", "what;now"), detect_delimiter)
detect_delimiter <- function(data, delimiters = c("_", "-", ";", "\n", ",")) {
  # Create patterns for each delimiter with potential surrounding whitespace
  patterns <- paste0("\\s*\\", delimiters, "+\\s*")

  # Check each pattern
  out <- sapply(patterns, \(.x){
    if (grepl(.x, data)) {
      # Extract the actual matched delimiter with whitespace
      regmatches(data, regexpr(.x, data))
    }
  })

  Reduce(c, out)
}


#' Determine if any variable in data frame character and contains recognized delimiters
#'
#' @param data vector or data.frame
#'
#' @returns logical
#' @export
#'
#' @examples
#' any(apply(mtcars, 2, is_splittable))
#' is_splittable(mtcars)
is_splittable <- function(data) {
  if (is.data.frame(data)) {
    return(apply(data, 2, is_splittable))
  }

  if (is.character(data)) {
    if (length(Reduce(c, unique(sapply(data, detect_delimiter)))) > 0) {
      TRUE
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}
