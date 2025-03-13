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
        out |> dplyr::mutate(dplyr::across(tidyselect::everything(),as.numeric))
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


cor_demo_app <- function() {
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
    data_correlations_server("data", data = shiny::reactive(default_parsing(mtcars)), cutoff = shiny::reactive(input$cor_cutoff))
  }
  shiny::shinyApp(ui, server)
}

cor_demo_app()
