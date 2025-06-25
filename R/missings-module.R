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
        data = NULL
      )

      rv$data <- shiny::reactive({
        df_tbl <- datar()
        by_var <- variabler()

        tryCatch(
          {
            if (!is.null(by_var) && by_var != "" && by_var %in% names(df_tbl)) {
              df_tbl[[by_var]] <- ifelse(is.na(df_tbl[[by_var]]), "Missing", "Non-missing")

              out <- gtsummary::tbl_summary(df_tbl, by = by_var) |>
                gtsummary::add_p()
            } else {
              out <- gtsummary::tbl_summary(df_tbl)
            }
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
          title <- "No missing observations"
        } else {
          title <- paste("Missing vs non-missing observations in", variabler())
        }

        rv$data() |>
          gtsummary::as_gt() |>
          gt::tab_header(title = gt::md(title))
      })
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

    observeEvent(input$modal_missings, {
      tryCatch(
        {
          modal_visual_missings(data = data_demo, id = "modal_missings")
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


modal_visual_missings <- function(data,
                                  title = "Visual overview of data classes and missing observations",
                                  easyClose = TRUE,
                                  size = "xl",
                                  footer = NULL,
                                  ...) {
  datar <- if (is.reactive(data)) data else reactive(data)

  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    tags$div(
      # apexcharter::renderApexchart({
      #   missings_apex_plot(datar(), ...)
      # })
      shiny::renderPlot({
        visdat::vis_dat(datar(),sort_type = FALSE) +
          ggplot2::guides(fill = ggplot2::guide_legend(title = "Data class")) +
          # ggplot2::theme_void() +
          ggplot2::theme(
            # legend.position = "none",
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            # axis.text.y = element_blank(),
            # axis.title.y = element_blank(),
            text = ggplot2::element_text(size = 18),
            # axis.text = ggplot2::element_blank(),
            # panel.background = ggplot2::element_rect(fill = "white"),
            # plot.background = ggplot2::element_rect(fill = "white"),
            # panel.border = ggplot2::element_blank()
            plot.title = ggplot2::element_blank()
          )
      })
    ),
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
  browser()

  df_plot <- purrr::map_df(data, \(x){
    ifelse(is.na(x),
      yes = NA,
      no = glue::glue_collapse(class(x),
        sep = "\n"
      )
    )
  }) %>%
    dplyr::mutate(rows = dplyr::row_number()) %>%
    tidyr::pivot_longer(
      cols = -rows,
      names_to = "variable", values_to = "valueType", values_transform = list(valueType = as.character)
    ) %>%
    dplyr::arrange(rows, variable, valueType)


  df_plot$valueType_num <- df_plot$valueType |>
    forcats::as_factor() |>
    as.numeric()


  df_plot$valueType[is.na(df_plot$valueType)] <- "NA"
  df_plot$valueType_num[is.na(df_plot$valueType_num)] <- max(df_plot$valueType_num, na.rm = TRUE) + 1

  labels <- setNames(unique(df_plot$valueType_num), unique(df_plot$valueType))

  if (any(df_plot$valueType == "NA")) {
    colors <- setNames(c(viridisLite::viridis(n = length(labels) - 1), "#999999"), names(labels))
  } else {
    colors <- setNames(viridisLite::viridis(n = length(labels)), names(labels))
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

  out <- apexcharter::apex(
    data = df_plot,
    type = "heatmap",
    mapping = apexcharter::aes(x = variable, y = rows, fill = valueType_num),
    ...
  ) %>%
    apexcharter::ax_stroke(width = NULL) |>
    apexcharter::ax_plotOptions(
      heatmap = apexcharter::heatmap_opts(
        radius = 0,
        enableShades = FALSE,
        colorScale = list(
          ranges = label_list
        ),
        useFillColorAsStroke = TRUE
      )
    ) %>%
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
