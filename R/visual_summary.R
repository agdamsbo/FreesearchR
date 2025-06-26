#' Data correlations evaluation module
#'
#' @param id Module id
#'
#' @name data-missings
#' @returns Shiny ui module
#' @export
visual_summary_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::plotOutput(outputId = ns("visual_plot"), height = "70vh")
  )
}

visual_summary_server <- function(id,
                                  data_r=shiny::reactive(NULL),
                                  ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      # ns <- session$ns
      rv <- shiny::reactiveValues(data = NULL)

      shiny::bindEvent(shiny::observe({
        data <- data_r()
        rv$data <- data
        # vars_num <- vapply(data, \(.x){
        #   is.numeric(.x) || is_datetime(.x)
        # }, logical(1))
        # vars_num <- names(vars_num)[vars_num]
        # shinyWidgets::updateVirtualSelect(
        #   inputId = "variable",
        #   choices = vars_num,
        #   selected = if (isTruthy(input$variable)) input$variable else vars_num[1]
        # )
      }), data_r(), input$hidden)

      # datar <- if (is.reactive(data)) data else reactive(data)


      # apexcharter::renderApexchart({
      #   missings_apex_plot(datar(), ...)
      # })
      output$visual_plot <- shiny::renderPlot(expr = {
        visual_summary(data = rv$data,...)
      })
    }
  )
}

visual_summary_demo_app <- function() {
  ui <- shiny::fluidPage(
    shiny::actionButton(
      inputId = "modal_missings",
      label = "Visual summary",
      width = "100%",
      disabled = FALSE
    )
  )
  server <- function(input, output, session) {
    data_demo <- mtcars
    data_demo[sample(1:32, 10), "cyl"] <- NA
    data_demo[sample(1:32, 8), "vs"] <- NA

    visual_summary_server(id = "data", data = shiny::reactive(data_demo))

    observeEvent(input$modal_missings, {
      tryCatch(
        {
          modal_visual_summary(id = "data")
        },
        error = function(err) {
          showNotification(paste0("We encountered the following error browsing your data: ", err), type = "err")
        }
      )
    })
  }
  shiny::shinyApp(ui, server)
}

visual_summary_demo_app()


modal_visual_summary <- function(id,
                                 title = "Visual overview of data classes and missing observations",
                                 easyClose = TRUE,
                                 size = "xl",
                                 footer = NULL,
                                 ...) {
  showModal(modalDialog(
    title = tagList(title, datamods:::button_close_modal()),
    visual_summary_ui(id = id),
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
  l <- data_summary_gather(data, ...)

  df_plot <- l$data

  out <- apexcharter::apex(
    data = df_plot,
    type = "heatmap",
    mapping = apexcharter::aes(x = variable, y = rows, fill = valueType_num),
    ...
  ) |>
    apexcharter::ax_stroke(width = NULL) |>
    apexcharter::ax_plotOptions(
      heatmap = apexcharter::heatmap_opts(
        radius = 0,
        enableShades = FALSE,
        colorScale = list(
          ranges = l$labels
        ),
        useFillColorAsStroke = TRUE
      )
    ) |>
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



#' Ggplot2 data summary visualisation based on visdat::vis_dat.
#'
#' @param data data
#' @param ... optional arguments passed to data_summary_gather()
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' data_demo <- mtcars
#' data_demo[sample(1:32, 10), "cyl"] <- NA
#' data_demo[sample(1:32, 8), "vs"] <- NA
#' visual_summary(data_demo)
#' visual_summary(data_demo, palette.fun = scales::hue_pal())
#' visual_summary(dplyr::storms)
#' visual_summary(dplyr::storms, summary.fun = data_type)
visual_summary <- function(data, legend.title = "Data class", ...) {
  l <- data_summary_gather(data, ...)

  df <- l$data

  df$valueType <- factor(df$valueType, levels = names(l$colors))
  df$variable <- factor(df$variable, levels = unique_short(names(data)))

  ggplot2::ggplot(data = df, ggplot2::aes(x = variable, y = rows)) +
    ggplot2::geom_raster(ggplot2::aes(fill = valueType)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 45,
      vjust = 1, hjust = 1
    )) +
    ggplot2::scale_fill_manual(values = l$colors) +
    ggplot2::labs(x = "", y = "Observations") +
    ggplot2::scale_y_reverse() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::guides(colour = "none") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title)) +
    # change the limits etc.
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Type")) +
    # add info about the axes
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0)) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 18),
      plot.title = ggplot2::element_blank()
    )
}

#' Data summary for printing visual summary
#'
#' @param data data.frame
#' @param fun summary function. Default is "class"
#' @param palette.fun optionally use specific palette functions. First argument
#' has to be the length.
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' mtcars |> data_summary_gather()
data_summary_gather <- function(data, summary.fun = class, palette.fun = viridisLite::viridis) {
  df_plot <- setNames(data, unique_short(names(data))) |>
    purrr::map_df(\(x){
      ifelse(is.na(x),
        yes = NA,
        no = glue::glue_collapse(summary.fun(x),
          sep = "\n"
        )
      )
    }) |>
    dplyr::mutate(rows = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = -rows,
      names_to = "variable", values_to = "valueType", values_transform = list(valueType = as.character)
    ) |>
    dplyr::arrange(rows, variable, valueType)


  df_plot$valueType_num <- df_plot$valueType |>
    forcats::as_factor() |>
    as.numeric()

  df_plot$valueType[is.na(df_plot$valueType)] <- "NA"
  df_plot$valueType_num[is.na(df_plot$valueType_num)] <- max(df_plot$valueType_num, na.rm = TRUE) + 1

  labels <- setNames(unique(df_plot$valueType_num), unique(df_plot$valueType)) |> sort()

  if (any(df_plot$valueType == "NA")) {
    colors <- setNames(c(palette.fun(length(labels) - 1), "#999999"), names(labels))
  } else {
    colors <- setNames(palette.fun(length(labels)), names(labels))
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

  list(data = df_plot, colors = colors, labels = label_list)
}



#' Create unique short names of character vector items based on index
#'
#' @description
#' The function will prefer original names, and only append index to long
#' strings.
#'
#'
#' @param data character vector
#' @param max maximum final name length
#'
#' @returns character vector
#' @export
#'
#' @examples
#' c("kahdleidnsallskdj", "hej") |> unique_short()
unique_short <- function(data, max = 15) {
  purrr::imap(data, \(.x, .i){
    if (nchar(.x) > max) {
      glue::glue("{substr(.x,1,(max-(nchar(.i)+1)))}_{.i}")
    } else {
      .x
    }
  }) |> unlist()
}
