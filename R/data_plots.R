# source(here::here("functions.R"))

#' Data correlations evaluation module
#'
#' @param id Module id. (Use 'ns("id")')
#'
#' @name data-correlations
#' @returns Shiny ui module
#' @export
#'
data_visuals_ui <- function(id, tab_title="Plots", ...) {
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
            format = shinyWidgets::wNumbFormat(decimals=0),
            color = datamods:::get_primary_color()
          ),
          shinyWidgets::noUiSliderInput(
            inputId = ns("width"),
            label = "Plot width (mm)",
            min = 50,
            max = 300,
            value = 100,
            step = 1,
            format = shinyWidgets::wNumbFormat(decimals=0),
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
          ggplot2::ggsave(filename = file,
                          plot = rv$plot(),
                          width = input$width,
                          height = input$height,
                          dpi = 300,
                          units = "mm",scale = 2)
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
#' @returns
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
#' @returns
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
      descr = "Stacked horizontal bars (Grotta bars)",
      primary.type = c("dichotomous", "ordinal"),
      secondary.type = c("dichotomous", "ordinal"),
      tertiary.type = c("dichotomous", "ordinal"),
      secondary.extra = "none"
    ),
    plot_violin = list(
      descr = "Violin plot",
      primary.type = c("continuous", "dichotomous", "ordinal"),
      secondary.type = c("dichotomous", "ordinal"),
      tertiary.type = c("dichotomous", "ordinal"),
      secondary.extra = "none"
    ),
    plot_ridge = list(
      descr = "Ridge plot",
      primary.type = "continuous",
      secondary.type = c("dichotomous", "ordinal"),
      tertiary.type = c("dichotomous", "ordinal"),
      secondary.extra = NULL
    ),
    plot_scatter = list(
      descr = "Scatter plot",
      primary.type = "continuous",
      secondary.type = c("continuous", "ordinal"),
      tertiary.type = c("dichotomous", "ordinal"),
      secondary.extra = NULL
    )
  )
}

#' Title
#'
#' @returns
#' @export
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
#' @param type plot type (derived from possible_plots() and matches custom function)
#' @param ... ignored for now
#'
#' @returns
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
#' @examples
#' mtcars |> plot_hbars(x = "carb", y = "cyl")
#' mtcars |> plot_hbars(x = "carb", y = NULL)
plot_hbars <- function(data, x, y, z = NULL) {
  out <- vertical_stacked_bars(data = data, score = x, group = y, strata = z)

  out
}


#' Vertical stacked bar plot wrapper
#'
#' @param data
#' @param score
#' @param group
#' @param strata
#' @param t.size
#'
#' @return
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
#'
#' @returns character string
#' @export
#'
#' @examples
#' mtcars |> get_label(var = "mpg")
#' mtcars$mpg |> get_label()
#' gtsummary::trial |> get_label(var = "trt")
#' 1:10 |> get_label()
get_label <- function(data, var = NULL) {
  if (!is.null(var)) {
    data <- data[[var]]
  }

  out <- REDCapCAST::get_attr(data = data, attr = "label")
  if (is.na(out)) {
    if (is.null(var)) {
      out <- deparse(substitute(data))
    } else {
      out <- gsub('\"', "", deparse(substitute(var)))
    }
  }
  out
}


#' Beatiful violin plot
#'
#' @returns ggplot2 object
#' @export
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
      response = x, xtitle = get_label(data, var = x), ytitle = get_label(data, var = y)
    )
  })

  patchwork::wrap_plots(out)
}


#' Beatiful violin plot
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' mtcars |> plot_scatter(x = "mpg", y = "wt")
plot_scatter <- function(data, x, y, z = NULL) {
  if (is.null(z)) {
    rempsyc::nice_scatter(
      data = data,
      predictor = y,
      response = x, xtitle = get_label(data, var = x), ytitle = get_label(data, var = y)
    )
  } else {
    rempsyc::nice_scatter(
      data = data,
      predictor = y,
      response = x,
      group = z
    )
  }
}

