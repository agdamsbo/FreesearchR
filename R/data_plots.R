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
    plot_ridge = list(
      descr = "Ridge plot",
      note = "An alternative option to visualise data distribution",
      primary.type = "continuous",
      secondary.type = c("dichotomous", "ordinal"),
      tertiary.type = c("dichotomous", "ordinal"),
      secondary.extra = NULL
    ),
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
#' mtcars |> get_label()
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
      response = x, xtitle = get_label(data, var = x), ytitle = get_label(data, var = y)
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

#' Readying data for sankey plot
#'
#' @param data
#' @param x
#' @param y
#' @param z
#'
#' @returns
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

  if (is.factor(data[[x]])){
    index <- match(levels(data[[x]]),str_remove_last(levels(out$lx),"\n"))
    out$lx <- factor(out$lx,levels=levels(out$lx)[index])
  }

  if (is.factor(data[[y]])){
    index <- match(levels(data[[y]]),str_remove_last(levels(out$ly),"\n"))
    out$ly <- factor(out$ly,levels=levels(out$ly)[index])
  }

  out
}

str_remove_last <- function(data,pattern="\n"){
  strsplit(data,split = pattern) |>
    lapply(\(.x)paste(unlist(.x[[-length(.x)]]),collapse=pattern)) |>
    unlist()
}

#' Line breaking at given number of characters for nicely plotting labels
#'
#' @param data
#' @param lineLength
#'
#' @returns
#' @export
#'
#' @examples
line_break <- function(data, lineLength = 20) {
  # gsub(paste0('(.{1,',lineLength,'})(\\s)'), '\\1\n', data)
  paste(strwrap(data, lineLength), collapse = "\n")
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
    plot_sankey_single(.ds,x = x, y = y,color.group = color.group, colors = colors)
  })

  patchwork::wrap_plots(out)
}

default_theme <- function() {
  theme_void()
}

#' Beautiful sankey plot
#'
#' @param color.group
#' @param colors
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' ds <- data.frame(g = sample(LETTERS[1:2], 100, TRUE), first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)), last = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)))
#' ds |> plot_sankey_single("first", "last")
#' ds |> plot_sankey_single("first", "last", color.group = "y")
plot_sankey_single <- function(data,x,y, color.group = "x", colors = NULL){
  data <- data |> sankey_ready(x = x, y = y)
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
