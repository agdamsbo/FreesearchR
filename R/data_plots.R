# source(here::here("functions.R"))

#' Data correlations evaluation module
#'
#' @param id Module id. (Use 'ns("id")')
#'
#' @name data-plots
#' @returns Shiny ui module
#' @export
#'
data_visuals_ui <- function(id, tab_title = "Plots", ...) {
  ns <- shiny::NS(id)

  list(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        bslib::accordion(
          multiple = FALSE,
          bslib::accordion_panel(
            title = "Creating plot",
            icon = bsicons::bs_icon("graph-up"),
            shiny::uiOutput(outputId = ns("primary")),
            shiny::helpText('Only non-text variables are available for plotting. Go the "Data" to reclass data to plot.'),
            shiny::tags$br(),
            shiny::uiOutput(outputId = ns("type")),
            shiny::uiOutput(outputId = ns("secondary")),
            shiny::uiOutput(outputId = ns("tertiary")),
            shiny::br(),
            shiny::actionButton(
              inputId = ns("act_plot"),
              label = "Plot",
              width = "100%",
              icon = shiny::icon("palette"),
              disabled = FALSE
            ),
            shiny::helpText('Adjust settings, then press "Plot".')
          ),
          bslib::accordion_panel(
            title = "Download",
            icon = bsicons::bs_icon("download"),
            shinyWidgets::noUiSliderInput(
              inputId = ns("height_slide"),
              label = "Plot height (mm)",
              min = 50,
              max = 300,
              value = 100,
              step = 1,
              format = shinyWidgets::wNumbFormat(decimals = 0),
              color = datamods:::get_primary_color(),
              inline = TRUE
            ),
            # shiny::numericInput(
            #   inputId = ns("height_numeric"),
            #   label = "Plot height (mm)",
            #   min = 50,
            #   max = 300,
            #   value = 100
            # ),
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
        ),
        shiny::p("We have collected a few notes on visualising data and details on the options included in FreesearchR:", shiny::tags$a(
          href = "https://agdamsbo.github.io/FreesearchR/articles/visuals.html",
          "View notes in new tab",
          target = "_blank",
          rel = "noopener noreferrer"
        ))
      ),
      shiny::plotOutput(ns("plot"), height = "70vh"),
      shiny::tags$br(),
      shiny::tags$br(),
      shiny::htmlOutput(outputId = ns("code_plot"))
    )
  )
  # )
}


#'
#' @param data data
#' @param ... ignored
#'
#' @name data-plots
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
        plot = NULL,
        code = NULL
      )

      # ## --- New attempt
      #
      # rv$plot.params <- shiny::reactive({
      #   get_plot_options(input$type) |> purrr::pluck(1)
      # })
      #
      # c(output,
      #            list(shiny::renderUI({
      #   columnSelectInput(
      #     inputId = ns("primary"),
      #     data = data,
      #     placeholder = "Select variable",
      #     label = "Response variable",
      #     multiple = FALSE
      #   )
      # }),
      # shiny::renderUI({
      #   shiny::req(input$primary)
      #   # browser()
      #
      #   if (!input$primary %in% names(data())) {
      #     plot_data <- data()[1]
      #   } else {
      #     plot_data <- data()[input$primary]
      #   }
      #
      #   plots <- possible_plots(
      #     data = plot_data
      #   )
      #
      #   plots_named <- get_plot_options(plots) |>
      #     lapply(\(.x){
      #       stats::setNames(.x$descr, .x$note)
      #     })
      #
      #   vectorSelectInput(
      #     inputId = ns("type"),
      #     selected = NULL,
      #     label = shiny::h4("Plot type"),
      #     choices = Reduce(c, plots_named),
      #     multiple = FALSE
      #   )
      # }),
      # shiny::renderUI({
      #   shiny::req(input$type)
      #
      #   cols <- c(
      #     rv$plot.params()[["secondary.extra"]],
      #     all_but(
      #       colnames(subset_types(
      #         data(),
      #         rv$plot.params()[["secondary.type"]]
      #       )),
      #       input$primary
      #     )
      #   )
      #
      #   columnSelectInput(
      #     inputId = ns("secondary"),
      #     data = data,
      #     selected = cols[1],
      #     placeholder = "Please select",
      #     label = if (isTRUE(rv$plot.params()[["secondary.multi"]])) "Additional variables" else "Secondary variable",
      #     multiple = rv$plot.params()[["secondary.multi"]],
      #     maxItems = rv$plot.params()[["secondary.max"]],
      #     col_subset = cols,
      #     none_label = "No variable"
      #   )
      # }),
      # shiny::renderUI({
      #   shiny::req(input$type)
      #   columnSelectInput(
      #     inputId = ns("tertiary"),
      #     data = data,
      #     placeholder = "Please select",
      #     label = "Grouping variable",
      #     multiple = FALSE,
      #     col_subset = c(
      #       "none",
      #       all_but(
      #         colnames(subset_types(
      #           data(),
      #           rv$plot.params()[["tertiary.type"]]
      #         )),
      #         input$primary,
      #         input$secondary
      #       )
      #     ),
      #     none_label = "No stratification"
      #   )
      # })
      # )|> setNames(c("primary","type","secondary","tertiary")),keep.null = TRUE)


      output$primary <- shiny::renderUI({
        shiny::req(data())
        columnSelectInput(
          inputId = ns("primary"),
          col_subset = names(data())[sapply(data(), data_type) != "text"],
          data = data,
          placeholder = "Select variable",
          label = "Response variable",
          multiple = FALSE
        )
      })

      # shiny::observeEvent(data, {
      #   if (is.null(data()) | NROW(data()) == 0) {
      #     shiny::updateActionButton(inputId = ns("act_plot"), disabled = TRUE)
      #   } else {
      #     shiny::updateActionButton(inputId = ns("act_plot"), disabled = FALSE)
      #   }
      # })


      output$type <- shiny::renderUI({
        shiny::req(input$primary)
        shiny::req(data())
        # browser()

        if (!input$primary %in% names(data())) {
          plot_data <- data()[1]
        } else {
          plot_data <- data()[input$primary]
        }

        plots <- possible_plots(
          data = plot_data
        )

        plots_named <- get_plot_options(plots) |>
          lapply(\(.x){
            stats::setNames(.x$descr, .x$note)
          })

        vectorSelectInput(
          inputId = ns("type"),
          selected = NULL,
          label = shiny::h4("Plot type"),
          choices = Reduce(c, plots_named),
          multiple = FALSE
        )
      })

      rv$plot.params <- shiny::reactive({
        get_plot_options(input$type) |> purrr::pluck(1)
      })

      output$secondary <- shiny::renderUI({
        shiny::req(input$type)

        cols <- c(
          rv$plot.params()[["secondary.extra"]],
          all_but(
            colnames(subset_types(
              data(),
              rv$plot.params()[["secondary.type"]]
            )),
            input$primary
          )
        )

        columnSelectInput(
          inputId = ns("secondary"),
          data = data,
          selected = cols[1],
          placeholder = "Please select",
          label = if (isTRUE(rv$plot.params()[["secondary.multi"]])) "Additional variables" else "Secondary variable",
          multiple = rv$plot.params()[["secondary.multi"]],
          maxItems = rv$plot.params()[["secondary.max"]],
          col_subset = cols,
          none_label = "No variable"
        )
      })

      output$tertiary <- shiny::renderUI({
        shiny::req(input$type)
        columnSelectInput(
          inputId = ns("tertiary"),
          data = data,
          placeholder = "Please select",
          label = "Grouping variable",
          multiple = FALSE,
          col_subset = c(
            "none",
            all_but(
              colnames(subset_types(
                data(),
                rv$plot.params()[["tertiary.type"]]
              )),
              input$primary,
              input$secondary
            )
          ),
          none_label = "No stratification"
        )
      })

      shiny::observeEvent(input$act_plot,
        {
          if (NROW(data()) > 0) {
            tryCatch(
              {
                parameters <- list(
                  type = rv$plot.params()[["fun"]],
                  pri = input$primary,
                  sec = input$secondary,
                  ter = input$tertiary
                )

                shiny::withProgress(message = "Drawing the plot. Hold tight for a moment..", {
                  rv$plot <- rlang::exec(
                    create_plot,
                    !!!append_list(
                      data(),
                      parameters,
                      "data"
                    )
                  )
                })

                rv$code <- glue::glue("FreesearchR::create_plot(df,{list2str(parameters)})")
              },
              # warning = function(warn) {
              #   showNotification(paste0(warn), type = "warning")
              # },
              error = function(err) {
                showNotification(paste0(err), type = "err")
              }
            )
          }
        },
        ignoreInit = TRUE
      )

      output$code_plot <- shiny::renderUI({
        shiny::req(rv$code)
        prismCodeBlock(paste0("#Plotting\n", rv$code))
      })

      shiny::observeEvent(
        list(
          data()
        ),
        {
          shiny::req(data())

          rv$plot <- NULL
        }
      )

      output$plot <- shiny::renderPlot({
        # shiny::req(rv$plot)
        # rv$plot
        if (!is.null(rv$plot)) {
          rv$plot
        } else {
          return(NULL)
        }
      })

      # shiny::observeEvent(input$height_numeric, {
      #   shinyWidgets::updateNoUiSliderInput(session, ns("height_slide"), value = input$height_numeric)
      # }, ignoreInit = TRUE)
      # shiny::observeEvent(input$height_slide, {
      #   shiny::updateNumericInput(session, ns("height_numeric"), value = input$height_slide)
      # }, ignoreInit = TRUE)


      output$download_plot <- shiny::downloadHandler(
        filename = shiny::reactive({
          paste0("plot.", input$plot_type)
        }),
        content = function(file) {
          if (inherits(rv$plot, "patchwork")) {
            plot <- rv$plot
          } else if (inherits(rv$plot, "ggplot")) {
            plot <- rv$plot
          } else {
            plot <- rv$plot[[1]]
          }
          # browser()
          shiny::withProgress(message = "Drawing the plot. Hold on for a moment..", {
            ggplot2::ggsave(
              filename = file,
              plot = plot,
              width = input$width,
              height = input$height_slide,
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
#' default_parsing(mtcars) |> subset_types(c("dichotomous", "categorical"))
#' #' default_parsing(mtcars) |> subset_types("factor",class)
subset_types <- function(data, types, type.fun = data_type) {
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
      fun = "plot_hbars",
      descr = "Stacked horizontal bars",
      note = "A classical way of visualising the distribution of an ordinal scale like the modified Ranking Scale and known as Grotta bars",
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = "none"
    ),
    plot_violin = list(
      fun = "plot_violin",
      descr = "Violin plot",
      note = "A modern alternative to the classic boxplot to visualise data distribution",
      primary.type = c("datatime", "continuous", "dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      secondary.extra = "none",
      tertiary.type = c("dichotomous", "categorical")
    ),
    # plot_ridge = list(
    #   descr = "Ridge plot",
    #   note = "An alternative option to visualise data distribution",
    #   primary.type = "continuous",
    #   secondary.type = c("dichotomous" ,"categorical"),
    #   tertiary.type = c("dichotomous" ,"categorical"),
    #   secondary.extra = NULL
    # ),
    plot_sankey = list(
      fun = "plot_sankey",
      descr = "Sankey plot",
      note = "A way of visualising change between groups",
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      secondary.extra = NULL,
      tertiary.type = c("dichotomous", "categorical")
    ),
    plot_scatter = list(
      fun = "plot_scatter",
      descr = "Scatter plot",
      note = "A classic way of showing the association between to variables",
      primary.type = c("datatime", "continuous"),
      secondary.type = c("datatime", "continuous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = NULL
    ),
    plot_box = list(
      fun = "plot_box",
      descr = "Box plot",
      note = "A classic way to plot data distribution by groups",
      primary.type = c("datatime", "continuous", "dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = "none"
    ),
    plot_euler = list(
      fun = "plot_euler",
      descr = "Euler diagram",
      note = "Generate area-proportional Euler diagrams to display set relationships",
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = TRUE,
      secondary.max = 4,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = NULL
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
#'   possible_plots()
#'
#' mtcars |>
#'   default_parsing() |>
#'   dplyr::select("mpg") |>
#'   possible_plots()
possible_plots <- function(data) {
  # browser()
  # data <- if (is.reactive(data)) data() else data
  if (is.data.frame(data)) {
    data <- data[[1]]
  }

  type <- data_type(data)

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
#' @param pri primary variable
#' @param sec secondary variable
#' @param ter tertiary variable
#' @param type plot type (derived from possible_plots() and matches custom function)
#' @param ... ignored for now
#'
#' @name data-plots
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' create_plot(mtcars, "plot_violin", "mpg", "cyl") |> attributes()
create_plot <- function(data, type, pri, sec, ter = NULL, ...) {
  if (!is.null(sec)) {
    if (!any(sec %in% names(data))) {
      sec <- NULL
    }
  }

  if (!is.null(ter)) {
    if (!ter %in% names(data)) {
      ter <- NULL
    }
  }

  parameters <- list(
    pri = pri,
    sec = sec,
    ter = ter,
    ...
  )

  out <- do.call(
    type,
    modifyList(parameters, list(data = data))
  )

  code <- rlang::call2(type, !!!parameters, .ns = "FreesearchR")

  attr(out, "code") <- code
  out
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
#' gtsummary::trial$trt |> get_label()
#' 1:10 |> get_label()
get_label <- function(data, var = NULL) {
  # data <- if (is.reactive(data)) data() else data
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
#' paste(sample(letters[1:10], 100, TRUE), collapse = "") |> line_break(force = TRUE)
line_break <- function(data, lineLength = 20, force = FALSE) {
  if (isTRUE(force)) {
    ## This eats some letters when splitting a sentence... ??
    gsub(paste0("(.{1,", lineLength, "})(\\s|[[:alnum:]])"), "\\1\n", data)
  } else {
    paste(strwrap(data, lineLength), collapse = "\n")
  }
  ## https://stackoverflow.com/a/29847221
}


#' Wrapping
#'
#' @param data list of ggplot2 objects
#' @param tag_levels passed to patchwork::plot_annotation if given. Default is NULL
#' @param title panel title
#' @param ... ignored for argument overflow
#'
#' @returns list of ggplot2 objects
#' @export
#'
wrap_plot_list <- function(data,
                           tag_levels = NULL,
                           title = NULL,
                           axis.font.family = NULL,
                           ...) {
  if (ggplot2::is_ggplot(data[[1]])) {
    if (length(data) > 1) {
      out <- data |>
        (\(.x){
          if (rlang::is_named(.x)) {
            purrr::imap(.x, \(.y, .i){
              .y + ggplot2::ggtitle(.i)
            })
          } else {
            .x
          }
        })() |>
        align_axes() |>
        patchwork::wrap_plots(
          guides = "collect",
          axes = "collect",
          axis_titles = "collect"
        )
      if (!is.null(tag_levels)) {
        out <- out + patchwork::plot_annotation(tag_levels = tag_levels)
      }
      if (!is.null(title)) {
        out <- out +
          patchwork::plot_annotation(
            title = title,
            theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 25))
          )
      }
    } else {
      out <- data[[1]]
    }
  } else {
    cli::cli_abort("Can only wrap lists of {.cls ggplot} objects")
  }

  if (inherits(x = out, what = "patchwork")) {
    out &
      ggplot2::theme(axis.text = ggplot2::element_text(family = axis.font.family))
  } else {
    out +
      ggplot2::theme(axis.text = ggplot2::element_text(family = axis.font.family))
  }
}


#' Aligns axes between plots
#'
#' @param ... ggplot2 objects or list of ggplot2 objects
#'
#' @returns list of ggplot2 objects
#' @export
#'
align_axes <- function(...) {
  # https://stackoverflow.com/questions/62818776/get-axis-limits-from-ggplot-object
  # https://github.com/thomasp85/patchwork/blob/main/R/plot_multipage.R#L150
  if (ggplot2::is_ggplot(..1)) {
    ## Assumes list of ggplots
    p <- list(...)
  } else if (is.list(..1)) {
    ## Assumes list with list of ggplots
    p <- ..1
  } else {
    cli::cli_abort("Can only align {.cls ggplot} objects or a list of them")
  }

  yr <- clean_common_axis(p, "y")

  xr <- clean_common_axis(p, "x")

  suppressWarnings({
    p |> purrr::map(~ .x + ggplot2::xlim(xr) + ggplot2::ylim(yr))
  })
}

#' Extract and clean axis ranges
#'
#' @param p plot
#' @param axis axis. x or y.
#'
#' @returns vector
#' @export
#'
clean_common_axis <- function(p, axis) {
  purrr::map(p, ~ ggplot2::layer_scales(.x)[[axis]]$get_limits()) |>
    unlist() |>
    (\(.x){
      if (is.numeric(.x)) {
        range(.x)
      } else {
        as.character(.x)
      }
    })() |>
    unique()
}
