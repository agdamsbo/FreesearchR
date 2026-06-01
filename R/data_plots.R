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
        shiny::actionButton(
          inputId = ns("act_plot"),
          label = i18n$t("Plot"),
          width = "100%",
          icon = phosphoricons::ph("paint-brush", weight = "bold"),
          # icon = shiny::icon("palette"),
          disabled = FALSE
        ),
        shiny::helpText(
          i18n$t('Adjust plot input and settings below, then press "Plot".')
        ),
        bslib::accordion(
          id = "acc_plot",
          multiple = FALSE,
          bslib::accordion_panel(
            value = "acc_pan_plot",
            title = i18n$t("Define plot"),
            icon = phosphoricons::ph("chart-line"),
            # icon = bsicons::bs_icon("graph-up"),
            shiny::uiOutput(outputId = ns("primary")),
            shiny::helpText(
              i18n$t(
                'Only non-text variables are available for plotting. Go the "Data" to reclass data to plot.'
              )
            ),
            shiny::tags$br(),
            shiny::uiOutput(outputId = ns("type")),
            shiny::h5(i18n$t("Other variables")),
            shiny::uiOutput(outputId = ns("secondary")),
            shiny::uiOutput(outputId = ns("tertiary"))
          ),
          bslib::accordion_panel(
            value = "acc_pan_params",
            title = i18n$t("Settings"),
            icon = phosphoricons::ph("gear"),
            shiny::uiOutput(outputId = ns("color_palette")),
            shiny::uiOutput(outputId = ns("basic_parameters")),
          ),
          bslib::accordion_panel(
            value = "acc_pan_download",
            title = "Download",
            icon = phosphoricons::ph("download-simple"),
            # icon = bsicons::bs_icon("download"),
            shinyWidgets::noUiSliderInput(
              inputId = ns("height_slide"),
              label = i18n$t("Plot height (mm)"),
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
              label = i18n$t("Plot width (mm)"),
              min = 50,
              max = 300,
              value = 100,
              step = 1,
              format = shinyWidgets::wNumbFormat(decimals = 0),
              color = datamods:::get_primary_color()
            ),
            shiny::selectInput(
              inputId = ns("plot_type"),
              label = i18n$t("File format"),
              choices = list("png", "tiff", "eps", "pdf", "jpeg", "svg")
            ),
            shiny::br(),
            # Button
            shiny::downloadButton(
              outputId = ns("download_plot"),
              label = i18n$t("Download plot"),
              icon = phosphoricons::ph("arrow-fat-down")
              # icon = shiny::icon("download")
            )
          )
        ),
        shiny::p(
          "We have collected a few notes on visualising data and details on the options included in FreesearchR:",
          shiny::tags$a(
            href = "https://freesearchr.github.io/FreesearchR-knowledge/app/visuals.html",
            "View notes in new tab",
            target = "_blank",
            rel = "noopener noreferrer"
          )
        )
      ),
      shiny::plotOutput(ns("plot"), height = "65vh"),
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
data_visuals_server <- function(id, data, palettes = color_choices(), ...) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      rv <- shiny::reactiveValues(plot.params = NULL,
                                  plot = NULL,
                                  code = NULL)

      shiny::observe({
        bslib::accordion_panel_update(
          id = "acc_plot",
          target = "acc_pan_plot",
          title = i18n$t("Create plot")
        )
        bslib::accordion_panel_update(id = "acc_plot",
                                      target = "acc_pan_download",
                                      title = i18n$t("Download"))
      })

      output$primary <- shiny::renderUI({
        shiny::req(data())
        columnSelectInput(
          inputId = ns("primary"),
          col_subset = names(data())[sapply(data(), data_type) != "text"],
          data = data,
          placeholder = i18n$t("Select variable"),
          label = i18n$t("Response variable"),
          multiple = FALSE
        )
      })

      # shiny::observeEvent(data, {
      #   if (is.null(data()) | NROW(data()) == 0) {
      #     shiny::updateActionButton(inputId = "act_plot", disabled = TRUE)
      #   } else {
      #     shiny::updateActionButton(inputId = "act_plot", disabled = FALSE)
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

        plots <- possible_plots(data = plot_data, source_list = available_plots())

        plots_named <- get_input_params(plots) |>
          lapply(\(.x) {
            stats::setNames(.x$descr, .x$note)
          })

        # plots_named <- get_plot_options(plots) |>
        #   lapply(\(.x) {
        #     stats::setNames(.x$descr, .x$note)
        #   })

        vectorSelectInput(
          inputId = ns("type"),
          selected = NULL,
          label = shiny::h5(i18n$t("Plot type")),
          choices = Reduce(c, plots_named),
          multiple = FALSE
        )
      })

      rv$plot.params <- shiny::reactive({
        get_input_params(input$type) |> purrr::pluck(1)
        # get_plot_options(input$type) |> purrr::pluck(1)
      })


      ### Include two additional variable inputs
      output$secondary <- shiny::renderUI({
        shiny::req(input$type)

        # Get the plot function name
        base_params <- rv$plot.params()[["base"]]

        filtered_params <- base_params[sapply(base_params, function(params) {
          params$id %in% "secondary"
        })][[1]]

        filtered_params$exclude <- input$primary

        create_input_element(
          input_id = "secondary",
          ns = ns,
          params = append_list(data(), filtered_params, "data")
        )

      })

      output$tertiary <- shiny::renderUI({
        shiny::req(input$type)
        # Get the plot function name
        base_params <- rv$plot.params()[["base"]]

        filtered_params <- base_params[sapply(base_params, function(params) {
          params$id %in% "tertiary"
        })][[1]]

        filtered_params$exclude <- c(input$primary, input$secondary)

        create_input_element(
          input_id = "tertiary",
          ns = ns,
          params = append_list(data(), filtered_params, "data")
        )
      })


      ### Generating additional parameter inputs if any specified
      output$basic_parameters <- renderUI({
        req(input$type, rv$plot.params)

        # Get the plot function name
        base_params <- rv$plot.params()[["base"]]

        filtered_params <- base_params[sapply(base_params, function(params) {
          !params$id %in% c("secondary", "tertiary")
        })]


        # Create UI elements for base parameters
        base_inputs <- lapply(filtered_params, function(params) {
          input_id <- paste0("base_", params$id)
          params$id <- NULL
          if (params$type %in% "select_variables") {
            params$data <- data()
          }

          create_input_element(params, ns, input_id)
        })
        tagList(base_inputs)

      })

      ### Color option
      output$color_palette <- shiny::renderUI({
        # shiny::req(input$type)
        colorSelectInput(
          inputId = ns("color_palette"),
          label = i18n$t("Choose color palette"),
          choices = palettes,
          previews = 5
        )
      })

      shiny::observeEvent(input$act_plot, {
        if (NROW(data()) > 0) {
          tryCatch({
            # Get all input values with prefixes
            base_inputs <- reactiveValuesToList(input)[grep("^base_", names(reactiveValuesToList(input)))]
            # advanced_inputs <- reactiveValuesToList(input)[grep("^advanced_", names(reactiveValuesToList(input)))]

            # Remove the prefix from names
            names(base_inputs) <- gsub("^base_", "", names(base_inputs))
            # names(advanced_inputs) <- gsub("^advanced_", "", names(advanced_inputs))

            base_inputs <- c(base_inputs,
                             list(color.palette = input$color_palette))

            # If any of the specified parameters are NULL/missing, the settings
            # accordion/panel was never opened, and they can be ignored, as
            # default settings will the be used.
            if (any(sapply(base_inputs, is.null))) {
              dynamic_params <- list()
            } else {
              dynamic_params <- base_inputs
            }

            # Build parameters for plotting function
            parameters <- list(
              type = rv$plot.params()[["fun"]],
              pri = input$primary,
              sec = input$secondary,
              ter = input$tertiary
            )

            parameters <- modifyList(parameters, dynamic_params)

            ## If the dictionary holds additional arguments to pass to the
            ## plotting function, these are included
            if (!is.null(rv$plot.params()[["fun.args"]])) {
              default_params <- rv$plot.params()[["fun.args"]]

              ## Ensure not to overwrite user defined parameters are overwritten
              ## This allows to define default parameters.
              ##
              ## This will create a strange edge case, where the plot looks in
              ## one way, when plotted initially, but may change, when the settings
              ## accordion is opened. Problem for future me. Really mostly an edge case.
              parameters <- modifyList(parameters, default_params[!names(default_params) %in% names(parameters)])
            }

            shiny::withProgress(message = i18n$t("Drawing the plot. Hold tight for a moment.."),
                                {
                                  rv$plot <- rlang::exec(create_plot,
                                                         !!!append_list(data(), parameters, "data"))
                                })

            rv$code <- glue::glue("FreesearchR::create_plot(df,{list2str(parameters)})")
          }, # warning = function(warn) {
          #   showNotification(paste0(warn), type = "warning")
          # },
          error = function(err) {
            showNotification(paste0(err), type = "error")
          })
        }
      }, ignoreInit = TRUE)

      output$code_plot <- shiny::renderUI({
        shiny::req(rv$code)
        prismCodeBlock(paste0(i18n$t("#Plotting\n"), rv$code))
      })

      shiny::observeEvent(list(data()), {
        shiny::req(data())

        rv$plot <- NULL
      })

      output$plot <- shiny::renderPlot({
        # shiny::req(rv$plot)
        # rv$plot
        if (!is.null(rv$plot)) {
          rv$plot
        } else {
          # Create a placeholder plot with instructions using ggplot2
          ggplot2::ggplot() +
            ggplot2::annotate(
              "text",
              x = 0.5,
              y = 0.5,
              label = i18n$t("Select variables and plot type,\nthen click 'Plot' to generate visualization"),
              size = 5,
              color = "gray50",
              lineheight = 0.8
            ) +
            ggplot2::xlim(0, 1) +
            ggplot2::ylim(0, 1) +
            ggplot2::theme_void() +
            ggplot2::theme(
              panel.background = ggplot2::element_rect(fill = "white"),
              plot.background = ggplot2::element_rect(fill = "white")
            )
          # return(NULL)
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
          shiny::withProgress(message = i18n$t("Drawing the plot. Hold tight for a moment.."), {
            ggplot2::ggsave(
              filename = file,
              plot = plot,
              width = input$width,
              height = input$height_slide,
              dpi = 300,
              units = "mm",
              scale = 2
            )
          })
        }
      )


      shiny::observe(return(rv$plot))
    }
  )
}
