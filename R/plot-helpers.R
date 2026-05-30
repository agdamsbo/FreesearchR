#' Implemented functions
#'
#' @description
#' Library of supported functions. The list name and "descr" element should be
#' unique for each element on list.
#'
#' -   fun: the plotting function
#'
#' -   fun.args: default parameters for the plotting function
#'
#' -   descr: Plot description
#'
#' -   note: Short note/description of the function for displaying in ui and docs
#'
#' -   primary.type: Primary variable data type (see [data_type])
#'
#' -   base: holds a list of parameters for plot input fields generation
#'     Secondary and tertiary variable input fields are mandatory.
#'
#'
#' @returns list
#' @export
#'
#' @examples
#' available_plots() |> str()
available_plots <- function() {
  list(
    plot_bar_rel = list(
      fun = "plot_bar",
      fun.args = list(style = "fill"),
      descr = i18n$t("Stacked relative barplot"),
      note = i18n$t(
        "Create relative stacked barplots to show the distribution of categorical levels"
      ),
      primary.type = c("dichotomous", "categorical"),
      ### Input definitions ###
      base = list(
        list(
          id = "secondary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          allow_none = FALSE,
          # inputId = "sec",
          label = i18n$t("Additional variable"),
          multiple = FALSE
        ),
        list(
          id = "tertiary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          # inputId = "sec",
          label = i18n$t("Grouping variable"),
          multiple = FALSE
        )
      ),
      advanced = list()
      #########
    ),
    plot_bar_abs = list(
      fun = "plot_bar",
      fun.args = list(style = "dodge"),
      descr = i18n$t("Side-by-side barplot"),
      note = i18n$t(
        "Create side-by-side barplot to show the distribution of categorical levels"
      ),
      primary.type = c("dichotomous", "categorical"),
      ### Input definitions ###
      base = list(
        list(
          id = "secondary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          allow_none = FALSE,
          # inputId = "sec",
          label = i18n$t("Secondary variable"),
          multiple = FALSE
        ),
        list(
          id = "tertiary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          # inputId = "sec",
          label = i18n$t("Grouping variable"),
          multiple = FALSE
        )
      ),
      advanced = list()
      #########
    ),
    plot_hbars = list(
      fun = "plot_hbars",
      descr = i18n$t("Stacked horizontal bars"),
      note = i18n$t(
        "A classical way of visualising the distribution of an ordinal scale like the modified Ranking Scale and known as Grotta bars"
      ),
      primary.type = c("dichotomous", "categorical"),
      ### Input definitions ###
      base = list(
        list(
          id = "secondary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          allow_none = TRUE,
          # inputId = "sec",
          label = i18n$t("Secondary variable"),
          multiple = FALSE
        ),
        list(
          id = "tertiary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          # inputId = "sec",
          label = i18n$t("Grouping variable"),
          multiple = FALSE
        ),
        list(
          id = "reverse",
          type = "select_input",
          label = i18n$t("Reverse colors"),
          choices = c(yes = TRUE, no = FALSE)
        )
      ),
      advanced = list()
      #########
    ),
    plot_violin = list(
      fun = "plot_violin",
      descr = i18n$t("Violin plot"),
      note = i18n$t(
        "A modern alternative to the classic boxplot to visualise data distribution"
      ),
      primary.type = c("datatime", "continuous"),
      ### Input definitions ###
      base = list(
        list(
          id = "secondary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          allow_none = TRUE,
          # inputId = "sec",
          label = i18n$t("Secondary variable"),
          multiple = FALSE
        ),
        list(
          id = "tertiary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          # inputId = "sec",
          label = i18n$t("Grouping variable"),
          multiple = FALSE
        )
      ),
      advanced = list()
      #########
    ),
    plot_sankey = list(
      fun = "plot_sankey",
      descr = i18n$t("Sankey plot"),
      note = i18n$t("A way of visualising change between groups"),
      primary.type = c("dichotomous", "categorical"),
      ### Input definitions ###
      base = list(
        list(
          id = "secondary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          allow_none = FALSE,
          # inputId = "sec",
          label = i18n$t("Secondary variable"),
          multiple = FALSE
        ),
        list(
          id = "tertiary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          # inputId = "sec",
          label = i18n$t("Grouping variable"),
          multiple = FALSE
        )
      ),
      advanced = list()
      #########
    ),
    plot_scatter = list(
      fun = "plot_scatter",
      descr = i18n$t("Scatter plot"),
      note = i18n$t("A classic way of showing the association between to variables"),
      primary.type = c("datatime", "continuous"),
      ### Input definitions ###
      base = list(
        list(
          id = "secondary",
          type = "select_variables",
          var_types = c("datatime", "continuous", "categorical"),
          allow_none = FALSE,
          # inputId = "sec",
          label = i18n$t("Secondary variable"),
          multiple = FALSE
        ),
        list(
          id = "tertiary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          # inputId = "sec",
          label = i18n$t("Grouping variable"),
          multiple = FALSE
        )
      ),
      advanced = list()
      #########
    ),
    plot_box = list(
      fun = "plot_box",
      descr = i18n$t("Box plot"),
      note = i18n$t("A classic way to plot data distribution by groups"),
      primary.type = c("datatime", "continuous"),
      ### Input definitions ###
      base = list(
        list(
          id = "secondary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          allow_none = TRUE,
          # inputId = "sec",
          label = i18n$t("Secondary variable"),
          multiple = FALSE
        ),
        list(
          id = "tertiary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          # inputId = "sec",
          label = i18n$t("Grouping variable"),
          multiple = FALSE
        )
      ),
      advanced = list()
      #########
    ),
    plot_euler = list(
      fun = "plot_euler",
      descr = i18n$t("Euler diagram"),
      note = i18n$t(
        "Generate area-proportional Euler diagrams to display set relationships"
      ),
      primary.type = c("dichotomous"),
      ### Input definitions ###
      base = list(
        list(
          id = "secondary",
          type = "select_variables",
          var_types = c("dichotomous"),
          allow_none = FALSE,
          # inputId = "sec",
          label = i18n$t("Secondary variable"),
          multiple = TRUE,
          maxItems = 4
        ),
        list(
          id = "tertiary",
          type = "select_variables",
          var_types = c("dichotomous"),
          # inputId = "sec",
          label = i18n$t("Grouping variable"),
          multiple = FALSE
        )
      ),
      advanced = list()
      #########
    ),
    plot_likert = list(
      fun = "plot_likert",
      descr = i18n$t("Likert diagram"),
      note = i18n$t("Plot survey results"),
      primary.type = c("dichotomous", "categorical"),
      ### Input definitions ###
      base = list(
        list(
          id = "secondary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          allow_none = TRUE,
          # inputId = "sec",
          label = i18n$t("Additional variables"),
          multiple = TRUE
        ),
        list(
          id = "tertiary",
          type = "select_variables",
          var_types = c("dichotomous", "categorical"),
          # inputId = "sec",
          label = i18n$t("Grouping variable"),
          multiple = FALSE
        )
      ),
      advanced = list()
      #########
    )
  )
}

# Helper function to create input elements dynamically
create_input_element <- function(params, ns, input_id) {
  # Add the namespaced inputId to the arguments
  params$inputId <- ns(input_id)

  # Map input types to Shiny functions
  input_function <- switch(
    params$type,
    "numeric_input" = shiny::numericInput,
    "select_input" = shiny::selectInput,
    "checkbox_input" = shiny::checkboxInput,
    "slider_input" = shiny::sliderInput,
    "text_input" = shiny::textInput,
    "select_variables" = selectPlotVariables
  )

  params$type <- NULL
  params$id <- NULL


  # Call the function with all arguments
  do.call(input_function, params)
}

#' Wrapper for columnSelectInput
#'
selectPlotVariables <- function(data,
                                exclude = NULL,
                                allow_none = TRUE,
                                var_types,
                                ...) {
  datar <- if (is.reactive(data)) {
    data
  } else {
    reactive(data)
  }

  cols <- all_but(colnames(subset_types(datar(), var_types)), exclude)

  if (isTRUE(allow_none)) {
    cols <- c("none", cols)
  }

  params <- list(...)

  params$none_label <- i18n$t("No variable")
  params$col_subset <- cols

  rlang::exec(columnSelectInput, !!!append_list(datar(), params, "data"))
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
    plot_bar_rel = list(
      fun = "plot_bar",
      fun.args = list(style = "fill"),
      descr = i18n$t("Stacked relative barplot"),
      note = i18n$t(
        "Create relative stacked barplots to show the distribution of categorical levels"
      ),
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = NULL
    ),
    plot_bar_abs = list(
      fun = "plot_bar",
      fun.args = list(style = "dodge"),
      descr = i18n$t("Side-by-side barplot"),
      note = i18n$t(
        "Create side-by-side barplot to show the distribution of categorical levels"
      ),
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = "none"
    ),
    plot_hbars = list(
      fun = "plot_hbars",
      descr = i18n$t("Stacked horizontal bars"),
      note = i18n$t(
        "A classical way of visualising the distribution of an ordinal scale like the modified Ranking Scale and known as Grotta bars"
      ),
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = "none"
    ),
    plot_violin = list(
      fun = "plot_violin",
      descr = i18n$t("Violin plot"),
      note = i18n$t(
        "A modern alternative to the classic boxplot to visualise data distribution"
      ),
      primary.type = c("datatime", "continuous"),
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
      descr = i18n$t("Sankey plot"),
      note = i18n$t("A way of visualising change between groups"),
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      secondary.extra = NULL,
      tertiary.type = c("dichotomous", "categorical")
    ),
    plot_scatter = list(
      fun = "plot_scatter",
      descr = i18n$t("Scatter plot"),
      note = i18n$t("A classic way of showing the association between to variables"),
      primary.type = c("datatime", "continuous"),
      secondary.type = c("datatime", "continuous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = NULL
    ),
    plot_box = list(
      fun = "plot_box",
      descr = i18n$t("Box plot"),
      note = i18n$t("A classic way to plot data distribution by groups"),
      primary.type = c("datatime", "continuous"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = FALSE,
      tertiary.type = c("dichotomous", "categorical"),
      secondary.extra = "none"
    ),
    plot_euler = list(
      fun = "plot_euler",
      descr = i18n$t("Euler diagram"),
      note = i18n$t(
        "Generate area-proportional Euler diagrams to display set relationships"
      ),
      primary.type = c("dichotomous"),
      secondary.type = c("dichotomous"),
      secondary.multi = TRUE,
      secondary.max = 4,
      tertiary.type = c("dichotomous"),
      secondary.extra = NULL
    ),
    plot_likert = list(
      fun = "plot_likert",
      descr = i18n$t("Likert diagram"),
      note = i18n$t("Plot survey results"),
      primary.type = c("dichotomous", "categorical"),
      secondary.type = c("dichotomous", "categorical"),
      secondary.multi = TRUE,
      secondary.extra = NULL,
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
possible_plots <- function(data, source_list = supported_plots()) {
  # browser()
  # data <- if (is.reactive(data)) data() else data
  if (is.data.frame(data)) {
    data <- data[[1]]
  }

  type <- data_type(data)

  if (type == "unknown") {
    out <- type
  } else {
    out <- source_list |>
      lapply(\(.x) {
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
    lapply(\(.x) {
      .x$descr
    }) |>
    unlist()
  supported_plots() |>
    (\(.x) {
      .x[match(data, descrs)]
    })()
}

#' Get the function parameters based on the selected function description
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
#'   get_input_params()
get_input_params <- function(data) {
  descr <- available_plots() |>
    lapply(\(.x) {
      .x$descr
    }) |>
    unlist()
  available_plots() |>
    (\(.x) {
      .x[match(data, descr)]
    })()
}


#' Wrapper to create plot based on provided type
#'
#' @param data data.frame
#' @param pri primary variable
#' @param sec secondary variable
#' @param ter tertiary variable
#' @param type plot type (derived from possible_plots() and matches custom function)
#' @param color.palette choose color palette. See \code{\link{plot_colors}} for support.
#' @param ... ignored for now
#'
#' @name data-plots
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' create_plot(mtcars, "plot_violin", "mpg", "cyl") |> attributes()
create_plot <- function(data,
                        type,
                        pri,
                        sec,
                        ter = NULL,
                        color.palette = "viridis",
                        ...) {
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
    color.palette = color.palette,
    ...
  )

  out <- do.call(type, modifyList(parameters, list(data = data)))

  code <- rlang::call2(type, !!!parameters, .ns = "FreesearchR")

  attr(out, "code") <- code
  out
}

#' Print label, and if missing print variable name for plots
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
line_break <- function(data,
                       lineLength = 20,
                       force = FALSE) {
  if (isTRUE(force)) {
    ## This eats some letters when splitting a sentence... ??
    gsub(paste0("(.{1,", lineLength, "})(\\s|[[:alnum:]])"),
         "\\1\n",
         data)
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
#' @param guides passed to patchwork::wrap_plots()
#' @param axes passed to patchwork::wrap_plots()
#' @param axis_titles passed to patchwork::wrap_plots()
#' @param ... passed to patchwork::wrap_plots()
#'
#' @returns list of ggplot2 objects
#' @export
#'
wrap_plot_list <- function(data,
                           tag_levels = NULL,
                           title = NULL,
                           axis.font.family = NULL,
                           guides = "collect",
                           axes = "collect",
                           axis_titles = "collect",
                           y.axis.percentage = FALSE,
                           ...) {
  if (ggplot2::is_ggplot(data[[1]])) {
    if (length(data) > 1) {
      out <- data |>
        (\(.x) {
          if (rlang::is_named(.x)) {
            purrr::imap(.x, \(.y, .i) {
              .y + ggplot2::ggtitle(.i)
            })
          } else {
            .x
          }
        })() |>
        align_axes(percentage = y.axis.percentage) |>
        patchwork::wrap_plots(guides = guides,
                              axes = axes,
                              axis_titles = axis_titles,
                              ...)
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

  if (!is.null(axis.font.family)) {
    if (inherits(x = out, what = "patchwork")) {
      out <- out &
        ggplot2::theme(axis.text = ggplot2::element_text(family = axis.font.family))
    } else {
      out <- out +
        ggplot2::theme(axis.text = ggplot2::element_text(family = axis.font.family))
    }
  }

  out
}


#' Aligns axes between plots
#'
#' @param ... ggplot2 objects or list of ggplot2 objects
#'
#' @returns list of ggplot2 objects
#' @export
#'
align_axes <- function(...,
                       x.axis = TRUE,
                       y.axis = TRUE,
                       percentage = FALSE) {
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
    p_out <- purrr::map(p, \(.x) {
      out <- .x
      if (isTRUE(x.axis)) {
        out <- out + ggplot2::xlim(xr)
      }
      if (isTRUE(y.axis)) {
        out <- out + ggplot2::ylim(yr)
      }
      out
    })
  })

  if (isTRUE(percentage)) {
    lapply(p_out, \(.x) {
      .x +
        ggplot2::scale_y_continuous(labels = scales::percent)
    })
  } else {
    p_out
  }
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
    (\(.x) {
      if (is.numeric(.x)) {
        range(.x)
      } else {
        as.character(.x)
      }
    })() |>
    unique()
}
