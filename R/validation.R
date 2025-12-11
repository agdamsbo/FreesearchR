# Description of warning with text description incl metric
# Color coded (green (OK) or yellow (WARNING))
# option to ignore/accept warnings ### to simplify things, this is gone for now ###
# Only show warnings based on performed analyses

## 250825
## Works in demo
## Not alert is printed in app interface
## I believe it comes down to the reactivity


########################################################################
############# Server and UI
########################################################################

#' @title Validation module
#'
#' @description Check that a dataset respect some validation expectations.
#'
#' @param id Module's ID.
#' @param max_height Maximum height for validation results element, useful if you have many rules.
#' @param ... Arguments passed to \code{actionButton} or \code{uiOutput} depending on display mode,
#'  you cannot use \code{inputId}/\code{outputId}, \code{label} or \code{icon} (button only).
#'
#' @return
#'  * UI: HTML tags that can be included in shiny's UI
#'  * Server: a \code{list} with two slots:
#'    + **status**: a \code{reactive} function returning the best status available between \code{"OK"}, \code{"Failed"} or \code{"Error"}.
#'    + **details**: a \code{reactive} function returning a \code{list} with validation details.
#' @export
#'
#' @rdname validation
#'
#' @example examples/validation_module_demo.R
validation_ui <- function(id, max_height = NULL, ...) {
  ns <- shiny::NS(id)

  max_height <- if (!is.null(max_height)) {
    paste0("overflow-y: auto; max-height:", htmltools::validateCssUnit(max_height), ";")
  }

  ui <- shiny::uiOutput(
    outputId = ns("results"),
    ...,
    style = max_height
  )

  htmltools::tagList(
    ui, datamods:::html_dependency_datamods()
  )
}

#' @export
#'
#' @param data a \code{reactive} function returning a \code{data.frame}.
#'
#' @rdname validation
#'
validation_server <- function(id,
                              data) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      valid_ui <- reactiveValues(x = NULL)

      data_r <- if (shiny::is.reactive(data)) data else shiny::reactive(data)

      # observeEvent(data_r(), {
      #   to_validate <- data()
      #   valid_dims <- check_data(to_validate, n_row = n_row, n_col = n_col)
      #
      #   if (all(c(valid_dims$nrows, valid_dims$ncols))) {
      #     valid_status <- "OK"
      #   } else {
      #     valid_status <- "Failed"
      #   }
      #
      #   valid_results <- lapply(
      #     X = c("nrows", "ncols"),
      #     FUN = function(x) {
      #       if (is.null(valid_dims[[x]]))
      #         return(NULL)
      #       label <- switch(
      #         x,
      #         "nrows" = n_row_label,
      #         "ncols" = n_col_label
      #       )
      #       list(
      #         status = ifelse(valid_dims[[x]], "OK", "Failed"),
      #         label = paste0("<b>", label, "</b>")
      #       )
      #     }
      #   )

      shiny::observeEvent(
        data_r(),
        {
          # browser()
          to_validate <- data_r()
          if (is.reactivevalues(to_validate)) {
            to_validate <- reactiveValuesToList(to_validate)
          }
          if (!is.data.frame(to_validate)) {
            # browser()
            out <- lapply(
              to_validate,
              make_validation_alerts
            ) |>
              purrr::list_flatten()
          } else if (length(to_validate) > 0) {
            out <- make_validation_alerts(to_validate)
          } else {
            ## Defaulting to an emptu output vector
            out <- character()
          }
          valid_ui$x <- tagList(out)
        }
      )

      output$results <- renderUI({
        valid_ui$x
      })
    }
  )
}


########################################################################
############# Validation functions
########################################################################

#' Dimensions validation
#'
#' @param before data before
#' @param after data after
#' @param fun dimension function. ncol or nrow
#'
#' @returns data.frame
#'
dim_change_call <- function(before, after, fun) {
  # browser()
  if (!0 %in% c(dim(before), dim(after))) {
    n_before <- fun(before)
    n_after <- fun(after)
    n_out <- n_before - n_after
    p_after <- n_after / fun(before) * 100
    p_out <- 100 - p_after

    data.frame(
      n_before = n_before,
      n_after = n_after,
      n_out = n_out,
      p_after = p_after,
      p_out = p_out
    ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(
            is.numeric
          ),
          \(.y) round(.y, 0)
        )
      )
  } else {
    data.frame(NULL)
  }
}

#' Variable filter test wrapper
#'
#' @param before data before
#' @param after data after
#'
#' @returns vector
#'
#' @examples
#' vars_filter_validate(mtcars, mtcars[1:6])
#' vars_filter_validate(mtcars, mtcars[0])
vars_filter_validate <- function(before, after) {
  dim_change_call(before, after, ncol)
}

#' Observations filter test wrapper
#'
#' @param before data before
#' @param after data after
#'
#' @returns vector
#'
obs_filter_validate <- function(before, after) {
  dim_change_call(before, after, nrow)
}

#' Validate function of missingness in data
#'
#' @param data data set
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' df <- mtcars
#' df[1, 2:4] <- NA
#' missings_validate(df)
missings_validate <- function(data) {
  if (!0 %in% dim(data)) {
    # browser()
    p_miss <- sum(is.na(data)) / prod(dim(data)) * 100
    data.frame(
      p_miss = p_miss
    ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::where(
            is.numeric
          ),
          \(.y) signif(.y, 2)
        )
      )
  } else {
    data.frame(NULL)
  }
}

#' Correlation pairs validation
#'
#' @param data data.frame
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' # correlation_pairs(mtcars) |> corr_pairs_validate()
corr_pairs_validate <- function(data) {
  data_s <- if (shiny::is.reactive(data)) data() else data
  if (!0 %in% dim(data_s)) {
    # browser()
    n_pairs <- nrow(data_s)
    data.frame(
      n_pairs = n_pairs
    )
  } else {
    data.frame(NULL)
  }
}

#' MCAR validation based on a gtsummary table bady
#'
#' @param data data
#' @param outcome outcome variable
#'
#' @returns data.frame
#' @export
#'
mcar_validate <- function(data, outcome=NULL) {
  data_s <- if (shiny::is.reactive(data)) data() else data

  if (is.data.frame(data_s) && "p.value" %in% names(data_s) && !is.null(outcome)) {
    # browser()
    n_nonmcar <- sum(data_s["p.value"][!is.na(data_s["p.value"])] < 0.05)

    data.frame(
      n_nonmcar = n_nonmcar,
      outcome = outcome
    )
  } else {
    data.frame(NULL)
  }
}


########################################################################
############# Collected validation functions in a library-like function
########################################################################


#' Validation library
#'
#' @param name Index name
#'
#' @returns list
#'
#' @examples
#' validation_lib()
#' validation_lib("missings")
validation_lib <- function(name = NULL) {
  ls <- list(
    "obs_filter" = function(x, y) {
      ## Validation function for observations filter
      list(
        string = i18n$t("You removed {p_out} % of observations."),
        summary.fun = obs_filter_validate,
        summary.fun.args = list(
          before = x,
          after = y
        ),
        test.fun = function(x, var, cut) {
          test.var <- x[var]
          ifelse(test.var > cut, "warning", "succes")
        },
        test.fun.args = list(var = "p_out", cut = 50)
      )
    },
    "var_filter" = function(x, y) {
      ## Validation function for variables filter
      list(
        string = i18n$t("You removed {p_out} % of variables."),
        summary.fun = vars_filter_validate,
        summary.fun.args = list(
          before = x,
          after = y
        ),
        test.fun = function(x, var, cut) {
          test.var <- x[var]
          ifelse(test.var > cut, "warning", "succes")
        },
        test.fun.args = list(var = "p_out", cut = 50)
      )
    },
    "missings" = function(x) {
      ### Placeholder for missingness validation
      list(
        string = i18n$t("There is a total of {p_miss} % missing observations."),
        summary.fun = missings_validate,
        summary.fun.args = list(
          data = x
        ),
        test.fun = function(x, var, cut) {
          test.var <- x[var]
          ifelse(test.var > cut, "warning", "succes")
        },
        test.fun.args = list(var = "p_miss", cut = 30)
      )
    },
    "mcar" = function(x, y) {
      ### Placeholder for missingness validation
      list(
        string = i18n$t("There is a significant difference in data missingness in {n_nonmcar} {ifelse(n_nonmcar==1,'variable','variables')} grouped by the selected outcome/grouping variable {outcome}."),
        summary.fun = mcar_validate,
        summary.fun.args = list(
          data = x,
          outcome = y
        ),
        test.fun = function(x, var, cut) {
          test.var <- x[var]
          ifelse(test.var > cut, "warning", "succes")
        },
        test.fun.args = list(var = "n_nonmcar", cut = 0)
      )
    },
    "corr_pairs" = function(x) {
      ### Placeholder for missingness validation
      list(
        string = i18n$t("Data includes {n_pairs} pairs of highly correlated variables."),
        summary.fun = corr_pairs_validate,
        summary.fun.args = list(
          data = x
        ),
        test.fun = function(x, var, cut) {
          test.var <- x[var]
          ifelse(test.var > cut, "warning", "succes")
        },
        test.fun.args = list(var = "n_pairs", cut = 0)
      )
    }
  )

  if (!is.null(name)) {
    name <- match.arg(name, choices = names(ls))
    ls[[name]]
  } else {
    ls
  }
}


########################################################################
############# Validation creation
########################################################################

#' Create validation data.frame
#'
#' @param ls validation list
#' @param ... magic dots
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' i18n <- shiny.i18n::Translator$new(translation_csvs_path = here::here("inst/translations"))
#' i18n$set_translation_language("en")
#' df_original <- mtcars
#' df_original[1, 2:4] <- NA
#' df_obs <- df_original |> dplyr::filter(carb == 4)
#' df_vars <- df_original[1:7]
#' val <- purrr::map2(
#'   .x = validation_lib(),
#'   .y = list(
#'     list(x = df_original, y = df_obs),
#'     list(x = df_original, y = df_vars),
#'     list(x = df_original)
#'   ),
#'   make_validation
#' )
#' val |> make_validation_alerts()
#'
#' val2 <- purrr::map2(
#'   .x = validation_lib()[2],
#'   .y = list(list(x = mtcars, y = mtcars[0])),
#'   make_validation
#' )
#' val2 |> make_validation_alerts()
#'
#' val3 <- make_validation(
#'   ls = validation_lib()[[2]],
#'   list(x = mtcars, y = mtcars[0])
#' )
make_validation <- function(ls, ...) {
  ls <- do.call(ls, ...)

  df <- do.call(ls$summary.fun, ls$summary.fun.args)

  if (!any(dim(df) == c(0))) {
    label <- with(df, {
      glue::glue(ls$string)
    })

    # browser()
    status <- do.call(ls$test.fun, modifyList(ls$test.fun.args, list(x = df)))

    data.frame(
      label = label,
      status = status[1]
    )
  } else {
    data.frame(NULL)
  }
}


#' Create alert from validation data.frame
#'
#' @param data
#'
#' @export
make_validation_alerts <- function(data) {
  # browser()
  if (is.data.frame(data)) {
    ls <- list(data)
  } else {
    ls <- data
  }

  lapply(
    X = ls,
    FUN = function(x) {
      # browser()
      if (!is.null(dim(x)) && !any(dim(x) == c(0))) {
        icon <- switch(x$status,
          "succes" = phosphoricons::ph("check", title = "OK"),
          "warning" = phosphoricons::ph("warning", title = "Warning")
        )

        shinyWidgets::alert(
          icon,
          htmltools::HTML(x$label),
          status = x$status,
          style = "margin-bottom: 10px; padding: 10px;"
        )
      } else {
        return(NULL)
      }
    }
  )
}
