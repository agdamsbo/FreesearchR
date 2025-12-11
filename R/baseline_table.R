#' Print a flexible baseline characteristics table
#'
#' @param data data set
#' @param fun.args list of arguments passed to
#' @param fun function to
#' @param vars character vector of variables to include
#'
#' @return object of standard class for fun
#' @export
#'
#' @examples
#' mtcars |> baseline_table()
#' mtcars |> baseline_table(fun.args = list(by = "gear"))
baseline_table <- function(data, fun.args = NULL, fun = gtsummary::tbl_summary, vars = NULL) {
  out <- do.call(fun, c(list(data = data), fun.args))
  return(out)
}



#' Create a baseline table
#'
#' @param data data
#' @param ... passed as fun.arg to baseline_table()
#' @param add.p add comparison/p-value
#' @param add.overall add overall column
#' @param by.var specify stratification variable
#' @param theme set table theme
#' @param detail_level specify detail level. Either "minimal" or "extended".
#'
#' @returns gtsummary table list object
#' @export
#'
#' @examples
#' mtcars |> create_baseline(by.var = "gear", add.p = "yes" == "yes")
#' mtcars |> create_baseline(by.var = "gear", detail_level = "extended")
#' mtcars |> create_baseline(by.var = "gear", detail_level = "extended",type = list(gtsummary::all_dichotomous() ~ "categorical"),theme="nejm")
#'
#' create_baseline(default_parsing(mtcars), by.var = "am", add.p = FALSE, add.overall = FALSE, theme = "lancet")
create_baseline <- function(data, ..., by.var, add.p = FALSE, add.diff=FALSE, add.overall = FALSE, theme = c("jama", "lancet", "nejm", "qjecon"), detail_level = c("minimal", "extended")) {
  theme <- match.arg(theme)

  detail_level <- match.arg(detail_level)

  if (by.var == "none" | !by.var %in% names(data)) {
    by.var <- NULL
  }

  ## These steps are to handle logicals/booleans, that messes up the order of columns
  ## Has been reported and should be fixed soon (02042025)

  if (!is.null(by.var)) {
    if (identical("logical", class(data[[by.var]]))) {
      data[by.var] <- as.character(data[[by.var]])
    }
  }

  suppressMessages(gtsummary::theme_gtsummary_journal(journal = theme))

  args <- list(...)

  # browser()

  if (!any(hasName(args, c("type", "statistic")))) {
    if (detail_level == "extended") {
      args <-
        modifyList(
          args,
          list(
            type = list(gtsummary::all_continuous() ~ "continuous2",
                        gtsummary::all_dichotomous() ~ "categorical"),
            statistic = list(gtsummary::all_continuous() ~ c(
              "{median} ({p25}, {p75})",
              "{mean} ({sd})",
              "{min}, {max}"))
          )
        )
    }
  }

  parameters <- list(
    data = data,
    fun.args = purrr::list_flatten(list(by = by.var, args))
  )


  # browser()
  out <- do.call(
    baseline_table,
    parameters
  )


  if (!is.null(by.var)) {
    if (isTRUE(add.overall)) {
      out <- out |> gtsummary::add_overall()
    }
    if (isTRUE(add.p)) {
      out <- out |>
        gtsummary::add_p() |>
        gtsummary::bold_p()
    }
    if (isTRUE(add.diff)) {
      out <- out |>
        gtsummary::add_difference()
    }
  }

  out
}
