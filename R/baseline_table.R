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
#' @param strat.var grouping/strat variable
#' @param add.p add comparison/p-value
#' @param add.overall add overall column
#'
#' @returns gtsummary table list object
#' @export
#'
#' @examples
#' mtcars |> create_baseline(by.var = "gear", add.p = "yes" == "yes")
#' create_baseline(default_parsing(mtcars), by.var = "am", add.p = FALSE, add.overall = FALSE, theme = "lancet")
create_baseline <- function(data, ..., by.var, add.p = FALSE, add.overall = FALSE, theme = c("jama", "lancet", "nejm", "qjecon")) {
  theme <- match.arg(theme)

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

  gtsummary::theme_gtsummary_journal(journal = theme)

  args <- list(...)

  parameters <- list(
    data = data,
    fun.args = list(by = by.var, ...)
  )

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
  }

  out
}
