#' Beautiful violin plot
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_scatter(pri = "mpg", sec = "wt")
plot_scatter <- function(data, pri, sec, ter = NULL) {
  if (is.null(ter)) {
    rempsyc::nice_scatter(
      data = data,
      predictor = sec,
      response = pri,
      xtitle = get_label(data, var = sec),
      ytitle = get_label(data, var = pri)
    )
  } else {
    rempsyc::nice_scatter(
      data = data,
      predictor = sec,
      response = pri,
      group = ter,
      xtitle = get_label(data, var = sec),
      ytitle = get_label(data, var = pri)
    )
  }
}
