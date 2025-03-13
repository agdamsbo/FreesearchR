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
      response = x, xtitle = get_label(data, var = y), ytitle = get_label(data, var = x)
    )
  } else {
    rempsyc::nice_scatter(
      data = data,
      predictor = y,
      response = x,
      group = z, xtitle = get_label(data, var = y), ytitle = get_label(data, var = x)
    )
  }
}
