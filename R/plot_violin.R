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
      response = x, xtitle = get_label(data, var = y), ytitle = get_label(data, var = x)
    )
  })

  wrap_plot_list(out)
  # patchwork::wrap_plots(out,guides = "collect")
}
