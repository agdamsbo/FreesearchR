#' Beautiful violin plot
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_violin(pri = "mpg", sec = "cyl")
#' mtcars |> plot_violin(pri = "mpg", sec = "cyl", ter = "gear", color.palette="Blues")
plot_violin <- function(data, pri, sec, ter = NULL, color.palette="viridis") {
  if (!is.null(ter)) {
    ds <- split(data, data[ter])
  } else {
    ds <- list(data)
  }

  # browser()
  suppressWarnings({
    out <- lapply(ds, \(.ds){
      rempsyc::nice_violin(
        data = .ds,
        group = sec,
        response = pri,
        xtitle = get_label(data, var = sec),
        ytitle = get_label(data, var = pri)
      )+
        scale_fill_generate(palette=color.palette)
    })

    wrap_plot_list(out, title = glue::glue(i18n$t("Grouped by {get_label(data,ter)}")))
  })
  # patchwork::wrap_plots(out,guides = "collect")
}
