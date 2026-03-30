#' Nice horizontal bar plot centred on the central category
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_likert(pri = "carb", sec = "cyl")
#' mtcars |> plot_likert(pri = "carb", sec = "cyl", ter="am")
#' mtcars |> plot_likert(pri = "cyl",color.palette="Blues")
#' mtcars |> plot_likert(pri = "carb", sec = NULL,color.palette="Magma")
#' mtcars |> plot_likert(pri = "carb", sec = c("cyl","am"),color.palette="Viridis")
plot_likert <- function(data,
                        pri,
                        sec = NULL,
                        ter = NULL,
                        color.palette = "viridis") {
  if (!is.null(ter)) {
    ds <- split(data, data[ter])
  } else {
    ds <- list(data)
  }
  out <- lapply(ds, \(.x) {
    .x[c(pri, sec)] |>
      # na.omit() |>
      plot_likert_single(color.palette = color.palette)
  })

  wrap_plot_list(out, title = glue::glue(i18n$t("Grouped by {get_label(data,ter)}")))
}


plot_likert_single <- function(data, color.palette = "viridis") {
  ggstats::gglikert(data = data) +
    scale_fill_generate(palette=color.palette)+
    ggplot2::theme(
      # legend.position = "none",
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      # axis.text.y = ggplot2::element_blank(),
      # axis.title.y = ggplot2::element_blank(),
      text = ggplot2::element_text(size = 12)
      # axis.text = ggplot2::element_blank(),
      # plot.title = element_blank(),
      # panel.background = ggplot2::element_rect(fill = "white"),
      # plot.background = ggplot2::element_rect(fill = "white"),
      # panel.border = ggplot2::element_blank()
    )
}
