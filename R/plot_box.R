#' Beautiful box plot(s)
#'
#' @param data data frame
#' @param pri primary variable
#' @param sec secondary variable
#' @param ter tertiary variable
#' @param ... passed on to wrap_plot_list
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_box(pri = "mpg", sec = "gear")
#' mtcars |> plot_box(pri = "mpg", sec="cyl")
#' mtcars |>
#'   default_parsing() |>
#'   plot_box(pri = "mpg", sec = "cyl", ter = "gear")
#' mtcars |>
#'   default_parsing() |>
#'   plot_box(pri = "mpg", sec = "cyl", ter = "gear",axis.font.family="mono")
plot_box <- function(data, pri, sec, ter = NULL,...) {
  if (!is.null(ter)) {
    ds <- split(data, data[ter])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.ds){
    plot_box_single(
      data = .ds,
      pri = pri,
      sec = sec
    )
  })

  wrap_plot_list(out,title=glue::glue("Grouped by {get_label(data,ter)}"),...)
}




#' Create nice box-plots
#'
#' @name data-plots
#'
#' @returns ggplot object
#' @export
#'
#' @examples
#' mtcars |> plot_box_single("mpg")
#' mtcars |> plot_box_single("mpg","cyl")
#' gtsummary::trial |> plot_box_single("age","trt")
plot_box_single <- function(data, pri, sec=NULL, seed = 2103) {
  set.seed(seed)

  if (is.null(sec)) {
    sec <- "All"
    data[[sec]] <- sec
  }

  discrete <- !data_type(data[[sec]]) %in% "continuous"

  data |>
    ggplot2::ggplot(ggplot2::aes(x = !!dplyr::sym(pri), y = !!dplyr::sym(sec), fill = !!dplyr::sym(sec), group = !!dplyr::sym(sec))) +
    ggplot2::geom_boxplot(linewidth = 1.8, outliers = FALSE) +
    ## THis could be optional in future
    ggplot2::geom_jitter(color = "black", size = 2, alpha = 0.9, width = 0.1, height = .2) +
    ggplot2::xlab(get_label(data,pri))+
    ggplot2::ylab(get_label(data,sec)) +
    ggplot2::coord_flip() +
    viridis::scale_fill_viridis(discrete = discrete, option = "D") +
    # ggplot2::theme_void() +
    ggplot2::theme_bw(base_size = 24) +
    ggplot2::theme(
      legend.position = "none",
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      # axis.text.y = element_blank(),
      # axis.title.y = element_blank(),
      # text = ggplot2::element_text(size = 20),
      # axis.text = ggplot2::element_blank(),
      # plot.title = element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(colour = "black")
    )
}
