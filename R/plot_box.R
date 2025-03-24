#' Beautiful box plot(s)
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' mtcars |> plot_box(x = "mpg", y = "cyl", z = "gear")
#' mtcars |>
#'   default_parsing() |>
#'   plot_box(x = "mpg", y = "cyl", z = "gear")
plot_box <- function(data, x, y, z = NULL) {
  if (!is.null(z)) {
    ds <- split(data, data[z])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.ds){
    plot_box_single(
      data = .ds,
      x = x,
      y = y
    )
  })

  wrap_plot_list(out)
  # patchwork::wrap_plots(out,guides = "collect")
}




#' Create nice box-plots
#'
#' @name data-plots
#'
#' @returns ggplot object
#' @export
#'
#' @examples
#' mtcars |> plot_box_single("mpg","cyl")
plot_box_single <- function(data, x, y=NULL, seed = 2103) {
  set.seed(seed)

  if (is.null(y)) {
    y <- "All"
    data[[y]] <- y
  }

  discrete <- !data_type(data[[y]]) %in% "continuous"

  data |>
    ggplot2::ggplot(ggplot2::aes(x = !!dplyr::sym(x), y = !!dplyr::sym(y), fill = !!dplyr::sym(y), group = !!dplyr::sym(y))) +
    ggplot2::geom_boxplot(linewidth = 1.8, outliers = FALSE) +
    ## THis could be optional in future
    ggplot2::geom_jitter(color = "black", size = 2, alpha = 0.9, width = 0.1, height = .5) +
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
