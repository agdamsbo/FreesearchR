#' Area proportional venn diagrams
#'
#' @description
#' THis is slightly modified from https://gist.github.com/danlooo/d23d8bcf8856c7dd8e86266097404ded
#'
#' This functions uses eulerr::euler to plot area proportional venn diagramms
#' but plots it using ggplot2
#'
#' @param combinations set relationships as a named numeric vector, matrix, or
#' data.frame(See `eulerr::euler`)
#' @param show_quantities whether to show number of intersecting elements
#' @param show_labels whether to show set names
#' @param ... further arguments passed to eulerr::euler
ggeulerr <- function(
    combinations,
    show_quantities = TRUE,
    show_labels = TRUE,
    ...) {
  # browser()
  data <-
    eulerr::euler(combinations = combinations, ...) |>
    plot(quantities = show_quantities) |>
    purrr::pluck("data")


  tibble::as_tibble(data$ellipses, rownames = "Variables") |>
    ggplot2::ggplot() +
    ggforce::geom_ellipse(
      mapping = ggplot2::aes(
        x0 = h, y0 = k, a = a, b = b, angle = 0, fill = Variables
      ),
      alpha = 0.5,
      linewidth = 1.5
    ) +
    ggplot2::geom_text(
      data = {
        data$centers |>
          dplyr::mutate(
            label = labels |> purrr::map2(quantities, ~ {
              if (!is.na(.x) && !is.na(.y) && show_labels) {
                paste0(.x, "\n", sprintf(.y, fmt = "%.2g"))
              } else if (!is.na(.x) && show_labels) {
                .x
              } else if (!is.na(.y)) {
                .y
              } else {
                ""
              }
            })
          )
      },
      mapping = ggplot2::aes(x = x, y = y, label = label),
      size = 8
    ) +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::coord_fixed() +
    ggplot2::scale_fill_hue()
}

#' Easily plot euler diagrams
#'
#' @param data data
#' @param x name of main variable
#' @param y name of secondary variables
#' @param z grouping variable
#' @param seed seed
#'
#' @returns patchwork object
#' @export
#'
#' @examples
#' data.frame(
#'   A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
#'   B = sample(c("A", "C"), 50, TRUE),
#'   C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE),
#'   D = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
#' ) |> plot_euler("A", c("B", "C"), "D", seed = 4)
#' mtcars |> plot_euler("vs", "am", seed = 1)
plot_euler <- function(data, x, y, z = NULL, seed = 2103) {
  set.seed(seed = seed)

  # data <- data[c(...,z)]

  if (!is.null(z)) {
    ds <- split(data, data[z])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.x){
    .x[c(x, y)] |>
      as.data.frame() |>
      plot_euler_single()
  })

  patchwork::wrap_plots(out, guides = "collect")
}


#' Easily plot single euler diagrams
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' data.frame(
#'   A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
#'   B = sample(c("A", "C"), 50, TRUE),
#'   C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE),
#'   D = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
#' ) |> plot_euler_single()
#' mtcars[c("vs", "am")] |> plot_euler_single()
plot_euler_single <- function(data) {
  data |>
    ggeulerr(shape = "circle") +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "right",
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      # axis.text.y = element_blank(),
      # axis.title.y = element_blank(),
      text = ggplot2::element_text(size = 20),
      axis.text = ggplot2::element_blank(),
      # plot.title = element_blank(),
      # panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_blank()
    )
}
