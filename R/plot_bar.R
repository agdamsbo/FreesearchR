plot_bar <- function(data, pri, sec, ter = NULL, style = c("stack", "dodge", "fill"), max_level = 30, ...) {
  style <- match.arg(style)

  if (!is.null(ter)) {
    ds <- split(data, data[ter])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.ds){
    plot_bar_single(
      data = .ds,
      pri = pri,
      sec = sec,
      style = style,
      max_level = max_level
    )
  })

  wrap_plot_list(out, title = glue::glue(i18n$t("Grouped by {get_label(data,ter)}")), ...)
}


#' Single vertical barplot
#'
#' @param style barplot style passed to geom_bar position argument.
#' One of c("stack", "dodge", "fill")
#'
#' @name data-plots
#'
#' @returns ggplot object
#' @export
#'
#' @examples
#' mtcars |>
#'   dplyr::mutate(cyl = factor(cyl), am = factor(am)) |>
#'   plot_bar_single(pri = "cyl", sec = "am", style = "fill")
#'
#' mtcars |>
#'   dplyr::mutate(cyl = factor(cyl), am = factor(am)) |>
#'   plot_bar_single(pri = "cyl", style = "stack")
plot_bar_single <- function(data, pri, sec = NULL, style = c("stack", "dodge", "fill"), max_level = 30) {
  style <- match.arg(style)

  if (identical(sec, "none")) {
    sec <- NULL
  }

  p_data <- as.data.frame(table(data[c(pri, sec)])) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(c(pri, sec)), forcats::as_factor),
      p = Freq / NROW(data)
    )


  if (nrow(p_data) > max_level) {
    # browser()
    p_data <- sort_by(
      p_data,
      p_data[["Freq"]],
      decreasing = TRUE
    ) |>
      head(max_level)
    # if (is.null(sec)){
    #   p_data <- sort_by(
    #     p_data,
    #     p_data[["Freq"]],
    #     decreasing=TRUE) |>
    #     head(max_level)
    # } else {
    #   split(p_data,p_data[[sec]]) |>
    #     lapply(\(.x){
    #       # browser()
    #     sort_by(
    #       .x,
    #       .x[["Freq"]],
    #       decreasing=TRUE) |>
    #       head(max_level)
    #   }) |> dplyr::bind_rows()
    # }
  }

  ## Shortens long level names
  p_data[[pri]] <- forcats::as_factor(unique_short(as.character(p_data[[pri]]), max = 20))

  if (!is.null(sec)) {
    fill <- sec
  } else {
    fill <- pri
  }

  p <- ggplot2::ggplot(
    p_data,
    ggplot2::aes(
      x = .data[[pri]],
      y = p,
      fill = .data[[fill]]
    )
  ) +
    ggplot2::geom_bar(position = style, stat = "identity") +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::ylab("Percentage") +
    ggplot2::xlab(get_label(data,pri))+
    ggplot2::guides(fill = ggplot2::guide_legend(title = get_label(data,fill)))

  ## To handle large number of levels and long level names

  if (nrow(p_data) > 10 | any(nchar(as.character(p_data[[pri]])) > 6)) {
    p <- p +
      # ggplot2::guides(fill = "none") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 90,
          vjust = 1, hjust = 1
        ))+
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(vjust = 0.5)
      )

    if (is.null(sec)){
      p <- p +
        ggplot2::guides(fill = "none")
    }
  }
  p
}
