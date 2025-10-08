#' Readying data for sankey plot
#'
#' @name data-plots
#'
#' @returns data.frame
#' @export
#'
#' @examples
#' ds <- data.frame(g = sample(LETTERS[1:2], 100, TRUE), first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)), last = sample(c(letters[1:4], NA), 100, TRUE, prob = c(rep(.23, 4), .08)))
#' ds |> sankey_ready("first", "last")
#' ds |> sankey_ready("first", "last", numbers = "percentage")
#' data.frame(
#'   g = sample(LETTERS[1:2], 100, TRUE),
#'   first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)),
#'   last = sample(c(TRUE, FALSE, FALSE), 100, TRUE)
#' ) |>
#'   sankey_ready("first", "last")
sankey_ready <- function(data, pri, sec, numbers = "count", ...) {
  ## TODO: Ensure ordering x and y

  ## Ensure all are factors
  data[c(pri, sec)] <- data[c(pri, sec)] |>
    dplyr::mutate(dplyr::across(!dplyr::where(is.factor), forcats::as_factor))

  out <- dplyr::count(data, !!dplyr::sym(pri), !!dplyr::sym(sec), .drop = FALSE)

  out <- out |>
    dplyr::group_by(!!dplyr::sym(pri)) |>
    dplyr::mutate(gx.sum = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::group_by(!!dplyr::sym(sec)) |>
    dplyr::mutate(gy.sum = sum(n)) |>
    dplyr::ungroup()

  if (numbers == "count") {
    out <- out |> dplyr::mutate(
      lx = factor(paste0(!!dplyr::sym(pri), "\n(n=", gx.sum, ")")),
      ly = factor(paste0(!!dplyr::sym(sec), "\n(n=", gy.sum, ")"))
    )
  } else if (numbers == "percentage") {
    out <- out |> dplyr::mutate(
      lx = factor(paste0(!!dplyr::sym(pri), "\n(", round((gx.sum / sum(n)) * 100, 1), "%)")),
      ly = factor(paste0(!!dplyr::sym(sec), "\n(", round((gy.sum / sum(n)) * 100, 1), "%)"))
    )
  }

  if (is.factor(data[[pri]])) {
    index <- match(levels(data[[pri]]), str_remove_last(levels(out$lx), "\n"))
    out$lx <- factor(out$lx, levels = levels(out$lx)[index])
  }

  if (is.factor(data[[sec]])) {
    index <- match(levels(data[[sec]]), str_remove_last(levels(out$ly), "\n"))
    out$ly <- factor(out$ly, levels = levels(out$ly)[index])
  }

  out
}

str_remove_last <- function(data, pattern = "\n") {
  strsplit(data, split = pattern) |>
    lapply(\(.x)paste(unlist(.x[[-length(.x)]]), collapse = pattern)) |>
    unlist()
}

#' Beautiful sankey plot with option to split by a tertiary group
#'
#' @returns ggplot2 object
#' @export
#'
#' @name data-plots
#'
#' @examples
#' ds <- data.frame(g = sample(LETTERS[1:2], 100, TRUE), first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)), last = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)))
#' ds |> plot_sankey("first", "last")
#' ds |> plot_sankey("first", "last", color.group = "sec")
#' ds |> plot_sankey("first", "last", ter = "g", color.group = "sec")
#' mtcars |>
#'   default_parsing() |>
#'   plot_sankey("cyl", "gear", "am", color.group = "pri")
#' ## In this case, the last plot as the secondary variable in wrong order
#' ## Dont know why...
#' mtcars |>
#'   default_parsing() |>
#'   plot_sankey("cyl", "gear", "vs", color.group = "pri")
plot_sankey <- function(data, pri, sec, ter = NULL, color.group = "pri", colors = NULL) {
  if (!is.null(ter)) {
    ds <- split(data, data[ter])
  } else {
    ds <- list(data)
  }

  out <- lapply(ds, \(.ds){
    plot_sankey_single(.ds, pri = pri, sec = sec, color.group = color.group, colors = colors)
  })

  patchwork::wrap_plots(out)
}

#' Beautiful sankey plot
#'
#' @param color.group set group to colour by. "x" or "y".
#' @param colors optinally specify colors. Give NA color, color for each level
#' in primary group and color for each level in secondary group.
#' @param ... passed to sankey_ready()
#'
#' @returns ggplot2 object
#' @export
#'
#' @examples
#' ds <- data.frame(g = sample(LETTERS[1:2], 100, TRUE), first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)), last = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)))
#' ds |> plot_sankey_single("first", "last")
#' ds |> plot_sankey_single("first", "last", color.group = "sec")
#' data.frame(
#'   g = sample(LETTERS[1:2], 100, TRUE),
#'   first = REDCapCAST::as_factor(sample(letters[1:4], 100, TRUE)),
#'   last = sample(c(TRUE, FALSE, FALSE), 100, TRUE)
#' ) |>
#'   plot_sankey_single("first", "last", color.group = "pri")
#' mtcars |>
#'   default_parsing() |>
#'   plot_sankey_single("cyl", "vs", color.group = "pri")
plot_sankey_single <- function(data, pri, sec, color.group = c("pri", "sec"), colors = NULL, ...) {
  color.group <- match.arg(color.group)

  data_orig <- data
  data[c(pri, sec)] <- data[c(pri, sec)] |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), forcats::fct_drop))

  # browser()

  data <- data |> sankey_ready(pri = pri, sec = sec, ...)

  na.color <- "#2986cc"
  box.color <- "#1E4B66"

  if (is.null(colors)) {
    if (color.group == "sec") {
      main.colors <- viridisLite::viridis(n = length(levels(data_orig[[sec]])))
      ## Only keep colors for included levels
      main.colors <- main.colors[match(levels(data[[sec]]), levels(data_orig[[sec]]))]

      secondary.colors <- rep(na.color, length(levels(data[[pri]])))
      label.colors <- Reduce(c, lapply(list(secondary.colors, rev(main.colors)), contrast_text))
    } else {
      main.colors <- viridisLite::viridis(n = length(levels(data_orig[[pri]])))
      ## Only keep colors for included levels
      main.colors <- main.colors[match(levels(data[[pri]]), levels(data_orig[[pri]]))]

      secondary.colors <- rep(na.color, length(levels(data[[sec]])))
      label.colors <- Reduce(c, lapply(list(rev(main.colors), secondary.colors), contrast_text))
    }
    colors <- c(na.color, main.colors, secondary.colors)
  } else {
    label.colors <- contrast_text(colors)
  }

  group_labels <- c(get_label(data, pri), get_label(data, sec)) |>
    sapply(line_break) |>
    unname()

  p <- ggplot2::ggplot(data, ggplot2::aes(y = n, axis1 = lx, axis2 = ly))

  if (color.group == "sec") {
    p <- p +
      ggalluvial::geom_alluvium(
        ggplot2::aes(
          fill = !!dplyr::sym(sec) # ,
          ## Including will print strings when levels are empty
          # color = !!dplyr::sym(sec)
        ),
        width = 1 / 16,
        alpha = .8,
        knot.pos = 0.4,
        curve_type = "sigmoid"
      ) + ggalluvial::geom_stratum(ggplot2::aes(fill = !!dplyr::sym(sec)),
        size = 2,
        width = 1 / 3.4
      )
  } else {
    p <- p +
      ggalluvial::geom_alluvium(
        ggplot2::aes(
          fill = !!dplyr::sym(pri) # ,
          # color = !!dplyr::sym(pri)
        ),
        width = 1 / 16,
        alpha = .8,
        knot.pos = 0.4,
        curve_type = "sigmoid"
      ) + ggalluvial::geom_stratum(ggplot2::aes(fill = !!dplyr::sym(pri)),
        size = 2,
        width = 1 / 3.4
      )
  }

  ## Will fail to use stat="stratum" if library is not loaded.
  library(ggalluvial)
  p +
    ggplot2::geom_text(
      stat = "stratum",
      ggplot2::aes(label = after_stat(stratum)),
      colour = label.colors,
      size = 8,
      lineheight = 1
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:2,
      labels = group_labels
    ) +
    ggplot2::scale_fill_manual(values = colors[-1], na.value = colors[1]) +
    # ggplot2::scale_color_manual(values = main.colors) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      # panel.grid.major = element_blank(),
      # panel.grid.minor = element_blank(),
      # axis.text.y = element_blank(),
      # axis.title.y = element_blank(),
      axis.text.x = ggplot2::element_text(size = 20),
      # text = element_text(size = 5),
      # plot.title = element_blank(),
      # panel.background = ggplot2::element_rect(fill = "white"),
      plot.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_blank()
    )
}
