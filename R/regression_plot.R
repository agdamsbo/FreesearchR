#' Regression coef plot from gtsummary. Slightly modified to pass on arguments
#'
#' @param x (`tbl_regression`, `tbl_uvregression`)\cr
#'   A 'tbl_regression' or 'tbl_uvregression' object
#' @param plot_ref (scalar `logical`)\cr
#'   plot reference values
#' @param remove_header_rows (scalar `logical`)\cr
#'   logical indicating whether to remove header rows
#'   for categorical variables. Default is `TRUE`
#' @param remove_reference_rows (scalar `logical`)\cr
#'   logical indicating whether to remove reference rows
#'   for categorical variables. Default is `FALSE`.
#' @param ... arguments passed to `ggstats::ggcoef_plot(...)`
#'
#' @returns ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' mod <- lm(mpg ~ ., default_parsing(mtcars))
#' p <- mod |>
#'   gtsummary::tbl_regression() |>
#'   plot(colour = "variable")
#' }
#'
plot.tbl_regression <- function(x,
                                plot_ref = TRUE,
                                remove_header_rows = TRUE,
                                remove_reference_rows = FALSE,
                                ...) {
  # check_dots_empty()
  gtsummary:::check_pkg_installed("ggstats")
  gtsummary:::check_not_missing(x)
  # gtsummary:::check_scalar_logical(remove_header_rows)
  # gtsummary:::check_scalar_logical(remove_reference_rows)

  df_coefs <- x$table_body

  if (isTRUE(remove_header_rows)) {
    df_coefs <- df_coefs |> dplyr::filter(!header_row %in% TRUE)
  }
  if (isTRUE(remove_reference_rows)) {
    df_coefs <- df_coefs |> dplyr::filter(!reference_row %in% TRUE)
  }

  # Removes redundant label
  df_coefs$label[df_coefs$row_type == "label"] <- ""
  # browser()
  # Add estimate value to reference level
  if (plot_ref == TRUE) {
    df_coefs[df_coefs$var_type %in% c("categorical", "dichotomous") & df_coefs$reference_row & !is.na(df_coefs$reference_row), "estimate"] <- if (x$inputs$exponentiate) 1 else 0
  }

  p <- df_coefs |>
    ggstats::ggcoef_plot(exponentiate = x$inputs$exponentiate, ...)

  if (x$inputs$exponentiate) {
    p <- symmetrical_scale_x_log10(p)
  }
  p
}


#' Wrapper to pivot gtsummary table data to long for plotting
#'
#' @param list a custom regression models list
#' @param model.names names of models to include
#'
#' @returns list
#' @export
#'
merge_long <- function(list, model.names) {
  l_subset <- list$tables[model.names]

  l_merged <- l_subset |> tbl_merge()

  df_body <- l_merged$table_body

  sel_list <- lapply(seq_along(l_subset), \(.i){
    endsWith(names(df_body), paste0("_", .i))
  }) |>
    setNames(names(l_subset))

  common <- !Reduce(`|`, sel_list)

  df_body_long <- sel_list |>
    purrr::imap(\(.l, .i){
      d <- dplyr::bind_cols(
        df_body[common],
        df_body[.l],
        model = .i
      )
      setNames(d, gsub("_[0-9]{,}$", "", names(d)))
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(model = REDCapCAST::as_factor(model))

  l_merged$table_body <- df_body_long

  l_merged$inputs$exponentiate <- !identical(class(list$models$Multivariable$model), "lm")

  l_merged
}


#' Easily round log scale limits for nice plots
#'
#' @param data data
#' @param fun rounding function (floor/ceiling)
#' @param ... ignored
#'
#' @returns numeric vector
#' @export
#'
#' @examples
#' limit_log(-.1, floor)
#' limit_log(.1, ceiling)
#' limit_log(-2.1, ceiling)
#' limit_log(2.1, ceiling)
limit_log <- function(data, fun, ...) {
  fun(10^-floor(data) * 10^data) / 10^-floor(data)
}

#' Create summetric log ticks
#'
#' @param data numeric vector
#'
#' @returns numeric vector
#' @export
#'
#' @examples
#' c(sample(seq(.1, 1, .1), 3), sample(1:10, 3)) |> create_log_tics()
create_log_tics <- function(data) {
  sort(round(unique(c(1 / data, data, 1)), 2))
}

#' Ensure symmetrical plot around 1 on a logarithmic x scale for ratio plots
#'
#' @param plot ggplot2 plot
#' @param breaks breaks used and mirrored
#' @param ... ignored
#'
#' @returns ggplot2 object
#' @export
#'
symmetrical_scale_x_log10 <- function(plot, breaks = c(1, 2, 3, 5, 10), ...) {
  rx <- ggplot2::layer_scales(plot)$x$get_limits()

  x_min <- floor(10 * rx[1]) / 10
  x_max <- ceiling(10 * rx[2]) / 10

  rx_min <- limit_log(rx[1], floor)
  rx_max <- limit_log(rx[2], ceiling)

  max_abs_x <- max(abs(c(x_min, x_max)))

  ticks <- log10(breaks) + (ceiling(max_abs_x) - 1)

  plot + ggplot2::scale_x_log10(limits = c(rx_min, rx_max), breaks = create_log_tics(10^ticks[ticks <= max_abs_x]))
}
