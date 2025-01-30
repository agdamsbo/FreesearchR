#' Regression coef plot from gtsummary. Slightly modified to pass on arguments
#'
#' @param x (`tbl_regression`, `tbl_uvregression`)\cr
#'   A 'tbl_regression' or 'tbl_uvregression' object
##  #' @param remove_header_rows (scalar `logical`)\cr
##  #'   logical indicating whether to remove header rows
##  #'   for categorical variables. Default is `TRUE`
##  #' @param remove_reference_rows (scalar `logical`)\cr
##  #'   logical indicating whether to remove reference rows
##  #'   for categorical variables. Default is `FALSE`.
#' @param ... arguments passed to `ggstats::ggcoef_plot(...)`
#'
#' @returns ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' mod <- lm(mpg ~ ., mtcars)
#' p <- mod |>
#'   gtsummary::tbl_regression() |>
#'   plot(colour = "variable")
#' }
#'
plot.tbl_regression <- function(x,
                                # remove_header_rows = TRUE,
                                # remove_reference_rows = FALSE,
                                ...) {
  # check_dots_empty()
  gtsummary:::check_pkg_installed("ggstats")
  gtsummary:::check_not_missing(x)
  # gtsummary:::check_scalar_logical(remove_header_rows)
  # gtsummary:::check_scalar_logical(remove_reference_rows)

  df_coefs <- x$table_body
  # if (isTRUE(remove_header_rows)) {
  #   df_coefs <- df_coefs |> dplyr::filter(!.data$header_row %in% TRUE)
  # }
  # if (isTRUE(remove_reference_rows)) {
  #   df_coefs <- df_coefs |> dplyr::filter(!.data$reference_row %in% TRUE)
  # }

  # browser()

  df_coefs$label[df_coefs$row_type == "label"] <- ""

  df_coefs %>%
    ggstats::ggcoef_plot(exponentiate = x$inputs$exponentiate, ...)
}


# default_parsing(mtcars) |> lapply(class)
#
# purrr::imap(mtcars,\(.x,.i){
#   if (.i %in% c("vs","am","gear","carb")){
#     as.factor(.x)
#   } else .x
#   }) |> dplyr::bind_cols()
#
#


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
    dplyr::bind_rows() |> dplyr::mutate(model=as_factor(model))

  l_merged$table_body <- df_body_long

  l_merged$inputs$exponentiate <- !identical(class(list$models$Multivariable$model), "lm")

  l_merged
}
