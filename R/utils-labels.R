# =============================================================================
# Column Label Utilities
#
# Coded with help from Claude to save time.
# Could be seperated for its own package.
# =============================================================================

#' Extract column labels from a data frame
#'
#' @param df A data frame.
#' @return A named character vector of label strings (only labelled columns included).
#' @export
extract_labels <- function(df) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  labels <- vapply(df, function(col) {
    lbl <- attr(col, "label")
    if (is.null(lbl)) NA_character_ else as.character(lbl)
  }, FUN.VALUE = character(1))

  labels[!is.na(labels)]
}


#' Apply a named label vector to a data frame
#'
#' @param df A data frame.
#' @param labels A named character vector (names = column names, values = labels).
#'   Typically the output of [extract_labels()]. Labels for absent columns are
#'   silently ignored.
#' @return `df` with `"label"` attributes set on matching columns.
#' @export
apply_labels <- function(df, labels) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)
  if (!is.character(labels) || is.null(names(labels))) {
    stop("`labels` must be a named character vector.", call. = FALSE)
  }

  for (col in intersect(names(labels), names(df))) {
    attr(df[[col]], "label") <- labels[[col]]
  }

  df
}


#' Restore column labels using a reference data frame
#'
#' Convenience wrapper around [extract_labels()] + [apply_labels()]. Labels are
#' matched by column name; new columns in `df_modified` are left unchanged.
#'
#' @param df_modified A data frame whose columns should receive labels.
#' @param df_reference A data frame carrying the authoritative `"label"` attributes.
#' @return `df_modified` with labels restored on all columns present in `df_reference`.
#' @export
restore_labels <- function(df_modified, df_reference) {
  if (!is.data.frame(df_modified))  stop("`df_modified` must be a data frame.",  call. = FALSE)
  if (!is.data.frame(df_reference)) stop("`df_reference` must be a data frame.", call. = FALSE)

  apply_labels(df_modified, extract_labels(df_reference))
}


#' Evaluate an expression while preserving column labels
#'
#' Snapshots labels from `df` before evaluating `expr`, then reapplies them to
#' matching columns in the result. New columns created inside `expr` receive no
#' label automatically.
#'
#' @param df A data frame carrying `"label"` attributes.
#' @param expr An unquoted expression that transforms `df` and returns a data frame.
#' @return The data frame produced by `expr`, with original labels restored.
#' @export
with_labels <- function(df, expr) {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)

  labels <- extract_labels(df)
  result <- eval(substitute(expr), parent.frame())

  if (!is.data.frame(result)) {
    stop("The expression passed to `with_labels()` must return a data frame.", call. = FALSE)
  }

  apply_labels(result, labels)
}


#' Print a tidy summary of column labels
#'
#' @param df A data frame.
#' @param missing_marker String used when a column has no label. Default: `"(no label)"`.
#' @return A `column / class / label` data frame, printed and returned invisibly.
#' @export
label_report <- function(df, missing_marker = "(no label)") {
  if (!is.data.frame(df)) stop("`df` must be a data frame.", call. = FALSE)
  if (!is.character(missing_marker) || length(missing_marker) != 1L) {
    stop("`missing_marker` must be a single character string.", call. = FALSE)
  }

  labels <- vapply(df, function(col) {
    lbl <- attr(col, "label")
    if (is.null(lbl)) missing_marker else as.character(lbl)
  }, FUN.VALUE = character(1))

  report <- data.frame(
    column = names(df),
    class  = vapply(df, function(x) paste(class(x), collapse = "/"), character(1)),
    label  = unname(labels),
    stringsAsFactors = FALSE
  )

  print(report, row.names = FALSE)
  invisible(report)
}
