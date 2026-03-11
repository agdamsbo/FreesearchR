# Tests for column label utilities (extract_labels, apply_labels,
# restore_labels, with_labels, label_report)

# --- extract_labels ----------------------------------------------------------

test_that("extract_labels returns named character vector of present labels", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  attr(df$a, "label") <- "Column A"
  attr(df$b, "label") <- "Column B"

  lbls <- extract_labels(df)

  expect_type(lbls, "character")
  expect_named(lbls, c("a", "b"))
  expect_equal(lbls[["a"]], "Column A")
  expect_equal(lbls[["b"]], "Column B")
  expect_false("c" %in% names(lbls))
})

test_that("extract_labels returns zero-length vector when no labels present", {
  expect_equal(length(extract_labels(data.frame(x = 1, y = 2))), 0L)
})

test_that("extract_labels errors on non-data-frame input", {
  expect_error(extract_labels(list(a = 1)), "`df` must be a data frame")
  expect_error(extract_labels(1:5),         "`df` must be a data frame")
})


# --- apply_labels ------------------------------------------------------------

test_that("apply_labels sets label attributes on matching columns", {
  df  <- data.frame(age = 1:3, income = c(10, 20, 30))
  df2 <- apply_labels(df, c(age = "Age (years)", income = "Income (USD)"))

  expect_equal(attr(df2$age,    "label"), "Age (years)")
  expect_equal(attr(df2$income, "label"), "Income (USD)")
})

test_that("apply_labels silently ignores labels for absent columns", {
  df <- data.frame(age = 1:3)
  expect_no_error(apply_labels(df, c(age = "Age", income = "Income")))
  expect_equal(attr(apply_labels(df, c(age = "Age", income = "Income"))$age, "label"), "Age")
})

test_that("apply_labels errors on bad inputs", {
  expect_error(apply_labels(list(),       c(a = "A")), "`df` must be a data frame")
  expect_error(apply_labels(data.frame(), c("A")),     "`labels` must be a named")
  expect_error(apply_labels(data.frame(), 123),        "`labels` must be a named")
})


# --- restore_labels ----------------------------------------------------------

test_that("restore_labels copies labels from reference to modified df", {
  df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
  attr(df$a, "label") <- "Variable A"
  attr(df$b, "label") <- "Variable B"

  df_mod <- df[df$a > 2, ]
  attr(df_mod$a, "label") <- NULL
  attr(df_mod$b, "label") <- NULL

  df_restored <- restore_labels(df_mod, df)

  expect_equal(attr(df_restored$a, "label"), "Variable A")
  expect_equal(attr(df_restored$b, "label"), "Variable B")
})

test_that("restore_labels does not error when modified df has extra columns", {
  df <- data.frame(x = 1:3)
  attr(df$x, "label") <- "X"

  df_mod <- df
  df_mod$y <- df$x * 2

  result <- restore_labels(df_mod, df)
  expect_equal(attr(result$x, "label"), "X")
  expect_null(attr(result$y, "label"))
})

test_that("restore_labels errors on non-data-frame inputs", {
  df <- data.frame(x = 1)
  expect_error(restore_labels(list(), df), "`df_modified` must be a data frame")
  expect_error(restore_labels(df, list()), "`df_reference` must be a data frame")
})


# --- with_labels -------------------------------------------------------------

test_that("with_labels preserves labels through a subsetting expression", {
  df <- data.frame(id = 1:5, age = c(25, 34, 45, 29, 52))
  attr(df$age, "label") <- "Age (years)"

  result <- with_labels(df, df[df$age > 30, ])
  expect_equal(attr(result$age, "label"), "Age (years)")
})

test_that("with_labels does not assign labels to new columns", {
  df <- data.frame(x = 1:3, y = 4:6)
  attr(df$x, "label") <- "X label"

  result <- with_labels(df, { df$z <- df$x + df$y; df })

  expect_equal(attr(result$x, "label"), "X label")
  expect_null(attr(result$z, "label"))
})

test_that("with_labels errors when expression does not return a data frame", {
  df <- data.frame(x = 1:3)
  expect_error(with_labels(df, sum(df$x)), "must return a data frame")
})

test_that("with_labels errors on non-data-frame df argument", {
  expect_error(with_labels(list(x = 1), list()), "`df` must be a data frame")
})


# --- label_report ------------------------------------------------------------

test_that("label_report returns correct structure", {
  df <- data.frame(a = 1L, b = "x", stringsAsFactors = FALSE)
  attr(df$a, "label") <- "Alpha"

  report <- label_report(df)

  expect_s3_class(report, "data.frame")
  expect_named(report, c("column", "class", "label"))
  expect_equal(nrow(report), 2L)
  expect_equal(report$label[report$column == "a"], "Alpha")
  expect_equal(report$label[report$column == "b"], "(no label)")
})

test_that("label_report respects custom missing_marker", {
  df     <- data.frame(x = 1)
  report <- label_report(df, missing_marker = "N/A")
  expect_equal(report$label[1], "N/A")
})

test_that("label_report errors on bad inputs", {
  expect_error(label_report(list()),               "`df` must be a data frame")
  expect_error(label_report(data.frame(), c("a", "b")), "`missing_marker`")
})
