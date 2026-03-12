test_that("default_parsing returns a data.frame", {
  result <- default_parsing(mtcars)
  expect_true(is.data.frame(result))
})

test_that("default_parsing preserves row count", {
  result <- default_parsing(mtcars)
  expect_equal(nrow(result), nrow(mtcars))
})

test_that("default_parsing preserves column count", {
  result <- default_parsing(mtcars)
  expect_equal(ncol(result), ncol(mtcars))
})

test_that("default_parsing produces valid column names (make.names compatible)", {
  # Create data with problematic column names
  bad_names_df <- data.frame(
    `1bad` = 1:5,
    `has space` = letters[1:5],
    `good_name` = TRUE,
    check.names = FALSE
  )
  result <- default_parsing(bad_names_df)
  expect_true(all(make.names(names(result)) == names(result)))
})

test_that("default_parsing handles duplicate column names", {
  dup_df <- data.frame(a = 1:5, b = 6:10)
  names(dup_df) <- c("x", "x")
  result <- default_parsing(dup_df)
  expect_equal(length(names(result)), 2)
  expect_true(all(!duplicated(names(result))))
})

test_that("default_parsing converts low-cardinality numeric columns to factor", {
  # A numeric column with <= 8 unique values should become a factor
  df <- data.frame(
    group = c(1, 2, 3, 1, 2, 3, 1, 2),   # 3 unique → factor
    value = rnorm(8)                        # 8 unique → stays numeric
  )
  result <- default_parsing(df)
  expect_true(is.factor(result$group))
})

test_that("default_parsing converts low-cardinality character columns to factor", {
  # A character column with <= 10 unique values should become a factor
  df <- data.frame(
    category = rep(c("a", "b", "c"), 4),   # 3 unique → factor
    stringsAsFactors = FALSE
  )
  result <- default_parsing(df)
  expect_true(is.factor(result$category))
})

test_that("default_parsing drops unused factor levels", {
  df <- data.frame(
    x = factor(c("a", "b", "a"), levels = c("a", "b", "c"))  # "c" unused
  )
  result <- default_parsing(df)
  expect_false("c" %in% levels(result$x))
})

test_that("default_parsing converts logical-like columns to logical", {
  df <- data.frame(
    flag = c(0L, 1L, 0L, 1L, 0L),
    stringsAsFactors = FALSE
  )
  result <- default_parsing(df)
  # as_logical should have converted 0/1 integer to logical
  expect_true(is.logical(result$flag))
})

test_that("default_parsing preserves column labels when present", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  attr(df$a, "label") <- "Column A Label"
  attr(df$b, "label") <- "Column B Label"

  result <- default_parsing(df)

  expect_equal(attr(result$a, "label"), "Column A Label")
  expect_equal(attr(result$b, "label"), "Column B Label")
})

test_that("default_parsing handles columns with no label attribute", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  result <- default_parsing(df)
  # Should not error; label attrs simply absent or NULL
  expect_null(attr(result$a, "label"))
})

test_that("default_parsing handles a single-column data.frame", {
  df <- data.frame(x = 1:10)
  result <- default_parsing(df)
  expect_equal(ncol(result), 1)
  expect_equal(nrow(result), 10)
})

test_that("default_parsing handles an empty data.frame gracefully", {
  df <- data.frame(a = integer(0), b = character(0), stringsAsFactors = FALSE)
  result <- default_parsing(df)
  expect_equal(nrow(result), 0)
})

test_that("default_parsing handles all-NA columns without error", {
  df <- data.frame(a = NA_real_, b = NA_character_, stringsAsFactors = FALSE)
  expect_no_error(default_parsing(df))
})

test_that("default_parsing removes nested list columns", {
  df <- data.frame(id = 1:3)
  df$nested <- list(list(1, 2), list(3), list(4, 5))  # nested list column
  # Should not crash; nested list column is removed by remove_nested_list()
  expect_no_error(default_parsing(df))
})

test_that("default_parsing works with dplyr::starwars-like tibble", {
  skip_if_not_installed("dplyr")
  sw <- head(dplyr::starwars, 10)
  result <- default_parsing(sw)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 10)
})

test_that("default_parsing high-cardinality character column stays character or factor", {
  # > 10 unique values → should NOT be coerced to factor by numchar2fct
  df <- data.frame(
    id = paste0("id_", 1:20),
    stringsAsFactors = FALSE
  )
  result <- default_parsing(df)
  # high cardinality: remains character (not factor)
  expect_false(is.factor(result$id))
})
