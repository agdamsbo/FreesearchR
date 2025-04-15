## With snapshots
##

test_that("Creates correct table",{
  ## This is by far the easiest way to test all functions. Based on examples.
  tbl <- create_baseline(mtcars,by.var = "gear", add.p = "yes" == "yes",add.overall = TRUE, theme = "lancet")

  expect_equal(length(tbl),5)

  expect_equal(NROW(tbl$table_body), 19)

  expect_equal(NCOL(tbl$table_body), 13)
  tbl$call_list
  expect_equal(names(tbl), c("table_body", "table_styling", "call_list", "cards", "inputs"))

  tbl <- create_baseline(mtcars,by.var = "none", add.p = FALSE,add.overall = FALSE, theme = "lancet")

  expect_equal(length(tbl),5)

  tbl <- create_baseline(mtcars,by.var = "test", add.p = FALSE,add.overall = FALSE, theme = "jama")

  expect_equal(length(tbl),5)

  tbl <- create_baseline(default_parsing(mtcars),by.var = "am", add.p = FALSE,add.overall = FALSE, theme = "nejm")

  expect_equal(length(tbl),5)
})

