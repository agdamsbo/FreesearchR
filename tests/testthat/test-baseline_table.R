## With snapshots
##

test_that("Creates correct table",{
  expect_snapshot(create_baseline(mtcars,by.var = "gear", add.p = "yes" == "yes",add.overall = TRUE, theme = "lancet"))
})

test_that("Creates table", {
  tbl <- mtcars |> baseline_table(fun.args = list(by = "gear"))

  expect_equal(length(tbl), 5)

  expect_equal(NROW(tbl$table_body), 19)

  expect_equal(NCOL(tbl$table_body), 8)

  expect_equal(names(tbl), c("table_body", "table_styling", "call_list", "cards", "inputs"))
})

test_that("Creates table", {
  tbl <- mtcars |> create_baseline(by.var = "gear", add.p = "yes" == "yes")

  expect_equal(length(tbl), 5)

  expect_equal(NROW(tbl$table_body), 19)

  expect_equal(NCOL(tbl$table_body), 13)

  expect_equal(names(tbl), c("table_body", "table_styling", "call_list", "cards", "inputs"))
})

test_that("Creates table", {
  tbl <- mtcars |> create_baseline(by.var = "gear", add.p = "yes" == "yes")

  expect_equal(length(tbl), 5)

  expect_equal(NROW(tbl$table_body), 19)

  expect_equal(NCOL(tbl$table_body), 13)

  expect_equal(names(tbl), c("table_body", "table_styling", "call_list", "cards", "inputs"))
})
