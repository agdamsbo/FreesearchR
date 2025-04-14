test_that("Create columnSelectInput", {
  expect_snapshot(columnSelectInput("x",label = "X",data = mtcars))
})
