test_that("Version is character string", {
  expect_equal(length(app_version()), 1)
  expect_true(is.character(app_version()))
})
