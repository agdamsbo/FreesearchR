## all_but
test_that("all_but works", {
  expect_snapshot(all_but(1:10, c(2, 3), 11, 5))
})

## subset_types
test_that("subset_types works", {
  expect_snapshot(
    default_parsing(mtcars) |> subset_types("continuous")
  )
  expect_snapshot(
    default_parsing(mtcars) |> subset_types(c("dichotomous", "ordinal", "categorical"))
  )
  expect_snapshot(
    default_parsing(mtcars) |> subset_types("test")
  )
})

## supported_plots
test_that("supported_plots works", {
  expect_true(is.list(supported_plots()))
})

## possible_plots
test_that("possible_plots works", {
  expect_snapshot(possible_plots(mtcars$mpg))

  expect_snapshot(default_parsing(mtcars)["cyl"] |>
    possible_plots())
})

## get_plot_options
test_that("get_plot_options works", {
  expect_snapshot(default_parsing(mtcars)["mpg"] |>
    possible_plots() |>
    (\(.x){
      .x[[1]]
    })() |>
    get_plot_options())
})

## create_plot and friends
test_that("create_plot works", {
  ## Violin
  p_list <- create_plot(mtcars, type = "plot_violin", pri = "mpg", sec = "cyl", ter = "am")
  p <- p_list[[1]] + ggplot2::labs(title = "Test plot")

  expect_equal(length(p_list), 2)
  expect_true(ggplot2::is_ggplot(p))

  # Includes helper functions
  #   wrap_plot_list
  #   align_axes
  #   clean_common_axis

  ## Scatter
  p_list <- list(
    create_plot(mtcars, type = "plot_scatter", pri = "mpg", sec = "cyl"),
    create_plot(mtcars, type = "plot_scatter", pri = "mpg", sec = "cyl", ter = "am")
  )

  lapply(p_list, \(.x){
    expect_true(ggplot2::is_ggplot(.x))
  })

  purrr::map2(p_list, list(11, 11), \(.x, .y){
    expect_equal(length(.x), .y)
  })
})

## get_label
test_that("get_label works", {
  expect_snapshot(mtcars |> get_label(var = "mpg"))
  expect_snapshot(mtcars |> get_label())
  expect_snapshot(mtcars$mpg |> get_label())
  expect_snapshot(gtsummary::trial |> get_label(var = "trt"))
  expect_snapshot(1:10 |> get_label())
})

## line_break
test_that("line_break works", {
  expect_snapshot("Lorem ipsum... you know the routine" |> line_break())
  expect_snapshot(paste(rep(letters, 5), collapse = "") |> line_break(force = TRUE, lineLength = 5))
  expect_snapshot(paste(rep(letters, 5), collapse = "") |> line_break(force = FALSE))
})
