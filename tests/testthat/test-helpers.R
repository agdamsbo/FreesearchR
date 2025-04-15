## getfun
test_that("getfun works", {
  expect_snapshot(
    getfun("stats::lm")
  )
})

## argsstring2list
test_that("argsstring2list works", {
  expect_snapshot(
    argsstring2list("A=1:5,b=2:4")
  )
})

## factorize
test_that("factorize works", {
  expect_snapshot(
    factorize(mtcars, names(mtcars))
  )
})

## default_parsing
test_that("default_parsing works", {
  expect_snapshot(
    default_parsing(mtcars)
  )
})

## remove_empty_attr
test_that("remove_empty_attr works", {
  ds <- mtcars |> lapply(\(.x) REDCapCAST::set_attr(.x, label = NA, attr = "label"))

  expect_snapshot(
    remove_empty_attr(ds)
  )

  expect_snapshot(
    remove_empty_attr(dplyr::bind_cols(ds))
  )

  expect_snapshot(
    remove_empty_attr(ds[[1]])
  )
})
## remove_empty_cols
test_that("remove_empty_cols works", {
  expect_snapshot(
    data.frame(a = 1:10, b = NA, c = c(2, NA)) |> remove_empty_cols(cutoff = .5)
  )
})
## append_list
test_that("append_list works", {
  ls_d <- list(test = c(1:20))
  ls_d <- list()

  expect_snapshot(
    data.frame(letters[1:20], 1:20) |> append_list(ls_d, "letters")
  )

  expect_snapshot(
    letters[1:20] |> append_list(ls_d, "letters")
  )
})


## missing_fraction
test_that("missing_fraction works", {
  expect_snapshot(
    c(NA, 1:10, rep(NA, 3)) |> missing_fraction()
  )
})


## data_description
test_that("data_description works", {
  expect_snapshot(
    data.frame(
      sample(1:8, 20, TRUE),
      sample(c(1:8, NA), 20, TRUE)
    ) |> data_description(data_text = "This data")
  )
})

## data_type_filter()

test_that("Data type filter works", {
  expect_snapshot(
    default_parsing(mtcars) |> data_type_filter(type = c("categorical", "continuous"))
  )

  expect_snapshot(
    default_parsing(mtcars) |> data_type_filter(type = NULL)
  )

  expect_error(default_parsing(mtcars) |> data_type_filter(type = "test"))
})

## sort_by
test_that("sort_by works", {
  expect_snapshot(
    sort_by(c("Multivariable", "Univariable"), c("Univariable", "Minimal", "Multivariable"))
  )
})

## if_not_missing
test_that("if_not_missing works", {
  expect_snapshot(
    NULL |> if_not_missing("new")
  )

  expect_snapshot(
    c(2, "a", NA) |> if_not_missing()
  )

  expect_snapshot(
    "See" |> if_not_missing()
  )
})

## merge_expression + pipe_string + expression_string
test_that("merge_expression, expression_string and pipe_string works", {
  expect_snapshot(
    list(
      rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
      rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
    ) |> merge_expression()
  )

  expect_snapshot(
    list(
      "mtcars",
      rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
      rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
    ) |>
      lapply(expression_string) |>
      pipe_string() |>
      expression_string("data<-")
  )

  expect_snapshot(
    list(
      as.symbol(paste0("mtcars$", "mpg")),
      rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"),
      rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")
    ) |>
      merge_expression() |>
      expression_string()
  )
})

## remove_nested_list
test_that("remove_nested_list works", {
  expect_snapshot(
    dplyr::tibble(a = 1:10, b = rep(list("a"), 10)) |> remove_nested_list()
  )

  expect_snapshot(
    dplyr::tibble(a = 1:10, b = rep(list(c("a", "b")), 10)) |>
      as.data.frame() |>
      remove_nested_list()
  )
})

## set_column_label
test_that("set_column_label works", {
  ls <- list("mpg" = "", "cyl" = "Cylinders", "disp" = "", "hp" = "", "drat" = "", "wt" = "", "qsec" = "", "vs" = "", "am" = "", "gear" = "", "carb" = "")
  ls2 <- c("mpg" = "", "cyl" = "Cylinders", "disp" = "", "hp" = "Horses", "drat" = "", "wt" = "", "qsec" = "", "vs" = "", "am" = "", "gear" = "", "carb" = "")
  ls3 <- c("mpg" = "", "cyl" = "", "disp" = "", "hp" = "Horses", "drat" = "", "wt" = "", "qsec" = "", "vs" = "", "am" = "", "gear" = "", "carb" = "")

  expect_snapshot(
    mtcars |>
      set_column_label(ls) |>
      set_column_label(ls2) |>
      set_column_label(ls3)
  )

  expect_snapshot(
    rlang::expr(FreesearchR::set_column_label(label = !!ls3)) |> expression_string()
  )
})
## append_column
test_that("append_column works", {
  expect_snapshot(
    mtcars |>
      dplyr::mutate(mpg_cut = mpg) |>
      append_column(mtcars$mpg, "mpg_cutter")
  )
})
