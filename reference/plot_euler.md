# Easily plot euler diagrams

Easily plot euler diagrams

## Usage

``` r
plot_euler(data, pri, sec, ter = NULL, seed = 2103, color.palette = "viridis")
```

## Arguments

- data:

  data

- seed:

  seed

- x:

  name of main variable

- y:

  name of secondary variables

- z:

  grouping variable

## Value

patchwork object

## Examples

``` r
data.frame(
  A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
  B = sample(c("A", "C"), 50, TRUE),
  C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE),
  D = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
) |> plot_euler("A", c("B", "C"), "D", seed = 4)
#> Error in dplyr::mutate(data$centers, label = purrr::map2(labels, quantities,     ~{        if (!is.na(.x) && !is.na(.y) && show_labels) {            paste0(.x, "\n", sprintf(.y, fmt = "%.4g"))        }        else if (!is.na(.x) && show_labels) {            .x        }        else if (!is.na(.y)) {            .y        }        else {            ""        }    })): ℹ In argument: `label = purrr::map2(...)`.
#> Caused by error in `purrr::map2()`:
#> ℹ In index: 1.
#> Caused by error in `sprintf()`:
#> ! invalid format '%.4g'; use format %s for character objects
mtcars |> plot_euler("vs", "am", seed = 1)
#> Error in dplyr::mutate(data$centers, label = purrr::map2(labels, quantities,     ~{        if (!is.na(.x) && !is.na(.y) && show_labels) {            paste0(.x, "\n", sprintf(.y, fmt = "%.4g"))        }        else if (!is.na(.x) && show_labels) {            .x        }        else if (!is.na(.y)) {            .y        }        else {            ""        }    })): ℹ In argument: `label = purrr::map2(...)`.
#> Caused by error in `purrr::map2()`:
#> ℹ In index: 1.
#> Caused by error in `sprintf()`:
#> ! invalid format '%.4g'; use format %s for character objects
mtcars |> plot_euler("vs", "am", "cyl", seed = 1)
#> Error in dplyr::mutate(data$centers, label = purrr::map2(labels, quantities,     ~{        if (!is.na(.x) && !is.na(.y) && show_labels) {            paste0(.x, "\n", sprintf(.y, fmt = "%.4g"))        }        else if (!is.na(.x) && show_labels) {            .x        }        else if (!is.na(.y)) {            .y        }        else {            ""        }    })): ℹ In argument: `label = purrr::map2(...)`.
#> Caused by error in `purrr::map2()`:
#> ℹ In index: 1.
#> Caused by error in `sprintf()`:
#> ! invalid format '%.4g'; use format %s for character objects
stRoke::trial |>
  dplyr::mutate(
    mfi_cut = cut(mfi_6, c(0, 12, max(mfi_6, na.rm = TRUE))),
    mdi_cut = cut(mdi_6, c(0, 20, max(mdi_6, na.rm = TRUE)))
  ) |>
  purrr::map2(
    c(sapply(stRoke::trial, \(.x)REDCapCAST::get_attr(.x, attr = "label")), "Fatigue", "Depression"),
    \(.x, .y){
      REDCapCAST::set_attr(.x, .y, "label")
    }
  ) |>
  dplyr::bind_cols() |>
  plot_euler("mfi_cut", "mdi_cut")
#> Error in dplyr::mutate(data$centers, label = purrr::map2(labels, quantities,     ~{        if (!is.na(.x) && !is.na(.y) && show_labels) {            paste0(.x, "\n", sprintf(.y, fmt = "%.4g"))        }        else if (!is.na(.x) && show_labels) {            .x        }        else if (!is.na(.y)) {            .y        }        else {            ""        }    })): ℹ In argument: `label = purrr::map2(...)`.
#> Caused by error in `purrr::map2()`:
#> ℹ In index: 1.
#> Caused by error in `sprintf()`:
#> ! invalid format '%.4g'; use format %s for character objects
stRoke::trial |>
  plot_euler(pri="male", sec=c("hypertension"))
#> Error in dplyr::mutate(data$centers, label = purrr::map2(labels, quantities,     ~{        if (!is.na(.x) && !is.na(.y) && show_labels) {            paste0(.x, "\n", sprintf(.y, fmt = "%.4g"))        }        else if (!is.na(.x) && show_labels) {            .x        }        else if (!is.na(.y)) {            .y        }        else {            ""        }    })): ℹ In argument: `label = purrr::map2(...)`.
#> Caused by error in `purrr::map2()`:
#> ℹ In index: 1.
#> Caused by error in `sprintf()`:
#> ! invalid format '%.4g'; use format %s for character objects
```
