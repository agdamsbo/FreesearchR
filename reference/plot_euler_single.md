# Easily plot single euler diagrams

Easily plot single euler diagrams

## Usage

``` r
plot_euler_single(data, color.palette = "viridis", ...)
```

## Value

ggplot2 object

## Examples

``` r
data.frame(
  A = sample(c(TRUE, TRUE, FALSE), 50, TRUE),
  B = sample(c("A", "C"), 50, TRUE),
  C = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE),
  D = sample(c(TRUE, FALSE, FALSE, FALSE), 50, TRUE)
) |> plot_euler_single()
#> Error in dplyr::mutate(data$centers, label = purrr::map2(labels, quantities,     ~{        if (!is.na(.x) && !is.na(.y) && show_labels) {            paste0(.x, "\n", sprintf(.y, fmt = "%.4g"))        }        else if (!is.na(.x) && show_labels) {            .x        }        else if (!is.na(.y)) {            .y        }        else {            ""        }    })): ℹ In argument: `label = purrr::map2(...)`.
#> Caused by error in `purrr::map2()`:
#> ℹ In index: 1.
#> Caused by error in `sprintf()`:
#> ! invalid format '%.4g'; use format %s for character objects
mtcars[c("vs", "am")] |> plot_euler_single("magma")
#> Error in dplyr::mutate(data$centers, label = purrr::map2(labels, quantities,     ~{        if (!is.na(.x) && !is.na(.y) && show_labels) {            paste0(.x, "\n", sprintf(.y, fmt = "%.4g"))        }        else if (!is.na(.x) && show_labels) {            .x        }        else if (!is.na(.y)) {            .y        }        else {            ""        }    })): ℹ In argument: `label = purrr::map2(...)`.
#> Caused by error in `purrr::map2()`:
#> ℹ In index: 1.
#> Caused by error in `sprintf()`:
#> ! invalid format '%.4g'; use format %s for character objects
```
