# Regression coef plot from gtsummary. Slightly modified to pass on arguments

Regression coef plot from gtsummary. Slightly modified to pass on
arguments

## Usage

``` r
# S3 method for class 'tbl_regression'
plot(
  x,
  plot_ref = TRUE,
  remove_header_rows = TRUE,
  remove_reference_rows = FALSE,
  ...
)
```

## Arguments

- x:

  (`tbl_regression`, `tbl_uvregression`)  
  A 'tbl_regression' or 'tbl_uvregression' object

- plot_ref:

  (scalar `logical`)  
  plot reference values

- remove_header_rows:

  (scalar `logical`)  
  logical indicating whether to remove header rows for categorical
  variables. Default is `TRUE`

- remove_reference_rows:

  (scalar `logical`)  
  logical indicating whether to remove reference rows for categorical
  variables. Default is `FALSE`.

- ...:

  arguments passed to `ggstats::ggcoef_plot(...)`

## Value

ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
mod <- lm(mpg ~ ., default_parsing(mtcars))
p <- mod |>
  gtsummary::tbl_regression() |>
  plot(colour = "variable")
} # }
```
