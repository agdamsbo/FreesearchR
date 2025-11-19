# Create regression summary table

Create regression summary table

## Usage

``` r
regression_table_create(
  x,
  ...,
  args.list = NULL,
  fun = "gtsummary::tbl_regression",
  theme = c("jama", "lancet", "nejm", "qjecon")
)
```

## Arguments

- x:

  (list of) regression model

- ...:

  ignored for now

- args.list:

  args.list for the summary function

- fun:

  table summary function. Default is "gtsummary::tbl_regression"

- theme:

  summary table theme

## Value

gtsummary list object
