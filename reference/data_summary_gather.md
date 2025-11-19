# Data summary for printing visual summary

Data summary for printing visual summary

## Usage

``` r
data_summary_gather(
  data,
  summary.fun = class,
  palette.fun = viridisLite::viridis,
  na.label = "NA",
  ...
)
```

## Arguments

- data:

  data.frame

- summary.fun:

  fun for summarising

- palette.fun:

  optionally use specific palette functions. First argument has to be
  the length.

- na.label:

  label for NA

- ...:

  overflow

## Value

data.frame

## Examples

``` r
mtcars |> data_summary_gather() |> names()
#> [1] "data"        "colors"      "labels"      "summary.fun"
```
