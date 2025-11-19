# Vertical stacked bar plot wrapper

Vertical stacked bar plot wrapper

## Usage

``` r
vertical_stacked_bars(
  data,
  score = "full_score",
  group = "pase_0_q",
  strata = NULL,
  t.size = 10,
  l.color = "black",
  l.size = 0.5,
  draw.lines = TRUE,
  label.str = "{n}\n{round(100 * p,0)}%"
)
```

## Arguments

- data:

  data.frame

- score:

  outcome variable

- group:

  grouping variable

- strata:

  stratifying variable

- t.size:

  text size

## Value

ggplot2 object
