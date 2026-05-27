# Create a baseline table

Create a baseline table

## Usage

``` r
create_baseline(
  data,
  ...,
  by.var,
  add.p = FALSE,
  add.diff = FALSE,
  add.overall = FALSE,
  theme = c("jama", "lancet", "nejm", "qjecon"),
  detail_level = c("minimal", "extended"),
  drop_empty = FALSE
)
```

## Arguments

- data:

  data

- ...:

  passed as fun.arg to baseline_table()

- by.var:

  specify stratification variable

- add.p:

  add comparison/p-value

- add.overall:

  add overall column

- theme:

  set table theme

- detail_level:

  specify detail level. Either "minimal" or "extended".

## Value

gtsummary table list object

## Examples

``` r
mtcars |> create_baseline(by.var = "gear", add.p = "yes" == "yes")


  

Characteristic
```
