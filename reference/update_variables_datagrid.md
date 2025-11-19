# Modified from the datamods pacakge

Modified from the datamods pacakge

## Usage

``` r
update_variables_datagrid(
  data,
  height = NULL,
  selectionId = NULL,
  buttonId = NULL
)
```

## Arguments

- data:

  data

- height:

  height

- selectionId:

  selectionId

- buttonId:

  buttonId

## Examples

``` r
mtcars |>
  summary_vars() |>
  update_variables_datagrid()
#> Error in update_variables_datagrid(summary_vars(mtcars)): could not find function "update_variables_datagrid"
```
