# Select, rename and convert variables

Select, rename and convert variables

## Usage

``` r
update_variables_ui(id, title = "")

update_variables_server(
  id,
  data,
  height = NULL,
  return_data_on_init = FALSE,
  try_silent = FALSE
)
```

## Arguments

- id:

  Module's ID

- title:

  Module's title, if `TRUE` use the default title, use `NULL` for no
  title or a `shiny.tag` for a custom one.

- data:

  a `data.frame` or a `reactive` function returning a `data.frame`.

- height:

  Height for the table.

- return_data_on_init:

  Return initial data when module is called.

- try_silent:

  logical: should the report of error messages be suppressed?

## Value

A [`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)
function returning the updated data.
