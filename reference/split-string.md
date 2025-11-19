# String split module based on tidyr::separate\_

String split module based on tidyr::separate\_

## Usage

``` r
string_split_ui(id)

string_split_server(id, data_r = reactive(NULL))

modal_string_split(
  id,
  title = i18n$t("Split character string"),
  easyClose = TRUE,
  size = "xl",
  footer = NULL
)
```

## Arguments

- id:

  id

- data_r:

  reactive data

- title:

  Modal title

- easyClose:

  easyClose

- size:

  size

- footer:

  footer

## Value

A shiny ui module

shiny module server

shiny modal
