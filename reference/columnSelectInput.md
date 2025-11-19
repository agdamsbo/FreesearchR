# A selectizeInput customized for data frames with column labels

Copied and modified from the IDEAFilter package Adds the option to
select "none" which is handled later

## Usage

``` r
columnSelectInput(
  inputId,
  label,
  data,
  selected = "",
  ...,
  col_subset = NULL,
  placeholder = "",
  onInitialize,
  none_label = "No variable selected",
  maxItems = NULL
)
```

## Arguments

- inputId:

  passed to
  [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)

- label:

  passed to
  [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)

- data:

  `data.frame` object from which fields should be populated

- selected:

  default selection

- ...:

  passed to
  [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)

- col_subset:

  a `vector` containing the list of allowable columns to select

- placeholder:

  passed to
  [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)
  options

- onInitialize:

  passed to
  [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)
  options

- none_label:

  label for "none" item

- maxItems:

  max number of items

## Value

a [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html)
dropdown element
