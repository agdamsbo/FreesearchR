# Data correlations evaluation module

Data correlations evaluation module

## Usage

``` r
data_correlations_ui(id, ...)

data_correlations_server(
  id,
  data,
  include.class = NULL,
  cutoff = 0.7,
  warning_str =
    i18n$t("The following variable pairs are highly correlated: {sentence_paste(.x,and_str)}.\nConsider excluding one {more}from the dataset to ensure variables are independent."),
  warning_no_str = i18n$t("No variables have a correlation measure above the threshold."),
  and_str = i18n$t("and"),
  ...
)
```

## Arguments

- id:

  id

- ...:

  arguments passed to toastui::datagrid

- data:

  data

- include.class:

  character vector of classes to include. Default is NULL

- cutoff:

  numeric

- warning_str:

  Character string. Exposed to allow dynamic translations

- warning_no_str:

  Character string. Exposed to allow dynamic translations

- and_strCharacter:

  string. Exposed to allow dynamic translations

## Value

Shiny ui module

shiny server module
