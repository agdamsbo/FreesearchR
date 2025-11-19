# Module to Convert Numeric to Factor

This module contain an interface to cut a numeric into several
intervals.

## Usage

``` r
cut_variable_ui(id)

cut_variable_server(id, data_r = reactive(NULL))

modal_cut_variable(
  id,
  title = i18n$t("Convert Numeric to Factor"),
  easyClose = TRUE,
  size = "l",
  footer = NULL
)
```

## Arguments

- id:

  Module ID.

- data_r:

  A [`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)
  function returning a `data.frame`.

- title:

  An optional title for the dialog.

- easyClose:

  If `TRUE`, the modal dialog can be dismissed by clicking outside the
  dialog box, or be pressing the Escape key. If `FALSE` (the default),
  the modal dialog can't be dismissed in those ways; instead it must be
  dismissed by clicking on a
  [`modalButton()`](https://rdrr.io/pkg/shiny/man/modalDialog.html), or
  from a call to
  [`removeModal()`](https://rdrr.io/pkg/shiny/man/showModal.html) on the
  server.

- size:

  One of `"s"` for small, `"m"` (the default) for medium, `"l"` for
  large, or `"xl"` for extra large. Note that `"xl"` only works with
  Bootstrap 4 and above (to opt-in to Bootstrap 4+, pass
  [`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html)
  to the `theme` argument of a page container like
  [`fluidPage()`](https://rdrr.io/pkg/shiny/man/fluidPage.html)).

- footer:

  UI for footer. Use `NULL` for no footer.

## Value

A [`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)
function returning the data.
