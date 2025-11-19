# Module to Reorder the Levels of a Factor Variable

This module contain an interface to reorder the levels of a factor
variable.

## Usage

``` r
update_factor_ui(id)

update_factor_server(id, data_r = reactive(NULL))

modal_update_factor(
  id,
  title = i18n$t("Update levels of a factor"),
  easyClose = TRUE,
  size = "l",
  footer = NULL
)

winbox_update_factor(
  id,
  title = i18n$t("Update levels of a factor"),
  options = shinyWidgets::wbOptions(),
  controls = shinyWidgets::wbControls()
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

- options:

  List of options, see
  [`wbOptions()`](https://dreamrs.github.io/shinyWidgets/reference/wbOptions.html).

- controls:

  List of controls, see
  [`wbControls()`](https://dreamrs.github.io/shinyWidgets/reference/wbControls.html).

## Value

A [`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)
function returning the data.
