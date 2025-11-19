# Display a table in a window

Display a table in a window

## Usage

``` r
show_data(
  data,
  title = NULL,
  options = NULL,
  show_classes = TRUE,
  type = c("popup", "modal", "winbox"),
  width = "65%",
  ...
)
```

## Arguments

- data:

  a data object (either a `matrix` or a `data.frame`).

- title:

  Title to be displayed in window.

- options:

  Arguments passed to
  [`toastui::datagrid()`](https://dreamrs.github.io/toastui/reference/datagrid.html).

- show_classes:

  Show variables classes under variables names in table header.

- type:

  Display table in a pop-up with
  [`shinyWidgets::show_alert()`](https://dreamrs.github.io/shinyWidgets/reference/sweetalert.html),
  in modal window with
  [`shiny::showModal()`](https://rdrr.io/pkg/shiny/man/showModal.html)
  or in a WinBox window with
  [`shinyWidgets::WinBox()`](https://dreamrs.github.io/shinyWidgets/reference/WinBox.html).

- width:

  Width of the window, only used if `type = "popup"` or
  `type = "winbox"`.

- ...:

  Additional options, such as `wbOptions = wbOptions()` or
  `wbControls = wbControls()`.

## Value

No value.

## Note

If you use `type = "winbox"`, you'll need to use
[`shinyWidgets::html_dependency_winbox()`](https://dreamrs.github.io/shinyWidgets/reference/html_dependency_winbox.html)
somewhere in your UI.
