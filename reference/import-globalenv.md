# Import data from an Environment

Let the user select a dataset from its own environment or from a
package's environment. Modified from datamods

## Usage

``` r
import_globalenv_ui(
  id,
  globalenv = TRUE,
  packages = datamods::get_data_packages(),
  title = TRUE
)

import_globalenv_server(
  id,
  btn_show_data = TRUE,
  show_data_in = c("popup", "modal"),
  trigger_return = c("button", "change"),
  return_class = c("data.frame", "data.table", "tbl_df", "raw"),
  reset = reactive(NULL)
)
```

## Arguments

- id:

  Module's ID.

- globalenv:

  Search for data in Global environment.

- packages:

  Name of packages in which to search data.

- title:

  Module's title, if `TRUE` use the default title, use `NULL` for no
  title or a `shiny.tag` for a custom one.

- btn_show_data:

  Display or not a button to display data in a modal window if import is
  successful.

- show_data_in:

  Where to display data: in a `"popup"` or in a `"modal"` window.

- trigger_return:

  When to update selected data: `"button"` (when user click on button)
  or `"change"` (each time user select a dataset in the list).

- return_class:

  Class of returned data: `data.frame`, `data.table`, `tbl_df` (tibble)
  or `raw`.

- reset:

  A `reactive` function that when triggered resets the data.
