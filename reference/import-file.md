# Import data from a file

Let user upload a file and import data

## Usage

``` r
import_file_ui(
  id,
  title = "",
  preview_data = TRUE,
  file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat",
    ".sav"),
  layout_params = c("dropdown", "inline")
)

import_file_server(
  id,
  btn_show_data = TRUE,
  show_data_in = c("popup", "modal"),
  trigger_return = c("button", "change"),
  return_class = c("data.frame", "data.table", "tbl_df", "raw"),
  reset = reactive(NULL),
  limit = 1e+05
)
```

## Arguments

- preview_data:

  Show or not a preview of the data under the file input.

- file_extensions:

  File extensions accepted by
  [`shiny::fileInput()`](https://rdrr.io/pkg/shiny/man/fileInput.html),
  can also be MIME type.

- layout_params:

  How to display import parameters : in a dropdown button or inline
  below file input.
