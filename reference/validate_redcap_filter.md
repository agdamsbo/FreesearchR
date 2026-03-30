# Validate a REDCap server-side filter string against a data dictionary

Checks that a REDCap filter expression is syntactically correct and
consistent with the field types defined in the project data dictionary.
Plain text without field references is always rejected. Multi-clause
filters joined by `AND` or `OR` are supported.

## Usage

``` r
validate_redcap_filter(filter, dictionary)
```

## Arguments

- filter:

  A single character string containing the filter expression, e.g.
  `"[age] > 18"` or `"[cohabitation] = '1' AND [age] > 18"`.

- dictionary:

  A data frame representing the REDCap data dictionary in API export
  format, as returned by e.g. `REDCapCAST::get_redcap_metadata()`. Must
  contain at least the columns `field_name` and `field_type`. The
  columns `text_validation_type_or_show_slider_number` and
  `select_choices_or_calculations` are used when present for stricter
  type and choice validation.

## Value

A named list with two elements:

- `valid`:

  Logical. `TRUE` if the filter passes all checks.

- `message`:

  Character. `"Filter is valid."` on success, or a newline-separated
  string of error messages describing every problem found.

## Details

Validation rules by field type:

- `calc`:

  Numeric fields. Value must be an unquoted number. All comparison
  operators (`=`, `!=`, `<`, `>`, `<=`, `>=`) are accepted.

- `text` with date validation:

  Fields with validation type `date_ymd`, `date_dmy`, `datetime_*`, etc.
  Value must be a quoted date/datetime string in `'YYYY-MM-DD'` format.
  All comparison operators are accepted.

- `text` with time validation:

  Fields with validation type `time_hh_mm_ss` or `time_mm_ss`. Value
  must be a quoted time string, e.g. `'14:30:00'`. All comparison
  operators are accepted.

- `radio` / `dropdown`:

  Categorical fields. Value must be a quoted choice code (e.g. `'1'`)
  that exists in the field's choice list. Only `=` and `!=` are
  accepted.

- `text` (plain):

  Free-text fields. Value must be a quoted string. Only `=` and `!=` are
  accepted.

## Examples

``` r
if (FALSE) { # \dontrun{
dict <- REDCapCAST::get_redcap_metadata(
  uri    = "https://redcap.example.com/api/",
  token  = Sys.getenv("REDCAP_TOKEN")
)

validate_redcap_filter("[age] > 18", dict)
#> list(valid = TRUE, message = "Filter is valid.")

validate_redcap_filter("only plain text", dict)
#> list(valid = FALSE, message = "Filter must contain at least one field ...")

validate_redcap_filter("[cohabitation] = '1' AND [age] > 18", dict)
#> list(valid = TRUE, message = "Filter is valid.")
} # }
```
