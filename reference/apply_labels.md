# Apply a named label vector to a data frame

Apply a named label vector to a data frame

## Usage

``` r
apply_labels(df, labels)
```

## Arguments

- df:

  A data frame.

- labels:

  A named character vector (names = column names, values = labels).
  Typically the output of
  [`extract_labels()`](https://agdamsbo.github.io/FreesearchR/reference/extract_labels.md).
  Labels for absent columns are silently ignored.

## Value

`df` with `"label"` attributes set on matching columns.
