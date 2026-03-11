# Restore column labels using a reference data frame

Convenience wrapper around
[`extract_labels()`](https://agdamsbo.github.io/FreesearchR/reference/extract_labels.md) +
[`apply_labels()`](https://agdamsbo.github.io/FreesearchR/reference/apply_labels.md).
Labels are matched by column name; new columns in `df_modified` are left
unchanged.

## Usage

``` r
restore_labels(df_modified, df_reference)
```

## Arguments

- df_modified:

  A data frame whose columns should receive labels.

- df_reference:

  A data frame carrying the authoritative `"label"` attributes.

## Value

`df_modified` with labels restored on all columns present in
`df_reference`.
