# Evaluate an expression while preserving column labels

Snapshots labels from `df` before evaluating `expr`, then reapplies them
to matching columns in the result. New columns created inside `expr`
receive no label automatically.

## Usage

``` r
with_labels(df, expr)
```

## Arguments

- df:

  A data frame carrying `"label"` attributes.

- expr:

  An unquoted expression that transforms `df` and returns a data frame.

## Value

The data frame produced by `expr`, with original labels restored.
