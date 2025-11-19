# Test if element is identical to the previous

Test if element is identical to the previous

## Usage

``` r
is_identical_to_previous(data, no.name = TRUE)
```

## Arguments

- data:

  data. vector, data.frame or list

- no.name:

  logical to remove names attribute before testing

## Value

logical vector

## Examples

``` r
c(1, 1, 2, 3, 3, 2, 4, 4) |> is_identical_to_previous()
#> [1] FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE
mtcars[c(1, 1, 2, 3, 3, 2, 4, 4)] |> is_identical_to_previous()
#> [1] FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE  TRUE
list(1, 1, list(2), "A", "a", "a") |> is_identical_to_previous()
#> [1] FALSE  TRUE FALSE FALSE FALSE  TRUE
```
