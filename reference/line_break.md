# Line breaking at given number of characters for nicely plotting labels

Line breaking at given number of characters for nicely plotting labels

## Usage

``` r
line_break(data, lineLength = 20, force = FALSE)
```

## Arguments

- data:

  string

- lineLength:

  maximum line length

- fixed:

  flag to force split at exactly the value given in lineLength. Default
  is FALSE, only splitting at spaces.

## Value

character string

## Examples

``` r
"Lorem ipsum... you know the routine" |> line_break()
#> [1] "Lorem ipsum... you\nknow the routine"
paste(sample(letters[1:10], 100, TRUE), collapse = "") |> line_break(force = TRUE)
#> [1] "ibgccehdajdebjihfdbc\nbacigaffbaadgbabdedh\ncjeaifgdbjhieacaagga\nagdfeediafjfibbfjihd\negiebfajgfhcgbi\n"
```
