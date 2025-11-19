# Detect delimiters in string based on allowed delimiters

Accepts any repeat of delimiters and includes surrounding whitespace

## Usage

``` r
detect_delimiter(data, delimiters = c("_", "-", ";", "\n", ","))
```

## Arguments

- delimiters:

  allowed delimiters

- text:

  character vector

## Value

character vector

## Examples

``` r
sapply(c("Walk - run", "Sel__Re", "what;now"), detect_delimiter)
#> Walk - run    Sel__Re   what;now 
#>      " - "       "__"        ";" 
```
