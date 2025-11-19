# Test class

Test class

## Usage

``` r
is_any_class(data, class.vec)
```

## Arguments

- data:

  data

- class.vec:

  vector of class names to test

## Value

factor

## Examples

``` r
if (FALSE) { # \dontrun{
vapply(REDCapCAST::redcapcast_data, \(.x){
  is_any_class(.x, c("hms", "Date", "POSIXct", "POSIXt"))
}, logical(1))
} # }
```
