# Convert string of arguments to list of arguments

Idea from the answer: https://stackoverflow.com/a/62979238

## Usage

``` r
argsstring2list(string)
```

## Arguments

- string:

  string to convert to list to use with do.call

## Value

list

## Examples

``` r
argsstring2list("A=1:5,b=2:4")
#> $A
#> [1] 1 2 3 4 5
#> 
#> $b
#> [1] 2 3 4
#> 
```
