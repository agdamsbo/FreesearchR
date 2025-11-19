# Simplified version of the snakecase packages to_snake_case

Simplified version of the snakecase packages to_snake_case

## Usage

``` r
simple_snake(data)
```

## Arguments

- data:

  character string vector

## Value

vector

## Examples

``` r
c("foo bar", "fooBar21", "!!Foo'B'a-r", "foo_bar", "F  OO bar") |> simple_snake()
#> [1] "foo_bar"   "foobar21"  "fooba-r"   "foo_bar"   "f__oo_bar"
```
