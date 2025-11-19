# Create unique short names of character vector items based on index

The function will prefer original names, and only append index to long
strings.

## Usage

``` r
unique_short(data, max = 15)
```

## Arguments

- data:

  character vector

- max:

  maximum final name length

## Value

character vector

## Examples

``` r
c("kahdleidnsallskdj", "hej") |> unique_short()
#> [1] "kahdleidnsall_1" "hej"            
```
