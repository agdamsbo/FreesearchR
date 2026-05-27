# Print a flexible baseline characteristics table

Print a flexible baseline characteristics table

## Usage

``` r
baseline_table(
  data,
  fun.args = NULL,
  fun = gtsummary::tbl_summary,
  vars = NULL
)
```

## Arguments

- data:

  data set

- fun.args:

  list of arguments passed to

- fun:

  function to

- vars:

  character vector of variables to include

## Value

object of standard class for fun

## Examples

``` r
mtcars |> baseline_table()


  

Characteristic
```
