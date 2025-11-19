# Filter function to filter data set by variable type

Filter function to filter data set by variable type

## Usage

``` r
data_type_filter(data, type)
```

## Arguments

- data:

  data frame

- type:

  vector of data types (recognised: data_types)

## Value

data.frame

## Examples

``` r
default_parsing(mtcars) |>
  data_type_filter(type = c("categorical", "continuous")) |>
  attributes()
#> $names
#> [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "gear" "carb"
#> 
#> $row.names
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32
#> 
#> $class
#> [1] "tbl_df"     "tbl"        "data.frame"
#> 
#> $code
#> FreesearchR::data_type_filter(type = c("categorical", "continuous"
#> ))
#> 
default_parsing(mtcars) |>
  data_type_filter(type = NULL) |>
  attributes()
#> $class
#> [1] "tbl_df"     "tbl"        "data.frame"
#> 
#> $row.names
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32
#> 
#> $names
#>  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
#> [11] "carb"
#> 
if (FALSE) { # \dontrun{
default_parsing(mtcars) |> data_type_filter(type = c("test", "categorical", "continuous"))
} # }
```
