# Limit the allowed data set size by number of cells

This function may act to guard a hosted app against very large data sets
in addition to the file size limitations. The function will limit the
data set by dropping rows. If limit is set to 0 or NULL, the original
data set is returned.

## Usage

``` r
limit_data_size(data, limit = NULL)
```

## Arguments

- data:

  data.frame

- limit:

  cell number limit. Default is NULL.

## Value

data.frame

## Examples

``` r
prod(dim(mtcars))
#> [1] 352
limit_data_size(mtcars,2)
#>           mpg cyl
#> Mazda RX4  21   6
limit_data_size(mtcars,100)
#>                    mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360        14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D         24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230          22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
```
