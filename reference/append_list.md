# Append list with named index

Append list with named index

## Usage

``` r
append_list(data, list, index)
```

## Arguments

- data:

  data to add to list

- list:

  list

- index:

  index name

## Value

list

## Examples

``` r
ls_d <- list(test = c(1:20))
ls_d <- list()
data.frame(letters[1:20], 1:20) |> append_list(ls_d, "letters")
#> $letters
#>    letters.1.20. X1.20
#> 1              a     1
#> 2              b     2
#> 3              c     3
#> 4              d     4
#> 5              e     5
#> 6              f     6
#> 7              g     7
#> 8              h     8
#> 9              i     9
#> 10             j    10
#> 11             k    11
#> 12             l    12
#> 13             m    13
#> 14             n    14
#> 15             o    15
#> 16             p    16
#> 17             q    17
#> 18             r    18
#> 19             s    19
#> 20             t    20
#> 
letters[1:20] |> append_list(ls_d, "letters")
#> $letters
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#> [20] "t"
#> 
```
