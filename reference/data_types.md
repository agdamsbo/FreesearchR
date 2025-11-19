# Recognised data types from data_type

Recognised data types from data_type

## Usage

``` r
data_types()
```

## Value

vector

## Examples

``` r
data_types()
#> $empty
#> $empty$descr
#> [1] "Variable of all NAs"
#> 
#> $empty$classes
#> [1] "Any class"
#> 
#> 
#> $monotone
#> $monotone$descr
#> [1] "Variable with only one unique value"
#> 
#> $monotone$classes
#> [1] "Any class"
#> 
#> 
#> $dichotomous
#> $dichotomous$descr
#> [1] "Variable with only two unique values"
#> 
#> $dichotomous$classes
#> [1] "Any class"
#> 
#> 
#> $categorical
#> $categorical$descr
#> [1] "Factor variable"
#> 
#> $categorical$classes
#> [1] "factor (ordered or unordered)"
#> 
#> 
#> $text
#> $text$descr
#> [1] "Character variable"
#> 
#> $text$classes
#> [1] "character"
#> 
#> 
#> $datetime
#> $datetime$descr
#> [1] "Variable of time, date or datetime values"
#> 
#> $datetime$classes
#> [1] "hms, Date, POSIXct and POSIXt"
#> 
#> 
#> $continuous
#> $continuous$descr
#> [1] "Numeric variable"
#> 
#> $continuous$classes
#> [1] "numeric, integer or double"
#> 
#> 
#> $unknown
#> $unknown$descr
#> [1] "Anything not falling within the previous"
#> 
#> $unknown$classes
#> [1] "Any other class"
#> 
#> 
```
