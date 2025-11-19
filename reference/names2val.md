# Set values as names and names as values

Set values as names and names as values

## Usage

``` r
names2val(data)
```

## Arguments

- data:

  data

## Value

named vector

## Examples

``` r
names2val(c("Cylinders" = "cyl", "Transmission" = "am", "Gears" = "gear"))
#>            cyl             am           gear 
#>    "Cylinders" "Transmission"        "Gears" 
```
