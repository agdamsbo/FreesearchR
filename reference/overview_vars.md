# Create a data overview data.frame ready for sparklines

Create a data overview data.frame ready for sparklines

## Usage

``` r
overview_vars(data)
```

## Arguments

- data:

  data

## Value

data.frame

## Examples

``` r
mtcars |> overview_vars()
#> # A tibble: 11 × 7
#>    icon    class   name  n_missing p_complete n_unique vals        
#>    <chr>   <chr>   <chr>     <dbl>      <dbl>    <int> <named list>
#>  1 numeric numeric mpg           0          1       25 <dbl [32]>  
#>  2 numeric numeric cyl           0          1        3 <dbl [32]>  
#>  3 numeric numeric disp          0          1       27 <dbl [32]>  
#>  4 numeric numeric hp            0          1       22 <dbl [32]>  
#>  5 numeric numeric drat          0          1       22 <dbl [32]>  
#>  6 numeric numeric wt            0          1       29 <dbl [32]>  
#>  7 numeric numeric qsec          0          1       30 <dbl [32]>  
#>  8 numeric numeric vs            0          1        2 <dbl [32]>  
#>  9 numeric numeric am            0          1        2 <dbl [32]>  
#> 10 numeric numeric gear          0          1        3 <dbl [32]>  
#> 11 numeric numeric carb          0          1        6 <dbl [32]>  
```
