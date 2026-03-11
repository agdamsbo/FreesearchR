# Create summetric log ticks

Create summetric log ticks

## Usage

``` r
create_log_tics(data)
```

## Arguments

- data:

  numeric vector

## Value

numeric vector

## Examples

``` r
c(sample(seq(.1, 1, .1), 3), sample(1:10, 3)) |> create_log_tics()
#>  [1] 0.12 0.30 0.33 0.60 0.70 1.00 1.43 1.67 3.00 3.33 8.00
```
