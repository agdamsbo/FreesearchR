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
#>  [1]  0.10  0.20  0.30  0.50  0.90  1.00  1.11  2.00  3.33  5.00 10.00
```
