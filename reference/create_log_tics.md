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
#>  [1]  0.10  0.12  0.20  0.25  0.50  1.00  2.00  4.00  5.00  8.00 10.00
```
