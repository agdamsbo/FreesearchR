# Extended cutting function with fall-back to the native base::cut

Extended cutting function with fall-back to the native base::cut

Simplify a factor to only the top or bottom n levels

Subset first part of string to factor

## Usage

``` r
cut_var(x, ...)

# Default S3 method
cut_var(x, ...)

# S3 method for class 'hms'
cut_var(x, breaks, ...)

# S3 method for class 'POSIXt'
cut_var(
  x,
  breaks,
  right = FALSE,
  include.lowest = TRUE,
  start.on.monday = TRUE,
  ...
)

# S3 method for class 'POSIXct'
cut_var(
  x,
  breaks,
  right = FALSE,
  include.lowest = TRUE,
  start.on.monday = TRUE,
  ...
)

# S3 method for class 'Date'
cut_var(x, breaks = NULL, start.on.monday = TRUE, ...)

# S3 method for class 'factor'
cut_var(x, breaks = NULL, type = c("top", "bottom"), other = "Other", ...)

# S3 method for class 'character'
cut_var(x, breaks = NULL, type = c("characters", "words"), ...)
```

## Arguments

- x:

  an object inheriting from class "POSIXct"

- ...:

  passed on

- type:

## Value

factor

factor

factor

## Examples

``` r
readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var(2)
#> [1] 01:00:20 01:00:20 01:00:20 03:01:20 <NA>     03:01:20
#> attr(,"brks")
#> 01:00:20
#> 03:01:20
#> 21:20:20
#> Levels: 01:00:20 03:01:20
readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var("min")
#> [1] 01:00:00 03:00:00 01:20:00 08:20:00 21:20:00 03:02:00
#> attr(,"brks")
#> 01:00:00
#> 01:20:00
#> 03:00:00
#> 03:02:00
#> 08:20:00
#> 21:20:00
#> 21:20:20
#> Levels: 01:00:00 01:20:00 03:00:00 03:02:00 08:20:00 21:20:00
readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var(breaks = "hour")
#> [1] 01:00:00 03:00:00 01:00:00 08:00:00 21:00:00 03:00:00
#> attr(,"brks")
#> 01:00:00
#> 03:00:00
#> 08:00:00
#> 21:00:00
#> 21:20:20
#> Levels: 01:00:00 03:00:00 08:00:00 21:00:00
readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "08:20:20", "21:20:20", "03:02:20")) |> cut_var(breaks = hms::as_hms(c("01:00:00", "03:01:20", "9:20:20")))
#> [1] 01:00:00 01:00:00 01:00:00 03:01:20 <NA>     03:01:20
#> attr(,"brks")
#> 01:00:00
#> 03:01:20
#> 09:20:20
#> Levels: 01:00:00 03:01:20
d_t <- readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA))
f <- d_t |> cut_var(2)
readr::parse_time(c("01:00:20", "03:00:20", "01:20:20", "03:02:20", NA)) |> cut_var(breaks = lubridate::as_datetime(c(hms::as_hms(levels(f)), hms::as_hms(max(d_t, na.rm = TRUE) + 1))), right = FALSE)
#> [1] 01:00:20 02:10:20 01:00:20 02:10:20 <NA>    
#> attr(,"brks")
#> 01:00:20
#> 02:10:20
#> 03:02:21
#> Levels: 01:00:20 02:10:20
readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(2)
#> [1] 1992-02-01 01:00:20 1992-02-01 01:00:20 1992-02-01 01:00:20
#> [4] 1992-02-01 01:00:20 1999-02-01 21:20:20 1992-02-01 01:00:20
#> Levels: 1992-02-01 01:00:20 1999-02-01 21:20:20
readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = "weekday")
#> Error in cut.POSIXt(x, ...): invalid specification of 'breaks'
readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = "month_only")
#> Error in cut.POSIXt(x, ...): invalid specification of 'breaks'
readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = NULL, format = "%A-%H")
#> Error in cut.POSIXt(x, ...): invalid specification of 'breaks'
readr::parse_datetime(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = NULL, format = "%W")
#> Error in cut.POSIXt(x, ...): invalid specification of 'breaks'
as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(2)
#> [1] 1992-02-01 1992-02-01 1992-02-01 1992-02-01 1999-02-01 1992-02-01
#> Levels: 1992-02-01 1999-02-01
as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(breaks = "weekday")
#> Error in cut.Date(x, ...): invalid specification of 'breaks'
as.Date(c("1992-02-01 01:00:20", "1992-02-06 03:00:20", "1992-05-01 01:20:20", "1992-09-01 08:20:20", "1999-02-01 21:20:20", "1992-12-01 03:02:20")) |> cut_var(format = "%W")
#> Error in cut.Date(x, ...): argument "breaks" is missing, with no default
mtcars$carb |>
  as.factor() |>
  cut_var(2) |>
  table()
#> 
#>     2     4 Other 
#>    10    10    12 

mtcars$carb |>
  as.factor() |>
  cut_var(20, "bottom") |>
  table()
#> 
#>     2     4     1 Other 
#>    10    10     7     5 
c("Sunday", "This week is short") |> cut_var(breaks = 3)
#> [1] Sun Thi
#> attr(,"brks")
#> [1] 3
#> Levels: Sun Thi
```
