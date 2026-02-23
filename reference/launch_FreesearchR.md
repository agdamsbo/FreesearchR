# Easily launch the FreesearchR app

All data.frames in the global environment will be accessible through the
app.

## Usage

``` r
launch_FreesearchR(
  inlcude_globalenv = TRUE,
  data_limit_default = 1000,
  data_limit_upper = 1e+05,
  data_limit_lower = 1,
  ...
)
```

## Arguments

- data_limit_default:

  default data set observations limit

- data_limit_upper:

  data set observations upper limit

- data_limit_lower:

  data set observations lower limit

- ...:

  passed on to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)

- include_globalenv:

  flag to include global env (local data) as option when loading data

## Value

shiny app

## Examples

``` r
if (FALSE) { # \dontrun{
data(mtcars)
launch_FreesearchR(launch.browser = TRUE)
} # }
```
