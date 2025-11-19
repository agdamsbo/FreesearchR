# Easily launch the FreesearchR app

All data.frames in the global environment will be accessible through the
app.

## Usage

``` r
launch_FreesearchR(...)
```

## Arguments

- ...:

  passed on to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)

## Value

shiny app

## Examples

``` r
if (FALSE) { # \dontrun{
data(mtcars)
launch_FreesearchR(launch.browser = TRUE)
} # }
```
