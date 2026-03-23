# Resolve the current app version

Tries two strategies in order:

1.  `utils::packageVersion(package_name)` – works when the package is
    installed locally (development, local
    [`runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html)).

2.  `app_version` argument – an explicit version string supplied by the
    caller, e.g. from an `app_version()` function bundled with the app.
    Used on shinyapps.io where the package is not installed.

## Usage

``` r
.resolve_app_version(package_name, app_version = NULL)
```

## Arguments

- package_name:

  Name of the package / repository.

- app_version:

  Optional fallback version string.

## Value

A character string with the version (e.g. "1.1.0"), or NULL if neither
strategy succeeds.
