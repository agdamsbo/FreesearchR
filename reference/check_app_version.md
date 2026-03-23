# Run a startup version check and return a banner UI element

Call this **outside** `server()` – typically in `global.R` or at the top
of `app.R` – and embed the returned value directly in your UI
definition. Because the check runs at startup the banner is present on
first render with no loading delay, and no `uiOutput()` / `renderUI()`
wiring is needed.

## Usage

``` r
check_app_version(
  github_user,
  github_repo,
  app_version = NULL,
  verbose = FALSE
)
```

## Arguments

- github_user:

  GitHub username or organisation that owns the repository.

- github_repo:

  Repository name. Also used as the package name for
  [`utils::packageVersion()`](https://rdrr.io/r/utils/packageDescription.html).

- app_version:

  Optional fallback version string for environments where the package is
  not installed (e.g. shinyapps.io). Pass the result of your
  `app_version()` function here. Ignored when
  [`packageVersion()`](https://rdrr.io/r/utils/packageDescription.html)
  succeeds.

- verbose:

  Logical; if `TRUE` a banner is always returned. Defaults to `FALSE`.

## Value

A
[`shinyWidgets::alert()`](https://dreamrs.github.io/shinyWidgets/reference/bootstrap-utils.html)
UI element, or `NULL` when there is nothing to show (up to date in
non-verbose mode).

## Details

**Normal mode** (`verbose = FALSE`): returns a banner only when a newer
version is available or when the check fails. Returns `NULL` when the
app is up to date (Shiny silently ignores `NULL` in the UI).

**Verbose / debug mode** (`verbose = TRUE`): always returns a banner –
including a success banner when up to date – so you can confirm the
check ran and inspect both version strings during development.

## Examples

``` r
if (FALSE) { # \dontrun{
# global.R or top of app.R
source("version_check.R")
version_banner <- check_app_version(
  github_user = "my-org",
  github_repo = "my-shiny-app",
  app_version = app_version()   # fallback for shinyapps.io
)

# ui.R
fluidPage(
  version_banner,
  # ... rest of UI
)

# Verbose mode for development
version_banner <- check_app_version(
  github_user = "my-org",
  github_repo = "my-shiny-app",
  app_version = app_version(),
  verbose     = TRUE
)
} # }
```
