# Build a shinyWidgets::alert() UI element for the version banner

Build a shinyWidgets::alert() UI element for the version banner

## Usage

``` r
.build_version_alert(
  current,
  latest,
  update_available,
  github_user,
  github_repo
)
```

## Arguments

- current:

  Current installed version string.

- latest:

  Latest GitHub release version string, or NULL when the check could not
  complete (e.g. no internet).

- update_available:

  Logical; whether latest \> current.

- github_user:

  GitHub username / organisation.

- github_repo:

  Repository name.

## Value

A
[`shinyWidgets::alert()`](https://dreamrs.github.io/shinyWidgets/reference/bootstrap-utils.html)
UI element.
