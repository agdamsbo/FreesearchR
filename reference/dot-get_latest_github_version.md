# Fetch the latest release version from a GitHub repository

Fetch the latest release version from a GitHub repository

## Usage

``` r
.get_latest_github_version(github_user, github_repo)
```

## Arguments

- github_user:

  GitHub username or organisation.

- github_repo:

  Repository name.

## Value

A character string with the version tag (e.g. "1.2.0"), or NULL on
failure.
