# Test if url is valid format for REDCap API

Test if url is valid format for REDCap API

## Usage

``` r
is_valid_redcap_url(url)
```

## Arguments

- url:

  url

## Value

logical

## Examples

``` r
url <- c(
  "www.example.com",
  "redcap.your.inst/api/",
  "https://redcap.your.inst/api/",
  "https://your.inst/redcap/api/",
  "https://www.your.inst/redcap/api/"
)
is_valid_redcap_url(url)
#> [1] FALSE FALSE  TRUE  TRUE  TRUE
```
