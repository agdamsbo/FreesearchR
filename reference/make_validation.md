# Create validation data.frame

Create validation data.frame

## Usage

``` r
make_validation(ls, ...)
```

## Arguments

- ls:

  validation list

- ...:

  magic dots

## Value

data.frame

## Examples

``` r
i18n <- shiny.i18n::Translator$new(translation_csvs_path = here::here("inst/translations"))
#> Warning: You didn't specify config translation yaml file. Default settings are used.
i18n$set_translation_language("en")
df_original <- mtcars
df_original[1, 2:4] <- NA
df_obs <- df_original |> dplyr::filter(carb == 4)
df_vars <- df_original[1:7]
val <- purrr::map2(
  .x = validation_lib(),
  .y = list(
    list(x = df_original, y = df_obs),
    list(x = df_original, y = df_vars),
    list(x = df_original)
  ),
  make_validation
)
#> Error in validation_lib(): could not find function "validation_lib"
val |> make_validation_alerts()
#> Error: object 'val' not found

val2 <- purrr::map2(
  .x = validation_lib()[2],
  .y = list(list(x = mtcars, y = mtcars[0])),
  make_validation
)
#> Error in validation_lib(): could not find function "validation_lib"
val2 |> make_validation_alerts()
#> Error: object 'val2' not found

val3 <- make_validation(
  ls = validation_lib()[[2]],
  list(x = mtcars, y = mtcars[0])
)
#> Error in validation_lib(): could not find function "validation_lib"
```
