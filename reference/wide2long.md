# Alternative pivoting method for easily pivoting based on name pattern

This function requires and assumes a systematic naming of variables. For
now only supports one level pivoting. Adding more levels would require
an added "ignore" string pattern or similarly. Example 2.

## Usage

``` r
wide2long(
  data,
  pattern,
  type = c("prefix", "infix", "suffix"),
  id.col = 1,
  instance.name = "instance"
)
```

## Arguments

- data:

  data

- pattern:

  pattern(s) to match. Character vector of length 1 or more.

- type:

  type of match. can be one of "prefix","infix" or "suffix".

- id.col:

  ID column. Will fill ID for all. Column name or numeric index. Default
  is "1", first column.

- instance.name:

## Value

data.frame

## Examples

``` r
data.frame(
  1:20, sample(70:80, 20, TRUE),
  sample(70:100, 20, TRUE),
  sample(70:100, 20, TRUE),
  sample(170:200, 20, TRUE)
) |>
  setNames(c("id", "age", "weight_0", "weight_1", "height_1")) |>
  wide2long(pattern = c("_0", "_1"), type = "suffix")
#>    id age instance weight height
#> 1   1  77        0     84     NA
#> 2   1  NA        1     85    170
#> 3   2  75        0     95     NA
#> 4   2  NA        1     98    193
#> 5   3  74        0     87     NA
#> 6   3  NA        1     92    196
#> 7   4  73        0     77     NA
#> 8   4  NA        1     71    188
#> 9   5  74        0     70     NA
#> 10  5  NA        1     73    186
#> 11  6  78        0     72     NA
#> 12  6  NA        1     76    191
#> 13  7  77        0     89     NA
#> 14  7  NA        1     87    179
#> 15  8  73        0     84     NA
#> 16  8  NA        1     84    178
#> 17  9  78        0     73     NA
#> 18  9  NA        1     89    172
#> 19 10  77        0     72     NA
#> 20 10  NA        1     98    193
#> 21 11  77        0     81     NA
#> 22 11  NA        1    100    198
#> 23 12  76        0     76     NA
#> 24 12  NA        1     83    191
#> 25 13  80        0     74     NA
#> 26 13  NA        1     81    181
#> 27 14  78        0     92     NA
#> 28 14  NA        1     88    175
#> 29 15  79        0     87     NA
#> 30 15  NA        1     70    182
#> 31 16  77        0     77     NA
#> 32 16  NA        1     87    184
#> 33 17  73        0     90     NA
#> 34 17  NA        1     85    170
#> 35 18  70        0     71     NA
#> 36 18  NA        1     79    183
#> 37 19  75        0     91     NA
#> 38 19  NA        1     87    177
#> 39 20  74        0     96     NA
#> 40 20  NA        1     75    189
data.frame(
  1:20, sample(70:80, 20, TRUE),
  sample(70:100, 20, TRUE),
  sample(70:100, 20, TRUE),
  sample(170:200, 20, TRUE)
) |>
  setNames(c("id", "age", "weight_0", "weight_a_1", "height_b_1")) |>
  wide2long(pattern = c("_0", "_1"), type = "suffix")
#>    id age instance weight weight_a height_b
#> 1   1  71        0     97       NA       NA
#> 2   1  NA        1     NA       71      178
#> 3   2  72        0     96       NA       NA
#> 4   2  NA        1     NA       73      181
#> 5   3  80        0     83       NA       NA
#> 6   3  NA        1     NA       99      182
#> 7   4  75        0     81       NA       NA
#> 8   4  NA        1     NA       85      193
#> 9   5  73        0     80       NA       NA
#> 10  5  NA        1     NA       97      176
#> 11  6  71        0     91       NA       NA
#> 12  6  NA        1     NA       83      181
#> 13  7  70        0     79       NA       NA
#> 14  7  NA        1     NA       71      197
#> 15  8  71        0     98       NA       NA
#> 16  8  NA        1     NA       83      197
#> 17  9  73        0     96       NA       NA
#> 18  9  NA        1     NA       82      171
#> 19 10  70        0     89       NA       NA
#> 20 10  NA        1     NA       75      194
#> 21 11  74        0     88       NA       NA
#> 22 11  NA        1     NA       93      192
#> 23 12  73        0     87       NA       NA
#> 24 12  NA        1     NA       84      194
#> 25 13  76        0     90       NA       NA
#> 26 13  NA        1     NA       90      174
#> 27 14  73        0     77       NA       NA
#> 28 14  NA        1     NA       74      198
#> 29 15  71        0     99       NA       NA
#> 30 15  NA        1     NA       76      172
#> 31 16  73        0    100       NA       NA
#> 32 16  NA        1     NA       86      179
#> 33 17  78        0     72       NA       NA
#> 34 17  NA        1     NA       81      182
#> 35 18  71        0     90       NA       NA
#> 36 18  NA        1     NA       86      179
#> 37 19  73        0     91       NA       NA
#> 38 19  NA        1     NA       83      196
#> 39 20  73        0     71       NA       NA
#> 40 20  NA        1     NA       80      172
# Optional filling of missing values by last observation carried forward
# Needed for mmrm analyses
long_missings |>
  # Fills record ID assuming none are missing
  tidyr::fill(record_id) |>
  # Grouping by ID for the last step
  dplyr::group_by(record_id) |>
  # Filling missing data by ID
  tidyr::fill(names(long_missings)[!names(long_missings) %in% new_names]) |>
  # Remove grouping
  dplyr::ungroup()
#> Error: object 'long_missings' not found
```
