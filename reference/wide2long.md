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
#> 1   1  70        0     73     NA
#> 2   1  NA        1     77    176
#> 3   2  77        0     97     NA
#> 4   2  NA        1     75    180
#> 5   3  78        0     70     NA
#> 6   3  NA        1     99    181
#> 7   4  78        0     87     NA
#> 8   4  NA        1     74    198
#> 9   5  73        0     81     NA
#> 10  5  NA        1     98    198
#> 11  6  72        0     90     NA
#> 12  6  NA        1     73    178
#> 13  7  72        0     71     NA
#> 14  7  NA        1    100    179
#> 15  8  79        0     96     NA
#> 16  8  NA        1     74    177
#> 17  9  77        0     88     NA
#> 18  9  NA        1     98    189
#> 19 10  78        0     88     NA
#> 20 10  NA        1     85    181
#> 21 11  79        0     83     NA
#> 22 11  NA        1     97    197
#> 23 12  76        0     79     NA
#> 24 12  NA        1     94    186
#> 25 13  74        0     77     NA
#> 26 13  NA        1     77    200
#> 27 14  70        0     88     NA
#> 28 14  NA        1    100    181
#> 29 15  72        0     95     NA
#> 30 15  NA        1     73    175
#> 31 16  70        0     99     NA
#> 32 16  NA        1     83    185
#> 33 17  80        0     71     NA
#> 34 17  NA        1     84    174
#> 35 18  79        0     77     NA
#> 36 18  NA        1     94    184
#> 37 19  72        0     70     NA
#> 38 19  NA        1     93    195
#> 39 20  77        0     77     NA
#> 40 20  NA        1     77    187
data.frame(
  1:20, sample(70:80, 20, TRUE),
  sample(70:100, 20, TRUE),
  sample(70:100, 20, TRUE),
  sample(170:200, 20, TRUE)
) |>
  setNames(c("id", "age", "weight_0", "weight_a_1", "height_b_1")) |>
  wide2long(pattern = c("_0", "_1"), type = "suffix")
#>    id age instance weight weight_a height_b
#> 1   1  77        0     84       NA       NA
#> 2   1  NA        1     NA       78      171
#> 3   2  70        0     89       NA       NA
#> 4   2  NA        1     NA       72      182
#> 5   3  72        0     98       NA       NA
#> 6   3  NA        1     NA       93      170
#> 7   4  73        0    100       NA       NA
#> 8   4  NA        1     NA       98      171
#> 9   5  73        0     83       NA       NA
#> 10  5  NA        1     NA       91      173
#> 11  6  72        0     81       NA       NA
#> 12  6  NA        1     NA       81      170
#> 13  7  76        0     88       NA       NA
#> 14  7  NA        1     NA       75      174
#> 15  8  74        0     70       NA       NA
#> 16  8  NA        1     NA       82      173
#> 17  9  76        0     87       NA       NA
#> 18  9  NA        1     NA       84      176
#> 19 10  71        0     85       NA       NA
#> 20 10  NA        1     NA       70      189
#> 21 11  77        0     79       NA       NA
#> 22 11  NA        1     NA       83      185
#> 23 12  74        0     87       NA       NA
#> 24 12  NA        1     NA       77      187
#> 25 13  71        0     75       NA       NA
#> 26 13  NA        1     NA       89      173
#> 27 14  75        0     70       NA       NA
#> 28 14  NA        1     NA       84      178
#> 29 15  80        0     93       NA       NA
#> 30 15  NA        1     NA       87      182
#> 31 16  76        0     96       NA       NA
#> 32 16  NA        1     NA       72      187
#> 33 17  71        0     88       NA       NA
#> 34 17  NA        1     NA       80      173
#> 35 18  73        0     86       NA       NA
#> 36 18  NA        1     NA       91      184
#> 37 19  76        0     91       NA       NA
#> 38 19  NA        1     NA       89      189
#> 39 20  71        0     79       NA       NA
#> 40 20  NA        1     NA       84      197
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
