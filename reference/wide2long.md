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
#> 1   1  70        0    100     NA
#> 2   1  NA        1     77    187
#> 3   2  77        0     81     NA
#> 4   2  NA        1     90    185
#> 5   3  77        0     75     NA
#> 6   3  NA        1     71    179
#> 7   4  75        0     85     NA
#> 8   4  NA        1     91    187
#> 9   5  74        0     74     NA
#> 10  5  NA        1     96    175
#> 11  6  73        0     84     NA
#> 12  6  NA        1     85    170
#> 13  7  74        0     95     NA
#> 14  7  NA        1     98    193
#> 15  8  78        0     87     NA
#> 16  8  NA        1     92    196
#> 17  9  77        0     77     NA
#> 18  9  NA        1     71    188
#> 19 10  73        0     70     NA
#> 20 10  NA        1     73    186
#> 21 11  78        0     72     NA
#> 22 11  NA        1     76    191
#> 23 12  77        0     89     NA
#> 24 12  NA        1     87    179
#> 25 13  77        0     84     NA
#> 26 13  NA        1     84    178
#> 27 14  76        0     73     NA
#> 28 14  NA        1     89    172
#> 29 15  80        0     72     NA
#> 30 15  NA        1     98    193
#> 31 16  78        0     81     NA
#> 32 16  NA        1    100    198
#> 33 17  79        0     76     NA
#> 34 17  NA        1     83    191
#> 35 18  77        0     74     NA
#> 36 18  NA        1     81    181
#> 37 19  73        0     92     NA
#> 38 19  NA        1     88    175
#> 39 20  70        0     87     NA
#> 40 20  NA        1     70    182
data.frame(
  1:20, sample(70:80, 20, TRUE),
  sample(70:100, 20, TRUE),
  sample(70:100, 20, TRUE),
  sample(170:200, 20, TRUE)
) |>
  setNames(c("id", "age", "weight_0", "weight_a_1", "height_b_1")) |>
  wide2long(pattern = c("_0", "_1"), type = "suffix")
#>    id age instance weight weight_a height_b
#> 1   1  70        0     82       NA       NA
#> 2   1  NA        1     NA      100      186
#> 3   2  77        0     87       NA       NA
#> 4   2  NA        1     NA       72      181
#> 5   3  73        0     73       NA       NA
#> 6   3  NA        1     NA       90      186
#> 7   4  71        0     84       NA       NA
#> 8   4  NA        1     NA       91      183
#> 9   5  72        0     89       NA       NA
#> 10  5  NA        1     NA       71      180
#> 11  6  80        0     97       NA       NA
#> 12  6  NA        1     NA       71      178
#> 13  7  75        0     96       NA       NA
#> 14  7  NA        1     NA       73      181
#> 15  8  73        0     83       NA       NA
#> 16  8  NA        1     NA       99      182
#> 17  9  71        0     81       NA       NA
#> 18  9  NA        1     NA       85      193
#> 19 10  70        0     80       NA       NA
#> 20 10  NA        1     NA       97      176
#> 21 11  71        0     91       NA       NA
#> 22 11  NA        1     NA       83      181
#> 23 12  73        0     79       NA       NA
#> 24 12  NA        1     NA       71      197
#> 25 13  70        0     98       NA       NA
#> 26 13  NA        1     NA       83      197
#> 27 14  74        0     96       NA       NA
#> 28 14  NA        1     NA       82      171
#> 29 15  73        0     89       NA       NA
#> 30 15  NA        1     NA       75      194
#> 31 16  76        0     88       NA       NA
#> 32 16  NA        1     NA       93      192
#> 33 17  73        0     87       NA       NA
#> 34 17  NA        1     NA       84      194
#> 35 18  71        0     90       NA       NA
#> 36 18  NA        1     NA       90      174
#> 37 19  73        0     77       NA       NA
#> 38 19  NA        1     NA       74      198
#> 39 20  78        0     99       NA       NA
#> 40 20  NA        1     NA       76      172
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
