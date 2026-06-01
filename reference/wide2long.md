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
#> 1   1  74        0     98     NA
#> 2   1  NA        1     91    173
#> 3   2  78        0     77     NA
#> 4   2  NA        1     91    189
#> 5   3  78        0     80     NA
#> 6   3  NA        1     73    184
#> 7   4  70        0     88     NA
#> 8   4  NA        1     95    199
#> 9   5  72        0     85     NA
#> 10  5  NA        1     83    179
#> 11  6  73        0     90     NA
#> 12  6  NA        1     75    192
#> 13  7  71        0     85     NA
#> 14  7  NA        1     78    195
#> 15  8  78        0     83     NA
#> 16  8  NA        1     99    198
#> 17  9  80        0     85     NA
#> 18  9  NA        1     70    175
#> 19 10  77        0     74     NA
#> 20 10  NA        1     79    186
#> 21 11  76        0     92     NA
#> 22 11  NA        1     80    193
#> 23 12  73        0     71     NA
#> 24 12  NA        1     84    194
#> 25 13  77        0     94     NA
#> 26 13  NA        1     72    198
#> 27 14  75        0     96     NA
#> 28 14  NA        1     89    183
#> 29 15  79        0     77     NA
#> 30 15  NA        1     94    178
#> 31 16  75        0     99     NA
#> 32 16  NA        1     73    189
#> 33 17  73        0     99     NA
#> 34 17  NA        1     75    185
#> 35 18  72        0     70     NA
#> 36 18  NA        1     83    172
#> 37 19  80        0     70     NA
#> 38 19  NA        1     79    172
#> 39 20  74        0     89     NA
#> 40 20  NA        1     97    179
data.frame(
  1:20, sample(70:80, 20, TRUE),
  sample(70:100, 20, TRUE),
  sample(70:100, 20, TRUE),
  sample(170:200, 20, TRUE)
) |>
  setNames(c("id", "age", "weight_0", "weight_a_1", "height_b_1")) |>
  wide2long(pattern = c("_0", "_1"), type = "suffix")
#>    id age instance weight weight_a height_b
#> 1   1  77        0     83       NA       NA
#> 2   1  NA        1     NA       97      197
#> 3   2  78        0     79       NA       NA
#> 4   2  NA        1     NA       94      186
#> 5   3  79        0     77       NA       NA
#> 6   3  NA        1     NA       77      200
#> 7   4  76        0     88       NA       NA
#> 8   4  NA        1     NA      100      181
#> 9   5  74        0     95       NA       NA
#> 10  5  NA        1     NA       73      175
#> 11  6  70        0     99       NA       NA
#> 12  6  NA        1     NA       83      185
#> 13  7  72        0     71       NA       NA
#> 14  7  NA        1     NA       84      174
#> 15  8  70        0     77       NA       NA
#> 16  8  NA        1     NA       94      184
#> 17  9  80        0     70       NA       NA
#> 18  9  NA        1     NA       93      195
#> 19 10  79        0     77       NA       NA
#> 20 10  NA        1     NA       77      187
#> 21 11  72        0     77       NA       NA
#> 22 11  NA        1     NA       76      177
#> 23 12  77        0     75       NA       NA
#> 24 12  NA        1     NA       80      170
#> 25 13  73        0     99       NA       NA
#> 26 13  NA        1     NA       81      172
#> 27 14  70        0     74       NA       NA
#> 28 14  NA        1     NA       98      189
#> 29 15  71        0     98       NA       NA
#> 30 15  NA        1     NA       98      184
#> 31 16  74        0     73       NA       NA
#> 32 16  NA        1     NA       78      173
#> 33 17  71        0    100       NA       NA
#> 34 17  NA        1     NA       79      172
#> 35 18  80        0     74       NA       NA
#> 36 18  NA        1     NA       77      181
#> 37 19  72        0     98       NA       NA
#> 38 19  NA        1     NA       89      176
#> 39 20  72        0     85       NA       NA
#> 40 20  NA        1     NA       81      174
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
