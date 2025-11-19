# Print a flexible baseline characteristics table

Print a flexible baseline characteristics table

## Usage

``` r
baseline_table(
  data,
  fun.args = NULL,
  fun = gtsummary::tbl_summary,
  vars = NULL
)
```

## Arguments

- data:

  data set

- fun.args:

  list of arguments passed to

- fun:

  function to

- vars:

  character vector of variables to include

## Value

object of standard class for fun

## Examples

``` r
mtcars |> baseline_table()


  

Characteristic
```

**N = 32**¹

mpg

19.2 (15.4, 22.8)

cyl

  

    4

11 (34%)

    6

7 (22%)

    8

14 (44%)

disp

196 (121, 334)

hp

123 (96, 180)

drat

3.70 (3.08, 3.92)

wt

3.33 (2.54, 3.65)

qsec

17.71 (16.89, 18.90)

vs

14 (44%)

am

13 (41%)

gear

  

    3

15 (47%)

    4

12 (38%)

    5

5 (16%)

carb

  

    1

7 (22%)

    2

10 (31%)

    3

3 (9.4%)

    4

10 (31%)

    6

1 (3.1%)

    8

1 (3.1%)

¹ Median (Q1, Q3); n (%)

mtcars \|\> baseline_table(fun.args =
[list](https://rdrr.io/r/base/list.html)(by = "gear"))

[TABLE]
