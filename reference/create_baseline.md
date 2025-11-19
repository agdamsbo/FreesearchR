# Create a baseline table

Create a baseline table

## Usage

``` r
create_baseline(
  data,
  ...,
  by.var,
  add.p = FALSE,
  add.overall = FALSE,
  theme = c("jama", "lancet", "nejm", "qjecon"),
  detail_level = c("minimal", "extended")
)
```

## Arguments

- data:

  data

- ...:

  passed as fun.arg to baseline_table()

- by.var:

  specify stratification variable

- add.p:

  add comparison/p-value

- add.overall:

  add overall column

- theme:

  set table theme

- detail_level:

  specify detail level. Either "minimal" or "extended".

## Value

gtsummary table list object

## Examples

``` r
mtcars |> create_baseline(by.var = "gear", add.p = "yes" == "yes")


  

Characteristic
```

**3**  
N = 15

**4**  
N = 12

**5**  
N = 5

**p-value**¹

mpg, Median (IQR)

15.5 (14.3 – 18.7)

22.8 (21.0 – 28.9)

19.7 (15.8 – 26.0)

\<0.001

cyl, n (%)

  

  

  

\<0.001

    4

1 (6.7)

8 (67)

2 (40)

  

    6

2 (13)

4 (33)

1 (20)

  

    8

12 (80)

0 (0)

2 (40)

  

disp, Median (IQR)

318 (276 – 400)

131 (79 – 160)

145 (120 – 301)

\<0.001

hp, Median (IQR)

180 (150 – 215)

94 (66 – 110)

175 (113 – 264)

\<0.001

drat, Median (IQR)

3.08 (3.00 – 3.21)

3.92 (3.90 – 4.10)

3.77 (3.62 – 4.22)

\<0.001

wt, Median (IQR)

3.73 (3.44 – 4.07)

2.70 (2.07 – 3.17)

2.77 (2.14 – 3.17)

\<0.001

qsec, Median (IQR)

17.42 (17.02 – 18.00)

18.76 (18.41 – 19.69)

15.50 (14.60 – 16.70)

0.002

vs, n (%)

3 (20)

10 (83)

1 (20)

0.001

am, n (%)

0 (0)

8 (67)

5 (100)

\<0.001

carb, n (%)

  

  

  

0.24

    1

3 (20)

4 (33)

0 (0)

  

    2

4 (27)

4 (33)

2 (40)

  

    3

3 (20)

0 (0)

0 (0)

  

    4

5 (33)

4 (33)

1 (20)

  

    6

0 (0)

0 (0)

1 (20)

  

    8

0 (0)

0 (0)

1 (20)

  

¹ Kruskal-Wallis rank sum test; Fisher’s exact test

mtcars \|\> create_baseline(by.var = "gear", detail_level = "extended")

[TABLE]

mtcars \|\> create_baseline(by.var = "gear", detail_level =
"extended",type =
[list](https://rdrr.io/r/base/list.html)(gtsummary::[all_dichotomous](https://www.danieldsjoberg.com/gtsummary/reference/select_helpers.html)()
~ "categorical"),theme="nejm")

[TABLE]

create_baseline([default_parsing](https://agdamsbo.github.io/FreesearchR/reference/default_parsing.md)(mtcars),
by.var = "am", add.p = FALSE, add.overall = FALSE, theme = "lancet")

[TABLE]
