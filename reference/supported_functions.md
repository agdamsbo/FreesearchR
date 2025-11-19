# Implemented functions

Library of supported functions. The list name and "descr" element should
be unique for each element on list.

## Usage

``` r
supported_functions()
```

## Value

list

## Examples

``` r
supported_functions()
#> $lm
#> $lm$descr
#> [1] "Linear regression model"
#> 
#> $lm$design
#> [1] "cross-sectional"
#> 
#> $lm$out.type
#> [1] "continuous"
#> 
#> $lm$fun
#> [1] "stats::lm"
#> 
#> $lm$args.list
#> NULL
#> 
#> $lm$formula.str
#> [1] "{outcome.str}~{paste(vars,collapse='+')}"
#> 
#> $lm$table.fun
#> [1] "gtsummary::tbl_regression"
#> 
#> $lm$table.args.list
#> $lm$table.args.list$exponentiate
#> [1] FALSE
#> 
#> 
#> 
#> $glm
#> $glm$descr
#> [1] "Logistic regression model"
#> 
#> $glm$design
#> [1] "cross-sectional"
#> 
#> $glm$out.type
#> [1] "dichotomous"
#> 
#> $glm$fun
#> [1] "stats::glm"
#> 
#> $glm$args.list
#> $glm$args.list$family
#> [1] "binomial"
#> 
#> 
#> $glm$formula.str
#> [1] "{outcome.str}~{paste(vars,collapse='+')}"
#> 
#> $glm$table.fun
#> [1] "gtsummary::tbl_regression"
#> 
#> $glm$table.args.list
#> list()
#> 
#> 
#> $polr
#> $polr$descr
#> [1] "Ordinal logistic regression model"
#> 
#> $polr$design
#> [1] "cross-sectional"
#> 
#> $polr$out.type
#> [1] "categorical"
#> 
#> $polr$fun
#> [1] "MASS::polr"
#> 
#> $polr$args.list
#> $polr$args.list$Hess
#> [1] TRUE
#> 
#> $polr$args.list$method
#> [1] "logistic"
#> 
#> 
#> $polr$formula.str
#> [1] "{outcome.str}~{paste(vars,collapse='+')}"
#> 
#> $polr$table.fun
#> [1] "gtsummary::tbl_regression"
#> 
#> $polr$table.args.list
#> list()
#> 
#> 
```
