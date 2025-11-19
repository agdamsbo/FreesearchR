# Create a regression model programatically

Output is a concatenated list of model information and model

## Usage

``` r
regression_model(
  data,
  outcome.str = NULL,
  auto.mode = FALSE,
  formula.str = NULL,
  args.list = NULL,
  fun = NULL,
  vars = NULL,
  ...
)

regression_model_uv(
  data,
  outcome.str,
  args.list = NULL,
  fun = NULL,
  vars = NULL,
  ...
)

regression_model_list(
  data,
  outcome.str,
  fun.descr,
  fun = NULL,
  formula.str = NULL,
  args.list = NULL,
  vars = NULL,
  ...
)

regression_model_uv_list(
  data,
  outcome.str,
  fun.descr,
  fun = NULL,
  formula.str = NULL,
  args.list = NULL,
  vars = NULL,
  ...
)
```

## Arguments

- data:

  data

- outcome.str:

  name of outcome variable

- auto.mode:

  Make assumptions on function dependent on outcome data format.
  Overwrites other arguments.

- formula.str:

  custom formula glue string. Default is NULL.

- args.list:

  custom character string to be converted using argsstring2list() or
  list of arguments. Default is NULL.

- fun:

  name of custom function. Default is NULL.

- vars:

  character vector of variables to include

- ...:

  ignored

- fun.descr:

  Description of chosen function matching description in
  "supported_functions()"

## Value

object of standard class for fun

object of standard class for fun

list

list

## Examples

``` r
gtsummary::trial |>
  regression_model(outcome.str = "age")
#> Error in str2lang(x): <text>:2:0: unexpected end of input
#> 1: age~
#>    ^
gtsummary::trial |>
  regression_model(
    outcome.str = "age",
    auto.mode = FALSE,
    fun = "stats::lm",
    formula.str = "{outcome.str}~.",
    args.list = NULL
  )
#> 
#> Call:
#> (function (formula, data, subset, weights, na.action, method = "qr", 
#>     model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
#>     contrasts = NULL, offset, ...) 
#> {
#>     ret.x <- x
#>     ret.y <- y
#>     cl <- match.call()
#>     mf <- match.call(expand.dots = FALSE)
#>     m <- match(c("formula", "data", "subset", "weights", "na.action", 
#>         "offset"), names(mf), 0L)
#>     mf <- mf[c(1L, m)]
#>     mf$drop.unused.levels <- TRUE
#>     mf[[1L]] <- quote(stats::model.frame)
#>     mf <- eval(mf, parent.frame())
#>     if (method == "model.frame") 
#>         return(mf)
#>     else if (method != "qr") 
#>         warning(gettextf("method = '%s' is not supported. Using 'qr'", 
#>             method), domain = NA)
#>     mt <- attr(mf, "terms")
#>     y <- model.response(mf, "numeric")
#>     w <- as.vector(model.weights(mf))
#>     if (!is.null(w) && !is.numeric(w)) 
#>         stop("'weights' must be a numeric vector")
#>     offset <- model.offset(mf)
#>     mlm <- is.matrix(y)
#>     ny <- if (mlm) 
#>         nrow(y)
#>     else length(y)
#>     if (!is.null(offset)) {
#>         if (!mlm) 
#>             offset <- as.vector(offset)
#>         if (NROW(offset) != ny) 
#>             stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
#>                 NROW(offset), ny), domain = NA)
#>     }
#>     if (is.empty.model(mt)) {
#>         x <- NULL
#>         z <- list(coefficients = if (mlm) matrix(NA_real_, 0, 
#>             ncol(y)) else numeric(), residuals = y, fitted.values = 0 * 
#>             y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
#>             0) else ny)
#>         if (!is.null(offset)) {
#>             z$fitted.values <- offset
#>             z$residuals <- y - offset
#>         }
#>     }
#>     else {
#>         x <- model.matrix(mt, mf, contrasts)
#>         z <- if (is.null(w)) 
#>             lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
#>                 ...)
#>         else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
#>             ...)
#>     }
#>     class(z) <- c(if (mlm) "mlm", "lm")
#>     z$na.action <- attr(mf, "na.action")
#>     z$offset <- offset
#>     z$contrasts <- attr(x, "contrasts")
#>     z$xlevels <- .getXlevels(mt, mf)
#>     z$call <- cl
#>     z$terms <- mt
#>     if (model) 
#>         z$model <- mf
#>     if (ret.x) 
#>         z$x <- x
#>     if (ret.y) 
#>         z$y <- y
#>     if (!qr) 
#>         z$qr <- NULL
#>     z
#> })(formula = age ~ ., data = structure(list(trt = structure(c(1L, 
#> 2L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 
#> 2L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 
#> 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 
#> 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 
#> 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 1L, 
#> 1L, 1L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 
#> 2L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 1L, 
#> 1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 2L, 1L, 
#> 2L, 2L, 2L, 2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 1L, 2L, 2L, 
#> 1L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 
#> 2L, 2L, 1L, 2L, 2L, 1L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 1L, 
#> 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 
#> 2L, 1L, 2L, 1L, 1L, 1L, 1L), levels = c("Drug A", "Drug B"), class = "factor", label = "Chemotherapy Treatment"), 
#>     age = structure(c(23, 9, 31, NA, 51, 39, 37, 32, 31, 34, 
#>     42, 63, 54, 21, 48, 71, 38, 49, 57, 46, 47, 52, 61, 38, 34, 
#>     49, 63, 67, 68, 78, 36, 37, 53, 36, 51, 48, 57, 31, 37, 28, 
#>     40, 49, 61, 56, 54, 71, 38, 31, 48, NA, 83, 52, 32, 53, 69, 
#>     60, 45, 39, NA, 38, 36, 71, 31, 43, 57, 53, 25, 44, 25, 30, 
#>     51, 40, NA, 43, 21, 54, 67, 43, 54, 41, 34, 34, 6, 39, 36, 
#>     58, 27, 47, NA, 50, 61, 47, 52, 51, 68, 33, 65, 34, 38, 60, 
#>     10, 49, 56, 50, 60, 49, 54, 39, 48, 65, 47, 61, 34, NA, NA, 
#>     58, 26, 44, 17, 68, 57, 66, 44, NA, 67, 48, 62, 35, 53, 53, 
#>     66, 55, 57, 47, 58, 43, 45, 44, 63, 59, 44, 53, 51, 28, 65, 
#>     63, 76, 61, 33, 48, 42, 36, 55, 20, 26, 50, 47, 74, 50, 31, 
#>     45, 51, 66, 76, 47, 48, 56, 70, 46, 43, 41, 41, 19, 49, 43, 
#>     43, 75, 52, 42, 37, 45, 35, 67, 38, 44, 45, 39, 46, NA, 42, 
#>     60, 31, 45, 38, NA, 19, 69, 66, NA, 64), label = "Age"), 
#>     marker = structure(c(0.16, 1.107, 0.277, 2.067, 2.767, 0.613, 
#>     0.354, 1.739, 0.144, 0.205, 0.513, 0.06, 0.831, 0.258, 0.128, 
#>     0.445, 2.083, 0.157, 0.066, 0.325, 0.266, 0.719, 1.713, 0.096, 
#>     0.105, 0.043, 0.981, 1.156, 0.105, 0.175, 0.309, 1.869, 2.008, 
#>     1.894, 0.16, 1.209, 0.108, 0.611, 0.222, 0.803, 0.37, NA, 
#>     0.177, 1.479, 0.161, 0.737, 0.124, 0.092, 0.385, 0.21, 0.475, 
#>     1.628, 0.583, NA, 0.702, 1.206, 2.213, 1.406, 0.101, 0.013, 
#>     2.032, 1.046, 0.408, 2.636, 1.263, NA, 2.447, 1.041, 0.531, 
#>     0.924, 1.087, 0.733, 2.157, 0.333, 1.527, 2.238, 0.153, 0.305, 
#>     0.131, 0.386, 1.645, 1.321, 0.229, 0.615, 1.976, 1.941, 0.22, 
#>     3.874, 0.982, 1.68, 1.091, 0.169, 0.511, 2.141, 0.599, NA, 
#>     0.389, 0.005, 0.075, 1.491, 0.358, 1.709, 0.056, 1.354, 2.522, 
#>     0.387, 0.592, 0.243, 0.215, 1.207, 0.29, 0.718, 0.589, 0.003, 
#>     1.328, 0.308, 0.691, 3.249, 0.039, 1.804, 0.238, 2.702, 1.441, 
#>     0.27, NA, NA, 0.062, 2.19, 0.976, 3.062, 0.124, 0.045, 1.892, 
#>     0.711, 1.079, 1.061, 0.239, 0.361, 0.033, 1.133, 1.225, 1.418, 
#>     3.751, 3.02, 0.086, 0.772, 1.882, 2.725, 2.41, 0.352, 0.895, 
#>     0.215, 0.141, 2.288, 1.658, 1.255, 1.306, 0.081, 0.667, 0.046, 
#>     0.662, 1.985, 1.063, 1.55, 0.015, 0.056, NA, 0.51, 0.929, 
#>     2.345, 0.25, 0.816, 0.022, 0.16, 0.547, 3.642, 0.092, 1.2, 
#>     1.512, 2.124, NA, 0.862, 0.182, 1.075, 0.021, 0.402, 0.063, 
#>     1.129, 0.61, NA, 0.717, 0.205, 0.946, 0.386, 0.37, 1.148, 
#>     NA, 0.136, 0.439, 1.148), label = "Marker Level (ng/mL)"), 
#>     stage = structure(c(1L, 2L, 1L, 3L, 4L, 4L, 1L, 1L, 1L, 3L, 
#>     1L, 3L, 4L, 4L, 1L, 4L, 4L, 2L, 1L, 1L, 2L, 2L, 4L, 4L, 4L, 
#>     2L, 4L, 1L, 4L, 3L, 1L, 2L, 3L, 3L, 3L, 3L, 1L, 1L, 4L, 4L, 
#>     3L, 1L, 4L, 3L, 4L, 1L, 1L, 2L, 1L, 4L, 1L, 2L, 2L, 3L, 3L, 
#>     2L, 4L, 1L, 4L, 2L, 4L, 1L, 4L, 1L, 4L, 1L, 1L, 1L, 4L, 1L, 
#>     2L, 2L, 2L, 1L, 4L, 4L, 2L, 2L, 4L, 4L, 3L, 2L, 4L, 3L, 2L, 
#>     4L, 1L, 2L, 1L, 4L, 3L, 3L, 1L, 3L, 2L, 3L, 2L, 2L, 3L, 4L, 
#>     4L, 3L, 3L, 2L, 3L, 2L, 2L, 3L, 2L, 1L, 1L, 3L, 4L, 1L, 4L, 
#>     3L, 3L, 2L, 4L, 2L, 1L, 2L, 1L, 4L, 3L, 3L, 3L, 2L, 1L, 2L, 
#>     1L, 2L, 2L, 1L, 3L, 2L, 1L, 1L, 1L, 3L, 3L, 4L, 1L, 2L, 2L, 
#>     1L, 3L, 4L, 2L, 4L, 2L, 1L, 2L, 3L, 1L, 3L, 4L, 2L, 2L, 1L, 
#>     2L, 2L, 3L, 1L, 2L, 2L, 1L, 1L, 3L, 2L, 3L, 1L, 1L, 2L, 4L, 
#>     2L, 4L, 4L, 2L, 3L, 4L, 3L, 4L, 4L, 1L, 1L, 4L, 4L, 4L, 1L, 
#>     2L, 2L, 2L, 4L, 3L, 3L, 2L, 2L, 4L, 3L), levels = c("T1", 
#>     "T2", "T3", "T4"), class = "factor", label = "T Stage"), 
#>     grade = structure(c(2L, 1L, 2L, 3L, 3L, 1L, 2L, 1L, 2L, 1L, 
#>     3L, 1L, 3L, 1L, 1L, 3L, 3L, 2L, 3L, 2L, 1L, 2L, 1L, 1L, 2L, 
#>     3L, 2L, 2L, 2L, 1L, 3L, 2L, 1L, 1L, 1L, 3L, 2L, 2L, 3L, 2L, 
#>     2L, 3L, 3L, 1L, 3L, 1L, 3L, 2L, 2L, 2L, 3L, 3L, 3L, 2L, 3L, 
#>     1L, 3L, 1L, 2L, 2L, 3L, 1L, 2L, 1L, 2L, 1L, 1L, 1L, 3L, 2L, 
#>     1L, 3L, 3L, 3L, 2L, 3L, 3L, 1L, 2L, 1L, 1L, 1L, 1L, 3L, 1L, 
#>     3L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 3L, 1L, 
#>     3L, 1L, 2L, 2L, 1L, 2L, 2L, 3L, 1L, 3L, 3L, 1L, 1L, 1L, 2L, 
#>     3L, 3L, 2L, 1L, 3L, 2L, 1L, 2L, 2L, 2L, 1L, 3L, 3L, 2L, 3L, 
#>     1L, 1L, 1L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 3L, 2L, 
#>     2L, 2L, 2L, 2L, 1L, 1L, 3L, 2L, 2L, 1L, 2L, 1L, 2L, 3L, 2L, 
#>     1L, 1L, 1L, 3L, 1L, 1L, 2L, 1L, 1L, 3L, 3L, 1L, 3L, 2L, 1L, 
#>     3L, 1L, 2L, 1L, 1L, 3L, 1L, 2L, 1L, 2L, 2L, 2L, 3L, 2L, 2L, 
#>     3L, 2L, 3L, 3L, 2L, 1L, 2L, 3L, 3L, 1L), levels = c("I", 
#>     "II", "III"), class = "factor", label = "Grade"), response = structure(c(0L, 
#>     1L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 
#>     1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 
#>     0L, 0L, 1L, 0L, NA, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 
#>     1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 
#>     0L, 0L, 0L, NA, 0L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 
#>     0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 
#>     0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, NA, 0L, 0L, 1L, 
#>     0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 
#>     0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 
#>     1L, 1L, NA, 1L, 1L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, NA, 1L, 0L, 
#>     0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 
#>     0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, NA, 0L, 1L, 
#>     0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, NA, 0L, 
#>     1L, 1L, 0L, 0L), label = "Tumor Response"), death = structure(c(0L, 
#>     0L, 0L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 
#>     0L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 
#>     1L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 0L, 
#>     0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
#>     0L, 1L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 
#>     1L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 
#>     1L, 1L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 
#>     1L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 
#>     0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 1L, 0L, 1L, 
#>     0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 
#>     0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 
#>     1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 
#>     0L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 
#>     1L, 0L, 1L, 0L), label = "Patient Died"), ttdeath = structure(c(24, 
#>     24, 24, 17.64, 16.43, 15.64, 24, 18.43, 24, 10.53, 24, 24, 
#>     14.34, 12.89, 22.68, 8.71, 24, 15.21, 24, 24, 24, 24, 16.92, 
#>     23.89, 6.32, 15.77, 24, 24, 15.45, 17.43, 24, 20.9, 24, 24, 
#>     24, 21.19, 12.52, 24, 15.59, 18, 18.02, 12.43, 12.1, 24, 
#>     17.42, 24, 24, 24, 12.19, 10.02, 18.23, 10.42, 24, 24, 19.34, 
#>     12.21, 14.46, 19.34, 10.16, 13.15, 10.12, 24, 22.77, 24, 
#>     24, 22.13, 24, 20.62, 23.23, 7.38, 24, 24, 24, 24, 24, 19.22, 
#>     7.27, 23.88, 16.23, 24, 14.06, 24, 24, 24, 16.44, 23.81, 
#>     24, 18.37, 11.44, 20.94, 5.33, 22.92, 10.33, 24, 24, 14.54, 
#>     19.14, 24, 21.19, 16.07, 9.97, 24, 24, 24, 19.75, 16.67, 
#>     11.18, 18.29, 24, 17.56, 17.45, 24, 22.86, 13.68, 24, 24, 
#>     17.46, 24, 24, 24, 24, 24, 13, 9.73, 15.65, 24, 3.53, 20.35, 
#>     23.41, 16.47, 24, 24, 14.65, 17.81, 24, 21.83, 24, 24, 21.49, 
#>     12.68, 24, 24, 24, 24, 10.07, 24, 24, 24, 8.37, 20.33, 24, 
#>     24, 21.33, 12.63, 13.08, 24, 15.1, 20.14, 10.55, 24, 24, 
#>     24, 24, 23.6, 24, 19.98, 15.55, 23.72, 22.41, 19.54, 16.57, 
#>     24, 24, 24, 21.91, 24, 12.53, 24, 18.63, 14.82, 16.46, 24, 
#>     9.24, 17.77, 24, 24, 9.92, 16.16, 10.51, 20.81, 24, 16.44, 
#>     24, 22.4, 11.76, 24, 21.6, 24, 19.81, 24), label = "Months to Death/Censor")), class = c("tbl_df", 
#> "tbl", "data.frame"), row.names = c(NA, -200L)))
#> 
#> Coefficients:
#> (Intercept)    trtDrug B       marker      stageT2      stageT3      stageT4  
#>     45.9247      -0.4052      -0.1435       2.0519       2.4437      -3.1573  
#>     gradeII     gradeIII     response        death      ttdeath  
#>      0.1750       1.6855       4.9885       3.0775      -0.1394  
#> 
gtsummary::trial |>
  default_parsing() |>
  regression_model(
    outcome.str = "trt",
    auto.mode = FALSE,
    fun = "stats::glm",
    args.list = list(family = binomial(link = "logit"))
  )
#> Error in str2lang(x): <text>:2:0: unexpected end of input
#> 1: trt~
#>    ^
m <- mtcars |>
  default_parsing() |>
  regression_model(
    outcome.str = "mpg",
    auto.mode = FALSE,
    fun = "stats::lm",
    formula.str = "{outcome.str}~{paste(vars,collapse='+')}",
    args.list = NULL,
    vars = c("mpg", "cyl")
  )
#> Warning: the response appeared on the right-hand side and was dropped
#> Warning: problem with term 1 in model.matrix: no columns are assigned
broom::tidy(m)
#> # A tibble: 3 × 5
#>   term        estimate std.error statistic  p.value
#>   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#> 1 (Intercept)    26.7      0.972     27.4  2.69e-22
#> 2 cyl6           -6.92     1.56      -4.44 1.19e- 4
#> 3 cyl8          -11.6      1.30      -8.90 8.57e-10
if (FALSE) { # \dontrun{
gtsummary::trial |>
  regression_model_uv(outcome.str = "age")
gtsummary::trial |>
  regression_model_uv(
    outcome.str = "age",
    fun = "stats::lm",
    args.list = NULL
  )
m <- gtsummary::trial |> regression_model_uv(
  outcome.str = "trt",
  fun = "stats::glm",
  args.list = list(family = stats::binomial(link = "logit"))
)
lapply(m, broom::tidy) |> dplyr::bind_rows()
} # }
if (FALSE) { # \dontrun{
gtsummary::trial |>
  regression_model(
    outcome.str = "age",
    fun = "stats::lm",
    formula.str = "{outcome.str}~.",
    args.list = NULL
  )
ls <- regression_model_list(data = default_parsing(mtcars), outcome.str = "cyl", fun.descr = "Ordinal logistic regression model")
summary(ls$model)
ls <- regression_model_list(data = default_parsing(mtcars), outcome.str = "mpg", fun.descr = "Linear regression model")

ls <- regression_model_list(data = default_parsing(gtsummary::trial), outcome.str = "trt", fun.descr = "Logistic regression model")
tbl <- gtsummary::tbl_regression(ls$model, exponentiate = TRUE)
m <- gtsummary::trial |>
  default_parsing() |>
  regression_model(
    outcome.str = "trt",
    fun = "stats::glm",
    formula.str = "{outcome.str}~.",
    args.list = list(family = "binomial")
  )
tbl2 <- gtsummary::tbl_regression(m, exponentiate = TRUE)
broom::tidy(ls$model)
broom::tidy(m)
} # }
if (FALSE) { # \dontrun{
gtsummary::trial |>
  regression_model_uv(
    outcome.str = "trt",
    fun = "stats::glm",
    args.list = list(family = stats::binomial(link = "logit"))
  ) |>
  lapply(broom::tidy) |>
  dplyr::bind_rows()
ms <- regression_model_uv_list(data = default_parsing(mtcars), outcome.str = "mpg", fun.descr = "Linear regression model")
ms$code
ls <- regression_model_uv_list(data = default_parsing(mtcars), outcome.str = "am", fun.descr = "Logistic regression model")
ls$code
lapply(ms$model, broom::tidy) |> dplyr::bind_rows()
} # }
```
