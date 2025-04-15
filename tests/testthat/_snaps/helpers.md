# getfun works

    Code
      getfun("stats::lm")
    Output
      function (formula, data, subset, weights, na.action, method = "qr", 
          model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
          contrasts = NULL, offset, ...) 
      {
          ret.x <- x
          ret.y <- y
          cl <- match.call()
          mf <- match.call(expand.dots = FALSE)
          m <- match(c("formula", "data", "subset", "weights", "na.action", 
              "offset"), names(mf), 0L)
          mf <- mf[c(1L, m)]
          mf$drop.unused.levels <- TRUE
          mf[[1L]] <- quote(stats::model.frame)
          mf <- eval(mf, parent.frame())
          if (method == "model.frame") 
              return(mf)
          else if (method != "qr") 
              warning(gettextf("method = '%s' is not supported. Using 'qr'", 
                  method), domain = NA)
          mt <- attr(mf, "terms")
          y <- model.response(mf, "numeric")
          w <- as.vector(model.weights(mf))
          if (!is.null(w) && !is.numeric(w)) 
              stop("'weights' must be a numeric vector")
          offset <- model.offset(mf)
          mlm <- is.matrix(y)
          ny <- if (mlm) 
              nrow(y)
          else length(y)
          if (!is.null(offset)) {
              if (!mlm) 
                  offset <- as.vector(offset)
              if (NROW(offset) != ny) 
                  stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                      NROW(offset), ny), domain = NA)
          }
          if (is.empty.model(mt)) {
              x <- NULL
              z <- list(coefficients = if (mlm) matrix(NA_real_, 0, 
                  ncol(y)) else numeric(), residuals = y, fitted.values = 0 * 
                  y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
                  0) else ny)
              if (!is.null(offset)) {
                  z$fitted.values <- offset
                  z$residuals <- y - offset
              }
          }
          else {
              x <- model.matrix(mt, mf, contrasts)
              z <- if (is.null(w)) 
                  lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
                      ...)
              else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
                  ...)
          }
          class(z) <- c(if (mlm) "mlm", "lm")
          z$na.action <- attr(mf, "na.action")
          z$offset <- offset
          z$contrasts <- attr(x, "contrasts")
          z$xlevels <- .getXlevels(mt, mf)
          z$call <- cl
          z$terms <- mt
          if (model) 
              z$model <- mf
          if (ret.x) 
              z$x <- x
          if (ret.y) 
              z$y <- y
          if (!qr) 
              z$qr <- NULL
          z
      }
      <bytecode: 0x12c7f2dd8>
      <environment: namespace:stats>

# argsstring2list works

    Code
      argsstring2list("A=1:5,b=2:4")
    Output
      $A
      [1] 1 2 3 4 5
      
      $b
      [1] 2 3 4
      

# factorize works

    Code
      factorize(mtcars, names(mtcars))
    Output
                           mpg cyl  disp  hp drat    wt  qsec vs am gear carb
      Mazda RX4             21   6   160 110  3.9  2.62 16.46  0  1    4    4
      Mazda RX4 Wag         21   6   160 110  3.9 2.875 17.02  0  1    4    4
      Datsun 710          22.8   4   108  93 3.85  2.32 18.61  1  1    4    1
      Hornet 4 Drive      21.4   6   258 110 3.08 3.215 19.44  1  0    3    1
      Hornet Sportabout   18.7   8   360 175 3.15  3.44 17.02  0  0    3    2
      Valiant             18.1   6   225 105 2.76  3.46 20.22  1  0    3    1
      Duster 360          14.3   8   360 245 3.21  3.57 15.84  0  0    3    4
      Merc 240D           24.4   4 146.7  62 3.69  3.19    20  1  0    4    2
      Merc 230            22.8   4 140.8  95 3.92  3.15  22.9  1  0    4    2
      Merc 280            19.2   6 167.6 123 3.92  3.44  18.3  1  0    4    4
      Merc 280C           17.8   6 167.6 123 3.92  3.44  18.9  1  0    4    4
      Merc 450SE          16.4   8 275.8 180 3.07  4.07  17.4  0  0    3    3
      Merc 450SL          17.3   8 275.8 180 3.07  3.73  17.6  0  0    3    3
      Merc 450SLC         15.2   8 275.8 180 3.07  3.78    18  0  0    3    3
      Cadillac Fleetwood  10.4   8   472 205 2.93  5.25 17.98  0  0    3    4
      Lincoln Continental 10.4   8   460 215    3 5.424 17.82  0  0    3    4
      Chrysler Imperial   14.7   8   440 230 3.23 5.345 17.42  0  0    3    4
      Fiat 128            32.4   4  78.7  66 4.08   2.2 19.47  1  1    4    1
      Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
      Toyota Corolla      33.9   4  71.1  65 4.22 1.835  19.9  1  1    4    1
      Toyota Corona       21.5   4 120.1  97  3.7 2.465 20.01  1  0    3    1
      Dodge Challenger    15.5   8   318 150 2.76  3.52 16.87  0  0    3    2
      AMC Javelin         15.2   8   304 150 3.15 3.435  17.3  0  0    3    2
      Camaro Z28          13.3   8   350 245 3.73  3.84 15.41  0  0    3    4
      Pontiac Firebird    19.2   8   400 175 3.08 3.845 17.05  0  0    3    2
      Fiat X1-9           27.3   4    79  66 4.08 1.935  18.9  1  1    4    1
      Porsche 914-2         26   4 120.3  91 4.43  2.14  16.7  0  1    5    2
      Lotus Europa        30.4   4  95.1 113 3.77 1.513  16.9  1  1    5    2
      Ford Pantera L      15.8   8   351 264 4.22  3.17  14.5  0  1    5    4
      Ferrari Dino        19.7   6   145 175 3.62  2.77  15.5  0  1    5    6
      Maserati Bora         15   8   301 335 3.54  3.57  14.6  0  1    5    8
      Volvo 142E          21.4   4   121 109 4.11  2.78  18.6  1  1    4    2

# default_parsing works

    Code
      default_parsing(mtcars)
    Output
      # A tibble: 32 x 11
           mpg cyl    disp    hp  drat    wt  qsec vs    am    gear  carb 
         <dbl> <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <lgl> <lgl> <fct> <fct>
       1  21   6      160    110  3.9   2.62  16.5 FALSE TRUE  4     4    
       2  21   6      160    110  3.9   2.88  17.0 FALSE TRUE  4     4    
       3  22.8 4      108     93  3.85  2.32  18.6 TRUE  TRUE  4     1    
       4  21.4 6      258    110  3.08  3.22  19.4 TRUE  FALSE 3     1    
       5  18.7 8      360    175  3.15  3.44  17.0 FALSE FALSE 3     2    
       6  18.1 6      225    105  2.76  3.46  20.2 TRUE  FALSE 3     1    
       7  14.3 8      360    245  3.21  3.57  15.8 FALSE FALSE 3     4    
       8  24.4 4      147.    62  3.69  3.19  20   TRUE  FALSE 4     2    
       9  22.8 4      141.    95  3.92  3.15  22.9 TRUE  FALSE 4     2    
      10  19.2 6      168.   123  3.92  3.44  18.3 TRUE  FALSE 4     4    
      # i 22 more rows

# remove_empty_attr works

    Code
      remove_empty_attr(ds)
    Output
      $mpg
       [1] 21.0 21.0 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 17.8 16.4 17.3 15.2 10.4
      [16] 10.4 14.7 32.4 30.4 33.9 21.5 15.5 15.2 13.3 19.2 27.3 26.0 30.4 15.8 19.7
      [31] 15.0 21.4
      
      $cyl
       [1] 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4
      
      $disp
       [1] 160.0 160.0 108.0 258.0 360.0 225.0 360.0 146.7 140.8 167.6 167.6 275.8
      [13] 275.8 275.8 472.0 460.0 440.0  78.7  75.7  71.1 120.1 318.0 304.0 350.0
      [25] 400.0  79.0 120.3  95.1 351.0 145.0 301.0 121.0
      
      $hp
       [1] 110 110  93 110 175 105 245  62  95 123 123 180 180 180 205 215 230  66  52
      [20]  65  97 150 150 245 175  66  91 113 264 175 335 109
      
      $drat
       [1] 3.90 3.90 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 3.92 3.07 3.07 3.07 2.93
      [16] 3.00 3.23 4.08 4.93 4.22 3.70 2.76 3.15 3.73 3.08 4.08 4.43 3.77 4.22 3.62
      [31] 3.54 4.11
      
      $wt
       [1] 2.620 2.875 2.320 3.215 3.440 3.460 3.570 3.190 3.150 3.440 3.440 4.070
      [13] 3.730 3.780 5.250 5.424 5.345 2.200 1.615 1.835 2.465 3.520 3.435 3.840
      [25] 3.845 1.935 2.140 1.513 3.170 2.770 3.570 2.780
      
      $qsec
       [1] 16.46 17.02 18.61 19.44 17.02 20.22 15.84 20.00 22.90 18.30 18.90 17.40
      [13] 17.60 18.00 17.98 17.82 17.42 19.47 18.52 19.90 20.01 16.87 17.30 15.41
      [25] 17.05 18.90 16.70 16.90 14.50 15.50 14.60 18.60
      
      $vs
       [1] 0 0 1 1 0 1 0 1 1 1 1 0 0 0 0 0 0 1 1 1 1 0 0 0 0 1 0 1 0 0 0 1
      
      $am
       [1] 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1
      
      $gear
       [1] 4 4 4 3 3 3 3 4 4 4 4 3 3 3 3 3 3 4 4 4 3 3 3 3 3 4 5 5 5 5 5 4
      
      $carb
       [1] 4 4 1 1 2 1 4 2 2 4 4 3 3 3 4 4 4 1 2 1 1 2 2 4 2 1 2 2 4 6 8 2
      

---

    Code
      remove_empty_attr(dplyr::bind_cols(ds))
    Output
      # A tibble: 32 x 11
           mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
       1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
       2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
       3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
       4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
       5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
       6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
       7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
       8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
       9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
      10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
      # i 22 more rows

---

    Code
      remove_empty_attr(ds[[1]])
    Output
       [1] 21.0 21.0 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 17.8 16.4 17.3 15.2 10.4
      [16] 10.4 14.7 32.4 30.4 33.9 21.5 15.5 15.2 13.3 19.2 27.3 26.0 30.4 15.8 19.7
      [31] 15.0 21.4

# remove_empty_cols works

    Code
      remove_empty_cols(data.frame(a = 1:10, b = NA, c = c(2, NA)), cutoff = 0.5)
    Output
          a  c
      1   1  2
      2   2 NA
      3   3  2
      4   4 NA
      5   5  2
      6   6 NA
      7   7  2
      8   8 NA
      9   9  2
      10 10 NA

# append_list works

    Code
      append_list(data.frame(letters[1:20], 1:20), ls_d, "letters")
    Output
      $letters
         letters.1.20. X1.20
      1              a     1
      2              b     2
      3              c     3
      4              d     4
      5              e     5
      6              f     6
      7              g     7
      8              h     8
      9              i     9
      10             j    10
      11             k    11
      12             l    12
      13             m    13
      14             n    14
      15             o    15
      16             p    16
      17             q    17
      18             r    18
      19             s    19
      20             t    20
      

---

    Code
      append_list(letters[1:20], ls_d, "letters")
    Output
      $letters
       [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
      [20] "t"
      

# missing_fraction works

    Code
      missing_fraction(c(NA, 1:10, rep(NA, 3)))
    Output
      [1] 0.2857143

# data_description works

    Code
      data_description(data.frame(sample(1:8, 20, TRUE), sample(c(1:8, NA), 20, TRUE)),
      data_text = "This data")
    Output
      [1] "This data has 20 observations and 2 variables, with 16 (80%) complete cases."

# Data type filter works

    Code
      data_type_filter(default_parsing(mtcars), type = c("categorical", "continuous"))
    Output
      # A tibble: 32 x 9
           mpg cyl    disp    hp  drat    wt  qsec gear  carb 
         <dbl> <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <fct> <fct>
       1  21   6      160    110  3.9   2.62  16.5 4     4    
       2  21   6      160    110  3.9   2.88  17.0 4     4    
       3  22.8 4      108     93  3.85  2.32  18.6 4     1    
       4  21.4 6      258    110  3.08  3.22  19.4 3     1    
       5  18.7 8      360    175  3.15  3.44  17.0 3     2    
       6  18.1 6      225    105  2.76  3.46  20.2 3     1    
       7  14.3 8      360    245  3.21  3.57  15.8 3     4    
       8  24.4 4      147.    62  3.69  3.19  20   4     2    
       9  22.8 4      141.    95  3.92  3.15  22.9 4     2    
      10  19.2 6      168.   123  3.92  3.44  18.3 4     4    
      # i 22 more rows

---

    Code
      data_type_filter(default_parsing(mtcars), type = NULL)
    Output
      # A tibble: 32 x 11
           mpg cyl    disp    hp  drat    wt  qsec vs    am    gear  carb 
         <dbl> <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <lgl> <lgl> <fct> <fct>
       1  21   6      160    110  3.9   2.62  16.5 FALSE TRUE  4     4    
       2  21   6      160    110  3.9   2.88  17.0 FALSE TRUE  4     4    
       3  22.8 4      108     93  3.85  2.32  18.6 TRUE  TRUE  4     1    
       4  21.4 6      258    110  3.08  3.22  19.4 TRUE  FALSE 3     1    
       5  18.7 8      360    175  3.15  3.44  17.0 FALSE FALSE 3     2    
       6  18.1 6      225    105  2.76  3.46  20.2 TRUE  FALSE 3     1    
       7  14.3 8      360    245  3.21  3.57  15.8 FALSE FALSE 3     4    
       8  24.4 4      147.    62  3.69  3.19  20   TRUE  FALSE 4     2    
       9  22.8 4      141.    95  3.92  3.15  22.9 TRUE  FALSE 4     2    
      10  19.2 6      168.   123  3.92  3.44  18.3 TRUE  FALSE 4     4    
      # i 22 more rows

# sort_by works

    Code
      sort_by(c("Multivariable", "Univariable"), c("Univariable", "Minimal",
        "Multivariable"))
    Output
      [1] "Univariable"   NA              "Multivariable"

# if_not_missing works

    Code
      if_not_missing(NULL, "new")
    Output
      [1] "new"

---

    Code
      if_not_missing(c(2, "a", NA))
    Output
      [1] "2" "a"

---

    Code
      if_not_missing("See")
    Output
      [1] "See"

# merge_expression, expression_string and pipe_string works

    Code
      merge_expression(list(rlang::call2(.fn = "select", !!!list(c("cyl", "disp")),
      .ns = "dplyr"), rlang::call2(.fn = "default_parsing", .ns = "FreesearchR")))
    Output
      dplyr::select(c("cyl", "disp")) %>% FreesearchR::default_parsing()

---

    Code
      expression_string(pipe_string(lapply(list("mtcars", rlang::call2(.fn = "select",
        !!!list(c("cyl", "disp")), .ns = "dplyr"), rlang::call2(.fn = "default_parsing",
        .ns = "FreesearchR")), expression_string)), "data<-")
    Output
      [1] "data<-mtcars|>\ndplyr::select(c('cyl','disp'))|>\nFreesearchR::default_parsing()"

---

    Code
      expression_string(merge_expression(list(as.symbol(paste0("mtcars$", "mpg")),
      rlang::call2(.fn = "select", !!!list(c("cyl", "disp")), .ns = "dplyr"), rlang::call2(
        .fn = "default_parsing", .ns = "FreesearchR"))))
    Output
      [1] "mtcars$mpg|>\ndplyr::select(c('cyl','disp'))|>\nFreesearchR::default_parsing()"

# remove_nested_list works

    Code
      remove_nested_list(dplyr::tibble(a = 1:10, b = rep(list("a"), 10)))
    Output
      # A tibble: 10 x 1
             a
         <int>
       1     1
       2     2
       3     3
       4     4
       5     5
       6     6
       7     7
       8     8
       9     9
      10    10

---

    Code
      remove_nested_list(as.data.frame(dplyr::tibble(a = 1:10, b = rep(list(c("a",
        "b")), 10))))
    Output
          a
      1   1
      2   2
      3   3
      4   4
      5   5
      6   6
      7   7
      8   8
      9   9
      10 10

# set_column_label works

    Code
      set_column_label(set_column_label(set_column_label(mtcars, ls), ls2), ls3)
    Output
      # A tibble: 32 x 11
           mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
       1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
       2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
       3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
       4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
       5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
       6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
       7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
       8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
       9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
      10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
      # i 22 more rows

---

    Code
      expression_string(rlang::expr(FreesearchR::set_column_label(label = !!ls3)))
    Output
      [1] "FreesearchR::set_column_label(label=c(mpg='',cyl='',disp='',hp='Horses',drat='',wt='',qsec='',vs='',am='',gear='',carb=''))"

# append_column works

    Code
      append_column(dplyr::mutate(mtcars, mpg_cut = mpg), mtcars$mpg, "mpg_cutter")
    Output
                           mpg cyl  disp  hp drat    wt  qsec vs am gear carb mpg_cut
      Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4    21.0
      Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4    21.0
      Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1    22.8
      Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1    21.4
      Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2    18.7
      Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1    18.1
      Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4    14.3
      Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2    24.4
      Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2    22.8
      Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4    19.2
      Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4    17.8
      Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3    16.4
      Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3    17.3
      Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3    15.2
      Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4    10.4
      Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4    10.4
      Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4    14.7
      Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1    32.4
      Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2    30.4
      Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1    33.9
      Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1    21.5
      Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2    15.5
      AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2    15.2
      Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4    13.3
      Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2    19.2
      Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1    27.3
      Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2    26.0
      Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2    30.4
      Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4    15.8
      Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6    19.7
      Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8    15.0
      Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2    21.4
                          mpg_cutter
      Mazda RX4                 21.0
      Mazda RX4 Wag             21.0
      Datsun 710                22.8
      Hornet 4 Drive            21.4
      Hornet Sportabout         18.7
      Valiant                   18.1
      Duster 360                14.3
      Merc 240D                 24.4
      Merc 230                  22.8
      Merc 280                  19.2
      Merc 280C                 17.8
      Merc 450SE                16.4
      Merc 450SL                17.3
      Merc 450SLC               15.2
      Cadillac Fleetwood        10.4
      Lincoln Continental       10.4
      Chrysler Imperial         14.7
      Fiat 128                  32.4
      Honda Civic               30.4
      Toyota Corolla            33.9
      Toyota Corona             21.5
      Dodge Challenger          15.5
      AMC Javelin               15.2
      Camaro Z28                13.3
      Pontiac Firebird          19.2
      Fiat X1-9                 27.3
      Porsche 914-2             26.0
      Lotus Europa              30.4
      Ford Pantera L            15.8
      Ferrari Dino              19.7
      Maserati Bora             15.0
      Volvo 142E                21.4

