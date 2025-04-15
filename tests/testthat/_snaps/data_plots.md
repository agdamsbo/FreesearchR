# all_but works

    Code
      all_but(1:10, c(2, 3), 11, 5)
    Output
      [1]  1  4  6  7  8  9 10

# subset_types works

    Code
      subset_types(default_parsing(mtcars), "continuous")
    Output
      # A tibble: 32 x 6
           mpg  disp    hp  drat    wt  qsec
         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
       1  21    160    110  3.9   2.62  16.5
       2  21    160    110  3.9   2.88  17.0
       3  22.8  108     93  3.85  2.32  18.6
       4  21.4  258    110  3.08  3.22  19.4
       5  18.7  360    175  3.15  3.44  17.0
       6  18.1  225    105  2.76  3.46  20.2
       7  14.3  360    245  3.21  3.57  15.8
       8  24.4  147.    62  3.69  3.19  20  
       9  22.8  141.    95  3.92  3.15  22.9
      10  19.2  168.   123  3.92  3.44  18.3
      # i 22 more rows

---

    Code
      subset_types(default_parsing(mtcars), c("dichotomous", "ordinal", "categorical"))
    Output
      # A tibble: 32 x 5
         cyl   vs    am    gear  carb 
         <fct> <lgl> <lgl> <fct> <fct>
       1 6     FALSE TRUE  4     4    
       2 6     FALSE TRUE  4     4    
       3 4     TRUE  TRUE  4     1    
       4 6     TRUE  FALSE 3     1    
       5 8     FALSE FALSE 3     2    
       6 6     TRUE  FALSE 3     1    
       7 8     FALSE FALSE 3     4    
       8 4     TRUE  FALSE 4     2    
       9 4     TRUE  FALSE 4     2    
      10 6     TRUE  FALSE 4     4    
      # i 22 more rows

---

    Code
      subset_types(default_parsing(mtcars), "test")
    Output
      # A tibble: 32 x 0

# possible_plots works

    Code
      possible_plots(mtcars$mpg)
    Output
      [1] "Violin plot"  "Scatter plot" "Box plot"    

---

    Code
      possible_plots(default_parsing(mtcars)["cyl"])
    Output
      [1] "Stacked horizontal bars" "Violin plot"            
      [3] "Sankey plot"             "Box plot"               

# get_plot_options works

    Code
      get_plot_options((function(.x) {
        .x[[1]]
      })(possible_plots(default_parsing(mtcars)["mpg"])))
    Output
      $plot_violin
      $plot_violin$fun
      [1] "plot_violin"
      
      $plot_violin$descr
      [1] "Violin plot"
      
      $plot_violin$note
      [1] "A modern alternative to the classic boxplot to visualise data distribution"
      
      $plot_violin$primary.type
      [1] "datatime"    "continuous"  "dichotomous" "ordinal"     "categorical"
      
      $plot_violin$secondary.type
      [1] "dichotomous" "ordinal"     "categorical"
      
      $plot_violin$secondary.multi
      [1] FALSE
      
      $plot_violin$secondary.extra
      [1] "none"
      
      $plot_violin$tertiary.type
      [1] "dichotomous" "ordinal"     "categorical"
      
      

# get_label works

    Code
      get_label(mtcars, var = "mpg")
    Output
      [1] "mpg"

---

    Code
      get_label(mtcars)
    Output
      [1] "mtcars"

---

    Code
      get_label(mtcars$mpg)
    Output
      [1] "mtcars$mpg"

---

    Code
      get_label(gtsummary::trial, var = "trt")
    Output
      [1] "Chemotherapy Treatment"

---

    Code
      get_label(1:10)
    Output
      [1] "1:10"

# line_break works

    Code
      line_break("Lorem ipsum... you know the routine")
    Output
      [1] "Lorem ipsum... you\nknow the routine"

---

    Code
      line_break(paste(sample(letters[1:10], 100, TRUE), collapse = ""), force = TRUE,
      lineLength = 5)
    Output
      [1] "cjijd\ncjcfb\nihfgi\nfcffh\neaddf\ngegjb\njeegi\nfdhbe\nbgcac\nibfbe\nejibi\nggedh\ngajhf\ngadca\nijeig\ncieeh\ncah\n"

---

    Code
      line_break(paste(sample(letters[1:10], 100, TRUE), collapse = ""), force = FALSE)
    Output
      [1] "idjcgcjceeefchffjdbjafabigaiadcfdcfgfgibibhcjbbbejabddeheafggcgbdfbcbeegijggbibaghfidjgeaefhcadbfjig"

