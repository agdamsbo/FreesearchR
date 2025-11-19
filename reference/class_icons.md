# Get data class icons

Get data class icons

## Usage

``` r
class_icons(x)
```

## Arguments

- x:

  character vector of data classes

## Value

list

## Examples

``` r
"numeric" |> class_icons()|> str()
#> List of 3
#>  $ name    : chr "i"
#>  $ attribs :List of 3
#>   ..$ class     : chr "fas fa-calculator"
#>   ..$ role      : chr "presentation"
#>   ..$ aria-label: chr "calculator icon"
#>  $ children: list()
#>  - attr(*, "class")= chr "shiny.tag"
#>  - attr(*, "browsable_html")= logi TRUE
#>  - attr(*, "html_dependencies")=List of 1
#>   ..$ :List of 10
#>   .. ..$ name      : chr "font-awesome"
#>   .. ..$ version   : chr "6.5.2"
#>   .. ..$ src       :List of 1
#>   .. .. ..$ file: chr "fontawesome"
#>   .. ..$ meta      : NULL
#>   .. ..$ script    : NULL
#>   .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. ..$ head      : NULL
#>   .. ..$ attachment: NULL
#>   .. ..$ package   : chr "fontawesome"
#>   .. ..$ all_files : logi TRUE
#>   .. ..- attr(*, "class")= chr "html_dependency"
mtcars |> sapply(class) |> class_icons() |> str()
#> List of 11
#>  $ mpg :List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
#>  $ cyl :List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
#>  $ disp:List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
#>  $ hp  :List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
#>  $ drat:List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
#>  $ wt  :List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
#>  $ qsec:List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
#>  $ vs  :List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
#>  $ am  :List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
#>  $ gear:List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
#>  $ carb:List of 3
#>   ..$ name    : chr "i"
#>   ..$ attribs :List of 3
#>   .. ..$ class     : chr "fas fa-calculator"
#>   .. ..$ role      : chr "presentation"
#>   .. ..$ aria-label: chr "calculator icon"
#>   ..$ children: list()
#>   ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "browsable_html")= logi TRUE
#>   ..- attr(*, "html_dependencies")=List of 1
#>   .. ..$ :List of 10
#>   .. .. ..$ name      : chr "font-awesome"
#>   .. .. ..$ version   : chr "6.5.2"
#>   .. .. ..$ src       :List of 1
#>   .. .. .. ..$ file: chr "fontawesome"
#>   .. .. ..$ meta      : NULL
#>   .. .. ..$ script    : NULL
#>   .. .. ..$ stylesheet: chr [1:2] "css/all.min.css" "css/v4-shims.min.css"
#>   .. .. ..$ head      : NULL
#>   .. .. ..$ attachment: NULL
#>   .. .. ..$ package   : chr "fontawesome"
#>   .. .. ..$ all_files : logi TRUE
#>   .. .. ..- attr(*, "class")= chr "html_dependency"
```
