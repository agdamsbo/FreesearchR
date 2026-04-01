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
#>  $ name    : chr "svg"
#>  $ attribs :List of 6
#>   ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   ..$ viewbox: chr "0 0 256 256"
#>   ..$ class  : chr "phosphor-svg"
#>   ..$ height : chr "1.33em"
#>   ..$ fill   : chr "currentColor"
#>   ..$ style  : chr "vertical-align:-0.25em;"
#>  $ children:List of 2
#>   ..$ :List of 3
#>   .. ..$ name    : chr "path"
#>   .. ..$ attribs :List of 1
#>   .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. ..$ children: list()
#>   .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..$ :List of 3
#>   .. ..$ name    : chr "title"
#>   .. ..$ attribs : Named list()
#>   .. ..$ children:List of 1
#>   .. .. ..$ : chr "calculator-light"
#>   .. ..- attr(*, "class")= chr "shiny.tag"
#>  - attr(*, "class")= chr "shiny.tag"
mtcars |> sapply(class) |> class_icons() |> str()
#> List of 11
#>  $ mpg :List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
#>  $ cyl :List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
#>  $ disp:List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
#>  $ hp  :List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
#>  $ drat:List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
#>  $ wt  :List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
#>  $ qsec:List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
#>  $ vs  :List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
#>  $ am  :List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
#>  $ gear:List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
#>  $ carb:List of 3
#>   ..$ name    : chr "svg"
#>   ..$ attribs :List of 6
#>   .. ..$ xmlns  : chr "http://www.w3.org/2000/svg"
#>   .. ..$ viewbox: chr "0 0 256 256"
#>   .. ..$ class  : chr "phosphor-svg"
#>   .. ..$ height : chr "1.33em"
#>   .. ..$ fill   : chr "currentColor"
#>   .. ..$ style  : chr "vertical-align:-0.25em;"
#>   ..$ children:List of 2
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "path"
#>   .. .. ..$ attribs :List of 1
#>   .. .. .. ..$ d: chr "M176,58H80a6,6,0,0,0-6,6v48a6,6,0,0,0,6,6h96a6,6,0,0,0,6-6V64A6,6,0,0,0,176,58Zm-6,48H86V70h84Zm30-80H56A14,14,"| __truncated__
#>   .. .. ..$ children: list()
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   .. ..$ :List of 3
#>   .. .. ..$ name    : chr "title"
#>   .. .. ..$ attribs : Named list()
#>   .. .. ..$ children:List of 1
#>   .. .. .. ..$ : chr "calculator-light"
#>   .. .. ..- attr(*, "class")= chr "shiny.tag"
#>   ..- attr(*, "class")= chr "shiny.tag"
```
