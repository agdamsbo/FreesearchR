## Inpiration:
##
## https://stackoverflow.com/questions/47445260/how-to-enable-syntax-highlighting-in-r-shiny-app-with-htmloutput

prismCodeBlock <- function(code) {
  tagList(
    HTML(html_code_wrap(code)),
    tags$script("Prism.highlightAll()")
  )
}

prismDependencies <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/prism.min.js"),
  tags$link(rel = "stylesheet", type = "text/css",
            href = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/themes/prism.min.css")
)

prismRDependency <- tags$head(
  tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/prism/1.8.4/components/prism-r.min.js")
)

html_code_wrap <- function(string,lang="r"){
  glue::glue("<pre><code class='language-{lang}'>{string}
  </code></pre>")
}
