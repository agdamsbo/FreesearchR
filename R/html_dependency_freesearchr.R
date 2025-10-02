html_dependency_FreesearchR <- function() {
  htmltools::htmlDependency(
    name = "FreesearchR",
    version = packageVersion("FreesearchR"),
    src = list(href = "FreesearchR", file = "assets"),
    package = "FreesearchR",
    script = "js/FreesearchR.js",
    stylesheet = "css/FreesearchR.css"
  )
}
