html_dependency_FreesearchR <- function() {
  htmltools::htmlDependency(
    name = "FreesearchR",
    version = packageVersion("FreesearchR"),
    src = list(href = "FreesearchR", file = "assets"),
    package = "FreesearchR",
    stylesheet = "css/FreesearchR.css"
  )
}
