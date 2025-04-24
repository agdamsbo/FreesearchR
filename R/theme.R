#' Custom theme based on unity
#'
#' @param ... everything passed on to bslib::bs_theme()
#'
#' @returns theme list
#' @export
custom_theme <- function(...,
                         version = 5,
                         primary = "#1E4A8F",
                         secondary = "#FF6F61",
                         bootswatch = "united",
                         base_font = bslib::font_google("Montserrat"),
                         heading_font = bslib::font_google("Public Sans", wght = "700"),
                         code_font = bslib::font_google("Open Sans")
                         # success = "#1E4A8F",
                         # info = ,
                         # warning = ,
                         # danger = ,
                         # fg = "#000",
                         # bg="#fff",
                         # base_font = bslib::font_google("Alice"),
                         # heading_font = bslib::font_google("Jost", wght = "800"),
                         # heading_font = bslib::font_google("Noto Serif"),
                         # heading_font = bslib::font_google("Alice"),
) {
  bslib::bs_theme(
    ...,
    "navbar-bg" = primary,
    version = version,
    primary = primary,
    secondary = secondary,
    bootswatch = bootswatch,
    base_font = base_font,
    heading_font = heading_font,
    code_font = code_font
  )
}

compliment_colors <- function() {
  c(
    "#00C896",
    "#FFB100",
    "#8A4FFF",
    "#11A0EC"
  )
}



#' GGplot default theme for plotting in Shiny
#'
#' @param data ggplot object
#'
#' @returns ggplot object
#' @export
#'
gg_theme_shiny <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 18),
    axis.text = ggplot2::element_text(size = 14),
    strip.text = ggplot2::element_text(size = 14),
    legend.title = ggplot2::element_text(size = 18),
    legend.text = ggplot2::element_text(size = 14),
    plot.title = ggplot2::element_text(size = 24),
    plot.subtitle = ggplot2::element_text(size = 18)
  )
}


#' GGplot default theme for plotting export objects
#'
#' @param data ggplot object
#'
#' @returns ggplot object
#' @export
#'
gg_theme_export <- function() {
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 18),
    axis.text.x = ggplot2::element_text(size = 14),
    legend.title = ggplot2::element_text(size = 18),
    legend.text = ggplot2::element_text(size = 14),
    plot.title = ggplot2::element_text(size = 24)
  )
}
