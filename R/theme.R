#' Custom theme based on unity
#'
#' @param ... everything passed on to bslib::bs_theme()
#'
#' @returns theme list
#' @export
custom_theme <- function(...,
                         version = 5,
                         primary = FreesearchR_colors("primary"),
                         secondary = FreesearchR_colors("secondary"),
                         bootswatch = "united",
                         base_font = bslib::font_google("Montserrat"),
                         heading_font = bslib::font_google("Public Sans", wght = "700"),
                         code_font = bslib::font_google("Open Sans"),
                         success = FreesearchR_colors("success"),
                         info = FreesearchR_colors("info"),
                         warning = FreesearchR_colors("warning"),
                         danger = FreesearchR_colors("danger")
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
    code_font = code_font,
    success=success,
    info=info,
    warning=warning,
    danger=danger
  )
}

FreesearchR_colors <- function(choose = NULL) {
  out <- c(
    primary = "#1E4A8F",
    secondary = "#FF6F61",
    success = "#00C896",
    warning = "#FFB100",
    danger = "#CC2E25",
    extra = "#8A4FFF",
    info = "#11A0EC",
    bg = "#FFFFFF",
    dark = "#2D2D42",
    fg = "#000000"
  )
  if (!is.null(choose)) {
    unname(out[choose])
  } else {
    out
  }
}

#' Use the FreesearchR colors
#'
#' @param n number of colors
#'
#' @returns character vector
#' @export
#'
#' @examples
#' FreesearchR_palette(n=7)
FreesearchR_palette <- function(n){
  rep_len(FreesearchR_colors(),n)
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
