#' Generate N Colors from a Specified Color Palette
#'
#' A flexible wrapper around multiple color palette libraries, returning N
#' colors as a character vector of hex codes. Supports palettes from
#' \pkg{viridisLite}, base R \pkg{grDevices}, and \pkg{RColorBrewer}.
#'
#' @param n \code{integer}. Number of colors to generate. Must be a positive
#'   integer.
#' @param palette \code{character(1)}. Name of the color palette to use.
#'   Case-insensitive. Supported options:
#'   \describe{
#'     \item{\strong{viridisLite}}{`"viridis"`, `"magma"`, `"plasma"`,
#'       `"inferno"`, `"cividis"`, `"mako"`, `"rocket"`, `"turbo"`}
#'     \item{\strong{grDevices}}{`"hcl"`, `"rainbow"`, `"heat"`,
#'       `"terrain"`, `"topo"`}
#'     \item{\strong{RColorBrewer}}{Any palette name from
#'       \code{RColorBrewer::brewer.pal.info}, e.g. `"Set1"`, `"Blues"`,
#'       `"Dark2"`. If \code{n} exceeds the palette maximum, colors are
#'       interpolated via \code{\link[grDevices]{colorRampPalette}}.}
#'   }
#' @param ... Additional arguments passed to the underlying palette function.
#'   For example, \code{alpha}, \code{direction}, \code{begin}, \code{end}
#'   are forwarded to \code{\link[viridisLite]{viridis}}; \code{palette} is
#'   forwarded to \code{\link[grDevices]{hcl.colors}}.
#'
#' @return A \code{character} vector of length \code{n} containing hex color
#'   codes (e.g. \code{"#440154FF"}).
#'
#' @examples
#' # viridisLite palettes
#' generate_colors(5, "viridis")
#' generate_colors(5, "plasma")
#' generate_colors(5, "viridis", alpha = 0.8, direction = -1)
#'
#' # Base R grDevices
#' generate_colors(5, "rainbow")
#' generate_colors(8, "hcl", palette = "Dark 3")
#'
#' # RColorBrewer
#' generate_colors(5, "Set1")
#' generate_colors(5, "Blues")
#' generate_colors(12, "Set1")  # interpolates beyond palette max of 9
#'
#' # Drop-in replacement for viridisLite::viridis()
#' # generate_colors(n = length(levels(data_orig[[pri]])), palette = "viridis")
#'
#' @seealso
#' \code{\link[viridisLite]{viridis}},
#' \code{\link[grDevices]{hcl.colors}},
#' \code{\link[RColorBrewer]{brewer.pal}}
#'
#' @importFrom viridisLite viridis
#' @importFrom grDevices hcl.colors rainbow heat.colors terrain.colors
#'   topo.colors colorRampPalette
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#'
#' @export
generate_colors <- function(n, palette = "viridis", ...) {
  if (!is.numeric(n) ||
      length(n) != 1 || n < 1 || n != as.integer(n)) {
    stop("`n` must be a single positive integer.")
  }

  # Function passthrough — call directly with n and ...
  if (is.function(palette)) {
    return(palette(n, ...))
  }

  if (!is.character(palette) || length(palette) != 1) {
    stop("`palette` must be a single character string or a function.")
  }

  if (!is.numeric(n) ||
      length(n) != 1 || n < 1 || n != as.integer(n)) {
    stop("`n` must be a single positive integer.")
  }
  if (!is.character(palette) || length(palette) != 1) {
    stop("`palette` must be a single character string.")
  }

  palette_lower <- tolower(palette)

  viridis_palettes <- c("viridis",
                        "magma",
                        "plasma",
                        "inferno",
                        "cividis",
                        "mako",
                        "rocket",
                        "turbo")

  if (palette_lower %in% viridis_palettes) {
    viridisLite::viridis(n = n, option = palette_lower, ...)

  } else if (palette_lower == "hcl") {
    grDevices::hcl.colors(n = n, ...)

  } else if (palette_lower == "rainbow") {
    grDevices::rainbow(n = n, ...)

  } else if (palette_lower == "heat") {
    grDevices::heat.colors(n = n, ...)

  } else if (palette_lower == "terrain") {
    grDevices::terrain.colors(n = n, ...)

  } else if (palette_lower == "topo") {
    grDevices::topo.colors(n = n, ...)

  } else if (palette %in% rownames(RColorBrewer::brewer.pal.info)) {
    max_n       <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
    fetch_n     <- max(min(n, max_n), 3L)          # clamp to [3, max_n] for brewer.pal()
    base_colors <- RColorBrewer::brewer.pal(n = fetch_n, name = palette)
    grDevices::colorRampPalette(base_colors)(n)

  } else if (palette %in% grDevices::palette.pals()) {
    grDevices::colorRampPalette(palette.colors(palette = palette))(n)

  } else if (palette %in% grDevices::hcl.pals()) {
    grDevices::hcl.colors(n = n, palette = palette, ...)

  } else {
    message(
      paste0(
        "Unknown palette: '",
        palette,
        "'. ",
        "Falling back to default R colors.\n",
        "Available options:\n",
        "  viridisLite  : viridis, magma, plasma, inferno, cividis, mako, rocket, turbo\n",
        "  grDevices    : hcl, rainbow, heat, terrain, topo\n",
        "  grDevices HCL: use grDevices::hcl.pals() to see all options\n",
        "  grDevices    : use grDevices::palette.pals() to see all options\n",
        "  RColorBrewer : use RColorBrewer::brewer.pal.info to see all options"
      )
    )
    viridisLite::viridis(n = n, option = "viridis")
    # grDevices::hcl.colors(n = n)
  }
}


#' Create a Continuous Color Function from a Palette
#'
#' Wraps \code{\link{generate_colors}} into a function that accepts a value
#' between 0 and 1 and returns the corresponding color. Useful for mapping
#' continuous variables to colors.
#'
#' @param palette Passed directly to [generate_colors()]. Either a palette
#'   name string or a function.
#' @param n \code{integer}. Resolution of the underlying color ramp — higher
#'   values give smoother gradients. Defaults to 256.
#' @param ... Additional arguments passed to [generate_colors()].
#'
#' @return A function that takes a numeric vector of values in \code{[0, 1]}
#'   and returns a character vector of hex colors.
#'
#' @examples
#' pal <- continuous_colors("viridis")
#' pal(0)    # first color
#' pal(1)    # last color
#' pal(0.5)  # midpoint
#'
#' # Map a continuous variable to colors
#' values <- seq(0, 1, length.out = 10)
#' pal(values)
#'
#' # Works with any palette generate_colors() accepts
#' pal <- continuous_colors("plasma", direction = -1)
#' pal <- continuous_colors(\(n) hcl.colors(n, palette = "Blue-Red"))
#'
#' @seealso [generate_colors()]
#' @export
continuous_colors <- function(palette = "viridis", n = 256, ...) {
  colors <- generate_colors(n, palette, ...)
  ramp   <- grDevices::colorRamp(colors)

  function(x) {
    if (any(x < 0 |
            x > 1, na.rm = TRUE))
      stop("Values must be in [0, 1].")
    rgb_vals <- ramp(x)
    grDevices::rgb(rgb_vals[, 1], rgb_vals[, 2], rgb_vals[, 3], maxColorValue = 255)
  }
}


#' Discrete and Continuous Fill Scale Using generate_colors
#'
#' Drop-in replacement for [viridis::scale_fill_viridis()] that works with
#' any palette supported by [generate_colors()].
#'
#' @param palette Passed to [generate_colors()]. Either a palette name string
#'   or a function.
#' @param discrete \code{logical}. If \code{TRUE} (default), a discrete scale
#'   is returned. If \code{FALSE}, a continuous scale is returned.
#' @param ... Additional arguments passed to [ggplot2::scale_fill_manual()]
#'   (discrete) or [ggplot2::scale_fill_gradientn()] (continuous).
#'
#' @examples
#' library(ggplot2)
#'
#' # Discrete
#' ggplot(mtcars, aes(x = wt, y = mpg, fill = factor(cyl))) +
#'   geom_col() +
#'   scale_fill_generate(palette = "Set1")
#'
#' # Continuous
#' ggplot(mtcars, aes(x = wt, y = mpg, fill = mpg)) +
#'   geom_point(shape = 21, size = 3) +
#'   scale_fill_generate(palette = "viridis", discrete = FALSE)
#'
#' @seealso [scale_color_generate()], [generate_colors()], [continuous_colors()]
#' @export
scale_fill_generate <- function(palette = "viridis",
                                discrete = TRUE,
                                ...) {
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "fill",
      palette    = function(n)
        generate_colors(n, palette),
      ...
    )
  } else {
    ggplot2::scale_fill_gradientn(colors = continuous_colors(palette)(seq(0, 1, length.out = 256)), ...)
  }
}

#' @rdname scale_fill_generate
#' @examples
#' ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
#'   geom_point() +
#'   scale_color_generate(palette = "Set1")
#' @export
scale_color_generate <- function(palette = "viridis",
                                 discrete = TRUE,
                                 ...) {
  if (discrete) {
    ggplot2::discrete_scale(
      aesthetics = "colour",
      palette    = function(n)
        generate_colors(n, palette),
      ...
    )
  } else {
    ggplot2::scale_color_gradientn(colors = continuous_colors(palette)(seq(0, 1, length.out = 256)), ...)
  }
}


color_choices <- function() {
  c(
    "Perceptual (blue-yellow)"   = "viridis",
    "Perceptual (fire)"          = "plasma",
    "Colour-blind friendly"      = "Okabe-Ito",
    "Diverging (red-yellow-green)"= "RdYlGn",
    "Diverging (red-blue)"       = "RdBu",
    "Sequential (blues)"         = "Blues",
    "Qualitative (paired)"       = "Paired",
    "Qualitative (bold)"         = "Dark 2",
    "Rainbow"                    = "Spectral",
    "Generic"                     = "Set1"
  )
}
