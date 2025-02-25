#' @title Contrast Text Color
#' @description Calculates the best contrast text color for a given
#' background color.
#' @param background A hex/named color value that represents the background.
#' @param light_text A hex/named color value that represents the light text
#' color.
#' @param dark_text A hex/named color value that represents the dark text color.
#' @param threshold A numeric value between 0 and 1 that is used to determine
#' the luminance threshold of the background color for text color.
#' @param method A character string that specifies the method for calculating
#' the luminance. Three different methods are available:
#' c("relative","perceived","perceived_2")
#' @param ... parameter overflow. Ignored.
#' @details
#' This function aids in deciding the font color to print on a given background.
#' The function is based on the example provided by teppo:
#' https://stackoverflow.com/a/66669838/21019325.
#' The different methods provided are based on the methods outlined in the
#' StackOverflow thread:
#' https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color
#' @return A character string that contains the best contrast text color.
#' @examples
#' contrast_text(c("#F2F2F2", "blue"))
#'
#' contrast_text(c("#F2F2F2", "blue"), method="relative")
#' @export
#'
#' @importFrom grDevices col2rgb
#'
contrast_text <- function(background,
                          light_text = 'white',
                          dark_text = 'black',
                          threshold = 0.5,
                          method = "perceived_2",
                          ...) {
  if (method == "relative") {
    luminance <-
      c(c(.2126, .7152, .0722) %*% grDevices::col2rgb(background) / 255)
  } else if (method == "perceived") {
    luminance <-
      c(c(.299, .587, .114) %*% grDevices::col2rgb(background) / 255)
  } else if (method == "perceived_2") {
    luminance <- c(sqrt(colSums((
      c(.299, .587, .114) * grDevices::col2rgb(background)
    ) ^ 2)) / 255)
  }

  ifelse(luminance < threshold,
         light_text,
         dark_text)
}
