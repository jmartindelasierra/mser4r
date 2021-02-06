
#' Image preparation
#'
#' @param image Cimg object.
#' @param scale Numeric.
#'
#' @return Cimg object.
#'
#' @importFrom magrittr %>%
#' @importFrom imager rm.alpha width height renorm imresize spectrum grayscale
#' @importFrom stats uniroot
#'
#' @noRd
#'
#' @examples
#'
image_prep <- function(image,
                       scale) {

  stopifnot(is.cimg(image),
            is.numeric(scale))

  # Remove alpha channel if exists
  image <- image %>%
    rm.alpha()

  # Autoscale
  if (scale == -1) {

    # Function to optimize
    optimize_scale <- function(x, obj) {
      # ratio <- width(image) / height(image)
      x^2 * width(image) * height(image) - obj
    }

    # Root (value of x such that f(x) = 0)
    # obj = 300 * 300 = 90000
    scale <- uniroot(f = optimize_scale, interval = c(0, 1), obj = 90000)$root

    message("Scale factor: ", round(scale, 3), " (auto)")
  }

  # Resize with scale factor
  image <- imresize(image, scale)

  # If more than one channel, convert to grayscale
  if (spectrum(image) > 1) {
    image <- image %>%
      grayscale()
  }

  # Set range of values
  image <- image %>%
    renorm(0, 255)

  # Trick to decrease memory usage
  mode(image) <- "integer"

  message("Output: ", width(image), "x", height(image))

  image
}
