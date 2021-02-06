
#' Load example
#'
#' @return Example cimg image.
#' @export
#'
#' @examples
#' image <- load_example()
#'
load_example <- function() {
  path <- system.file("./images/can.jpg", package = "mser4r")
  load.image(file = path)
}
