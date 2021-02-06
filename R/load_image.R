
#' Load image
#'
#' @param path String. Path to file.
#'
#' @return Cimg object.
#'
#' @importFrom imager load.image
#' @export
#'
#' @examples
#' path <- system.file("./images/medicine.jpg", package = "mser4r")
#' image <- load_image(path)
#'
load_image <- function(path) {
  load.image(file = path)
}
