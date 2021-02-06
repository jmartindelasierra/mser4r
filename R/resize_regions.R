
#' Returns region object to original image size
#'
#' @param regions Regions object from extract_stable(merge = TRUE).
#' @param image CImg object.
#'
#' @return
#'
#' @importFrom imager resize
#'
#' @noRd
#'
#' @examples
#'
resize_regions <- function(regions, image_ref) {

  message("Resizing regions... ", appendLF = FALSE)

  # Rescale regions to the original image size
  regions <- resize(regions, size_x = width(image_ref), size_y = height(image_ref))

  message("done")

  regions
}
