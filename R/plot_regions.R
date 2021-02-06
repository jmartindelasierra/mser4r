
#' Plot merged regions
#'
#' @param regions Regions object from mser() when merge = TRUE.
#' @param type String. Possible values are "border", "solid" or "mask".
#' @param image_bg Cimg object. Original image.
#'
#' @return Plot.
#'
#' @importFrom imager imfill highlight colorise mult add.color isoblur imlist
#' @export
#'
#' @examples
#' image <- load_example()
#' regions <- mser(image, max_area = 2000)
#' plot_regions(regions, type = "mask", image_bg = image)
#'
plot_regions <- function(regions,
                         type = "solid",
                         image_bg = NULL) {

  if (is.list(regions)) {
    stop("'regions' can't be a list. Consider 'merge = TRUE' in mser().", call. = FALSE)
  }

  stopifnot(is.cimg(regions), is.character(type))

  if (!is.null(image_bg)) {
    if (width(image_bg) * height(image_bg) != width(regions) * height(regions)) {
      stop("Regions and image sizes don't match.", call. = FALSE)
    }
  }

  # Solid
  if (type == "solid") {
    if (is.null(image_bg)) {
      plot(regions, axes = FALSE, interpolate = FALSE)
    } else {
      colorise(image_bg,
               regions %>% add.color() %>% isoblur(2),
               col = "red", alpha = 0.5) %>%
        plot(axes = FALSE, interpolate = FALSE)
    }
  }

  # Border
  if (type == "border") {
    if (is.null(image_bg)) {
      plot(imfill(x = width(regions),
                  y = height(regions)),
           axes = FALSE, interpolate = FALSE)
    } else {
      plot(image_bg, axes = FALSE, interpolate = FALSE)
    }
    highlight(regions)
  }

  # Mask
  if (type == "mask") {
    if(is.null(image_bg)) {
      message("The background image must be provided.")
    } else {
      mult(imlist(image_bg, regions %>% add.color() %>% isoblur(2))) %>%
        plot(axes = FALSE, interpolate = FALSE)
    }
  }

}
