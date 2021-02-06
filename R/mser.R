
#' Maximally Stable Extremal Regions
#'
#' @param image Cimg object. Input image.
#' @param scale Numeric. Scale factor for image resizing. When scale = -1, the scale factor is automatically computed such that the image is ~90000 pixels (this is 300x300 for a square image). Larger images result in longer computation. Default: -1.
#' @param steps Numeric. Number of steps in the sequence from 0 to 255. This determines the intensity levels where the region extraction takes place.
#' @param min_area Numeric. Minimum area in pixels to be considered a region.
#' @param max_area Numeric. Maximum area in pixels to be considered a region.
#' @param polarity String. Use "positive" to extract homogeneous areas with darker boundaries, "negative" to extract homogeneous areas with brighter boundaries, or "both" for both "positive" and "negative". Default: "positive".
#' @param max_psi Numeric. Stability score to determine whether a region is stable or not. Recommended values go from 0.2 to 1. Default: 0.2.
#' @param merge Logical. If TRUE, all the regions are merged into one single region. Default: TRUE.
#'
#' @return Cimg or imlist object.
#'
#' @importFrom imager is.cimg
#' @export
#'
#' @examples
#' image <- load_example()
#' regions <- mser(image, max_area = 2000)
#' plot_regions(regions, type = "mask", image_bg = image)
#'
mser <- function(image,
                 scale = -1,
                 steps = 50,
                 min_area = 10,
                 max_area = 10000,
                 polarity = "positive",
                 max_psi = 0.2,
                 merge = TRUE) {

  stopifnot(is.cimg(image),
            is.numeric(scale),
            is.character(polarity),
            is.numeric(max_psi),
            is.logical(merge))

  if (min_area >= max_area) {
    stop("min_area must be smaller than max_area.", call. = FALSE)
  }

  if (!polarity %in% c("positive", "negative", "both")) {
    stop("Allowed values for polarity are 'positive', 'negative' and 'both'.", call. = FALSE)
  }

  if (max_psi < 0) {
    stop("max_psi must be greater than 0", call. = FALSE)
  }

  # Image preparation
  image_pr <- image_prep(image, scale = scale)

  # Regions extraction
  regions <- extract_regions(image_pr,
                             steps = steps,
                             min_area = min_area,
                             max_area = max_area,
                             polarity = polarity)

  # Regions connection
  tree <- connect_regions(regions)

  # Stable regions extraction
  regions <- extract_stables(regions, tree, max_psi = max_psi, merge = merge)

  # Merge all regions into one
  if (merge) {
    regions <- resize_regions(regions, image)
  }

  regions
}
