
#' Extract regions
#'
#' @param image Cimg object from image_prep().
#' @param steps Numeric.
#' @param min_area Numeric.
#' @param max_area Numeric.
#' @param polarity String.
#'
#' @return
#'
#' @importFrom imager is.cimg nPix threshold split_connected
#' @importFrom purrr keep flatten
#' @importFrom stats median
#'
#' @noRd
#'
#' @examples
#'
extract_regions <- function(image,
                            steps,
                            min_area,
                            max_area,
                            polarity) {

  stopifnot(is.cimg(image),
            is.numeric(steps),
            is.numeric(min_area),
            is.numeric(max_area),
            is.character(polarity))

  # Intensity levels according to the number of steps in the sequence
  thr_steps <- seq(from = 0, to = 255, length.out = steps) %>%
    as.integer()

  message("Thr. steps: ", steps, " (Delta ~ ", median(diff(thr_steps)), ")")
  message("Min. area: ", min_area, " (", round((min_area / nPix(image)) * 100, 2), "%)")
  message("Max. area: ", max_area, " (", round((max_area / nPix(image)) * 100, 2), "%)")

  regions <- list()

  if (polarity %in% c("positive", "both")) {
    message("Extracting positive regions... ", appendLF = FALSE)

    for (t in thr_steps) {
      image_th <- image %>%
        threshold(t)

      if (sum(image_th) > 0) {
        image_sp <- image_th %>%
          split_connected() %>%
          keep(~sum(.x) >= min_area) %>%
          keep(~sum(.x) <= max_area)

        if (length(image_sp) > 0) {
          regions[[length(regions) + 1]] <- image_sp
        }
      }
    }
    message("done")
  }

  if (polarity %in% c("negative", "both")) {
    image <- abs(image - 255)

    message("Extracting negative regions... ", appendLF = FALSE)

    for (t in thr_steps) {
      image_th <- image %>%
        threshold(t)

      if (sum(image_th) > 0) {
        image_sp <- image_th %>%
          split_connected() %>%
          keep(~sum(.x) >= min_area) %>%
          keep(~sum(.x) <= max_area)

        if (length(image_sp) > 0) {
          regions[[length(regions) + 1]] <- image_sp
        }
      }
    }
    message("done")
  }

  message("Extracted regions: ", length(flatten(regions)))

  regions
}
