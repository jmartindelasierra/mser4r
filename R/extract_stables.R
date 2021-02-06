
#' Extract stable regions
#'
#' @param regions Regions object from extract_regions().
#' @param graph igraph object from connect_regions().
#' @param max_psi Numeric.
#' @param merge Logical.
#'
#' @return
#'
#' @importFrom igraph neighbors all_shortest_paths V<-
#' @importFrom stringr str_extract_all
#' @importFrom imager as.imlist add
#'
#' @noRd
#'
#' @examples
#'
extract_stables <- function(regions, graph, max_psi, merge) {

  message("Extracting maximally stable regions... ", appendLF = FALSE)

  roots <- names(which(sapply(V(graph), function(x) {
    length(neighbors(graph, x, mode = "in"))
  }) == 0))

  leaves <- names(which(sapply(V(graph), function(x) {
    length(neighbors(graph, x, mode = "out"))
  }) == 0))

  # Region labels
  stables <- list()

  for (r in 1:length(roots)) {

    # Shortest path from each root to a leaf
    paths <- all_shortest_paths(graph, from = V(graph)[roots[r]], to = leaves)$res

    # Three nodes required to compute stability
    paths <- keep(paths[], ~length(.x) > 2)

    # Paths have at least three nodes
    if (length(paths) > 0) {
      for (i in 1:length(paths)) {
        for (j in 2:(length(V(graph)[paths[[i]]]) -1)) {
          # Stability score as in the original paper
          psi <- (V(graph)[paths[[i]]]$area[j - 1] - V(graph)[paths[[i]]]$area[j + 1]) / V(graph)[paths[[i]]]$area[j]
          V(graph)[V(graph)[paths[[i]]]$name[j]]$psi <- psi

          # # Area variation in %. Only valid for small psi.
          # area_var <- (V(graph)[paths[[i]]]$area[j + 1] - V(graph)[paths[[i]]]$area[j - 1]) / V(graph)[paths[[i]]]$area[j - 1]
          # V(graph)[V(graph)[paths[[i]]]$name[j]]$area_var <- area_var
        }

        # Stable if below the max. stability score
        if (min(V(graph)[V(graph)[paths[[i]]]]$psi, na.rm = TRUE) < max_psi) {
          stables[[length(stables) + 1]] <- V(graph)[V(graph)[paths[[i]]]]$name[which.min(V(graph)[V(graph)[paths[[i]]]]$psi)]
        }

        # # Stable if below the max. area variation. Only valid for small psi.
        # if(min(abs(V(graph)[V(graph)[paths[[i]]]]$area_var), na.rm = TRUE) < max_var) {
        #   stables[[length(stables) + 1]] <- V(graph)[V(graph)[paths[[i]]]]$name[which.min(abs(V(graph)[V(graph)[paths[[i]]]]$area_var))]
        # }
      }
    }
  }


  if (length(stables) == 0) {
    stop("No stable regions detected", call. = FALSE)
  }

  # Do not repeat regions
  stables <- stables %>%
    unlist() %>%
    unique()

  # Level - region IDs extraction
  region_ids <- sapply(stables, function(x) {
    str_extract_all(x, "\\d+")
  })

  region_ids <- unlist(region_ids) %>%
    as.integer() %>%
    matrix(ncol = 2, byrow = TRUE)

  # Pixset list
  stables <- list()

  # Extract the pixset by ID
  for (i in 1:nrow(region_ids)) {
    stables[[i]] <- regions[[region_ids[i, 1]]][[region_ids[i, 2]]]
  }

  stables <- stables %>%
    as.imlist()

  message("done")
  message("Maximally stable regions: ", length(stables))

  if (merge) {
    stables <- stables %>%
      add() %>%
      threshold(0)
    message("Regions merged")
  }

  stables
}
