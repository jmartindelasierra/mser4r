
#' Connect regions
#'
#' @param regions Regions object from extract_regions().
#'
#' @return
#'
#' @importFrom igraph make_empty_graph V add_vertices add_edges layout_as_tree
#'
#' @noRd
#'
#' @examples
#'
connect_regions <- function(regions) {

  stopifnot(is.list(regions), length(regions) >= 2)

  message("Connecting regions... ", appendLF = FALSE)

  # Graph initialization
  graph <- make_empty_graph(directed = TRUE)

  # For each level starting at the second
  for (i in 2:length(regions)) {

    # Possible combinations
    regions_comb <- expand.grid(reg_from_lvl1 = 1:length(regions[[i - 1]]),
                                reg_from_lvl2 = 1:length(regions[[i]]))

    # For each row in region combinations
    for (j in 1:nrow(regions_comb)) {
      if (any(regions[[i - 1]][[regions_comb[j, 1]]] &
              regions[[i]][[regions_comb[j, 2]]])) {

        # Initial and final areas
        area_from <- regions[[i - 1]][[regions_comb[j, 1]]] %>% sum()
        area_to <- regions[[i]][[regions_comb[j, 2]]] %>% sum()

        # Area variation (not used for MSER)
        area_var <- (area_to - area_from) / area_from

        # Region IDs
        region_from <- paste0("L", i - 1, "R", regions_comb[j, 1])
        region_to <- paste0("L", i, "R", regions_comb[j, 2])

        # Add start node
        if (!region_from %in% V(graph)$name) {
          graph <- graph %>%
            add_vertices(nv = 1, name = region_from, area = area_from)
        }

        # Add end node
        if (!region_to %in% V(graph)$name) {
          graph <- graph %>%
            add_vertices(nv = 1, name = region_to, area = area_to)
        }

        # Add edge
        graph <- graph %>%
          add_edges(c(region_from, region_to), area_var = area_var)
      }
    }
  }

  message("done")

  graph
}
