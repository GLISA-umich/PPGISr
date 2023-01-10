#' createMap
#'
#' @description A function to return a basemap in PPGISr
#'
#' @return The return value, if any, from executing the function.
#' @
#' @noRd
createMap <- function() {
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
    fitBounds(base_map_bounds[1], base_map_bounds[2], base_map_bounds[3], base_map_bounds[4])
  return(m)
}
