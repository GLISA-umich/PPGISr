#' A function to run the PPGISr Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom sf st_read
#' @importFrom golem with_golem_options
#' @importFrom stats setNames
#'
#' @param editable_map A vector file (e.g., multipolygon, polygon)
#' @param base_map A vector (e.g., multipolygon, polygon) or raster (i.e. tif, grid) file for visualization
#' @param basemap_name The legend name given to your imported base map
#' @param mapping_categories Vector of names for mapping categories options (i.e. "Trees", "Flowers","Water")
#' @param mapping_colors Vector of colors corresponding to vector of names (i.e. "green", "red","blue")
#' @param editable_map_info_icon_message Character string of the message for your editable info button
#' @param basemap_info_icon_message Character string of the message for basemap info button
#'
#' @examples
#' \dontrun{
#'
#' PPGISr::run_app()
#' ## Customize your PPGISr interface by adding mapping categories
#' run_app(mapping_categories = c("flowers", "lake", "trees"))
#'
#' ## Customize your PPGISr interface by specifying colors for your mapping categories
#' ## This must be a vector of the same length as the mapping categories
#' run_app(mapping_categories = c("flowers", "lake", "trees"),
#'         mapping_colors = c("green", "red", "blue"))
#'
#' ## Customize your PPGISr interface by specifying an editable map
#' ## We recommend using the sf library to load spatial vector data
#' library(sf)
#' ## Spatial data should use World Geodetic System 1984 (WGS84) coordinate reference system(crs)
#' editable_map <- st_read(system.file("shape/nc.shp", package="sf"))
#'
#' run_app(editable_map = editable_map, base_map = NULL)
#' }
#'
#'
run_app <- function(
  editable_map = PPGISr::duluthEditablemap,
  base_map = PPGISr::duluthBasemap,
  basemap_name = 'Reference Basemap',
  mapping_categories = c("High Density Development",
                         "Street Trees",
                         "Infrastructure Need"),
  mapping_colors = c("#880015",
                      "#22b14c",
                      "#00a2e8"),
  editable_map_info_icon_message =
    "Click the circle to left to choose mapping categories you want to add to the map. Click the map to indicate these preferences",
  basemap_info_icon_message =
    "This allows you to view data for making your mapping decisions",
  #onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      #onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
    editable_map = editable_map,
    base_map = base_map,
    basemap_name = basemap_name,
    mapping_categories = stats::setNames(c(NA, 1:length(mapping_categories)), c("No Category", mapping_categories)),
    mapping_colors = mapping_colors,
    editable_map_info_icon_message = editable_map_info_icon_message,
    basemap_info_icon_message = basemap_info_icon_message
))}
