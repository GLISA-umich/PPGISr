#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom sf st_read
#' @importFrom golem with_golem_options
#' @importFrom stats setnames
run_app <- function(
  editable_map = PPGISr::duluthEditablemap %>%
    dplyr::mutate(PPGIS_CODE = as.character(dplyr::row_number()),SELECTED = NA) %>%
    dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
    sf::st_transform(4326),
  base_map = PPGISr::duluthBasemap,
  mapping_categories = c("High Density Development", "Street Trees", "Infrastructure Need"),
  mapping_colors = c("#880015", "#22b14c", "#00a2e8"),
  editable_map_info_icon_message = "Click the circle to left to choose mapping categories you want to add to the map. Click the map to indicate these preferences",
  basemap_info_icon_message = "This allows you to view data for making your mapping decisions",
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
    editable_map = editable_map %>%
      dplyr::mutate(PPGIS_CODE = as.character(dplyr::row_number()),SELECTED = NA) %>%
      dplyr::select(PPGIS_CODE, SELECTED, geometry) %>% ## everything()
      sf::st_transform(4326),
    base_map = base_map,
    mapping_categories = stats::setNames(c(NA, 1:length(mapping_categories)), c("No Category", mapping_categories)),
    mapping_colors = mapping_colors,
    editable_map_info_icon_message = editable_map_info_icon_message,
    basemap_info_icon_message = basemap_info_icon_message
))}
