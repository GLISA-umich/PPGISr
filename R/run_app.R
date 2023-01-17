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
#' @importFrom stats setNames
#'
#' @param editable_map A vector file
#' @param base_map A base map
#' @param basemap_name Name of your imported base map
#' @param mapping_categories Names of your chosen categories
#' @param mapping_colors Supplied as hex values
#' @param editable_map_info_icon_message Message for your editable map
#' @param basemap_info_icon_message Message for baseman info button
#'
#' @examples
#' \dontrun{
#'
#' run_app()
#'
#' run_app(mapping_categories = c("flowers", "lake", "trees"))
#'
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
  editable_map_info_icon_message = paste0(
    "Click the circle to left to choose mapping categories you want to add",
    " to the map. Click the map to indicate these preferences"
    ),
  basemap_info_icon_message =
    "This allows you to view data for making your mapping decisions",
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
    editable_map = editable_map,
    base_map = base_map,
    basemap_name = basemap_name,
    mapping_categories = stats::setNames(c(NA, 1:length(mapping_categories)), c("No Category", mapping_categories)),
    mapping_colors = mapping_colors,
    editable_map_info_icon_message = editable_map_info_icon_message,
    basemap_info_icon_message = basemap_info_icon_message
))}
