#' PPGISr_editable_map
#'
#' @description ## function to output a useable editable map This checks the type of object fed
## in, reads it if a filepath or takes it as-is if an object, and then processes
## it with the right attributes and coordinates for use as a clickable map.
#'
#' @return VECTOR_FILE sf object with WGS 84 CRS
#'
#' @noRd

PPGISr_editable_map <- function (path_or_object){

  if (class(path_or_object)[1] == 'character'){  # if a filepath is supplied
    if (tools::file_ext(path_or_object) %in% c('shp', 'geojson', 'gpkg')){
      VECTOR_FILE <- sf::st_read(path_or_object) %>%
        dplyr::mutate(PPGIS_CODE = as.character(dplyr::row_number()),SELECTED = NA) %>%
        dplyr::select(PPGIS_CODE, SELECTED, geometry) %>%
        sf::st_transform(4326)
    } else {  # if file extension is invalid, use default instead and show a warning in the console
      warning('Editable map must be a .shp, .geojson, or .gpkg file, or be suppled as a sf object. Using default instead.')
      VECTOR_FILE <- PPGISr::duluthEditablemap %>%
        dplyr::mutate(PPGIS_CODE = as.character(dplyr::row_number()),SELECTED = NA) %>%
        dplyr::select(PPGIS_CODE, SELECTED, geometry) %>%
        sf::st_transform(4326)
    }
  } else if (class(path_or_object)[1] == 'sf'){  # if supplied with a sf object
    VECTOR_FILE <- path_or_object %>%
      dplyr::mutate(PPGIS_CODE = as.character(dplyr::row_number()),SELECTED = NA) %>%
      dplyr::select(PPGIS_CODE, SELECTED, geometry) %>%
      sf::st_transform(4326)
  } else {  # if supplied with something else
    warning('Editable map must be a .shp, .geojson, or .gpkg file, or be suppled as a sf object. Using default file instead.')
    VECTOR_FILE <- PPGISr::duluthEditablemap %>%
      dplyr::mutate(PPGIS_CODE = as.character(dplyr::row_number()),SELECTED = NA) %>%
      dplyr::select(PPGIS_CODE, SELECTED, geometry) %>%
      sf::st_transform(4326)
  }

  return(VECTOR_FILE)
}
