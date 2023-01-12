#' PPGISr_base_map
#'
#' @description function to output a useable basemap. This checks the type of object fed in,
## reads it if a filepath or takes it as-is if an object, and then reprojects
## it to WGS 84 coordinates for display in leaflet.
#'
#' @return sf or raster object with WGS 84 CRS
#'
#' @noRd
PPGISr_base_map <- function (path_or_object){

  # if a filepath is supplied
  if (class(path_or_object)[1] == 'character'){
    # if tif
    if (tools::file_ext(path_or_object) == 'tif'){
      raster_bmap <- raster::raster(path_or_object) %>% raster::projectRaster(crs = sf::st_crs(4326)[[2]])
      return(raster_bmap)
    } else if (tools::file_ext(path_or_object) %in% c('shp', 'geojson', 'gpkg')){ # if vector
      vector_bmap <- sf::st_read(path_or_object) %>% sf::st_transform(4326)
      return(vector_bmap)
    } else {
      warning('Basemap must be a .tif, .shp, .geojson, or .gpkg file, or be suppled as a raster or sf object. Defaulting to NULL.')
      return(NULL)
    }
  } else if (class(path_or_object)[1] == 'RasterLayer'){  # if a raster is supplied
    raster_bmap <- path_or_object %>% raster::projectRaster(crs = sf::st_crs(4326)[[2]])
    return(raster_bmap)
  } else if(class(path_or_object)[1] == 'sf'){  # if a vector map is supplied
    vector_bmap <- path_or_object %>% sf::st_transform(4326)
    return(vector_bmap)
  } else if(is.null(path_or_object) | isFALSE(path_or_object)) {
    return(NULL)
  } else {
    warning('Basemap must be a .tif, .shp, .geojson, or .gpkg file, or be suppled as a raster or sf object. Defaulting to NULL.')
    return(NULL)
  }
}
