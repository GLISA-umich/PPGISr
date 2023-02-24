#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom sf st_write
#' @import  leaflet
#' @importFrom shinyscreenshot screenshot
#' @importFrom zip zipr
#' @importFrom shinyWidgets awesomeRadio actionBttn downloadBttn
#' @importFrom dplyr mutate select left_join row_number
#' @importFrom stats setNames quantile
#' @importFrom raster raster
#' @importFrom stringr str_split_i
#' @noRd
app_server <- function(input, output, session) {

  vector_file <<- PPGISr_editable_map(golem::get_golem_options("editable_map"))
  base_map_bounds <<- vector_file %>%
    sf::st_bbox() %>%
    as.character()

  user_basemap <<- PPGISr_base_map(golem::get_golem_options("base_map"))

  if (is.null(user_basemap)){
    basemap_groups <<- c("OSM (default)", "Toner", "Toner Lite",
                         "Open Topo Map", "ESRI World Imagery")
  } else {
    basemap_name <<- golem::get_golem_options("basemap_name")
    basemap_groups <<- c("OSM (default)", "Toner", "Toner Lite",
                         "Open Topo Map", "ESRI World Imagery", basemap_name)

    # handle whether the basemap is a raster or vector file
    if (class(user_basemap)[1] == 'RasterLayer'){
      basemap_type <<- 'raster'
      bmap_fields <<- NULL
    } else {
      basemap_type <<- 'vector'
      bmap_fields <<- colnames(user_basemap %>%
                                 dplyr::select(tidyselect::where(is.numeric)))
      bmap_fields <<- bmap_fields[!bmap_fields %in% c('geometry', 'geom')]
      # select non-geometry numeric fields for display options
      updateSelectInput(inputId = 'field', choices = bmap_fields)
    }
  }

  COLOR_PAL2 = c("#ffffff", golem::get_golem_options("mapping_colors"))
  # for legend
  map_palette <<- colorFactor(palette =
                                golem::get_golem_options("mapping_colors"),
                              domain=1:length(golem:: get_golem_options("mapping_colors")), na.color = "#FFFFFF00") # for fill
  map_palette2 <<- colorFactor(palette = golem::get_golem_options("mapping_colors"), domain=1:length(golem:: get_golem_options("mapping_colors")), na.color = "black") # for borders

  # Renders the map output
  output$PPGISmap <- renderLeaflet({
    createMap() %>%
      addMapPane('base_layers', 410) %>%  # This ensures the base layers will render below the clickable polygon layer
      addMapPane('poly_layer', 450) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") %>%
      addPolygons(
        data=vector_file,
        layerId=~PPGIS_CODE,
        group='base_polygons',
        weight=1.5,
        fillOpacity=0,
        color = 'black',
        options = pathOptions(pane = "poly_layer")
      )  %>%
      addLayersControl(
        baseGroups = basemap_groups,
        options = layersControlOptions(collapsed = FALSE)) %>%
      addLegend(
        position='bottomleft',
        title="Legend of Categories",
        opacity=0.6,
        colors = COLOR_PAL2,
        labels = names(golem::get_golem_options("mapping_categories"))
      ) %>%
      #  If there is a basemap, either display the raster or vector file.
      {if(is.null(user_basemap)) .
        else if (basemap_type == 'raster') addRasterImage(map = ., x = user_basemap, group = basemap_name)
        else addPolygons(map = .,
                         data = user_basemap,
                         group = basemap_name,
                         weight = 0.5,
                         fillOpacity = 0,
                         color = 'blue',
                         options = pathOptions(pane = "base_layers"))} %>%
      addLayersControl(
        baseGroups = basemap_groups,
        options = layersControlOptions(collapsed = FALSE))
  })


  # Event to take a screenshot
  shiny::observeEvent(input$go, {
    screenshot(id="PPGISmap")
  })

  #  When users select a basemap field to display:
  #  First, check if a valid basemap exists and has fields to display. Then,
  #  Create a simple palette for a quintile classification of the selected
  #  field, then update the layer to use the selected symbology
  shiny::observeEvent(input$field, {
    if (is.null(user_basemap) || is.null(bmap_fields)){
      return()
    }
    else {
      basemap_var <- input$field
      bins <- quantile(user_basemap[[basemap_var]], na.rm=TRUE)
      bmap_pal <- colorBin("YlOrRd", domain = user_basemap[[basemap_var]], bins = bins)  # may want to change color ramp

      leafletProxy(mapId='PPGISmap') %>%
        removeShape(user_basemap) %>%
        addPolygons(data = user_basemap,
                    group = basemap_name,
                    weight = 0.5,
                    fillOpacity = 0.25,
                    color = 'white',
                    fillColor = ~bmap_pal(user_basemap[[basemap_var]]),
                    options = pathOptions(pane = "base_layers"))

    }
  }, ignoreInit = TRUE)


  # Event to handle clicking polygons to assign categories
  shiny::observeEvent(input$PPGISmap_shape_click, {
    polygon_clicked <- input$PPGISmap_shape_click

    if (is.null(polygon_clicked)) { return() }
    if (is.null(polygon_clicked$id)) {return()}  # User-inputted vector basemaps will have no unique id value, so this line prevents them from being clicked

    row_idx <- which(vector_file$PPGIS_CODE == polygon_clicked$id)

    #print('polygon_clicked:')
    #print(polygon_clicked$id)
    #print('row_idx:')
    #print(row_idx)

    is_selected <- vector_file[row_idx, ]$SELECTED

    #print(is_selected)

    if (!is.na(is_selected)) { # if polygon is already selected

      vector_file[row_idx, ]$SELECTED <<- NA # zeros out polygon selected value

      # isolates polygon that needs to be redrawn
      vector_file_selected <- vector_file[row_idx, ]


      # redraws polygon without any color (base settings)
      leafletProxy(mapId='PPGISmap') %>%
        removeShape(vector_file[row_idx, ]$PPGIS_CODE) %>%
        addPolygons(
          data=vector_file_selected,
          layerId=~PPGIS_CODE,
          group='base_polygons',
          weight=1,
          fillOpacity=0,
          color = 'black',
          options = pathOptions(pane = "poly_layer")
        )

      #print(vector_file_selected)
    }
    else { # if polygon is not selected
      if(is.null(input$radioInt)){
        shiny::showNotification('Please select a category to assign.', '', duration = 5, type = 'warning')
        return()}


      palette_code_selected <- as.numeric(input$radioInt)

      # substitutes selected value for polygon with group number
      vector_file[row_idx, ]$SELECTED <<- palette_code_selected

      #isolates polygon that needs to be redrawn
      vector_file_selected <- vector_file[row_idx, ]


      # redraws polygon with correct color (defined by global palette)
      leafletProxy(mapId='PPGISmap') %>%
        removeShape(vector_file[row_idx, ]$PPGIS_CODE) %>%
        addPolygons(
          data=vector_file,
          layerId=~PPGIS_CODE,
          group='base_polygons',
          weight=1.5,
          fillOpacity=0.5,
          color = ~map_palette2(as.factor(SELECTED)),
          fillColor = ~map_palette(as.factor(SELECTED)),
          options = pathOptions(pane = "poly_layer")
        )
      #print(vector_file$SELECTED)
    }
  })

  # Download shapefile
  output$download_shp <- shiny::downloadHandler(

    filename <- function() {"Data_shpExport.zip"},
    content = function(file) {
      shiny::withProgress(message = "Exporting Data", {

        shiny::incProgress(0.5)
        tmp.path <- dirname(file)

        name.base <- file.path(tmp.path, "PivotOutput")
        name.glob <- paste0(name.base, ".*")
        name.shp  <- paste0(name.base, ".shp")
        name.zip  <- paste0(name.base, ".zip")

        print(tmp.path)
        print(name.glob)

        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))
        vector_file %>%
          dplyr::left_join(data.frame(values = golem::get_golem_options("mapping_categories"), CAT = names(golem::get_golem_options("mapping_categories"))), by=c('SELECTED' = 'values')) %>%
          sf::st_write(dsn = name.shp, ## layer = "shpExport",
                       driver = "ESRI Shapefile", quiet = TRUE)

        zip::zipr(zipfile = name.zip, files = Sys.glob(name.glob))
        shiny::req(file.copy(name.zip, file))

        if (length(Sys.glob(name.glob)) > 0) file.remove(Sys.glob(name.glob))

        shiny::incProgress(0.5)
      })
    }
  )

}

