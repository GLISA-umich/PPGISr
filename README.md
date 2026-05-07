# PPGISr <a href="https://github.com/GLISA-umich/PPGISr"><img src="inst/figures/README_hex.png" align="right" height="132" /></a>

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Welcome to PPGISr, an R package for Public participatory GIS (PPGIS).

### Install PPGISr

``` r
library(devtools)
install_github("https://github.com/GLISA-umich/PPGISr")
```

### Run an example map

PPGISr comes with bundled Duluth, Minnesota example data. By default,
`run_app()` opens a mobile-friendly mapping app with an editable polygon
layer, a reference basemap layer, and three mapping classes.

``` r
PPGISr::run_app()
```

### Create your own categories

Use `mapping_categories` to define the classes users can assign to map
features. Use `mapping_colors` to set the display color for each class,
and `mapping_descriptions` to explain what each class means in the side
panel.

``` r
PPGISr::run_app(
  mapping_categories = c("Trees", "Water", "Housing"),
  mapping_colors = c("green", "blue", "orange"),
  mapping_descriptions = c(
    "Places where more shade, canopy, or street trees are desired.",
    "Places with flooding, drainage, shoreline, or water access concerns.",
    "Places where housing density, affordability, or redevelopment should be considered."
  )
)
```

The vectors for categories, colors, and descriptions must be the same
length.

### Add your own editable map

Users can specify the editable map layer, allowing for different
locations and geographic units such as parcels, census tracts, or
hexagons. Spatial vector data should use the WGS84 coordinate reference
system (`EPSG:4326`).

``` r
library(sf)

editable_map <- st_read(system.file("shape/nc.shp", package = "sf"))

PPGISr::run_app(editable_map = editable_map, base_map = NULL)
```

### Add your own reference basemap

Use `base_map` for contextual information that should appear below the
editable layer, such as vulnerability data, land use, flood zones, or
planning districts.

``` r
PPGISr::run_app(
  editable_map = PPGISr::duluthEditablemap,
  base_map = PPGISr::duluthBasemap,
  basemap_name = "Reference Basemap"
)
```

### Create a hexagon basemap

`create_hex_basemap()` creates an `sf` hexagon grid from an existing
`sf` object's bounding box or from explicit bounding box coordinates.

``` r
hex_map <- PPGISr::create_hex_basemap(
  bbox = PPGISr::duluthEditablemap,
  cellsize = 0.02
)

PPGISr::run_app(
  editable_map = hex_map,
  base_map = PPGISr::duluthBasemap
)
```

You can also provide the bounding box directly:

``` r
hex_map <- PPGISr::create_hex_basemap(
  xmin = -92.31,
  ymin = 46.64,
  xmax = -91.91,
  ymax = 46.94,
  cellsize = 0.03
)
```
