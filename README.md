
# PPGISr <a href="https://github.com/GLISA-umich/PPGISr"><img src="inst/figures/README_hex.png" align="right" height="132" /></a>

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

Welcome to PPGISr, an R package for Public participatory GIS (PPGIS).

### Run an example map

PPGISr comes with a sample map that loads as default. The example map
provides an area of Minnesota and three land cover categories to choose
from.

``` r
PPGISr::run_app()
```

### Create your own categorise

You can also suggest which categories of land cover you would like to
map.

``` r
PPGISr::run_app(mapping_categories = c("Trees", 
                                       "Flowers",
                                       "Water"))
```

## Issues and bugs

This package requires an internet connection as well as a connection to
the Flickr API, which may not be constantly available.

If you discover a bug that is not already a [reported
issue](https://github.com/GLISA-umich/PPGISr/issues), please [open a new
issue](https://github.com/GLISA-umich/PPGISr/issues/new) providing a
reproducible example.
