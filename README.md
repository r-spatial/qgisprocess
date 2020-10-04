
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qgisprocess

<!-- badges: start -->

[![R build
status](https://github.com/paleolimbot/qgisprocess/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/qgisprocess/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `qgisprocess` is to provide an R interface to the popular
and open source desktop geographic information system (GIS) program
[QGIS](https://qgis.org/en/site/). The package is a re-implementation of
functionality provided by the archived
[`RQGIS`](https://cran.r-project.org/package=RQGIS) package, which was
partially revived in the [`RQGIS3`](https://github.com/r-spatial/RQGIS3)
package.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/qgisprocess")
```

The `qgis_process` command-line utility will be available in the next
release of QGIS; for now you will need a development version of QGIS
installed. This is not trivial, but a Dockerfile is provided as a
development environment.

    # do this once:
    # docker build . --tag qgisprocess-devel
    
    # do this to run an interactive R session in the docker image
    docker run --rm -it -v $(pwd):/qgisprocess -w /qgisprocess qgisprocess-devel R

Once in the container, you can use `devtools::load_all()`,
`devtools::test()`, and `devtools::check()` to develop the package.

## Examples

This is a basic example which tests that the package can detect a
working version of `qgis_process`:

``` r
library(qgisprocess)

# basic QGIS info
qgis_path()
#> [1] "qgis_process"
qgis_version()
#> [1] "3.14.16-Pi"
qgis_algorithms()
#> # A tibble: 988 x 5
#>    provider provider_title algorithm         algorithm_id    algorithm_title    
#>    <chr>    <chr>          <chr>             <chr>           <chr>              
#>  1 3d       QGIS (3D)      3d:tessellate     tessellate      Tessellate         
#>  2 gdal     GDAL           gdal:aspect       aspect          Aspect             
#>  3 gdal     GDAL           gdal:assignproje… assignprojecti… Assign projection  
#>  4 gdal     GDAL           gdal:buffervecto… buffervectors   Buffer vectors     
#>  5 gdal     GDAL           gdal:buildvirtua… buildvirtualra… Build virtual rast…
#>  6 gdal     GDAL           gdal:buildvirtua… buildvirtualve… Build virtual vect…
#>  7 gdal     GDAL           gdal:cliprasterb… cliprasterbyex… Clip raster by ext…
#>  8 gdal     GDAL           gdal:cliprasterb… cliprasterbyma… Clip raster by mas…
#>  9 gdal     GDAL           gdal:clipvectorb… clipvectorbyex… Clip vector by ext…
#> 10 gdal     GDAL           gdal:clipvectorb… clipvectorbypo… Clip vector by mas…
#> # … with 978 more rows

# get help
qgis_show_help("native:filedownloader")
#> Download file (native:filedownloader)
#> 
#> ----------------
#> Description
#> ----------------
#> This algorithm downloads a URL on the file system.
#> 
#> ----------------
#> Arguments
#> ----------------
#> 
#> URL: URL
#>  Argument type:  string
#>  Acceptable values:
#>      - String value
#> OUTPUT: File destination
#>  Argument type:  fileDestination
#>  Acceptable values:
#>      - Path for new file
#> 
#> ----------------
#> Outputs
#> ----------------
#> 
#> OUTPUT: <outputFile>
#>  File destination

# run the algorithm!
qgis_run_algorithm(
  "native:filedownloader", 
  URL = "https://httpbin.org/get",
  OUTPUT = "test-file.json"
)
#> Running qgis_process run 'native:filedownloader' \
#>   '--URL=https://httpbin.org/get' '--OUTPUT=test-file.json'
#> 
#> ----------------
#> Inputs
#> ----------------
#> 
#> OUTPUT:  test-file.json
#> URL: https://httpbin.org/get
#> 
#> 0...10...20...30...40...50...60...70...80...90...100 - done.
#> 
#> ----------------
#> Results
#> ----------------
#> 
#> OUTPUT:  test-file.json
#> $status
#> [1] 0
#> 
#> $stdout
#> [1] "\n----------------\nInputs\n----------------\n\nOUTPUT:\ttest-file.json\nURL:\thttps://httpbin.org/get\n\n0...10...20...30...40...50...60...70...80...90...100 - done.\n\n----------------\nResults\n----------------\n\nOUTPUT:\ttest-file.json\n"
#> 
#> $stderr
#> [1] "Traceback (most recent call last):\n  File \"/usr/lib/python3/dist-packages/qgis/utils.py\", line 334, in _startPlugin\n    plugins[packageName] = package.classFactory(iface)\n  File \"/home/robin/.local/share/QGIS/QGIS3/profiles/default/python/plugins/shapetools/__init__.py\", line 8, in classFactory\n    return ShapeTools(iface)\n  File \"/home/robin/.local/share/QGIS/QGIS3/profiles/default/python/plugins/shapetools/shapeTools.py\", line 23, in __init__\n    self.canvas = iface.mapCanvas()\nAttributeError: 'NoneType' object has no attribute 'mapCanvas'\n\nerror starting plugin: shapetools\n\nTraceback (most recent call last):\n  File \"/usr/lib/python3/dist-packages/qgis/utils.py\", line 334, in _startPlugin\n    plugins[packageName] = package.classFactory(iface)\n  File \"/home/robin/.local/share/QGIS/QGIS3/profiles/default/python/plugins/QuickOSM/__init__.py\", line 12, in classFactory\n    return QuickOSMPlugin(iface)\n  File \"/home/robin/.local/share/QGIS/QGIS3/profiles/default/python/plugins/QuickOSM/quick_osm.py\", line 57, in __init__\n    self.toolbar = self.iface.addToolBar('QuickOSM')\nAttributeError: 'NoneType' object has no attribute 'addToolBar'\n\nerror starting plugin: QuickOSM\n\n"
#> 
#> $timeout
#> [1] FALSE
```

The following examples demonstrates the
[buffer](https://docs.qgis.org/testing/en/docs/user_manual/processing_algs/qgis/vectorgeometry.html#buffer)
algorithm in action.

``` r
input_file <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
output_file <- file.path(tempdir(), "nc_buffered.gpkg")
qgis_run_algorithm(
  "native:buffer",
  INPUT = input_file,
  DISTANCE = 1,
  SEGMENTS = 10,
  DISSOLVE = 'True',
  END_CAP_STYLE = 0,
  JOIN_STYLE = 0,
  MITER_LIMIT = 10,
  OUTPUT = output_file
)
#> Running qgis_process run 'native:buffer' \
#>   '--INPUT=/tmp/RtmpGnKZjL/file7e816ddecb6.shp' '--DISTANCE=1' \
#>   '--SEGMENTS=10' '--DISSOLVE=True' '--END_CAP_STYLE=0' '--JOIN_STYLE=0' \
#>   '--MITER_LIMIT=10' '--OUTPUT=/tmp/RtmpGnKZjL/nc_buffered.gpkg'
#> 
#> ----------------
#> Inputs
#> ----------------
#> 
#> DISSOLVE:    True
#> DISTANCE:    1
#> END_CAP_STYLE:   0
#> INPUT:   /tmp/RtmpGnKZjL/file7e816ddecb6.shp
#> JOIN_STYLE:  0
#> MITER_LIMIT: 10
#> OUTPUT:  /tmp/RtmpGnKZjL/nc_buffered.gpkg
#> SEGMENTS:    10
#> 
#> 0...10...20...30...40...50...60...70...80...90...
#> ----------------
#> Results
#> ----------------
#> 
#> OUTPUT:  /tmp/RtmpGnKZjL/nc_buffered.gpkg
#> $status
#> [1] 0
#> 
#> $stdout
#> [1] "\n----------------\nInputs\n----------------\n\nDISSOLVE:\tTrue\nDISTANCE:\t1\nEND_CAP_STYLE:\t0\nINPUT:\t/tmp/RtmpGnKZjL/file7e816ddecb6.shp\nJOIN_STYLE:\t0\nMITER_LIMIT:\t10\nOUTPUT:\t/tmp/RtmpGnKZjL/nc_buffered.gpkg\nSEGMENTS:\t10\n\n0...10...20...30...40...50...60...70...80...90...\n----------------\nResults\n----------------\n\nOUTPUT:\t/tmp/RtmpGnKZjL/nc_buffered.gpkg\n"
#> 
#> $stderr
#> [1] "Traceback (most recent call last):\n  File \"/usr/lib/python3/dist-packages/qgis/utils.py\", line 334, in _startPlugin\n    plugins[packageName] = package.classFactory(iface)\n  File \"/home/robin/.local/share/QGIS/QGIS3/profiles/default/python/plugins/shapetools/__init__.py\", line 8, in classFactory\n    return ShapeTools(iface)\n  File \"/home/robin/.local/share/QGIS/QGIS3/profiles/default/python/plugins/shapetools/shapeTools.py\", line 23, in __init__\n    self.canvas = iface.mapCanvas()\nAttributeError: 'NoneType' object has no attribute 'mapCanvas'\n\nerror starting plugin: shapetools\n\nTraceback (most recent call last):\n  File \"/usr/lib/python3/dist-packages/qgis/utils.py\", line 334, in _startPlugin\n    plugins[packageName] = package.classFactory(iface)\n  File \"/home/robin/.local/share/QGIS/QGIS3/profiles/default/python/plugins/QuickOSM/__init__.py\", line 12, in classFactory\n    return QuickOSMPlugin(iface)\n  File \"/home/robin/.local/share/QGIS/QGIS3/profiles/default/python/plugins/QuickOSM/quick_osm.py\", line 57, in __init__\n    self.toolbar = self.iface.addToolBar('QuickOSM')\nAttributeError: 'NoneType' object has no attribute 'addToolBar'\n\nerror starting plugin: QuickOSM\n\n"
#> 
#> $timeout
#> [1] FALSE
output_sf <- sf::read_sf(output_file)
sf::st_crs(output_sf)
#> Coordinate Reference System:
#>   User input: NAD27 
#>   wkt:
#> GEOGCRS["NAD27",
#>     DATUM["North American Datum 1927",
#>         ELLIPSOID["Clarke 1866",6378206.4,294.978698213898,
#>             LENGTHUNIT["metre",1]]],
#>     PRIMEM["Greenwich",0,
#>         ANGLEUNIT["degree",0.0174532925199433]],
#>     CS[ellipsoidal,2],
#>         AXIS["geodetic latitude (Lat)",north,
#>             ORDER[1],
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>         AXIS["geodetic longitude (Lon)",east,
#>             ORDER[2],
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>     USAGE[
#>         SCOPE["unknown"],
#>         AREA["North America - NAD27"],
#>         BBOX[7.15,167.65,83.17,-47.74]],
#>     ID["EPSG",4267]]
plot(sf::st_geometry(output_sf))
```

<img src="man/figures/README-buffer-1.png" width="100%" />

## Further reading

  - A
    [paper](https://journal.r-project.org/archive/2017/RJ-2017-067/index.html)
    on the original RQGIS package published in the R Journal
  - A [discussion](https://github.com/r-spatial/discuss/issues/41)
    options for running QGIS from R that led to this package
  - The [pull request](https://github.com/qgis/QGIS/pull/34617) in the
    QGIS repo that led to the development of the `qgis_process`
    command-line utility
