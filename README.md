
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
[RQGIS](https://cran.r-project.org/package=RQGIS) package, which was
partially revived in the [RQGIS3](https://github.com/r-spatial/RQGIS3)
package.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/qgisprocess")
```

The qgisprocess package wraps the `qgis_process` command-line utility,
which is available in QGIS \>=
[3.14.16](https://github.com/qgis/QGIS/releases/tag/final-3_14_16),
[released](https://qgis.org/en/site/getinvolved/development/roadmap.html)
in September 2020. MacOS users will have to install the a recent nightly
build until QGIS 3.16 has been released. Download instructions for all
platforms are available at <https://download.qgis.org/>. If a recent
version of QGIS isn’t available for your OS, you can use one of the
[Geocomputation with R Docker
images](https://github.com/geocompr/docker) with QGIS installed.

## Example

The following example demonstrates the
[buffer](https://docs.qgis.org/testing/en/docs/user_manual/processing_algs/qgis/vectorgeometry.html#buffer)
algorithm in action. The passing of [sf](https://r-spatial.github.io/sf)
and [raster](https://cran.r-project.org/package=raster) objects is
experimentally supported (and will be well-supported in the future\!).

``` r
library(qgisprocess)
#> Using 'qgis_process' at '/Applications/QGIS.app/Contents/MacOS/bin/qgis_process'.
#> Run `qgis_configure()` for details.
input <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
output_file <- file.path(tempdir(), "nc_buffered.gpkg")

qgis_run_algorithm(
  "native:buffer",
  INPUT = input,
  DISTANCE = 1,
  SEGMENTS = 10,
  DISSOLVE = 'True',
  END_CAP_STYLE = 0,
  JOIN_STYLE = 0,
  MITER_LIMIT = 10,
  OUTPUT = output_file
)
#> Running /Applications/QGIS.app/Contents/MacOS/bin/qgis_process run \
#>   'native:buffer' \
#>   '--INPUT=/var/folders/bq/2rcjstv90nx1_wrt8d3gqw6m0000gn/T//RtmpSh4lGW/filebb976563eff4.gpkg' \
#>   '--DISTANCE=1' '--SEGMENTS=10' '--DISSOLVE=True' '--END_CAP_STYLE=0' \
#>   '--JOIN_STYLE=0' '--MITER_LIMIT=10' \
#>   '--OUTPUT=/var/folders/bq/2rcjstv90nx1_wrt8d3gqw6m0000gn/T//RtmpSh4lGW/nc_buffered.gpkg'
#> 
#> ----------------
#> Inputs
#> ----------------
#> 
#> DISSOLVE:    True
#> DISTANCE:    1
#> END_CAP_STYLE:   0
#> INPUT:   /var/folders/bq/2rcjstv90nx1_wrt8d3gqw6m0000gn/T//RtmpSh4lGW/filebb976563eff4.gpkg
#> JOIN_STYLE:  0
#> MITER_LIMIT: 10
#> OUTPUT:  /var/folders/bq/2rcjstv90nx1_wrt8d3gqw6m0000gn/T//RtmpSh4lGW/nc_buffered.gpkg
#> SEGMENTS:    10
#> 
#> 
#> 0...10...20...30...40...50...60...70...80...90...
#> ----------------
#> Results
#> ----------------
#> 
#> OUTPUT:  /var/folders/bq/2rcjstv90nx1_wrt8d3gqw6m0000gn/T//RtmpSh4lGW/nc_buffered.gpkg
#> $status
#> [1] 0
#> 
#> $stdout
#> [1] "\n----------------\nInputs\n----------------\n\nDISSOLVE:\tTrue\nDISTANCE:\t1\nEND_CAP_STYLE:\t0\nINPUT:\t/var/folders/bq/2rcjstv90nx1_wrt8d3gqw6m0000gn/T//RtmpSh4lGW/filebb976563eff4.gpkg\nJOIN_STYLE:\t0\nMITER_LIMIT:\t10\nOUTPUT:\t/var/folders/bq/2rcjstv90nx1_wrt8d3gqw6m0000gn/T//RtmpSh4lGW/nc_buffered.gpkg\nSEGMENTS:\t10\n\n\n0...10...20...30...40...50...60...70...80...90...\n----------------\nResults\n----------------\n\nOUTPUT:\t/var/folders/bq/2rcjstv90nx1_wrt8d3gqw6m0000gn/T//RtmpSh4lGW/nc_buffered.gpkg\n"
#> 
#> $stderr
#> [1] "\"<font color=\\\"red\\\">Couldn't load PyQGIS.<br>Python support will be disabled.</font><br><pre><br>Traceback (most recent call last):<br>&nbsp; File \\\"<string>\\\", line 1, in <module><br>&nbsp; File \\\"/Applications/QGIS.app/Contents/MacOS/bin/../../Resources/python/qgis/core/__init__.py\\\", line 25, in <module><br>&nbsp; &nbsp; from qgis._core import *<br>ImportError: dlopen(/Applications/QGIS.app/Contents/MacOS/bin/../../Resources/python/qgis/_core.so, 2): Symbol not found: __ZN25QgsPalettedRasterRenderer8setLabelEiRK7QString<br>&nbsp; Referenced from: /Applications/QGIS.app/Contents/MacOS/bin/../../Resources/python/qgis/_core.so<br>&nbsp; Expected in: /Applications/QGIS.app/Contents/MacOS/qgis_process.app/Contents/MacOS/../../../../Frameworks/qgis_core.framework/Versions/3.15/qgis_core<br> in /Applications/QGIS.app/Contents/MacOS/bin/../../Resources/python/qgis/_core.so<br><br></pre>Python version:<br>3.7.7 (default, Sep 22 2020, 10:25:18) <br>[Clang 12.0.0 (clang-1200.0.32.2)]<br><br>QGIS version:<br>3.15.0-Master 'Master', cfba539e0c<br><br>Python path:<br>['/Applications/QGIS.app/Contents/MacOS/bin/../../Resources/python', '/Users/dewey/Library/Application Support/QGIS/QGIS3/profiles/default/python', '/Users/dewey/Library/Application Support/QGIS/QGIS3/profiles/default/python/plugins', '/Applications/QGIS.app/Contents/MacOS/bin/../../Resources/python/plugins', '/Applications/QGIS.app/Contents/MacOS/lib/python37.zip', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/lib-dynload', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/numpy-1.19.1-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/pyproj-2.6.0-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/Rtree-0.9.4-py3.7.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/GDAL-3.1.2-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/netCDF4-1.5.3-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/cftime-1.2.1-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/pandas-1.1.0-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/patsy-0.5.1-py3.7.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/Pillow-7.2.0-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/Fiona-1.8.13.post1-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/click_plugins-1.1.1-py3.7.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/matplotlib-3.3.0-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/rasterio-1.1.5-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/snuggs-1.4.7-py3.7.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/affine-2.3.0-py3.7.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/scipy-1.5.1-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/numba-0.50.1-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/opencv_contrib_python-4.3.0.36-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/statsmodels-0.11.1-py3.7-macosx-10.13.0-x86_64.egg', '/Applications/QGIS.app/Contents/MacOS/lib/python3.7/site-packages/geopandas-0.8.1-py3.7.egg']\"\n\"<font color=\\\"red\\\">An error occurred during execution of following code:<br><tt>qgis.utils.updateAvailablePlugins()</tt></font><br><pre><br>SystemError: PyEval_EvalCodeEx: NULL globals<br><br></pre>Python version:<br><br><br>QGIS version:<br>3.15.0-Master 'Master', cfba539e0c<br><br>Python path:<br>\"\n"
#> 
#> $timeout
#> [1] FALSE
output_sf <- sf::read_sf(output_file)
unlink(output_file)

plot(sf::st_geometry(output_sf))
```

<img src="man/figures/README-buffer-1.png" width="100%" />

You can read the help associated with an algorithm using
`qgis_show_help()`. It may also be useful to run an algorithm in the
QGIS GUI and examine the console output to determine how the various
input values are translated to processing arguments.

``` r
qgis_show_help("native:buffer")
#> Buffer (native:buffer)
#> 
#> ----------------
#> Description
#> ----------------
#> This algorithm computes a buffer area for all the features in an input layer, using a fixed or dynamic distance.
#> 
#> The segments parameter controls the number of line segments to use to approximate a quarter circle when creating rounded offsets.
#> 
#> The end cap style parameter controls how line endings are handled in the buffer.
#> 
#> The join style parameter specifies whether round, miter or beveled joins should be used when offsetting corners in a line.
#> 
#> The miter limit parameter is only applicable for miter join styles, and controls the maximum distance from the offset curve to use when creating a mitered join.
#> 
#> ----------------
#> Arguments
#> ----------------
#> 
#> INPUT: Input layer
#>  Argument type:  source
#>  Acceptable values:
#>      - Path to a vector layer
#> DISTANCE: Distance
#>  Argument type:  distance
#>  Acceptable values:
#>      - A numeric value
#> SEGMENTS: Segments
#>  The segments parameter controls the number of line segments to use to approximate a quarter circle when creating rounded offsets.
#>  Argument type:  number
#>  Acceptable values:
#>      - A numeric value
#> END_CAP_STYLE: End cap style
#>  Argument type:  enum
#>  Available values:
#>      - 0: Round
#>      - 1: Flat
#>      - 2: Square
#>  Acceptable values:
#>      - Number of selected option, e.g. '1'
#>      - Comma separated list of options, e.g. '1,3'
#> JOIN_STYLE: Join style
#>  Argument type:  enum
#>  Available values:
#>      - 0: Round
#>      - 1: Miter
#>      - 2: Bevel
#>  Acceptable values:
#>      - Number of selected option, e.g. '1'
#>      - Comma separated list of options, e.g. '1,3'
#> MITER_LIMIT: Miter limit
#>  Argument type:  number
#>  Acceptable values:
#>      - A numeric value
#> DISSOLVE: Dissolve result
#>  Argument type:  boolean
#>  Acceptable values:
#>      - 1 for true/yes
#>      - 0 for false/no
#> OUTPUT: Buffered
#>  Argument type:  sink
#>  Acceptable values:
#>      - Path for new vector layer
#> 
#> ----------------
#> Outputs
#> ----------------
#> 
#> OUTPUT: <outputVector>
#>  Buffered
```

## Further reading

  - A
    [paper](https://journal.r-project.org/archive/2017/RJ-2017-067/index.html)
    on the original RQGIS package published in the R Journal
  - A [discussion](https://github.com/r-spatial/discuss/issues/41)
    options for running QGIS from R that led to this package
  - The [pull request](https://github.com/qgis/QGIS/pull/34617) in the
    QGIS repo that led to the development of the `qgis_process`
    command-line utility
