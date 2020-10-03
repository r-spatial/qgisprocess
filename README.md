
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qgisprocess

<!-- badges: start -->

[![R build
status](https://github.com/paleolimbot/qgisprocess/workflows/R-CMD-check/badge.svg)](https://github.com/paleolimbot/qgisprocess/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of qgisprocess is to …

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
developmet environment.

    # do this once:
    # docker build . --tag qgisprocess-devel
    
    # do this to run an interactive R session in the docker image
    docker run --rm -it -v $(pwd):/qgisprocess -w /qgisprocess qgisprocess-devel R

Once in the container, you can use `devtools::load_all()`,
`devtools::test()`, and `devtools::check()` to develop the package.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(qgisprocess)

# basic QGIS info
qgis_path()
#> [1] "qgis_process"
qgis_version()
#> [1] "3.15.0-Master"
qgis_algorithms()
#> # A tibble: 197 x 5
#>    provider provider_title  algorithm       algorithm_id    algorithm_title     
#>    <chr>    <chr>           <chr>           <chr>           <chr>               
#>  1 3d       QGIS (3D)       3d:tessellate   tessellate      Tessellate          
#>  2 native   QGIS (native c… native:addauto… addautoincreme… Add autoincremental…
#>  3 native   QGIS (native c… native:addfiel… addfieldtoattr… Add field to attrib…
#>  4 native   QGIS (native c… native:adduniq… adduniquevalue… Add unique value in…
#>  5 native   QGIS (native c… native:addxyfi… addxyfields     Add X/Y fields to l…
#>  6 native   QGIS (native c… native:affinet… affinetransform Affine transform    
#>  7 native   QGIS (native c… native:aggrega… aggregate       Aggregate           
#>  8 native   QGIS (native c… native:angleto… angletonearest  Align points to fea…
#>  9 native   QGIS (native c… native:antimer… antimeridiansp… Geodesic line split…
#> 10 native   QGIS (native c… native:arrayof… arrayoffsetlin… Array of offset (pa…
#> # … with 187 more rows

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
#> [1] "\n----------------\nInputs\n----------------\n\nOUTPUT:\ttest-file.json\nURL:\thttps://httpbin.org/get\n\n\n0...10...20...30...40...50...60...70...80...90...100 - done.\n\n----------------\nResults\n----------------\n\nOUTPUT:\ttest-file.json\n"
#> 
#> $stderr
#> [1] "QStandardPaths: XDG_RUNTIME_DIR not set, defaulting to '/tmp/runtime-root'\n../../src/core/qgsproviderregistry.cpp:245 : (init) [178ms] Loaded 24 providers (DB2;OAPIF;WFS;arcgisfeatureserver;arcgismapserver;delimitedtext;gdal;geonode;gpx;grass;grassraster;mdal;memory;mesh_memory;mssql;ogr;ows;postgres;postgresraster;spatialite;vectortile;virtual;wcs;wms) \n../../src/providers/grass/qgsgrass.cpp:518 : (lock) [3ms] lock\n../../src/providers/grass/qgsgrass.cpp:317 : (init) [0ms] do init\n../../src/providers/grass/qgsgrass.cpp:2660 : (defaultGisbase) [0ms] gisbase from envar = \n../../src/providers/grass/qgsgrass.cpp:2686 : (defaultGisbase) [0ms] gisbase = /usr/lib/grass78\n../../src/providers/grass/qgsgrass.cpp:483 : (isValidGrassBaseDir) [0ms] isValidGrassBaseDir()\n../../src/providers/grass/qgsgrass.cpp:2660 : (defaultGisbase) [0ms] gisbase from envar = \n../../src/providers/grass/qgsgrass.cpp:2686 : (defaultGisbase) [0ms] gisbase = /usr/lib/grass78\n../../src/providers/grass/qgsgrass.cpp:383 : (init) [0ms] Valid GRASS gisbase is: /usr/lib/grass78\n../../src/providers/grass/qgsgrass.cpp:2660 : (defaultGisbase) [0ms] gisbase from envar = \n../../src/providers/grass/qgsgrass.cpp:2686 : (defaultGisbase) [0ms] gisbase = /usr/lib/grass78\n../../src/providers/grass/qgsgrass.cpp:2660 : (defaultGisbase) [0ms] gisbase from envar = /usr/lib/grass78\n../../src/providers/grass/qgsgrass.cpp:2660 : (defaultGisbase) [0ms] gisbase from envar = /usr/lib/grass78\n../../src/providers/grass/qgsgrass.cpp:419 : (init) [0ms] sGrassModulesPaths = /usr/lib/grass78/bin,/usr/lib/grass78/scripts,/usr/share/qgis/grass/scripts,/usr/lib/qgis/grass/modules\n../../src/providers/grass/qgsgrass.cpp:524 : (unlock) [3ms] unlock\n../../src/process/qgsprocess.cpp:116 : (loadPythonSupport) [75ms] load library /usr/lib/qgispython (3.15.0)\n"
#> 
#> $timeout
#> [1] FALSE
```
