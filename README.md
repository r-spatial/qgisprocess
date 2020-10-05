
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

## Installing QGIS on your computer

The `qgis_process` command-line utility is available in QGIS \>=
[3.14.16](https://github.com/qgis/QGIS/releases/tag/final-3_14_16),
[released](https://qgis.org/en/site/getinvolved/development/roadmap.html)
in September 2020. You can install this version of QGIS on Linux, Mac
and (via the OSGeo4W package) Windows, as described at
[qgis.org](https://qgis.org/en/site/forusers/download.html).

## qgisprocessing in Docker

You can also use Docker images containing the necessary dependencies to
run this package in a container. The
[`geocompr/geocompr:qgis`](https://github.com/geocompr/docker) image,
for example, has QGIS and RStudio preinstalled for interactive use and
can be downloaded from Dockerhub and run as follows:

    docker run -d -p 8788:8787 -e USERID=$UID -e PASSWORD=ps -v ${pwd}:/home/rstudio/ geocompr/geocompr:qgis

After running this command (having set a more secure password as
appropriate) you should be able to access RStudio Server from
<http://localhost:8788/>. Once inside you can install `qgisprocess` as
documented here.

You can run also `qgisprocessing` in a Dockerfile provided in this repo
as follows:

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
#> # A tibble: 191 x 5
#>    provider provider_title  algorithm       algorithm_id    algorithm_title     
#>    <chr>    <chr>           <chr>           <chr>           <chr>               
#>  1 3d       QGIS (3D)       3d:tessellate   tessellate      Tessellate          
#>  2 native   QGIS (native c… native:addauto… addautoincreme… Add autoincremental…
#>  3 native   QGIS (native c… native:addfiel… addfieldtoattr… Add field to attrib…
#>  4 native   QGIS (native c… native:adduniq… adduniquevalue… Add unique value in…
#>  5 native   QGIS (native c… native:addxyfi… addxyfields     Add X/Y fields to l…
#>  6 native   QGIS (native c… native:affinet… affinetransform Affine transform    
#>  7 native   QGIS (native c… native:aggrega… aggregate       Aggregate           
#>  8 native   QGIS (native c… native:antimer… antimeridiansp… Geodesic line split…
#>  9 native   QGIS (native c… native:arrayof… arrayoffsetlin… Array of offset (pa…
#> 10 native   QGIS (native c… native:arraytr… arraytranslate… Array of translated…
#> # … with 181 more rows

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
#> [1] "QStandardPaths: XDG_RUNTIME_DIR not set, defaulting to '/tmp/runtime-rstudio'\n"
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
#>   '--INPUT=/tmp/RtmpKdpNHj/file3ef81f2da9.shp' '--DISTANCE=1' '--SEGMENTS=10' \
#>   '--DISSOLVE=True' '--END_CAP_STYLE=0' '--JOIN_STYLE=0' '--MITER_LIMIT=10' \
#>   '--OUTPUT=/tmp/RtmpKdpNHj/nc_buffered.gpkg'
#> 
#> ----------------
#> Inputs
#> ----------------
#> 
#> DISSOLVE:    True
#> DISTANCE:    1
#> END_CAP_STYLE:   0
#> INPUT:   /tmp/RtmpKdpNHj/file3ef81f2da9.shp
#> JOIN_STYLE:  0
#> MITER_LIMIT: 10
#> OUTPUT:  /tmp/RtmpKdpNHj/nc_buffered.gpkg
#> SEGMENTS:    10
#> 
#> 0...10...20...30...40...50...60...70...80...90...
#> ----------------
#> Results
#> ----------------
#> 
#> OUTPUT:  /tmp/RtmpKdpNHj/nc_buffered.gpkg
#> $status
#> [1] 0
#> 
#> $stdout
#> [1] "\n----------------\nInputs\n----------------\n\nDISSOLVE:\tTrue\nDISTANCE:\t1\nEND_CAP_STYLE:\t0\nINPUT:\t/tmp/RtmpKdpNHj/file3ef81f2da9.shp\nJOIN_STYLE:\t0\nMITER_LIMIT:\t10\nOUTPUT:\t/tmp/RtmpKdpNHj/nc_buffered.gpkg\nSEGMENTS:\t10\n\n0...10...20...30...40...50...60...70...80...90...\n----------------\nResults\n----------------\n\nOUTPUT:\t/tmp/RtmpKdpNHj/nc_buffered.gpkg\n"
#> 
#> $stderr
#> [1] "QStandardPaths: XDG_RUNTIME_DIR not set, defaulting to '/tmp/runtime-rstudio'\n"
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
