
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qgisprocess

<img src="man/figures/qgisprocess.svg" align="right" hspace="10" vspace="0" width="20%">

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/qgisprocess)](https://CRAN.R-project.org/package=qgisprocess)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8260794.svg)](https://doi.org/10.5281/zenodo.8260794)
[![R-CMD-check](https://github.com/r-spatial/qgisprocess/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/r-spatial/qgisprocess/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/r-spatial/qgisprocess/branch/main/graph/badge.svg)](https://app.codecov.io/gh/r-spatial/qgisprocess/tree/main)
<!-- badges: end -->

The goal of **qgisprocess** is to provide an R interface to the
geoprocessing algorithms of [QGIS](https://qgis.org/en/site/), a popular
and open source desktop geographic information system (GIS) program. The
package is a re-implementation of functionality provided by the archived
[RQGIS](https://cran.r-project.org/package=RQGIS) package, which was
partially revived in the [RQGIS3](https://github.com/r-spatial/RQGIS3)
package.

## Installation

### qgisprocess

To install the latest CRAN release, just run:

``` r
install.packages("qgisprocess")
```

You can install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("r-spatial/qgisprocess")
```

### QGIS

The **qgisprocess** package wraps the [standalone `qgis_process`
command-line
utility](https://docs.qgis.org/latest/en/docs/user_manual/processing/standalone.html),
which is available in QGIS \>= 3.16.

The package is meant to support *current* QGIS releases, i.e. both the
latest and the long-term release. Although older QGIS releases are not
officially supported, it may work since QGIS 3.16. Installation
instructions for all platforms are available at
<https://download.qgis.org/>.

If a recent version of QGIS isn’t available for your OS, you can use the
[Geocomputation with R Docker
image](https://github.com/geocompx/docker/pkgs/container/docker) with
QGIS installed (`docker pull ghcr.io/geocompx/docker:qgis`). See the
vignette on ‘getting started’ for more information.

### Package configuration

If the automatic configuration fails (or if you have more than one QGIS
installation and would like to choose which one is used), you can set
`options(qgisprocess.path = "path/to/qgis_process")`. Specify the
`qgisprocess.path` option in your `.Rprofile`, to make your choices
persistent between sessions. You can run `qgis_configure()` to
reconfigure the package, or just
`qgis_configure(use_cached_data = TRUE)` to see the gritty details!

``` r
library(qgisprocess)
#> Attempting to load the package cache ... Success!
#> QGIS version: 3.36.3-Maidenhead
#> Having access to 1987 algorithms from 15 QGIS processing providers.
#> Run `qgis_configure(use_cached_data = TRUE)` to reload cache and get more details.
#> >>> Run `qgis_enable_plugins()` to enable 3 disabled plugins and access
#>     their algorithms: ViewshedAnalysis, networks, valhalla
```

## Functionality

Most functions start with the `qgis_` prefix, so that functions can be
found more easily using tab completion.

The main function is `qgis_run_algorithm(algorithm = , ...)`. It
specifies the geoprocessing algorithm to be called with a
`"provider:algorithm"` formatted identifier, e.g. `"native:convexhull"`
or `"gdal:hillshade"`, and it passes the algorithm arguments as R
function arguments.

Additional functions are provided to discover available geoprocessing
algorithms, retrieve their documentation, handle processing results,
manage QGIS plugins, and more.

Spatial layers can be passed to `qgis_run_algorithm()` as file paths but
also as [sf](https://r-spatial.github.io/sf/),
[stars](https://r-spatial.github.io/stars/),
[terra](https://rspatial.github.io/terra/) or
[raster](https://cran.r-project.org/package=raster) objects.

A structured overview of functions is available at
<https://r-spatial.github.io/qgisprocess/reference/index.html>. To get
started, read the ‘getting started’ vignette and use the [cheat
sheets](https://r-spatial.github.io/qgisprocess/articles/)!

Note that R package
[**qgis**](https://github.com/JanCaha/r_package_qgis) extends on
**qgisprocess** by providing a separate R function for each
geoprocessing algorithm. In addition, it makes the QGIS algorithm
documentation available in the corresponding R function documentation.

### Example

The following example demonstrates the
[buffer](https://docs.qgis.org/latest/en/docs/user_manual/processing_algs/qgis/vectorgeometry.html#buffer)
algorithm in action.

``` r
input <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

result <- qgis_run_algorithm(
  "native:buffer",
  INPUT = input,
  DISTANCE = 1,
  DISSOLVE = TRUE
)
#> Argument `SEGMENTS` is unspecified (using QGIS default value).
#> Using `END_CAP_STYLE = "Round"`
#> Using `JOIN_STYLE = "Round"`
#> Argument `MITER_LIMIT` is unspecified (using QGIS default value).
#> Argument `SEPARATE_DISJOINT` is unspecified (using QGIS default value).
#> Using `OUTPUT = qgis_tmp_vector()`
#> Using non-preferred coordinate operation between EPSG:4267 and EPSG:4326. Using +proj=pipeline +step +proj=unitconvert +xy_in=deg +xy_out=rad +step +proj=push +v_3 +step +proj=cart +ellps=clrk66 +step +proj=helmert +x=-10 +y=158 +z=187 +step +inv +proj=cart +ellps=WGS84 +step +proj=pop +v_3 +step +proj=unitconvert +xy_in=rad +xy_out=deg, preferred +proj=pipeline +step +proj=unitconvert +xy_in=deg +xy_out=rad +step +proj=hgridshift +grids=ca_nrc_ntv2_0.tif +step +proj=unitconvert +xy_in=rad +xy_out=deg.
```

``` r

result
#> <Result of `qgis_run_algorithm("native:buffer", ...)`>
#> List of 1
#>  $ OUTPUT: 'qgis_outputVector' chr "/tmp/Rtmplc8DaG/file73c05163e20c/file73c047b8eb9b.gpkg"
```

``` r

output_sf <- sf::st_as_sf(result)
plot(sf::st_geometry(output_sf))
```

<img src="man/figures/README-buffer-1.png" width="60%" />

### Some tips

You can search for algorithms with `qgis_search_algorithms()` (string
matching with regex).

``` r
qgis_search_algorithms(algorithm = "buffer", group = "[Vv]ector")
#> # A tibble: 10 × 5
#>    provider provider_title    group                algorithm     algorithm_title
#>    <chr>    <chr>             <chr>                <chr>         <chr>          
#>  1 gdal     GDAL              Vector geoprocessing gdal:bufferv… Buffer vectors 
#>  2 gdal     GDAL              Vector geoprocessing gdal:oneside… One side buffer
#>  3 grass    GRASS             Vector (v.*)         grass:v.buff… v.buffer       
#>  4 native   QGIS (native c++) Vector geometry      native:buffer Buffer         
#>  5 native   QGIS (native c++) Vector geometry      native:buffe… Variable width…
#>  6 native   QGIS (native c++) Vector geometry      native:multi… Multi-ring buf…
#>  7 native   QGIS (native c++) Vector geometry      native:singl… Single sided b…
#>  8 native   QGIS (native c++) Vector geometry      native:taper… Tapered buffers
#>  9 native   QGIS (native c++) Vector geometry      native:wedge… Create wedge b…
#> 10 sagang   SAGA Next Gen     Vector general       sagang:shape… Shapes buffer
```

You can read the help associated with an algorithm using
`qgis_show_help()`.

``` r
qgis_show_help("native:buffer")
```

A full list of available algorithms is returned by `qgis_algorithms()`.

``` r
qgis_algorithms()
#> # A tibble: 1,987 × 24
#>    provider  provider_title algorithm               algorithm_id algorithm_title
#>    <chr>     <chr>          <chr>                   <chr>        <chr>          
#>  1 3d        QGIS (3D)      3d:tessellate           tessellate   Tessellate     
#>  2 NetworkGT NetworkGT      NetworkGT:1D Flow       1D Flow      1D Flow        
#>  3 NetworkGT NetworkGT      NetworkGT:2D Flow       2D Flow      2D Flow        
#>  4 NetworkGT NetworkGT      NetworkGT:Aperture      Aperture     Aperture       
#>  5 NetworkGT NetworkGT      NetworkGT:Branches and… Branches an… Branches and N…
#>  6 NetworkGT NetworkGT      NetworkGT:Clusters      Clusters     Define Clusters
#>  7 NetworkGT NetworkGT      NetworkGT:Configure     Configure    Configure Netw…
#>  8 NetworkGT NetworkGT      NetworkGT:Connect Y No… Connect Y N… Connect Y Nodes
#>  9 NetworkGT NetworkGT      NetworkGT:Contour Grid  Contour Grid Contour Grid   
#> 10 NetworkGT NetworkGT      NetworkGT:Define Fract… Define Frac… Define Fractur…
#> # ℹ 1,977 more rows
#> # ℹ 19 more variables: provider_can_be_activated <lgl>,
#> #   provider_is_active <lgl>, provider_long_name <chr>, provider_version <chr>,
#> #   provider_warning <chr>, can_cancel <lgl>, deprecated <lgl>, group <chr>,
#> #   has_known_issues <lgl>, help_url <chr>, requires_matching_crs <lgl>,
#> #   short_description <chr>, tags <list>, default_raster_file_extension <chr>,
#> #   default_vector_file_extension <chr>, …
```

It may also be useful to run an algorithm in the QGIS GUI to determine
how the various input values are translated to string processing
arguments. This can be done using the ‘Advanced’ dropdown, by copying
either the `qgis_process` command string or the JSON string:

![](man/figures/copy_as_json.png)

Note that the JSON string can be passed directly to
`qgis_run_algorithm()`!

## Code of Conduct

Please note that the qgisprocess project is released with a [Contributor
Code of
Conduct](https://r-spatial.github.io/qgisprocess/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## More information

### Presentations

- FOSS4G 2023:
  [slides](https://florisvdh.github.io/foss4g-2023-qgisprocess/) &
  [video](https://www.youtube.com/watch?v=Qt5DzWThWqI)
- FOSS4G 2021:
  [slides](https://dewey.dunnington.ca/slides/qgisprocess2021/) &
  [video](https://www.youtube.com/watch?v=iA0OQ2Icn6Y&t=1912s)

### In the wild

(Draft)

Following case studies have used the package:

- \<Reference to your work\>. Source code: \<URL\>.

*If you used the package in your work and would like to see it
referenced here, then give a shout in issue
[\#211](https://github.com/r-spatial/qgisprocess/issues/211) or make a
pull request!*

### Further reading

- A
  [paper](https://journal.r-project.org/archive/2017/RJ-2017-067/index.html)
  on the original RQGIS package published in the R Journal
- A [discussion](https://github.com/r-spatial/discuss/issues/41) about
  options for running QGIS from R that led to this package
- The [pull request](https://github.com/qgis/QGIS/pull/34617) in the
  QGIS repo that led to the development of the `qgis_process`
  command-line utility
