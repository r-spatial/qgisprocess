
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
```
