
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
if (has_qgis()) {
  qgis_version()
  qgis_algorithms()
}
```
