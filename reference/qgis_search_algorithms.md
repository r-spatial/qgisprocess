# Search geoprocessing algorithms

Searches for algorithms using a regular expression. In its simplest form
that is just a string that must match part of a character value.

## Usage

``` r
qgis_search_algorithms(
  algorithm = NULL,
  provider = NULL,
  group = NULL,
  include_deprecated = FALSE
)
```

## Arguments

- algorithm:

  Regular expression to match the `algorithm` or `algorithm_title` value
  from the output of
  [`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md).

- provider:

  Regular expression to match the `provider` or `provider_title` value
  from the output of
  [`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md).

- group:

  Regular expression to match the `group` value from the output of
  [`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md).

- include_deprecated:

  Logical. Should deprecated algorithms be included?

## Value

A tibble.

## Details

When using multiple arguments in combination, only the algorithms are
returned that fulfill all conditions.

All regular expressions that
[`stringr::str_detect()`](https://stringr.tidyverse.org/reference/str_detect.html)
can handle, are accepted. Have a look at
[`stringi::search_regex()`](https://rdrr.io/pkg/stringi/man/about_search_regex.html)
to get a nice overview.

## See also

Other topics about information on algorithms & processing providers:
[`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md),
[`qgis_show_help()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md)

## Examples

``` r
qgis_search_algorithms(
  algorithm = "point.*line",
  provider = "^native$"
)
#> # A tibble: 4 × 5
#>   provider provider_title    group           algorithm           algorithm_title
#>   <chr>    <chr>             <chr>           <chr>               <chr>          
#> 1 native   QGIS (native c++) Check geometry  native:checkgeomet… Points outside…
#> 2 native   QGIS (native c++) Vector geometry native:interpolate… Interpolate po…
#> 3 native   QGIS (native c++) Vector geometry native:pointsalong… Points along g…
#> 4 native   QGIS (native c++) Vector creation native:randompoint… Random points …
```
