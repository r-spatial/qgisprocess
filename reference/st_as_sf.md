# Convert a qgis_result object or one of its elements to an sf object

Convert a qgis_result object or one of its elements to an sf object

## Usage

``` r
# S3 method for class 'qgis_result'
st_as_sf(x, ...)

# S3 method for class 'qgis_outputVector'
st_as_sf(x, ...)

# S3 method for class 'qgis_outputLayer'
st_as_sf(x, ...)
```

## Arguments

- x:

  A `qgis_result` object from
  [`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md)
  or a `qgis_output*` object from one of the
  [`qgis_extract_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md)
  functions.

- ...:

  Arguments passed to
  [`sf::read_sf()`](https://r-spatial.github.io/sf/reference/st_read.html).

## Value

An `sf` object.

## Details

The sf package must be loaded explicitly to use these methods.

## See also

Other topics about coercing processing output:
[`qgis_as_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md),
[`qgis_as_terra()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_terra.md),
[`st_as_stars`](https://r-spatial.github.io/qgisprocess/reference/st_as_stars.md)

Other topics about accessing or managing processing results:
[`qgis_as_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md),
[`qgis_as_terra()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_terra.md),
[`qgis_clean_result()`](https://r-spatial.github.io/qgisprocess/reference/qgis_clean_result.md),
[`qgis_extract_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md),
[`qgis_result_status()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md),
[`st_as_stars`](https://r-spatial.github.io/qgisprocess/reference/st_as_stars.md)

## Examples

``` r
# \donttest{
# not running below examples in R CMD check to save time
result <- qgis_run_algorithm(
  "native:buffer",
  INPUT = system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
  DISTANCE = 10
)
#> Argument `SEGMENTS` is unspecified (using QGIS default value).
#> Using `END_CAP_STYLE = "Round"`
#> Using `JOIN_STYLE = "Round"`
#> Argument `MITER_LIMIT` is unspecified (using QGIS default value).
#> Argument `DISSOLVE` is unspecified (using QGIS default value).
#> Argument `SEPARATE_DISJOINT` is unspecified (using QGIS default value).
#> Using `OUTPUT = qgis_tmp_vector()`

# most direct approach, autoselecting a `qgis_outputVector` type
# output from the `result` object and reading as sf object:
sf::st_as_sf(result)
#> Simple feature collection with 64 features and 2 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 409957.1 ymin: 5083344 xmax: 411668.7 ymax: 5084787
#> Projected CRS: NAD83 / UTM zone 20N
#> # A tibble: 64 × 3
#>    WAYPOINT_I DEPTH_M                                                       geom
#>         <dbl>   <dbl>                                         <MULTIPOLYGON [m]>
#>  1          2     0.8 (((411668.7 5084501, 411668.2 5084498, 411666.8 5084496, …
#>  2          3     0.9 (((411640.3 5084560, 411639.8 5084556, 411638.4 5084554, …
#>  3          5     0.8 (((411563.4 5084601, 411562.9 5084598, 411561.5 5084595, …
#>  4          6     0.8 (((411486.4 5084600, 411485.9 5084597, 411484.5 5084594, …
#>  5          8     1.4 (((411476.8 5084488, 411476.3 5084484, 411474.9 5084482, …
#>  6         10     0.6 (((411476.4 5084410, 411475.9 5084407, 411474.4 5084404, …
#>  7         12     1.4 (((411389.1 5084490, 411388.6 5084487, 411387.2 5084485, …
#>  8         16     0.8 (((411331.2 5084721, 411330.8 5084718, 411329.3 5084715, …
#>  9         17     1.4 (((411302.9 5084670, 411302.4 5084667, 411301 5084665, 41…
#> 10         19     1.5 (((411300.8 5084593, 411300.3 5084590, 411298.9 5084588, …
#> # ℹ 54 more rows

# if you need more control, extract the needed output element first:
output_vector <- qgis_extract_output(result, "OUTPUT")
sf::st_as_sf(output_vector)
#> Simple feature collection with 64 features and 2 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 409957.1 ymin: 5083344 xmax: 411668.7 ymax: 5084787
#> Projected CRS: NAD83 / UTM zone 20N
#> # A tibble: 64 × 3
#>    WAYPOINT_I DEPTH_M                                                       geom
#>         <dbl>   <dbl>                                         <MULTIPOLYGON [m]>
#>  1          2     0.8 (((411668.7 5084501, 411668.2 5084498, 411666.8 5084496, …
#>  2          3     0.9 (((411640.3 5084560, 411639.8 5084556, 411638.4 5084554, …
#>  3          5     0.8 (((411563.4 5084601, 411562.9 5084598, 411561.5 5084595, …
#>  4          6     0.8 (((411486.4 5084600, 411485.9 5084597, 411484.5 5084594, …
#>  5          8     1.4 (((411476.8 5084488, 411476.3 5084484, 411474.9 5084482, …
#>  6         10     0.6 (((411476.4 5084410, 411475.9 5084407, 411474.4 5084404, …
#>  7         12     1.4 (((411389.1 5084490, 411388.6 5084487, 411387.2 5084485, …
#>  8         16     0.8 (((411331.2 5084721, 411330.8 5084718, 411329.3 5084715, …
#>  9         17     1.4 (((411302.9 5084670, 411302.4 5084667, 411301 5084665, 41…
#> 10         19     1.5 (((411300.8 5084593, 411300.3 5084590, 411298.9 5084588, …
#> # ℹ 54 more rows
# }
```
