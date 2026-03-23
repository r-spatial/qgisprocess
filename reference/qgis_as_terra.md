# Convert a qgis_result object or one of its elements to a terra object

This function performs coercion to one of the terra classes
`SpatRaster`, `SpatVector` or `SpatVectorProxy` (add `proxy = TRUE` for
the latter). The distinction between `SpatRaster` and `SpatVector` is
based on the output type.

## Usage

``` r
qgis_as_terra(x, ...)

# S3 method for class 'qgis_outputRaster'
qgis_as_terra(x, ...)

# S3 method for class 'qgis_outputLayer'
qgis_as_terra(x, ...)

# S3 method for class 'qgis_outputVector'
qgis_as_terra(x, ...)

# S3 method for class 'qgis_result'
qgis_as_terra(x, ...)
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
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html),
  depending on the output type of `x` (or one of its elements, if `x` is
  a `qgis_result`).

## Value

A `SpatRaster`, `SpatVector` or `SpatVectorProxy` object.

## See also

Other topics about coercing processing output:
[`qgis_as_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md),
[`st_as_sf`](https://r-spatial.github.io/qgisprocess/reference/st_as_sf.md),
[`st_as_stars`](https://r-spatial.github.io/qgisprocess/reference/st_as_stars.md)

Other topics about accessing or managing processing results:
[`qgis_as_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md),
[`qgis_clean_result()`](https://r-spatial.github.io/qgisprocess/reference/qgis_clean_result.md),
[`qgis_extract_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md),
[`qgis_result_status()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md),
[`st_as_sf`](https://r-spatial.github.io/qgisprocess/reference/st_as_sf.md),
[`st_as_stars`](https://r-spatial.github.io/qgisprocess/reference/st_as_stars.md)

## Examples

``` r
# \donttest{
# not running below examples in R CMD check to save time
result <- qgis_run_algorithm(
  "native:slope",
  INPUT = system.file("longlake/longlake_depth.tif", package = "qgisprocess")
)
#> Argument `Z_FACTOR` is unspecified (using QGIS default value).
#> Using `OUTPUT = qgis_tmp_raster()`

# most direct approach, autoselecting a `qgis_outputRaster` type
# output from the `result` object and reading as SpatRaster:
qgis_as_terra(result)
#> class       : SpatRaster 
#> size        : 100, 100, 1  (nrow, ncol, nlyr)
#> resolution  : 18.27273, 15.53535  (x, y)
#> extent      : 409939.9, 411767.1, 5083307, 5084861  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / UTM zone 20N (EPSG:26920) 
#> source      : file2f7b3bc66bd6.tif 
#> name        : file2f7b3bc66bd6 

# if you need more control, extract the needed output element first:
output_raster <- qgis_extract_output(result, "OUTPUT")
qgis_as_terra(output_raster)
#> class       : SpatRaster 
#> size        : 100, 100, 1  (nrow, ncol, nlyr)
#> resolution  : 18.27273, 15.53535  (x, y)
#> extent      : 409939.9, 411767.1, 5083307, 5084861  (xmin, xmax, ymin, ymax)
#> coord. ref. : NAD83 / UTM zone 20N (EPSG:26920) 
#> source      : file2f7b3bc66bd6.tif 
#> name        : file2f7b3bc66bd6 

# Same holds for coercion to SpatVector
result2 <- qgis_run_algorithm(
  "native:buffer",
  INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
  DISTANCE = 100
)
#> Argument `SEGMENTS` is unspecified (using QGIS default value).
#> Using `END_CAP_STYLE = "Round"`
#> Using `JOIN_STYLE = "Round"`
#> Argument `MITER_LIMIT` is unspecified (using QGIS default value).
#> Argument `DISSOLVE` is unspecified (using QGIS default value).
#> Argument `SEPARATE_DISJOINT` is unspecified (using QGIS default value).
#> Using `OUTPUT = qgis_tmp_vector()`

qgis_as_terra(result2)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 1, 1  (geometries, attributes)
#>  extent      : 409850.6, 411856.3, 5083216, 5084951  (xmin, xmax, ymin, ymax)
#>  source      : file2f7b73d043d6.gpkg
#>  coord. ref. : NAD83 / UTM zone 20N (EPSG:26920) 
#>  names       :     label
#>  type        :     <chr>
#>  values      : Long Lake
output_vector <- qgis_extract_output(result2, "OUTPUT")
qgis_as_terra(output_vector)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 1, 1  (geometries, attributes)
#>  extent      : 409850.6, 411856.3, 5083216, 5084951  (xmin, xmax, ymin, ymax)
#>  source      : file2f7b73d043d6.gpkg
#>  coord. ref. : NAD83 / UTM zone 20N (EPSG:26920) 
#>  names       :     label
#>  type        :     <chr>
#>  values      : Long Lake

# SpatVectorProxy:
qgis_as_terra(result2, proxy = TRUE)
#>  class       : SpatVectorProxy
#>  geometry    : polygons 
#>  dimensions  : 1, 1  (geometries, attributes)
#>  extent      : 409850.6, 411856.3, 5083216, 5084951  (xmin, xmax, ymin, ymax)
#>  source      : file2f7b73d043d6.gpkg
#>  coord. ref. : NAD83 / UTM zone 20N (EPSG:26920) 
#>  names       : label
#>  type        : <chr>
# }
```
