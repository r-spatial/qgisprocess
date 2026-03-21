# Convert a qgis_result object or one of its elements to a raster object

Convert a qgis_result object or one of its elements to a raster object

## Usage

``` r
qgis_as_raster(x, ...)

qgis_as_brick(x, ...)

# S3 method for class 'qgis_outputRaster'
qgis_as_raster(x, ...)

# S3 method for class 'qgis_outputRaster'
qgis_as_brick(x, ...)

# S3 method for class 'qgis_outputLayer'
qgis_as_raster(x, ...)

# S3 method for class 'qgis_outputLayer'
qgis_as_brick(x, ...)

# S3 method for class 'qgis_result'
qgis_as_raster(x, ...)

# S3 method for class 'qgis_result'
qgis_as_brick(x, ...)
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
  [`raster::raster()`](https://rdrr.io/pkg/raster/man/raster.html) or
  [`raster::brick()`](https://rdrr.io/pkg/raster/man/brick.html).

## Value

A `RasterLayer` or a `RasterBrick` object.

## See also

Other topics about coercing processing output:
[`qgis_as_terra()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_terra.md),
[`st_as_sf`](https://r-spatial.github.io/qgisprocess/reference/st_as_sf.md),
[`st_as_stars`](https://r-spatial.github.io/qgisprocess/reference/st_as_stars.md)

Other topics about accessing or managing processing results:
[`qgis_as_terra()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_terra.md),
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
# output from the `result` object and reading as RasterLayer:
qgis_as_raster(result)
#> class      : RasterLayer 
#> dimensions : 100, 100, 10000  (nrow, ncol, ncell)
#> resolution : 18.27273, 15.53535  (x, y)
#> extent     : 409939.9, 411767.1, 5083307, 5084861  (xmin, xmax, ymin, ymax)
#> crs        : +proj=utm +zone=20 +datum=NAD83 +units=m +no_defs 
#> source     : file2f34c3c33eb.tif 
#> names      : file2f34c3c33eb 
#> 

# if you need more control, extract the needed output element first:
output_raster <- qgis_extract_output(result, "OUTPUT")
qgis_as_raster(output_raster)
#> class      : RasterLayer 
#> dimensions : 100, 100, 10000  (nrow, ncol, ncell)
#> resolution : 18.27273, 15.53535  (x, y)
#> extent     : 409939.9, 411767.1, 5083307, 5084861  (xmin, xmax, ymin, ymax)
#> crs        : +proj=utm +zone=20 +datum=NAD83 +units=m +no_defs 
#> source     : file2f34c3c33eb.tif 
#> names      : file2f34c3c33eb 
#> 
# }
```
