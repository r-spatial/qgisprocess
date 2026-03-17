# Convert a qgis_result object or one of its elements to a stars object

Convert a qgis_result object or one of its elements to a stars object

## Usage

``` r
# S3 method for class 'qgis_outputRaster'
st_as_stars(x, ...)

# S3 method for class 'qgis_outputLayer'
st_as_stars(x, ...)

# S3 method for class 'qgis_result'
st_as_stars(x, ...)
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
  [`stars::read_stars()`](https://r-spatial.github.io/stars/reference/read_stars.html).

## Value

A `stars` or a `stars_proxy` object.

## Details

The stars package must be loaded explicitly to use these methods.

## See also

Other topics about coercing processing output:
[`qgis_as_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md),
[`qgis_as_terra()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_terra.md),
[`st_as_sf`](https://r-spatial.github.io/qgisprocess/reference/st_as_sf.md)

Other topics about accessing or managing processing results:
[`qgis_as_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md),
[`qgis_as_terra()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_terra.md),
[`qgis_clean_result()`](https://r-spatial.github.io/qgisprocess/reference/qgis_clean_result.md),
[`qgis_extract_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md),
[`qgis_result_status()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md),
[`st_as_sf`](https://r-spatial.github.io/qgisprocess/reference/st_as_sf.md)

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
# output from the `result` object and reading as stars or stars_proxy:
stars::st_as_stars(result)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>                              Min.   1st Qu.    Median      Mean   3rd Qu.
#> file2f504783f407.tif  0.008276554 0.2864364 0.4271587 0.4269891 0.5803983
#>                            Max. NA's
#> file2f504783f407.tif  0.8075118 6475
#> dimension(s):
#>   from  to  offset  delta               refsys point x/y
#> x    1 100  409940  18.27 NAD83 / UTM zone 20N FALSE [x]
#> y    1 100 5084861 -15.54 NAD83 / UTM zone 20N FALSE [y]
stars::st_as_stars(result, proxy = TRUE)
#> stars_proxy object with 1 attribute in 1 file(s):
#> $file2f504783f407.tif
#> [1] "[...]/file2f504783f407.tif"
#> 
#> dimension(s):
#>   from  to  offset  delta               refsys point x/y
#> x    1 100  409940  18.27 NAD83 / UTM zone 20N FALSE [x]
#> y    1 100 5084861 -15.54 NAD83 / UTM zone 20N FALSE [y]

# if you need more control, extract the needed output element first:
output_raster <- qgis_extract_output(result, "OUTPUT")
stars::st_as_stars(output_raster)
#> stars object with 2 dimensions and 1 attribute
#> attribute(s):
#>                              Min.   1st Qu.    Median      Mean   3rd Qu.
#> file2f504783f407.tif  0.008276554 0.2864364 0.4271587 0.4269891 0.5803983
#>                            Max. NA's
#> file2f504783f407.tif  0.8075118 6475
#> dimension(s):
#>   from  to  offset  delta               refsys point x/y
#> x    1 100  409940  18.27 NAD83 / UTM zone 20N FALSE [x]
#> y    1 100 5084861 -15.54 NAD83 / UTM zone 20N FALSE [y]
# }
```
