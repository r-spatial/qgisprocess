# Clean processing results

Deletes any temporary files that are defined in a `qgis_result` object.
These may comprise both input and output files.

## Usage

``` r
qgis_clean_result(x)
```

## Arguments

- x:

  A `qgis_result` object returned by
  [`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md).

## Value

The `qgis_result` object passed to the function is returned invisibly.

## See also

Other topics about accessing or managing processing results:
[`qgis_as_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md),
[`qgis_as_terra()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_terra.md),
[`qgis_extract_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md),
[`qgis_result_status()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md),
[`st_as_sf`](https://r-spatial.github.io/qgisprocess/reference/st_as_sf.md),
[`st_as_stars`](https://r-spatial.github.io/qgisprocess/reference/st_as_stars.md)

## Examples

``` r
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

file.exists(qgis_extract_output(result))
#> [1] TRUE
qgis_clean_result(result)
file.exists(qgis_extract_output(result))
#> [1] FALSE
```
