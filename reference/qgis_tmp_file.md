# Manage temporary files

These functions create temporary files that can be used in calls to
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md)
or elsewhere. These files are created in a special temporary directory
(`qgis_tmp_base()`) that should be periodically cleaned up using
`qgis_clean_tmp()`. You can set your preferred vector and/or raster file
extension using `options(qgisprocess.tmp_vector_ext = "...")` and/or
`options(qgisprocess.tmp_raster_ext = "...")`, respectively.

## Usage

``` r
qgis_tmp_file(ext)

qgis_tmp_folder()

qgis_tmp_vector(ext = getOption("qgisprocess.tmp_vector_ext", ".gpkg"))

qgis_tmp_raster(ext = getOption("qgisprocess.tmp_raster_ext", ".tif"))

qgis_tmp_base()

qgis_clean_tmp()
```

## Arguments

- ext:

  The file extension to be used.

## Value

A character vector indicating the location of a (not yet created)
temporary file.

## See also

Other topics about programming or debugging utilities:
[`qgis_result_status()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md),
[`qgis_run()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run.md),
[`qgis_unconfigure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_unconfigure.md),
[`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)

## Examples

``` r
qgis_tmp_base()
#> [1] "/tmp/RtmpNcrB7h/file2f5016d84622"
qgis_tmp_file(".csv")
#> [1] "/tmp/RtmpNcrB7h/file2f5016d84622/file2f50988de05.csv"
qgis_tmp_vector()
#> [1] "/tmp/RtmpNcrB7h/file2f5016d84622/file2f5027b4f4f6.gpkg"
qgis_tmp_raster()
#> [1] "/tmp/RtmpNcrB7h/file2f5016d84622/file2f505cc4617b.tif"
```
