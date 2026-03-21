# Create a wrapper function that runs one algorithm

As opposed to
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md),
`qgis_function()` creates a callable function based on the argument
metadata provided by
[`qgis_get_argument_specs()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md).

## Usage

``` r
qgis_function(algorithm, ...)
```

## Arguments

- algorithm:

  A qualified algorithm name (e.g., `"native:buffer"`).

- ...:

  Algorithm arguments. These values are evaluated once and immediately,
  so you shouldn't call
  [`qgis_tmp_file()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md)
  here.

## Value

A function.

## Details

The logic of `qgis_function()` has been implemented in R package
[qgis](https://github.com/JanCaha/r_package_qgis). This package also
provides the QGIS documentation of each processing algorithm as
corresponding R function documentation.

## Examples

``` r
qgis_buffer <- qgis_function("native:buffer")
qgis_buffer(
  system.file(
    "longlake/longlake_depth.gpkg",
    package = "qgisprocess"
  ),
  DISTANCE = 10
)
#> Argument `SEGMENTS` is unspecified (using QGIS default value).
#> Using `END_CAP_STYLE = "Round"`
#> Using `JOIN_STYLE = "Round"`
#> Argument `MITER_LIMIT` is unspecified (using QGIS default value).
#> Argument `DISSOLVE` is unspecified (using QGIS default value).
#> Argument `SEPARATE_DISJOINT` is unspecified (using QGIS default value).
#> Using `OUTPUT = qgis_tmp_vector()`
#> <Result of `qgis_run_algorithm("native:buffer", ...)`>
#> List of 1
#>  $ OUTPUT: 'qgis_outputVector' chr "/tmp/RtmpNRpHJl/file2f345349896/file2f347f26ea51.gpkg"
```
