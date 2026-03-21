# Run an algorithm using 'qgis_process': pipe-friendly wrapper

`qgis_run_algorithm_p()` wraps
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md),
passing its first argument to the first argument of the QGIS
`algorithm`. This makes it more convenient in a pipeline (hence '\_p' in
the name).

## Usage

``` r
qgis_run_algorithm_p(
  .data,
  algorithm,
  ...,
  .select = "OUTPUT",
  .clean = TRUE,
  .quiet = TRUE
)
```

## Arguments

- .data:

  Passed to the first input of `algorithm`. If `.data` is a
  `qgis_result` (the result of a previous processing step),
  `.data[[.select]]` is passed instead.

- algorithm:

  A qualified algorithm name (e.g., `"native:buffer"`).

- ...:

  Other algorithm arguments. These values are evaluated once and
  immediately, so you shouldn't call
  [`qgis_tmp_file()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md)
  here.

- .select:

  String. The name of the element to select from `.data` if the latter
  is a `qgis_result`. Defaults to `"OUTPUT"`.

- .clean:

  Logical. Should an incoming `qgis_result` be cleaned (using
  [`qgis_clean_result()`](https://r-spatial.github.io/qgisprocess/reference/qgis_clean_result.md))
  after processing?

- .quiet:

  Use `FALSE` to get extra output from 'qgis_process'. This can be
  useful in debugging.

## Value

A `qgis_result` object.

## Details

Uses
[`qgis_function()`](https://r-spatial.github.io/qgisprocess/reference/qgis_function.md)
under the hood.

## See also

Other functions to run one geoprocessing algorithm:
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md)

## Examples

``` r
system.file(
  "longlake/longlake_depth.gpkg",
  package = "qgisprocess"
) |>
  qgis_run_algorithm_p(
    "native:buffer",
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
#>  $ OUTPUT: 'qgis_outputVector' chr "/tmp/RtmpNRpHJl/file2f345349896/file2f3446f6b753.gpkg"
```
