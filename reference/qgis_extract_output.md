# Access processing output

These functions extract one output element from the result of
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md),
potentially more than one in the case of
`qgis_extract_output_by_class()`. An output element can be extracted
based on its name, its position in the printed `qgis_result` object
returned by
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md),
or its class.

`qgis_extract_output()` is an alias to `qgis_extract_output_by_name()`.

## Usage

``` r
qgis_extract_output_by_name(x, name = "OUTPUT", first = TRUE)

qgis_extract_output(x, name = "OUTPUT", first = TRUE)

qgis_extract_output_by_position(x, which)

qgis_extract_output_by_class(x, class, single = TRUE)
```

## Arguments

- x:

  A `qgis_result` object returned by
  [`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md).

- name:

  The name of an output.

- first:

  Logical. Should `qgis_extract_output_by_name()` fall back to the first
  output element if the default `OUTPUT` or `output` element is not
  available? Only takes effect if `name` is equal to `OUTPUT` or
  `output`, but not found.

- which:

  The index of an output.

- class:

  Character vector of classes. At least one class must be inherited by
  an element of `x` for that element to be selected.

- single:

  Logical. Ensures the selection of a single output in
  `qgis_extract_output_by_class()`. The `OUTPUT` or `output` element is
  taken if available and on condition that it inherits a specified
  class; otherwise falls back to the first element that inherits a
  specified class.

## Value

A `qgis_output*` object.

## See also

Other topics about accessing or managing processing results:
[`qgis_as_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md),
[`qgis_as_terra()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_terra.md),
[`qgis_clean_result()`](https://r-spatial.github.io/qgisprocess/reference/qgis_clean_result.md),
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

# the print() method of a qgis_result only prints its output elements:
result
#> <Result of `qgis_run_algorithm("native:buffer", ...)`>
#> List of 1
#>  $ OUTPUT: 'qgis_outputVector' chr "/tmp/RtmpNRpHJl/file2f345349896/file2f3433df4884.gpkg"

# nevertheless, more elements are included:
length(result)
#> [1] 5
names(result)
#> [1] "OUTPUT"           ".algorithm"       ".args"            ".raw_json_input" 
#> [5] ".processx_result"

# extract the output element 'OUTPUT':
qgis_extract_output(result)
#> [1] "/tmp/RtmpNRpHJl/file2f345349896/file2f3433df4884.gpkg"
#> attr(,"class")
#> [1] "qgis_outputVector"
```
