# Access processing results: extra tools

A `qgis_result` object is a list that, next to the output elements, also
contains other elements that can be useful in scripting. Several of
these can be extracted with convenience functions: the exit status of
the process, standard output and standard error of 'qgis_process',
arguments passed to 'qgis_process'.

## Usage

``` r
qgis_result_status(x)

qgis_result_stdout(x)

qgis_result_stderr(x)

qgis_result_args(x)
```

## Arguments

- x:

  A `qgis_result` object returned by
  [`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md).

## Value

- A number in case of `qgis_result_status()`.

- A string in case of `qgis_result_stdout()` and `qgis_result_stderr()`.

- A list in case of `qgis_result_args()`.

## See also

Other topics about programming or debugging utilities:
[`qgis_run()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run.md),
[`qgis_tmp_file()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md),
[`qgis_unconfigure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_unconfigure.md),
[`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)

Other topics about accessing or managing processing results:
[`qgis_as_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md),
[`qgis_as_terra()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_terra.md),
[`qgis_clean_result()`](https://r-spatial.github.io/qgisprocess/reference/qgis_clean_result.md),
[`qgis_extract_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md),
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

qgis_result_status(result)
#> [1] 0
stdout <- qgis_result_stdout(result)
cat(substr(stdout, 1, 335))
#> {
#>   "algorithm_details": {
#>     "can_cancel": true,
#>     "deprecated": false,
#>     "documentation_flags": [
#>       "regenerates_primary_key_in_some_scenarios"
#>     ],
#>     "group": "Vector geometry",
#>     "has_known_issues": false,
#>     "help_url": null,
#>     "id": "native:buffer",
#>     "name": "Buffer",
#>     "requires_matching_crs": false,
#>    
qgis_result_stderr(result)
#> [1] ""
qgis_result_args(result)
#> $INPUT
#> [1] "/home/runner/work/_temp/Library/qgisprocess/longlake/longlake_depth.gpkg"
#> 
#> $DISTANCE
#> [1] 10
#> 
#> $END_CAP_STYLE
#> [1] 0
#> 
#> $JOIN_STYLE
#> [1] 0
#> 
#> $OUTPUT
#> [1] "/tmp/RtmpI7zuRM/file307d29539b8c/file307d16c6b8a6.gpkg"
#> 
```
