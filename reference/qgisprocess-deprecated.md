# Deprecated functions

- Instead of `qgis_output()`, use
  [`qgis_extract_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md)
  and related functions.

- Instead of `qgis_result_single()`, use
  [`qgis_extract_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md)
  and related functions.

- Instead of `qgis_detect_windows()`, use
  [`qgis_detect_windows_paths()`](https://r-spatial.github.io/qgisprocess/reference/qgis_detect_paths.md).

- Instead of `qgis_detect_macos()`, use
  [`qgis_detect_macos_paths()`](https://r-spatial.github.io/qgisprocess/reference/qgis_detect_paths.md).

- Instead of `qgis_use_json_input()`, use
  [`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md).

- Instead of `qgis_use_json_output()`, use
  [`qgis_using_json_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md).

- Instead of `qgis_description()`, use
  [`qgis_get_description()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md).

- Instead of `qgis_arguments()`, use
  [`qgis_get_argument_specs()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md).

- Instead of `qgis_outputs()`, use
  [`qgis_get_output_specs()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md).

- Instead of `qgis_pipe()`, use
  [`qgis_run_algorithm_p()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm_p.md).

- Instead of `qgis_tmp_clean()`, use
  [`qgis_clean_tmp()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md).

- Instead of `qgis_result_clean()`, use
  [`qgis_clean_result()`](https://r-spatial.github.io/qgisprocess/reference/qgis_clean_result.md).

## Usage

``` r
qgis_output(x, which)

qgis_result_single(x, what)

qgis_detect_windows(...)

qgis_detect_macos()

qgis_use_json_input()

qgis_use_json_output(...)

qgis_description(...)

qgis_arguments(...)

qgis_outputs(...)

qgis_pipe(...)

qgis_tmp_clean(...)

qgis_result_clean(...)
```

## Arguments

- x:

  A `qgis_result` object returned by
  [`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md).

- which:

  The index of an output.

- what:

  Character vector of classes.

- ...:

  Arguments passed to the new function. This is done for functions where
  only the function name changed at time of deprecation.

## Value

A value as described in the documentation of the corresponding new
function.
