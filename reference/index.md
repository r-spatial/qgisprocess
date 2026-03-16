# Package index

## Discover and learn geoprocessing algorithms

- [`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
  [`qgis_providers()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
  [`qgis_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
  : List algorithms, processing providers or plugins
- [`qgis_search_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_search_algorithms.md)
  : Search geoprocessing algorithms
- [`qgis_show_help()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md)
  [`qgis_get_description()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md)
  [`qgis_get_argument_specs()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md)
  [`qgis_get_output_specs()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md)
  : Get detailed information about one algorithm

## Prepare special input arguments

- [`qgis_list_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_list_input.md)
  [`qgis_dict_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_list_input.md)
  : Prepare a compound input argument

## Run geoprocessing algorithms

- [`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md)
  : Run an algorithm using 'qgis_process'
- [`qgis_run_algorithm_p()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm_p.md)
  : Run an algorithm using 'qgis_process': pipe-friendly wrapper
- [`qgis_function()`](https://r-spatial.github.io/qgisprocess/reference/qgis_function.md)
  : Create a wrapper function that runs one algorithm

## Handle processing results

- [`qgis_extract_output_by_name()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md)
  [`qgis_extract_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md)
  [`qgis_extract_output_by_position()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md)
  [`qgis_extract_output_by_class()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md)
  : Access processing output
- [`qgis_clean_result()`](https://r-spatial.github.io/qgisprocess/reference/qgis_clean_result.md)
  : Clean processing results

## Coerce processing output

- [`qgis_as_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md)
  [`qgis_as_brick()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_raster.md)
  : Convert a qgis_result object or one of its elements to a raster
  object
- [`qgis_as_terra()`](https://r-spatial.github.io/qgisprocess/reference/qgis_as_terra.md)
  : Convert a qgis_result object or one of its elements to a terra
  object
- [`st_as_sf(`*`<qgis_result>`*`)`](https://r-spatial.github.io/qgisprocess/reference/st_as_sf.md)
  [`st_as_sf(`*`<qgis_outputVector>`*`)`](https://r-spatial.github.io/qgisprocess/reference/st_as_sf.md)
  [`st_as_sf(`*`<qgis_outputLayer>`*`)`](https://r-spatial.github.io/qgisprocess/reference/st_as_sf.md)
  : Convert a qgis_result object or one of its elements to an sf object
- [`st_as_stars(`*`<qgis_outputRaster>`*`)`](https://r-spatial.github.io/qgisprocess/reference/st_as_stars.md)
  [`st_as_stars(`*`<qgis_outputLayer>`*`)`](https://r-spatial.github.io/qgisprocess/reference/st_as_stars.md)
  [`st_as_stars(`*`<qgis_result>`*`)`](https://r-spatial.github.io/qgisprocess/reference/st_as_stars.md)
  : Convert a qgis_result object or one of its elements to a stars
  object

## Explore and manage QGIS and qgisprocess state

- [`has_qgis()`](https://r-spatial.github.io/qgisprocess/reference/has_qgis.md)
  [`qgis_has_plugin()`](https://r-spatial.github.io/qgisprocess/reference/has_qgis.md)
  [`qgis_has_provider()`](https://r-spatial.github.io/qgisprocess/reference/has_qgis.md)
  [`qgis_has_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/has_qgis.md)
  : Check availability of QGIS, a plugin, a provider or an algorithm
- [`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
  [`qgis_providers()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
  [`qgis_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
  : List algorithms, processing providers or plugins
- [`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md)
  : Configure qgisprocess
- [`qgis_detect_paths()`](https://r-spatial.github.io/qgisprocess/reference/qgis_detect_paths.md)
  [`qgis_detect_windows_paths()`](https://r-spatial.github.io/qgisprocess/reference/qgis_detect_paths.md)
  [`qgis_detect_macos_paths()`](https://r-spatial.github.io/qgisprocess/reference/qgis_detect_paths.md)
  : Detect QGIS installations with 'qgis_process' on Windows and macOS
- [`qgis_enable_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_enable_plugins.md)
  [`qgis_disable_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_enable_plugins.md)
  : Enable or disable QGIS plugins
- [`qgis_path()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md)
  [`qgis_version()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md)
  : Get metadata about the used 'qgis_process' command
- [`qgis_search_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_search_algorithms.md)
  : Search geoprocessing algorithms
- [`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)
  [`qgis_using_json_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)
  : Report if JSON objects are used for input to and output from
  'qgis_process'

## Programming and debugging

- [`qgis_result_status()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md)
  [`qgis_result_stdout()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md)
  [`qgis_result_stderr()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md)
  [`qgis_result_args()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md)
  : Access processing results: extra tools
- [`qgis_run()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run.md)
  : Call the 'qgis_process' command directly
- [`qgis_tmp_file()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md)
  [`qgis_tmp_folder()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md)
  [`qgis_tmp_vector()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md)
  [`qgis_tmp_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md)
  [`qgis_tmp_base()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md)
  [`qgis_clean_tmp()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md)
  : Manage temporary files
- [`qgis_unconfigure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_unconfigure.md)
  : Clean the package cache
- [`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)
  [`qgis_using_json_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)
  : Report if JSON objects are used for input to and output from
  'qgis_process'
