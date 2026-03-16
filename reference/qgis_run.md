# Call the 'qgis_process' command directly

`qgis_run()` offers full access to 'qgis_process'. Run
`cat(qgis_run("--help")$stdout)` to get the command's help.

## Usage

``` r
qgis_run(args = character(), ..., env = qgis_env(), path = qgis_path())
```

## Arguments

- args:

  Command-line arguments

- ...:

  Passed to
  [`processx::run()`](http://processx.r-lib.org/reference/run.md).

- env:

  A [`list()`](https://rdrr.io/r/base/list.html) of environment
  variables. Defaults to
  `getOption("qgisprocess.env", list(QT_QPA_PLATFORM = "offscreen"))`.

- path:

  A path to the 'qgis_process' executable. Defaults to
  [`qgis_path()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md).

## Value

A [`processx::run()`](http://processx.r-lib.org/reference/run.md) return
value, i.e. a list with `status`, `stdout`, `stderr` and `timeout`
elements.

## See also

Other topics about programming or debugging utilities:
[`qgis_result_status()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md),
[`qgis_tmp_file()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md),
[`qgis_unconfigure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_unconfigure.md),
[`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)

Other topics about configuring QGIS and qgisprocess:
[`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md),
[`qgis_enable_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_enable_plugins.md)

## Examples

``` r
processx_list <- qgis_run(args = "--help")
cat(processx_list$stdout)
#> QGIS Processing Executor - 3.44.7-Solothurn 'Solothurn' (3.44.7-Solothurn)
#> Usage: /usr/bin/qgis_process.bin [--help] [--version] [--json] [--verbose] [--no-python] [--skip-loading-plugins] [command] [algorithm id, path to model file, or path to Python script] [parameters]
#> 
#> Options:
#>  --help or -h        Output the help
#>  --version or -v     Output all versions related to QGIS Process
#>  --json          Output results as JSON objects
#>  --verbose       Output verbose logs
#>  --no-python     Disable Python support (results in faster startup)
#>  --skip-loading-plugins  Avoid loading enabled plugins (results in faster startup)
#> Available commands:
#>  plugins     list available and active plugins
#>  plugins enable  enables an installed plugin. The plugin name must be specified, e.g. "plugins enable cartography_tools"
#>  plugins disable disables an installed plugin. The plugin name must be specified, e.g. "plugins disable cartography_tools"
#>  list        list all available processing algorithms
#>  help        show help for an algorithm. The algorithm id or a path to a model file must be specified.
#>  run     runs an algorithm. The algorithm id or a path to a model file and parameter values must be specified. Parameter values are specified after -- with PARAMETER=VALUE syntax. Ordered list values for a parameter can be created by specifying the parameter multiple times, e.g. --LAYERS=layer1.shp --LAYERS=layer2.shp
#>          Alternatively, a '-' character in place of the parameters argument indicates that the parameters should be read from STDIN as a JSON object. The JSON should be structured as a map containing at least the "inputs" key specifying a map of input parameter values. This implies the --json option for output as a JSON object.
#>          If required, the ellipsoid to use for distance and area calculations can be specified via the "--ELLIPSOID=name" argument.
#>          If required, an existing QGIS project to use during the algorithm execution can be specified via the "--PROJECT_PATH=path" argument.
#>          When passing parameters as a JSON object from STDIN, these extra arguments can be provided as an "ellipsoid" and a "project_path" key respectively.
```
