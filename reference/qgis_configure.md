# Configure qgisprocess

Run `qgis_configure()` to bring the package configuration in line with
QGIS and to save this configuration to a persistent cache. See the
*Details* section for more information about setting the path of the
'qgis_process' command line tool.

## Usage

``` r
qgis_configure(quiet = FALSE, use_cached_data = FALSE)
```

## Arguments

- quiet:

  Use `FALSE` to display more information, possibly useful for
  debugging.

- use_cached_data:

  Use the cached algorithm list and `path` found when configuring
  qgisprocess during the last session. This saves some time loading the
  package.

## Value

The result of
[`processx::run()`](http://processx.r-lib.org/reference/run.md).

## Details

The qgisprocess package is a wrapper around the 'qgis_process' command
line tool distributed with QGIS (\>=3.14). Several functions use
heuristics to detect the location of the 'qgis_process' executable.

When loading the package, the configuration is automatically read from
the cache with `qgis_configure(use_cached_data = TRUE, quiet = TRUE)` in
order to save time. Run `qgis_configure(use_cached_data = TRUE)`
manually to get more details.

Use
[`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md),
[`qgis_providers()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md),
[`qgis_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md),
[`qgis_using_json_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md),
[`qgis_path()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md)
and
[`qgis_version()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md)
to inspect cache contents.

If the configuration fails or you have more than one QGIS installation,
you can set `options(qgisprocess.path = "path/to/qgis_process")` or the
`R_QGISPROCESS_PATH` environment variable (useful on CI). On Linux the
'qgis_process' executable is generally available on the user's PATH, on
MacOS the executable is within the QGIS\*.app/Contents/MacOS/bin folder,
and on Windows the executable is named qgis_process-qgis.bat or
qgis_process-qgis-dev.bat and is located in Program Files/QGIS\*/bin or
OSGeo4W(64)/bin.

## See also

[`qgis_unconfigure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_unconfigure.md)

[`qgis_path()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md),
[`qgis_version()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md)

Other topics about configuring QGIS and qgisprocess:
[`qgis_enable_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_enable_plugins.md),
[`qgis_run()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run.md)

## Examples

``` r
# \donttest{
# not running in R CMD check to save time
qgis_configure(use_cached_data = TRUE)
#> Checking configuration in cache file (/home/runner/.cache/R-qgisprocess/cache-0.4.1.9000.rds)
#> Checking cached QGIS version with version reported by 'qgis_process' ...
#> QGIS versions match! (3.44.7-Solothurn)
#> Checking cached QGIS plugins (and state) with those reported by 'qgis_process' ...
#> QGIS plugins match! (0 processing provider plugin(s) enabled)
#> 
#> >>> Run `qgis_enable_plugins()` to enable 3 disabled plugins and access
#>     their algorithms: grassprovider, processing,
#>     processing_saga_nextgen
#> 
#> Restoring configuration from '/home/runner/.cache/R-qgisprocess/cache-0.4.1.9000.rds'
#> QGIS version: 3.44.7-Solothurn
#> Using 'qgis_process' in the system PATH.
#> >>> If you need another installed QGIS instance, run `qgis_configure()`;
#>     see `?qgis_configure` if you need to preset the path of 'qgis_process'.
#> Using JSON for output serialization.
#> Using JSON for input serialization.
#> 0 out of 3 available processing provider plugins are enabled.
#> Having access to 409 algorithms from 4 QGIS processing providers.
#> Use qgis_algorithms(), qgis_providers(), qgis_plugins(), qgis_path() and
#> qgis_version() to inspect the cache environment.
# }

if (FALSE) { # \dontrun{
# package reconfiguration
# (not run in example() as it rewrites the package cache file)
qgis_configure()
} # }
```
