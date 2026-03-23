# Clean the package cache

Empties the qgisprocess cache environment.

## Usage

``` r
qgis_unconfigure()
```

## Value

`NULL`, invisibly.

## See also

Other topics about programming or debugging utilities:
[`qgis_result_status()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md),
[`qgis_run()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run.md),
[`qgis_tmp_file()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md),
[`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# not running this function in example() as it clears the cache environment.
qgis_unconfigure()
} # }

# undoing qgis_unconfigure() by repopulating the cache environment from file:
# \donttest{
# not running in R CMD check to save time
qgis_configure(use_cached_data = TRUE)
#> Checking configuration in cache file (/home/runner/.cache/R-qgisprocess/cache-0.4.2.9000.rds)
#> Checking cached QGIS version with version reported by 'qgis_process' ...
#> QGIS versions match! (3.44.7-Solothurn)
#> Checking cached QGIS plugins (and state) with those reported by 'qgis_process' ...
#> QGIS plugins match! (0 processing provider plugin(s) enabled)
#> 
#> >>> Run `qgis_enable_plugins()` to enable 3 disabled plugins and access
#>     their algorithms: grassprovider, processing,
#>     processing_saga_nextgen
#> 
#> Restoring configuration from '/home/runner/.cache/R-qgisprocess/cache-0.4.2.9000.rds'
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
```
