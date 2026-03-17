# Get metadata about the used 'qgis_process' command

`qgis_path()` returns the filepath of the 'qgis_process' command, while
`qgis_version()` returns the QGIS version.

## Usage

``` r
qgis_path(query = FALSE, quiet = TRUE)

qgis_version(query = FALSE, quiet = TRUE, full = TRUE, debug = FALSE)
```

## Arguments

- query:

  Use `TRUE` to refresh the cached value.

- quiet:

  Use `FALSE` to display more information, possibly useful for
  debugging.

- full:

  Logical. If `FALSE`, only return the `"x.y.z"` version string instead
  of the full version string that includes the name. Defaults to `TRUE`;
  ignored if `debug = TRUE`.

- debug:

  Logical. If `TRUE`, also output the version of QGIS, the operating
  system and all relevant libraries, as reported by the 'qgis_process'
  command.

## Value

A string.

## See also

[`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md)

Other topics about reporting the QGIS state:
[`has_qgis()`](https://r-spatial.github.io/qgisprocess/reference/has_qgis.md),
[`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md),
[`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)

## Examples

``` r
qgis_path()
#> [1] "qgis_process"
qgis_path(quiet = FALSE)
#> Using 'qgis_process' in the system PATH.
#> >>> If you need another installed QGIS instance, run `qgis_configure()`;
#>     see `?qgis_configure` if you need to preset the path of 'qgis_process'.
#> [1] "qgis_process"
qgis_version()
#> [1] "3.44.7-Solothurn"
qgis_version(full = FALSE)
#> [1] "3.44.7"
qgis_version(debug = TRUE)
#> [1] "3.44.7-Solothurn"
#> 
#> Using qgisprocess 0.4.1.9229
#> 
#> Versions reported by 'qgis_process':
#> ------------------------------------
#> QGIS 3.44.7-Solothurn 'Solothurn' (ea262bc5ed8)
#> QGIS code revision ea262bc5ed8
#> Qt version 5.15.13
#> Python version 3.12.3
#> GDAL/OGR version 3.8.4
#> PROJ version 9.4.0
#> EPSG Registry database version v11.004 (2024-02-24)
#> GEOS version 3.12.1-CAPI-1.18.1
#> SQLite version 3.45.1
#> OS Ubuntu 24.04.3 LTS
```
