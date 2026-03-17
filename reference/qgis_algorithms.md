# List algorithms, processing providers or plugins

Functions that return metadata about the installed and enabled
algorithms or processing providers, or about the installed plugins that
implement processing providers. See the [QGIS
docs](https://docs.qgis.org/latest/en/docs/user_manual/processing_algs/qgis/index.html)
for a detailed description of the algorithms provided 'out of the box'
on QGIS.

## Usage

``` r
qgis_algorithms(query = FALSE, quiet = TRUE, include_deprecated = TRUE)

qgis_providers(query = FALSE, quiet = TRUE, include_deprecated = TRUE)

qgis_plugins(which = "all", query = FALSE, quiet = TRUE, ...)
```

## Arguments

- query:

  Use `TRUE` to refresh the cached value.

- quiet:

  Use `FALSE` to display more information, possibly useful for
  debugging.

- include_deprecated:

  Logical. Should deprecated algorithms be included?

- which:

  String defining which plugins to select, based on their status in QGIS
  (enabled or disabled). Must be one of: `"all"`, `"enabled"`,
  `"disabled"`.

- ...:

  Only used by other functions calling this function.

## Value

A tibble of algorithms, processing providers or plugins, with metadata.

## Details

The `include_deprecated` argument in `qgis_algorithms()` does not affect
the cached value. The latter always includes deprecated algorithms if
these are returned by 'qgis_process' (this requires the JSON output
method).

## See also

[`qgis_enable_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_enable_plugins.md),
[`qgis_disable_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_enable_plugins.md)

Other topics about information on algorithms & processing providers:
[`qgis_search_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_search_algorithms.md),
[`qgis_show_help()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md)

Other topics about reporting the QGIS state:
[`has_qgis()`](https://r-spatial.github.io/qgisprocess/reference/has_qgis.md),
[`qgis_path()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md),
[`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)

## Examples

``` r
qgis_algorithms()
#> # A tibble: 409 × 25
#>    provider provider_title algorithm                algorithm_id algorithm_title
#>    <chr>    <chr>          <chr>                    <chr>        <chr>          
#>  1 3d       QGIS (3D)      3d:tessellate            tessellate   Tessellate     
#>  2 gdal     GDAL           gdal:aspect              aspect       Aspect         
#>  3 gdal     GDAL           gdal:assignprojection    assignproje… Assign project…
#>  4 gdal     GDAL           gdal:buffervectors       buffervecto… Buffer vectors 
#>  5 gdal     GDAL           gdal:buildvirtualraster  buildvirtua… Build virtual …
#>  6 gdal     GDAL           gdal:buildvirtualvector  buildvirtua… Build virtual …
#>  7 gdal     GDAL           gdal:cliprasterbyextent  cliprasterb… Clip raster by…
#>  8 gdal     GDAL           gdal:cliprasterbymaskla… cliprasterb… Clip raster by…
#>  9 gdal     GDAL           gdal:clipvectorbyextent  clipvectorb… Clip vector by…
#> 10 gdal     GDAL           gdal:clipvectorbypolygon clipvectorb… Clip vector by…
#> # ℹ 399 more rows
#> # ℹ 20 more variables: provider_can_be_activated <lgl>,
#> #   provider_is_active <lgl>, provider_long_name <chr>, provider_version <chr>,
#> #   provider_warning <chr>, can_cancel <lgl>, deprecated <lgl>, group <chr>,
#> #   has_known_issues <lgl>, help_url <chr>, requires_matching_crs <lgl>,
#> #   short_description <chr>, tags <list>, default_raster_file_format <chr>,
#> #   default_raster_file_extension <chr>, default_vector_file_extension <chr>, …
qgis_algorithms(include_deprecated = FALSE)
#> # A tibble: 388 × 25
#>    provider provider_title algorithm                algorithm_id algorithm_title
#>    <chr>    <chr>          <chr>                    <chr>        <chr>          
#>  1 3d       QGIS (3D)      3d:tessellate            tessellate   Tessellate     
#>  2 gdal     GDAL           gdal:aspect              aspect       Aspect         
#>  3 gdal     GDAL           gdal:assignprojection    assignproje… Assign project…
#>  4 gdal     GDAL           gdal:buffervectors       buffervecto… Buffer vectors 
#>  5 gdal     GDAL           gdal:buildvirtualraster  buildvirtua… Build virtual …
#>  6 gdal     GDAL           gdal:buildvirtualvector  buildvirtua… Build virtual …
#>  7 gdal     GDAL           gdal:cliprasterbyextent  cliprasterb… Clip raster by…
#>  8 gdal     GDAL           gdal:cliprasterbymaskla… cliprasterb… Clip raster by…
#>  9 gdal     GDAL           gdal:clipvectorbyextent  clipvectorb… Clip vector by…
#> 10 gdal     GDAL           gdal:clipvectorbypolygon clipvectorb… Clip vector by…
#> # ℹ 378 more rows
#> # ℹ 20 more variables: provider_can_be_activated <lgl>,
#> #   provider_is_active <lgl>, provider_long_name <chr>, provider_version <chr>,
#> #   provider_warning <chr>, can_cancel <lgl>, deprecated <lgl>, group <chr>,
#> #   has_known_issues <lgl>, help_url <chr>, requires_matching_crs <lgl>,
#> #   short_description <chr>, tags <list>, default_raster_file_format <chr>,
#> #   default_raster_file_extension <chr>, default_vector_file_extension <chr>, …
qgis_providers()
#> # A tibble: 4 × 3
#>   provider provider_title    algorithm_count
#>   <chr>    <chr>                       <int>
#> 1 gdal     GDAL                           57
#> 2 qgis     QGIS                           35
#> 3 3d       QGIS (3D)                       1
#> 4 native   QGIS (native c++)             316
qgis_plugins(quiet = FALSE)
#> Reading plugin list from the qgisprocess cache.
#>   If you changed plugin availability or status in the QGIS GUI since you loaded the
#>   qgisprocess package, then you must run `qgis_configure()` or reload the package
#>   to capture these changes.
#> 0 out of 3 available processing provider plugins are enabled.
#> # A tibble: 3 × 2
#>   name                    enabled
#>   <chr>                   <lgl>  
#> 1 grassprovider           FALSE  
#> 2 processing              FALSE  
#> 3 processing_saga_nextgen FALSE  
qgis_plugins(which = "disabled")
#> # A tibble: 3 × 2
#>   name                    enabled
#>   <chr>                   <lgl>  
#> 1 grassprovider           FALSE  
#> 2 processing              FALSE  
#> 3 processing_saga_nextgen FALSE  
```
