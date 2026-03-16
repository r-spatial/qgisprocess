# Check availability of QGIS, a plugin, a provider or an algorithm

`has_qgis()` checks whether the loaded qgisprocess cache is populated,
which means that a QGIS installation was accessible and responsive when
loading the package. `qgis_has_plugin()`, `qgis_has_provider()` and
`qgis_has_algorithm()` check for the availability of one or several
plugins, processing providers and algorithms, respectively. They are
vectorized.

## Usage

``` r
has_qgis()

qgis_has_plugin(plugin, query = FALSE, quiet = TRUE)

qgis_has_provider(provider, query = FALSE, quiet = TRUE)

qgis_has_algorithm(algorithm, query = FALSE, quiet = TRUE)
```

## Arguments

- plugin:

  A plugin name (e.g., `"native"`). Can be a vector of names.

- query:

  Use `TRUE` to refresh the cached value.

- quiet:

  Use `FALSE` to display more information, possibly useful for
  debugging.

- provider:

  A provider name (e.g., `"native"`). Can be a vector of names.

- algorithm:

  A qualified algorithm name (e.g., `"native:buffer"`). Can be a vector
  of names.

## Value

A logical, with length 1 in case of `has_qgis()`.

## Note

Only plugins that implement processing providers are supported.

## See also

Other topics about reporting the QGIS state:
[`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md),
[`qgis_path()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md),
[`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)

## Examples

``` r
has_qgis()
#> [1] TRUE
if (has_qgis()) qgis_has_algorithm("native:filedownloader")
#> [1] TRUE
if (has_qgis()) qgis_has_provider("native")
#> [1] TRUE
if (has_qgis()) qgis_has_plugin(c("grassprovider", "processing_saga_nextgen"))
#> [1] TRUE TRUE
```
