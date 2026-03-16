# Enable or disable QGIS plugins

Processing plugins, installed in QGIS, can be in an 'enabled' or
'disabled' state in QGIS. The plugin state can be controlled from R.
`qgis_enable_plugins()` enables plugins while `qgis_disable_plugins()`
does the reverse.

## Usage

``` r
qgis_enable_plugins(names = NULL, quiet = FALSE)

qgis_disable_plugins(names = NULL, quiet = FALSE)
```

## Arguments

- names:

  Optional character vector of plugin names.

- quiet:

  Use `FALSE` to display more information, possibly useful for
  debugging.

## Value

A tibble of plugins, invisibly.

## Details

The cache is immediately updated upon enabling or disabling plugins from
R.

Run
[`qgis_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
to list the available plugins that implement processing providers.

If you installed, removed, enabled or disabled plugins in the QGIS GUI,
then run
[`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md)
to make those changes available in R.

If `names` is not provided to `qgis_enable_plugins()`, it is assumed
that all *disabled* plugins are to be enabled. If `names` is not
provided to `qgis_disable_plugins()`, it is assumed that all *enabled*
plugins are to be disabled. Note that the 'processing' plugin is
ignored, because it is always available to 'qgis_process' (not QGIS
though).

## Note

Only plugins that implement processing providers are supported.
Installing or removing plugins is not supported.

## See also

[`qgis_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)

Other topics about configuring QGIS and qgisprocess:
[`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md),
[`qgis_run()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run.md)

## Examples

``` r
qgis_enable_plugins("name_of_plugin")
#> Ignoring unknown plugins: name_of_plugin
#> No QGIS plugins to be handled; exiting.
```
