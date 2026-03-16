# Report if JSON objects are used for input to and output from 'qgis_process'

Returns a logical that reveals whether the JSON input and output methods
are used, respectively.

## Usage

``` r
qgis_using_json_input()

qgis_using_json_output(query = FALSE, quiet = TRUE)
```

## Arguments

- query:

  Logical. Should the outcome of `qgis_using_json_output()` ignore the
  cached value? The argument has effect on condition that no user
  setting 'use_json_output' is in place (see Details).

  - If set as `TRUE`, the function simply returns `TRUE` and the cached
    value for the current session is set as `TRUE`.

  - If set as `FALSE` (default), the function returns the cached value
    on condition that it does not conflict with a 'use_json\_**in**put'
    user setting.

- quiet:

  Use `FALSE` to display more information, possibly useful for
  debugging.

## Value

A logical of length 1.

## Details

Since QGIS 3.24 the JSON input method of 'qgis_process' is used by
default when calling the command. It allows to use certain algorithms
that require a more complex input argument, e.g. a list of lists (see
[`qgis_list_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_list_input.md)).

Likewise, JSON output is the default output format requested from
'qgis_process'. Using the JSON input method of 'qgis_process'
automatically implies using the JSON output method; when *not* using the
JSON input method it is possible (but not the default) to also not use
the JSON output method.

The defaults can be overruled with the options
`qgisprocess.use_json_input` or `qgisprocess.use_json_output`, and with
the environment variables `R_QGISPROCESS_USE_JSON_INPUT` or
`R_QGISPROCESS_USE_JSON_OUTPUT`.

The returned JSON output method is always cached during the current
session by `qgis_using_json_output()`. Given that
`qgis_using_json_output()` is called by various functions in the
package, having a user setting 'use_json_output' in place (see above)
will have effect during subsequent usage of the package. To cache the
value between sessions,
[`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md)
needs to be called to update the value stored in the persistent package
cache file.

The JSON input method is not cached but simply determined on the fly,
based on QGIS version, the JSON output method and the user setting if
present.

There is good reason for having 'use_json_output' in the persistent
package cache: the values of
[`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
and
[`qgis_plugins()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
are different with or without the JSON output method, and are also
stored in the cache.

## See also

Other topics about programming or debugging utilities:
[`qgis_result_status()`](https://r-spatial.github.io/qgisprocess/reference/qgis_result_status.md),
[`qgis_run()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run.md),
[`qgis_tmp_file()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md),
[`qgis_unconfigure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_unconfigure.md)

Other topics about reporting the QGIS state:
[`has_qgis()`](https://r-spatial.github.io/qgisprocess/reference/has_qgis.md),
[`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md),
[`qgis_path()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md)

## Examples

``` r
qgis_using_json_input()
#> [1] TRUE
qgis_using_json_output()
#> [1] TRUE
```
