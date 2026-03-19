# Run an algorithm using 'qgis_process'

Runs an algorithm using 'qgis_process'. See the [QGIS
docs](https://docs.qgis.org/latest/en/docs/user_manual/processing_algs/)
for a detailed description of the algorithms provided 'out of the box'
on QGIS.

## Usage

``` r
qgis_run_algorithm(
  algorithm,
  ...,
  PROJECT_PATH = NULL,
  ELLIPSOID = NULL,
  .raw_json_input = NULL,
  .quiet = TRUE
)
```

## Arguments

- algorithm:

  A qualified algorithm name (e.g., `"native:buffer"`) or a path to a
  QGIS model file.

- ...:

  Named key-value pairs as arguments for the algorithm. Features of
  [`rlang::list2()`](https://rlang.r-lib.org/reference/list2.html) are
  supported. These arguments are converted to strings using
  [`as_qgis_argument()`](https://r-spatial.github.io/qgisprocess/reference/as_qgis_argument.md).

- PROJECT_PATH, ELLIPSOID:

  Global values for QGIS project file and ellipsoid name for distance
  calculations.

- .raw_json_input:

  The raw JSON to use as input in place of `...`. See *Details* section.

- .quiet:

  Use `FALSE` to get extra output from 'qgis_process'. This can be
  useful in debugging.

## Value

A `qgis_result` object.

## Details

`qgis_run_algorithm()` accepts various R objects as algorithm arguments.
An overview is given by
[`vignette("qgis_arguments")`](https://r-spatial.github.io/qgisprocess/articles/qgis_arguments.md).
Examples include an R matrix or data frame for the argument type
'matrix', R colors for the argument type 'color', sf or terra
(SpatVector) objects for the argument type 'vector' and
raster/terra/stars objects for the argument type 'raster', but there are
many more. `qgis_run_algorithm()` preprocesses the provided objects into
the format that QGIS expects for a given argument.

Providing R objects that cannot be converted to the applicable argument
type will lead to an error.

Algorithm arguments can be passed as arguments of
`qgis_run_algorithm()`, but they can also be combined as a JSON string
and fed into the `.raw_json_input` argument. A JSON string can be
obtained from the QGIS GUI, either from the processing tool dialog or
from the processing history dialog, by selecting 'Copy as JSON' in the
'Advanced' dropdown menu. So a user can first try out a geoprocessing
step in the QGIS GUI, and once the chosen algorithm arguments are
satisfactory, copy the JSON string to reproduce the operation in R. A
screenshot is available at the package homepage.

## Running QGIS models and Python scripts

QGIS models and Python scripts can be added to the Processing Toolbox in
the QGIS GUI, by pointing at their corresponding file. This will put the
model or script below the provider 'Models' or 'Scripts', respectively.
Next, it is necessary to run
[`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md)
in R in order to make the model or script available to qgisprocess (even
reloading the package won't detect it, since these providers have
dynamic content, not tied to a plugin or to a QGIS version). You can
check the outcome with
[`qgis_providers()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
and
[`qgis_search_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_search_algorithms.md).
Now, just as with other algorithms, you can provide the `model:<name>`
or `script:<name>` identifier to the `algorithm` argument of
`qgis_run_algorithm()`.

As the output argument name of a QGIS model can have an R-unfriendly
syntax, you may need to take the JSON parameter string from the QGIS
processing dialog and feed the JSON string to the `.raw_json_input`
argument of `qgis_run_algorithm()` instead of providing separate
arguments.

Although the 'qgis_process' backend also supports replacing the
'algorithm' parameter by the file path of a model file or a Python
script, it is not planned to implement this in qgisprocess, as it would
bypass argument preprocessing in R (including checks).

## See also

[`vignette("qgis_arguments")`](https://r-spatial.github.io/qgisprocess/articles/qgis_arguments.md)

Other functions to run one geoprocessing algorithm:
[`qgis_run_algorithm_p()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm_p.md)

## Examples

``` r
qgis_run_algorithm(
  "native:buffer",
  INPUT = system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
  DISTANCE = 10
)
#> Argument `SEGMENTS` is unspecified (using QGIS default value).
#> Using `END_CAP_STYLE = "Round"`
#> Using `JOIN_STYLE = "Round"`
#> Argument `MITER_LIMIT` is unspecified (using QGIS default value).
#> Argument `DISSOLVE` is unspecified (using QGIS default value).
#> Argument `SEPARATE_DISJOINT` is unspecified (using QGIS default value).
#> Using `OUTPUT = qgis_tmp_vector()`
#> <Result of `qgis_run_algorithm("native:buffer", ...)`>
#> List of 1
#>  $ OUTPUT: 'qgis_outputVector' chr "/tmp/RtmpTMZCFu/file2f894edd5885/file2f89da27cf6.gpkg"
```
