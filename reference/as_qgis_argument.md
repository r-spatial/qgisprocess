# Type coercion for arguments to QGIS processing algorithms

Calls to
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md)
can and should contain R objects that need to be serialized before they
are passed to the command line. In some cases (e.g., sf objects),
temporary files need to be written and cleaned up. The
`as_qgis_argument()` and `qgis_clean_argument()` S3 generics provide
hooks for argument values to be serialized correctly.

## Usage

``` r
as_qgis_argument(x, spec = qgis_argument_spec(), use_json_input = FALSE)

qgis_clean_argument(value)
```

## Arguments

- x:

  An object passed to a QGIS processing algorithm

- spec:

  A [`list()`](https://rdrr.io/r/base/list.html) with values for
  `algorithm`, `name`, `description`, and `qgis_type`. See
  [`qgis_argument_spec()`](https://r-spatial.github.io/qgisprocess/reference/qgis_argument_spec.md)
  to create a blank `spec` for testing.

- use_json_input:

  TRUE if the arguments will be serialized as JSON instead of serialized
  as a command-line argument.

- value:

  The result of `as_qgis_argument()` after the QGIS processing algorithm
  has been run.

## Value

The returned object class and form depends on the class and form of `x`
and on the targeted `qgis_type`.

If `x` is a `qgis_list_input` or a `qgis_dict_input` object, the same
class is returned but with `as_qgis_argument()` applied to each element.

In all other cases, the outcome can depend on the value of
`use_json_input` and this also holds for the elements of
`qgis_list_input` and `qgis_dict_input` objects:

- if `use_json_input = FALSE`: a string.

- if `use_json_input = TRUE`: various classes can be returned that will
  be correctly serialized as JSON.

## Examples

``` r
qgisprocess::as_qgis_argument(
  c("a", "b"),
  spec = list(qgis_type = "range"),
  use_json_input = FALSE
)
#> [1] "a,b"
qgisprocess::as_qgis_argument(
  c(1, 2),
  spec = list(qgis_type = "range"),
  use_json_input = FALSE
)
#> [1] "1,2"
qgisprocess::as_qgis_argument(
  c("a", "b"),
  spec = list(qgis_type = "range"),
  use_json_input = TRUE
)
#> [1] "a" "b"
qgisprocess::as_qgis_argument(
  c(1, 2),
  spec = list(qgis_type = "range"),
  use_json_input = TRUE
)
#> [1] 1 2
mat <- matrix(1:12, ncol = 3)
mat
#>      [,1] [,2] [,3]
#> [1,]    1    5    9
#> [2,]    2    6   10
#> [3,]    3    7   11
#> [4,]    4    8   12
qgisprocess::as_qgis_argument(
  mat,
  spec = list(qgis_type = "matrix"),
  use_json_input = FALSE
)
#> [1] "1,5,9,2,6,10,3,7,11,4,8,12"
qgisprocess::as_qgis_argument(
  mat,
  spec = list(qgis_type = "matrix"),
  use_json_input = TRUE
)
#>  [1]  1  5  9  2  6 10  3  7 11  4  8 12
```
