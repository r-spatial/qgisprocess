# Prepare a compound input argument

Some algorithm arguments require a compound object, consisting of
several layers or elements. These functions apply strict validation
rules when generating this object and are recommended.

## Usage

``` r
qgis_list_input(...)

qgis_dict_input(...)
```

## Arguments

- ...:

  Named values for `qgis_dict_input()` or unnamed values for
  `qgis_list_input()`.

## Value

- `qgis_list_input()`: An object of class 'qgis_list_input'

- `qgis_dict_input()`: An object of class 'qgis_dict_input'

## Details

`qgis_list_input()` generates an unnamed list of class
`qgis_list_input`. The use of `qgis_list_input()` instead of list() is
*required* for compound arguments *in case of no-JSON input* (see
[`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)).
Since it applies strict validation rules, it is recommended in all cases
though.

`qgis_dict_input()` generates a named list of class `qgis_dict_input`.
`qgis_dict_input()` is only supported when the JSON input method applies
(see
[`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)),
where it can be interchanged with a named
[`list()`](https://rdrr.io/r/base/list.html). It can only be used for
arguments requiring *named* lists. Since it applies strict validation
rules, it is recommended above
[`list()`](https://rdrr.io/r/base/list.html).

Some QGIS argument types that need a compount object are the
`multilayer`, `aggregates`, `fields_mapping`, `tininputlayers` and
`vectortilewriterlayers` argument types.

## Examples

``` r
qgis_list_input(1, 2, 3)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> attr(,"class")
#> [1] "qgis_list_input"
qgis_dict_input(a = 1, b = 2, c = 3)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3
#> 
#> attr(,"class")
#> [1] "qgis_dict_input"
```
