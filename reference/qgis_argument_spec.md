# Specify QGIS argument types

Specify QGIS argument types

## Usage

``` r
qgis_argument_spec(
  algorithm = NA_character_,
  name = NA_character_,
  description = NA_character_,
  qgis_type = NA_character_,
  available_values = character(0),
  acceptable_values = character(0)
)
```

## Arguments

- algorithm:

  A qualified algorithm name (e.g., `"native:buffer"`) or a path to a
  QGIS model file.

- name, description, qgis_type, available_values, acceptable_values:

  Column values of `arguments` denoting the argument name, description,
  and acceptable values.

## Value

A [`list()`](https://rdrr.io/r/base/list.html) with an element for each
input argument.

## Note

This is an internal function.
