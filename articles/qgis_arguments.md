# Passing R objects as algorithm arguments

``` r
library(qgisprocess)
```

This article addresses the question how algorithm arguments in
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md)
should be formatted.

When you run
[`qgis_show_help()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md)
or
[`qgis_get_argument_specs()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md)
for a given algorithm, you will quickly find out that QGIS has a diverse
set of possible argument types.

As an example, take a look at the `qgis_type` column below:

``` r
qgis_get_argument_specs("native:joinbynearest") |> 
  subset(select = name:qgis_type)
#> # A tibble: 9 × 3
#>   name                description                                      qgis_type
#>   <chr>               <chr>                                            <chr>    
#> 1 INPUT               Input layer                                      source   
#> 2 INPUT_2             Input layer 2                                    source   
#> 3 FIELDS_TO_COPY      Layer 2 fields to copy (leave empty to copy all… field    
#> 4 DISCARD_NONMATCHING Discard records which could not be joined        boolean  
#> 5 PREFIX              Joined field prefix                              string   
#> 6 NEIGHBORS           Maximum nearest neighbors                        number   
#> 7 MAX_DISTANCE        Maximum distance                                 distance 
#> 8 OUTPUT              Joined layer                                     sink     
#> 9 NON_MATCHING        Unjoinable features from first layer             sink
```

## String or R object?

Although you can pass a string [¹](#fn1) to a QGIS argument in
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md),
[qgisprocess](https://r-spatial.github.io/qgisprocess/) makes it
possible to pass familiar R objects that naturally match the QGIS
argument type. This is often easier than constructing a string in the
format required by QGIS.

The tables further below show which R objects can be passed to each QGIS
argument type! You can find a bit more background information in issue
[\#13](https://github.com/r-spatial/qgisprocess/issues/13).

## Defaults

Some arguments will receive a default value if they are unspecified.
Defaults are provided by either
[qgisprocess](https://r-spatial.github.io/qgisprocess/) or QGIS. Typical
defaults by [qgisprocess](https://r-spatial.github.io/qgisprocess/) are
temporary filepaths for (missing) output argument types `sink`,
`vectorDestination`, `rasterDestination`, `fileDestination` and
`folderDestination`.

## Supported R objects

### For spatial QGIS arguments

| QGIS argument type                                                        | Supported R object                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|---------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `source`, `vector`                                                        | a string (filepath) or an appropriate spatial vector object (`sf`, `SpatVector`, `SpatVectorProxy`)                                                                                                                                                                                                                                                                                                                                                                                          |
| `raster`                                                                  | a string (filepath) or an appropriate spatial raster object (`SpatRaster`, `stars`, `stars_proxy`, `RasterLayer`, `RasterBrick`)                                                                                                                                                                                                                                                                                                                                                             |
| `layer`                                                                   | a string (filepath) or an appropriate spatial object (`sf`, `SpatVector`, `SpatVectorProxy`, `SpatRaster`, `stars`, `stars_proxy`, `RasterLayer`, `RasterBrick`)                                                                                                                                                                                                                                                                                                                             |
| `multilayer`                                                              | a list of layers created by [`qgis_list_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_list_input.md) (alternatively, repeat the same argument providing each layer in turn)                                                                                                                                                                                                                                                                                               |
| `aggregates`, `field_mapping`, `tininputlayers`, `vectortilewriterlayers` | a nested list created by [`qgis_list_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_list_input.md) (unnamed list) or [`qgis_dict_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_list_input.md) (named list) or a combination of both (a `native:aggregate` example is found [here](https://github.com/r-spatial/qgisprocess/issues/133#issuecomment-1488490056)). These argument types are typically not supported by the legacy (no-JSON) input method. |
| `point`                                                                   | a vector of 2 point coordinates, a simple feature geometry (`sfg`) of class `POINT`, or an `sfc` (geometry set) or `sf` object with exactly one `POINT` geometry                                                                                                                                                                                                                                                                                                                             |
| `band`                                                                    | an integer value                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| `extent`                                                                  | a vector of the form `c(xmin, xmax, ymin, ymax)`, a `bbox` object from [sf](https://r-spatial.github.io/sf/), a `SpatExtent` object from [terra](https://rspatial.org/) or an `Extent` object from [raster](https://rspatial.org/raster)                                                                                                                                                                                                                                                     |
| `crs`                                                                     | a `crs` object from [sf](https://r-spatial.github.io/sf/), a `CRS` object from [raster](https://rspatial.org/raster) or a WKT2 string (e.g. obtained with [`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html))                                                                                                                                                                                                                                                             |
| `coordinateoperation`                                                     | PROJ string of a coordinate operation, possibly obtained using [`sf::sf_proj_pipelines()`](https://r-spatial.github.io/sf/reference/proj_tools.html)                                                                                                                                                                                                                                                                                                                                         |
| `sink`, `vectorDestination`                                               | a string: filepath to a vector file format (defaults to a temporary GeoPackage if argument is missing)                                                                                                                                                                                                                                                                                                                                                                                       |
| `rasterDestination`                                                       | a string: filepath to a raster file format (defaults to a temporary GeoTIFF file if argument is missing)                                                                                                                                                                                                                                                                                                                                                                                     |

#### Note

An important group of spatial QGIS argument types are those used in
specifying an input layer. QGIS essentially needs a filepath string
here. If a spatial R object is provided instead,
[qgisprocess](https://r-spatial.github.io/qgisprocess/) will:

- either write out the object to a temporary file and pass the filepath
  to QGIS,
- or use the object’s filepath metadata if present (supported by some
  classes of [terra](https://rspatial.org/) and
  [stars](https://r-spatial.github.io/stars/)).

However if the spatial R object simply results from reading a spatial
file and if its filepath is *not* included in the object’s metadata
(e.g. for `sf` or `SpatVector` objects), then you will get most
efficiency if you pass the original filepath directly.

### For non-spatial QGIS arguments

| QGIS argument types                                                | Supported R object                                                                                                                                                                                                                                                                                                          |
|--------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `enum`                                                             | a character vector with one or more acceptable string values. A numeric vector with the corresponding index number(s) can also be provided, but contrary to the character vector its acceptability won’t be checked.                                                                                                        |
| `range`                                                            | a vector of length 2, defining minimum and maximum value respectively                                                                                                                                                                                                                                                       |
| `file`, `field`, `layout`, `layoutitem`, `maptheme`, `execute_sql` | string                                                                                                                                                                                                                                                                                                                      |
| `string`                                                           | any string, including data-defined overriding (`"field:..."` or `"expression:..."`)                                                                                                                                                                                                                                         |
| `distance`, `number`                                               | numeric (length 1), or a string for data-defined overriding (`"field:..."` or `"expression:..."`)                                                                                                                                                                                                                           |
| `boolean`                                                          | logical (length 1), or a string for data-defined overriding (`"field:..."` or `"expression:..."`)                                                                                                                                                                                                                           |
| `color`                                                            | a colour string that [`col2rgb()`](https://rdrr.io/r/grDevices/col2rgb.html) understands (e.g. `"pink1"` or `"#1A664D80"`), or a string for data-defined overriding (`"field:..."` or `"expression:..."`)                                                                                                                   |
| `expression`, `raster_calc_expression`                             | a string (formatted as `"expression:..."`). See [`vignette("qgis_expressions")`](https://r-spatial.github.io/qgisprocess/articles/qgis_expressions.md).                                                                                                                                                                     |
| `matrix`                                                           | a matrix or a data frame with contents as required by the algorithm                                                                                                                                                                                                                                                         |
| `relief_colors`                                                    | a matrix or a data frame with three columns and with rows corresponding to intervals. The first two columns define the interval (minimum and maximum respectively) and the third column must be a colour string that [`col2rgb()`](https://rdrr.io/r/grDevices/col2rgb.html) understands (e.g. `"pink1"` or `"#1A664D80"`). |
| `fileDestination`, `folderDestination`                             | a string: path to a file or directory (defaults to a temporary file or directory if argument is missing)                                                                                                                                                                                                                    |

## Passing output from a previous processing step

The object returned by
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md)
is a `qgis_result`. It contains one or several *output elements*, shown
when printing the `qgis_result` object.

A single output element can be extracted with
[`qgis_extract_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_extract_output.md),
and it has one of the following classes: `qgis_outputFile`,
`qgis_outputFolder`, `qgis_outputLayer`, `qgis_outputMultilayer`,
`qgis_outputNumber`, `qgis_outputRaster`, `qgis_outputString` or
`qgis_outputVector`.

These objects essentially represent a string. They can be passed
directly to an appropriate argument in a next
[`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md)
step.

Also note that the pipe-friendly function
[`qgis_run_algorithm_p()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm_p.md)
(notice the `_p`) is able to accept an ‘incoming’ `qgis_result` object
as its first argument. It will extract the appropriate output element on
the fly.

------------------------------------------------------------------------

1.  A string is a character vector of length 1.
