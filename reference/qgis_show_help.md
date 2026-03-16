# Get detailed information about one algorithm

Get detailed information about one algorithm

## Usage

``` r
qgis_show_help(algorithm)

qgis_get_description(algorithm)

qgis_get_argument_specs(algorithm, ...)

qgis_get_output_specs(algorithm, ...)
```

## Arguments

- algorithm:

  A qualified algorithm name (e.g., `"native:buffer"`).

- ...:

  For internal use only.

## Value

- `qgis_get_description()`: a string.

- `qgis_get_argument_specs()`, `qgis_get_output_specs()`: a tibble.

- `qgis_show_help()`: the algorithm name, invisibly.

## See also

Other topics about information on algorithms & processing providers:
[`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md),
[`qgis_search_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_search_algorithms.md)

## Examples

``` r
qgis_get_description("native:filedownloader")
#>                                                                                                                                            native:filedownloader 
#> "Downloads a URL to the file system with an HTTP(S) GET or POST request.\nThis algorithm downloads a URL to the file system with an HTTP(S) GET or POST request" 
# \donttest{
# not running below examples in R CMD check to save time
qgis_get_argument_specs("native:filedownloader")
#> # A tibble: 4 × 6
#>   name   description  qgis_type default_value available_values acceptable_values
#>   <chr>  <chr>        <chr>     <list>        <list>           <list>           
#> 1 URL    URL          string    <NULL>        <NULL>           <chr [3]>        
#> 2 METHOD Method       enum      <int [1]>     <chr [2]>        <chr [2]>        
#> 3 DATA   Data         string    <NULL>        <NULL>           <chr [3]>        
#> 4 OUTPUT File destin… fileDest… <NULL>        <NULL>           <chr [1]>        
qgis_get_output_specs("native:filedownloader")
#> # A tibble: 1 × 3
#>   name   description      qgis_output_type
#>   <chr>  <chr>            <chr>           
#> 1 OUTPUT File destination outputFile      
qgis_show_help("native:filedownloader")
#> Download file via HTTP(S) (native:filedownloader)
#> 
#> ----------------
#> Description
#> ----------------
#> Downloads a URL to the file system with an HTTP(S) GET or POST request.
#> This algorithm downloads a URL to the file system with an HTTP(S) GET or POST request
#> 
#> ----------------
#> Arguments
#> ----------------
#> 
#> URL: URL
#>  Argument type:  string
#>  Acceptable values:
#>      - String value
#>      - field:FIELD_NAME to use a data defined value taken from the FIELD_NAME field
#>      - expression:SOME EXPRESSION to use a data defined value calculated using a custom QGIS expression
#> METHOD: Method
#>  Default value:  0
#>  The HTTP method to use for the request
#>  Argument type:  enum
#>  Available values:
#>      - 0: GET
#>      - 1: POST
#>  Acceptable values:
#>      - Number of selected option, e.g. '1'
#>      - Comma separated list of options, e.g. '1,3'
#> DATA: Data (optional)
#>  The data to add in the body if the request is a POST
#>  Argument type:  string
#>  Acceptable values:
#>      - String value
#>      - field:FIELD_NAME to use a data defined value taken from the FIELD_NAME field
#>      - expression:SOME EXPRESSION to use a data defined value calculated using a custom QGIS expression
#> OUTPUT: File destination
#>  Argument type:  fileDestination
#>  Acceptable values:
#>      - Path for new file
#> 
#> ----------------
#> Outputs
#> ----------------
#> 
#> OUTPUT: <outputFile>
#>  File destination
#> 
#> 
#> 
# }
```
