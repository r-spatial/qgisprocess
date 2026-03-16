# Detect QGIS installations with 'qgis_process' on Windows and macOS

Discovers existing 'qgis_process' executables on the system and returns
their filepath. Only available for Windows and macOS systems.
`qgis_detect_paths()` is a shortcut to `qgis_detect_windows_paths()` on
Windows and `qgis_detect_macos_paths()` on macOS.

## Usage

``` r
qgis_detect_paths(drive_letter = strsplit(R.home(), ":")[[1]][1])

qgis_detect_windows_paths(drive_letter = strsplit(R.home(), ":")[[1]][1])

qgis_detect_macos_paths()
```

## Arguments

- drive_letter:

  The drive letter on which to search. By default, this is the same
  drive letter as the R executable. Only applicable to Windows.

## Value

A character vector of possible paths to the 'qgis_process' executable.

## Note

These functions do not verify whether the discovered 'qgis_process'
executables successfully run. You can run
`qgis_path(query = TRUE, quiet = FALSE)` to discover and cache the first
'qgis_process' in the list that works.

## See also

[`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md),
[`qgis_path()`](https://r-spatial.github.io/qgisprocess/reference/qgis_path.md)

## Examples

``` r
if (.Platform$OS.type == "windows") {
  qgis_detect_paths()
  identical(qgis_detect_windows_paths(), qgis_detect_paths())
}
if (Sys.info()["sysname"] == "Darwin") {
  qgis_detect_paths()
  identical(qgis_detect_macos_paths(), qgis_detect_paths())
}
```
