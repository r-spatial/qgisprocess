# Configuration with options or environment variables

Options and environment variables are available to manage
[qgisprocess](https://r-spatial.github.io/qgisprocess/) behaviour.
Several options have a corresponding environment variable. When both
have a value, the *option* will be given priority.

Options are typically harder to isolate from the R code that you
collaborate on. Consequently, it is advised to:

- use [`options()`](https://rdrr.io/r/base/options.html) where this
  affects behaviour that must be the same across users and machines for
  reproducibility. Put these inside your script, or at least in a
  `.Rprofile` file that is shared together with the other project files.
- use environment variables where behaviour must be machine-specific.
  For example, you can create a `.Renviron` file in your working
  directory and ignore it in distributed version control. Or you can set
  the environment variable at a higher level, e.g. in a `.Renviron` file
  in your home directory. See
  [`base::Startup`](https://rdrr.io/r/base/Startup.html) for more
  information.

## Overview

Below table lists the available options and environment variables.

| Option                             | Environment variable                 | Type                    |
|------------------------------------|--------------------------------------|-------------------------|
| `qgisprocess.path`                 | `R_QGISPROCESS_PATH`                 | string (filepath)       |
| `qgisprocess.tmp_vector_ext`       | not available                        | string (file extension) |
| `qgisprocess.tmp_raster_ext`       | not available                        | string (file extension) |
| `qgisprocess.detect_newer_qgis`    | `R_QGISPROCESS_DETECT_NEWER_QGIS`    | logical                 |
| `qgisprocess.use_json_input`       | `R_QGISPROCESS_USE_JSON_INPUT`       | logical                 |
| `qgisprocess.use_json_output`      | `R_QGISPROCESS_USE_JSON_OUTPUT`      | logical                 |
| `qgisprocess.cachefiles_days_keep` | `R_QGISPROCESS_CACHEFILES_DAYS_KEEP` | numeric                 |
| `qgisprocess.use_cached_help`      | `R_QGISPROCESS_USE_CACHED_HELP`      | logical                 |
| `qgisprocess.env`                  | not available                        | named list              |

## Descriptions

Following descriptions are applicable to both option and environment
variable:

- `qgisprocess.path`: sets the path to ‘qgis_process’; see
  [`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md)
- `qgisprocess.tmp_vector_ext`: sets the vector file extension for
  temporary files; see
  [`qgis_tmp_vector()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md)
- `qgisprocess.tmp_raster_ext`: sets the raster file extension for
  temporary files; see
  [`qgis_tmp_raster()`](https://r-spatial.github.io/qgisprocess/reference/qgis_tmp_file.md)
- `qgisprocess.detect_newer_qgis`: for Windows and macOS; will report
  availability of an installed (standalone) QGIS version that is more
  recent than the cached version
- `qgisprocess.use_json_input`: control whether the JSON input method is
  used; see
  [`qgis_using_json_input()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)
- `qgisprocess.use_json_output`: control whether the JSON output method
  is used; see
  [`qgis_using_json_output()`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)
- `qgisprocess.cachefiles_days_keep`: control a cache file’s age (as
  days) that must be exceeded for it to be deleted when loading the
  package (default is 90); see
  [`?qgis_delete_old_cachefiles`](https://r-spatial.github.io/qgisprocess/reference/qgis_delete_old_cachefiles.md)
- `qgisprocess.use_cached_help`: whether to fetch an algorithm’s
  documentation from a cached file (corresponding to the QGIS and
  [qgisprocess](https://r-spatial.github.io/qgisprocess/) versions in
  use) if this file exists (default is `TRUE`; if `FALSE`, always
  (re)query documentation from QGIS)
- `qgisprocess.env`: named list of environment variable values used in
  each call to ‘qgis_process’; see
  [`qgis_run()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run.md)
