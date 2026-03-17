# Changelog

## qgisprocess (development version)

- Add support for QGIS 4.0
  ([\#229](https://github.com/r-spatial/qgisprocess/issues/229)).

## qgisprocess 0.4.1

CRAN release: 2024-10-06

### Fix for non-UTF-8 locales

- Don’t set `encoding = "UTF-8"` in some
  [`processx::run()`](http://processx.r-lib.org/reference/run.md) calls
  that used that setting. This adds support for
  [qgisprocess](https://r-spatial.github.io/qgisprocess/) in non-UTF-8
  locales
  ([\#219](https://github.com/r-spatial/qgisprocess/issues/219)).

## qgisprocess 0.4.0

CRAN release: 2024-07-07

### Enhancements

- Speed up various calls to the `qgis_process` backend by automatically
  implementing `--skip-loading-plugins` where possible
  ([\#201](https://github.com/r-spatial/qgisprocess/issues/201)). This
  benefits package startup time and the timing of
  [`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md),
  [`qgis_show_help()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md),
  and several other functions. This feature needs QGIS \>= 3.36.
- Add new vignettes
  ([\#213](https://github.com/r-spatial/qgisprocess/issues/213)):
  - [`vignette("qgis_arguments")`](https://r-spatial.github.io/qgisprocess/articles/qgis_arguments.md):
    passing R objects as algorithm arguments.
  - [`vignette("options")`](https://r-spatial.github.io/qgisprocess/articles/options.md):
    configuration with options or environment variables.

### Minor changes

- Don’t renew the package cache when no plugins were successfully
  enabled or disabled
  ([\#212](https://github.com/r-spatial/qgisprocess/issues/212)).
- Update the ‘getting started’ vignette
  ([\#206](https://github.com/r-spatial/qgisprocess/issues/206),
  [\#207](https://github.com/r-spatial/qgisprocess/issues/207)):
  - advise to use
    [`qgis_search_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_search_algorithms.md).
  - in QGIS \>= 3.36 the GRASS GIS provider is called `grass` instead of
    `grass7`.

## qgisprocess 0.3.0

CRAN release: 2024-02-06

### Enhancements

- QGIS or third-party providers can expose deprecated algorithms that
  may be removed from future versions.
  [qgisprocess](https://r-spatial.github.io/qgisprocess/) now handles
  these algorithms explicitly
  ([\#198](https://github.com/r-spatial/qgisprocess/issues/198)):
  - [`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md)
    and other functions (such as
    [`qgis_show_help()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md)
    and
    [`qgis_get_description()`](https://r-spatial.github.io/qgisprocess/reference/qgis_show_help.md))
    will warn if a deprecated algorithm is passed (issues
    [\#193](https://github.com/r-spatial/qgisprocess/issues/193),
    [\#194](https://github.com/r-spatial/qgisprocess/issues/194)).
  - [`qgis_search_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_search_algorithms.md)
    now **excludes** deprecated algorithms by default; they can still be
    included by setting the `include_deprecated` argument to `TRUE`.
  - [`qgis_algorithms()`](https://r-spatial.github.io/qgisprocess/reference/qgis_algorithms.md)
    can *optionally* restrict its results to non-deprecated algorithms
    (set the `include_deprecated` argument to `FALSE`). By default they
    are included, just as before.
- More consistent and intuitive handling of JSON input / output user
  settings
  ([\#195](https://github.com/r-spatial/qgisprocess/issues/195),
  [\#196](https://github.com/r-spatial/qgisprocess/issues/196); see
  [`?qgis_using_json_output`](https://r-spatial.github.io/qgisprocess/reference/qgis_using_json_input.md)).

### Fixes

- Fix bug in support for environment variable
  `R_QGISPROCESS_DETECT_NEWER_QGIS`
  ([\#197](https://github.com/r-spatial/qgisprocess/issues/197)).
- Fix unit test to comply with [terra](https://rspatial.org/) \> 1.7-65
  ([\#202](https://github.com/r-spatial/qgisprocess/issues/202)).

## qgisprocess 0.2.0

CRAN release: 2023-12-21

### New features

- Add vector support for [terra](https://rspatial.org/)
  ([\#184](https://github.com/r-spatial/qgisprocess/issues/184)). This
  makes it possible to use `SpatVector` or `SpatVectorProxy` objects as
  input arguments, and to coerce processing results to `SpatVector` or
  `SpatVectorProxy`.
- [`qgis_detect_windows_paths()`](https://r-spatial.github.io/qgisprocess/reference/qgis_detect_paths.md)
  and
  [`qgis_detect_macos_paths()`](https://r-spatial.github.io/qgisprocess/reference/qgis_detect_paths.md)
  put paths at the top that contain a QGIS version string and these
  paths are sorted according to decreasing QGIS version
  ([\#189](https://github.com/r-spatial/qgisprocess/issues/189)). This
  lets
  [`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md)
  select the newest QGIS version among `qgis_process` file paths that
  have a version string. Furthermore, a wrapper
  [`qgis_detect_paths()`](https://r-spatial.github.io/qgisprocess/reference/qgis_detect_paths.md)
  has been added that works on both Windows and macOS
  ([\#192](https://github.com/r-spatial/qgisprocess/issues/192)).

### Minor changes

- Allow half-configured states with abundant messages, so that remaining
  functionality can be used in debugging or even for some real stuff
  ([\#177](https://github.com/r-spatial/qgisprocess/issues/177)).
- [`qgis_run_algorithm()`](https://r-spatial.github.io/qgisprocess/reference/qgis_run_algorithm.md)
  documentation gains a section on QGIS models and scripts
  ([8a20669](https://github.com/r-spatial/qgisprocess/commit/8a20669ea50b4b9c14194dd864ed119e137732a9)).
- An option `qgisprocess.detect_newer_qgis` is available (mirrored by
  environment variable `R_QGISPROCESS_DETECT_NEWER_QGIS`) for Windows
  and macOS
  ([\#192](https://github.com/r-spatial/qgisprocess/issues/192)). If set
  as `TRUE`, during package loading
  [qgisprocess](https://r-spatial.github.io/qgisprocess/) will check
  whether a more recent (standalone) QGIS version is also installed
  while the package cache still dictates to use an older version. In
  this specific scenario a question will be asked to switch to the newer
  version. Without setting this option default behaviour remains in
  place, i.e. the user must manually intervene either by running
  [`qgis_configure()`](https://r-spatial.github.io/qgisprocess/reference/qgis_configure.md),
  by setting the `qgisprocess.path` option, or by uninstalling the older
  QGIS version.
- Solve a CRAN check error on `r-oldrel-macos-x86_64`, by adding support
  for [stars](https://r-spatial.github.io/stars/) 0.5-5
  ([\#175](https://github.com/r-spatial/qgisprocess/issues/175)).

## qgisprocess 0.1.0

CRAN release: 2023-08-17

- Initial CRAN release.
- While the package incubated as a development version for a few years,
  quite a few namespace changes have been made more recently
  ([\#153](https://github.com/r-spatial/qgisprocess/issues/153)). Old
  function names still work but these are deprecated. They will be
  dropped at a later stage.
