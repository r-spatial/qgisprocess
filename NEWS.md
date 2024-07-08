# qgisprocess 0.4.0

## Enhancements

- Speed up various calls to the `qgis_process` backend by automatically implementing `--skip-loading-plugins` where possible (#201).
This benefits package startup time and the timing of `qgis_run_algorithm()`, `qgis_show_help()`, and several other functions.
This feature needs QGIS >= 3.36.
- Add new vignettes (#213):
  - `vignette("qgis_arguments")`: passing R objects as algorithm arguments.
  - `vignette("options")`: configuration with options or environment variables.

## Minor changes

- Don't renew the package cache when no plugins were successfully enabled or disabled (#212).
- Update the 'getting started' vignette (#206, #207):
  - advise to use `qgis_search_algorithms()`.
  - in QGIS >= 3.36 the GRASS GIS provider is called `grass` instead of `grass7`.

# qgisprocess 0.3.0

## Enhancements

- QGIS or third-party providers can expose deprecated algorithms that may be removed from future versions.
`{qgisprocess}` now handles these algorithms explicitly (#198):
  - `qgis_run_algorithm()` and other functions (such as `qgis_show_help()` and `qgis_get_description()`) will warn if a deprecated algorithm is passed (issues #193, #194).
  - `qgis_search_algorithms()` now **excludes** deprecated algorithms by default; they can still be included by setting the `include_deprecated` argument to `TRUE`.
  - `qgis_algorithms()` can _optionally_ restrict its results to non-deprecated algorithms (set the `include_deprecated` argument to `FALSE`). By default they are included, just as before.
- More consistent and intuitive handling of JSON input / output user settings (#195, #196; see `?qgis_using_json_output`).

## Fixes

- Fix bug in support for environment variable `R_QGISPROCESS_DETECT_NEWER_QGIS` (#197).
- Fix unit test to comply with `{terra}` > 1.7-65 (#202).

# qgisprocess 0.2.0

## New features

- Add vector support for `{terra}` (#184).
This makes it possible to use `SpatVector` or `SpatVectorProxy` objects as input arguments, and to coerce processing results to `SpatVector` or `SpatVectorProxy`.
- `qgis_detect_windows_paths()` and `qgis_detect_macos_paths()` put paths at the top that contain a QGIS version string and these paths are sorted according to decreasing QGIS version (#189).
This lets `qgis_configure()` select the newest QGIS version among `qgis_process` file paths that have a version string.
Furthermore, a wrapper `qgis_detect_paths()` has been added that works on both Windows and macOS (#192).

## Minor changes

- Allow half-configured states with abundant messages, so that remaining functionality can be used in debugging or even for some real stuff (#177).
- `qgis_run_algorithm()` documentation gains a section on QGIS models and scripts ([8a20669](https://github.com/r-spatial/qgisprocess/commit/8a20669ea50b4b9c14194dd864ed119e137732a9)).
- An option `qgisprocess.detect_newer_qgis` is available (mirrored by environment variable `R_QGISPROCESS_DETECT_NEWER_QGIS`) for Windows and macOS (#192).
If set as `TRUE`, during package loading `{qgisprocess}` will check whether a more recent (standalone) QGIS version is also installed while the package cache still dictates to use an older version.
In this specific scenario a question will be asked to switch to the newer version.
Without setting this option default behaviour remains in place, i.e. the user must manually intervene either by running `qgis_configure()`, by setting the `qgisprocess.path` option, or by uninstalling the older QGIS version.
- Solve a CRAN check error on `r-oldrel-macos-x86_64`, by adding support for `{stars}` 0.5-5 (#175).

# qgisprocess 0.1.0

- Initial CRAN release.
- While the package incubated as a development version for a few years, quite a few namespace changes have been made more recently (#153).
Old function names still work but these are deprecated.
They will be dropped at a later stage.
