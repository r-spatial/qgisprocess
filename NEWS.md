# qgisprocess (development version)

## New features

- Add vector support for {terra} (#184).
This makes it possible to use `SpatVector` or `SpatVectorProxy` objects as input arguments, and to coerce processing results to `SpatVector` or `SpatVectorProxy`.
- `qgis_detect_windows_paths()` and `qgis_detect_macos_paths()` put paths at the top that contain a QGIS version string and these paths are sorted according to decreasing QGIS version (#189).
This lets `qgis_configure()` prefer the newest QGIS version from `qgis_process` file paths that have a version string.

## Other changes

- Allow half-configured states with abundant messages, so that remaining functionality can be used in debugging or even for some real stuff (#177).
- `qgis_run_algorithm()` documentation gains a section on QGIS models and scripts (8a20669).
- Solve a CRAN check error on `r-oldrel-macos-x86_64`, by adding support for {stars} 0.5-5 (#175).

# qgisprocess 0.1.0

- Initial CRAN release.
- While the package incubated as a development version for a few years, quite a few namespace changes have been made more recently (#153).
Old function names still work but these are deprecated.
They will be dropped at a later stage.
