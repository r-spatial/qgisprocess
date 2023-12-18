# qgisprocess (development version)

- Solve a CRAN check error on `r-oldrel-macos-x86_64`, by adding support for {stars} 0.5-5 (#175).
- Allow half-configured states with abundant messages, so that remaining functionality can be used in debugging or even for some real stuff (#177).
- Add vector support for {terra} (#184).
This makes it possible to use SpatVector(Proxy) objects as input arguments, and to coerce processing results to SpatVector(Proxy).
- `qgis_run_algorithm()` documentation gains a section on QGIS models and scripts (8a20669).

# qgisprocess 0.1.0

- Initial CRAN release.
- While the package incubated as a development version for a few years, quite a few namespace changes have been made more recently (#153).
Old function names still work but these are deprecated.
They will be dropped at a later stage.
