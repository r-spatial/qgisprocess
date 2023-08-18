## Resubmission

This is a resubmission, addressing the CRAN review comments received at Tue Aug 15 2023 06:20:29 UTC. Thank you for the continued review of our package.

Following changes have been made, in order to address each comment:

- In order to keep the examples for the S3 methods of generics `sf::st_as_sf` and `stars::st_as_stars` and in order to keep sf and stars in `Suggested`, we now make use of R's ability to do delayed registration of generics in suggested packages in `NAMESPACE`. Since the feature needs R >= 3.6.0, this requirement has been added in `Depends`. Before, these S3 methods were registered dynamically using `vctrs::s3_register()` but not exported.
  - Note: the given output in the CRAN review appears incorrect: in `st_as_sf.Rd` there is no `qgis_detect_macos_paths()`; in `st_as_stars.Rd` there is no `qgis_enable_plugins()` (explains why I had not understood earlier). However these two Rd-files were indeed involved.
- With relation to examples:
  - Example code that should not be run by `example()` has been wrapped by `\dontrun{}`. It has been taken care of to also keep executed example code in these cases.
  - Several examples have been wrapped in `\donttest{}` in order to avoid lengthy execution of examples in `R CMD check`. These example blocks were selected based on reports by `R CMD check` in our GitHub Actions workflow on several platforms.
    - Nonetheless, the lengthy execution times are not due to complex or long examples, but due to the nature of the package. For example, `qgis_configure(use_cached_data = TRUE)` makes several requests to QGIS to validate the cache file, which takes several seconds. Also, several other examples need to execute a simple processing step in QGIS, using simple data, but the interaction with QGIS and sometimes the QGIS processing itself takes several seconds.
- The graphical parameters (`par()`) have been reset in the 'qgis_expressions' vignette. Likewise, the `options()` statement in the 'qgisprocess' vignette has been reset in a more general manner, using `oldopt <- options(...); options(oldopt)` instead of an `options()` statement that manually sets back the old value.


## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... [11s] NOTE

  Maintainer: 'Floris Vanderhaeghe <floris.vanderhaeghe@inbo.be>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    QGIS (27:71)
    
  >  This is a false positive, triggered by inserting author 'QGIS.org' as
  advised in <https://qgis.org/en/site/getinvolved/faq/index.html#how-to-cite-qgis>.
  
  Suggests or Enhances not in mainstream repositories:
    spDataLarge
  Availability using Additional_repositories specification:
    spDataLarge   yes   https://geocompr.r-universe.dev
    
  > See below.
  
* checking package dependencies ... NOTE

  Package suggested but not available for checking: 'spDataLarge'
  
  > Relates to incoming feasibility note. The 'spDataLarge' package is available
  from the https://geocompr.r-universe.dev repository, which is included in the
  Additional_repositories field of DESCRIPTION. The package is used in a
  vignette.
  
