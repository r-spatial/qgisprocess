## This version includes a change at CRAN’s request

This version includes a fix to solve the ERRORs in the CRAN check results page at <https://cran.r-project.org/web/checks/check_results_qgisprocess.html> (as consulted 6 Feb 2024). A unit test for 'terra' compatibility failed. It was the unit test itself that needed updating in order to comply with current 'terra' behaviour.

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... [11s] NOTE

  Maintainer: 'Floris Vanderhaeghe <floris.vanderhaeghe@inbo.be>'
  
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

## revdepcheck results

0 reverse dependencies present on CRAN.
  
