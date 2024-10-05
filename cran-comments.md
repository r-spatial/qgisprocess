## R CMD check results

0 errors | 0 warnings | 3 notes

* checking DESCRIPTION meta-information ... NOTE
  
  Author field differs from that derived from Authors@R

  > I think this a false positive:

    - in the source code, the DESCRIPTION only contains Authors@R, which is formatted correctly. The field went unchanged since several CRAN versions, and was not altered in this submission.
    - 'Author' has been automatically derived  by R CMD build. (using R 4.4.1)
    - the difference is that the names of the 'comment' elements are present in Authors@R, but not in Author (typical example: the term 'ORCID')

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
  
