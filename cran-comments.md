## Resubmission

This is a resubmission, addressing the CRAN review comments received at Thu Jul 20 2023 18:43:38 UTC. Thank you for reviewing our package.

Following changes have been made, in order to address each comment:

- Properly cite the QGIS URL in the Description field of DESCRIPTION, using 'QGIS.org' as author following advice in <https://qgis.org/en/site/getinvolved/faq/index.html#how-to-cite-qgis>.
- Add `\value` section to `as_qgis_argument.Rd` and `qgisprocess-deprecated.Rd`.
- Avoid the use of `:::` in examples.
- Drop examples in the documentation of unexported functions. This was only the case in `qgis_argument_spec.Rd`.
- With relation to writing in the user's home filespace:
  - In `tests/testthat/test-compat-sf.R` one test wrote a temporary file (deleted on exit) outside the R temporary directory. This has been fixed.
  - Several functions write output files. Their default filepath is a subdirectory of R's temporary directory (`tempdir()`), so this required no further changes.
  - The package stores cache files and tries to do this in an appropriate, OS-dependent directory.
    - In order to also support **R < 4.0**, we use a different (OS dependent) cache directory than the ones offered by `tools::R_user_dir()`, which first appeared in R 4.0. We still adhere to the XDG Base Directory Specification, using `rappdirs::user_cache_dir("R-qgisprocess")` as a cache directory.
    - Contrary to the previous submission, this cache directory is now actively managed. Cache files older than 90 days are removed at package loading by default (this can be changed by setting the appropriate option or environment variable). Consequently the cache directory size will typically remain under 1 MB.
- Use `SuppressWarnings()` in `tests/testthat/test-qgisprocess-deprecated.R` instead of `withr::local_options(warn = -1)`.
- Drop the `<<-` symbol in `.onLoad()` and use a separate environment to store the object.


## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... [11s] NOTE

  Maintainer: 'Floris Vanderhaeghe <floris.vanderhaeghe@inbo.be>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    QGIS (27:71)
    
  >  This is a false positive, triggered by inserting author 'QGIS.org' as
  advised in <https://qgis.org/en/site/getinvolved/faq/index.html#how-to-cite-qgis>.
  See also first bullet under 'Resubmission' above.
  
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
  
