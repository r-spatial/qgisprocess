# Flip qgis_using_json_input() and rerun test-qgis-run-algorithm.R

if (has_qgis() && package_version(qgis_version(full = FALSE)) >= "3.23.0") {
  withr::local_envvar(c(JSON_INPUT = qgis_using_json_input()))
  withr::local_options(qgisprocess.use_json_input = !qgis_using_json_input())

  test_that("Flipping JSON input method before re-testing works", {
    expect_identical(
      as.logical(Sys.getenv("JSON_INPUT")),
      !qgis_using_json_input()
    )
  })

  source("test-qgis-run-algorithm.R", echo = FALSE, local = TRUE)
}
