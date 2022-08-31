# Flip qgis_use_json_input() and rerun test-qgis-run-algorithm.R

withr::local_envvar(c(JSON_INPUT = qgis_use_json_input()))
withr::local_options(qgisprocess.use_json_input = !qgis_use_json_input())

if (has_qgis() && package_version(strsplit(qgis_version(), "-")[[1]][1]) >= "3.23.0") {

  test_that("Flipping JSON input method before re-testing works", {
    expect_identical(
      as.logical(Sys.getenv("JSON_INPUT")),
      !qgis_use_json_input()
      )
  })

  source("test-qgis-run-algorithm.R", echo = FALSE, local = TRUE)

}
