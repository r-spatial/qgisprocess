test_that("qgis_output() works", {
  withr::local_options(warn = -1)
  expect_error(
    qgis_output(list(abcde = 1), "a"),
    "does not inherit from class qgis_result"
  )
  qres <- structure(list(a = 1, .args = "foo"), class = "qgis_result")
  expect_identical(qgis_output(qres, "a"), 1)
  expect_identical(qgis_output(qres, 1), 1)
  expect_error(qgis_output(qres, "b"), "Result has no output")
  expect_error(qgis_output(qres, ".args"), "Result has no output")
})


test_that("qgis_result_single() works", {
  skip_if_not(has_qgis())
  withr::local_options(warn = -1)

  tmp_gpkg <- qgis_tmp_vector(".gpkg")
  tmp_gpkg2 <- qgis_tmp_vector(".gpkg")

  result <- qgis_run_algorithm(
    "native:extractbyattribute",
    INPUT = system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
    FIELD = "WAYPOINT_I",
    OPERATOR = 0,
    VALUE = "10",
    OUTPUT = tmp_gpkg,
    FAIL_OUTPUT = tmp_gpkg2,
    .quiet = TRUE
  )

  expect_error(qgis_result_single(result, "numeric"), "zero outputs of type")
  expect_identical(
    qgis_result_single(result, "qgis_outputVector"),
    result$OUTPUT
  )
  expect_identical(
    qgis_result_single(result),
    result$OUTPUT
  )
})
