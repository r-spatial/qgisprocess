
test_that("qgis_output() works", {
  expect_identical(qgis_output(list(a = 1), "a"), 1)
  expect_identical(qgis_output(list(a = 1), "b", default = 2), 2)
  expect_error(qgis_output(list(a = 1), "b"), "Result has no output")
})

test_that("qgis_result_*() functions work", {
  skip_if_not(has_qgis())

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

  expect_true(all(file.exists(tmp_gpkg, tmp_gpkg2)))
  qgis_result_clean(result)
  expect_false(any(file.exists(tmp_gpkg, tmp_gpkg2)))

  expect_is(result, "qgis_result")
  expect_true(is_qgis_result(result))
  expect_output(print(result), "^<Result")
  expect_true(
    all(c("INPUT", "FIELD", "OPERATOR", "VALUE", "OUTPUT", "FAIL_OUTPUT") %in%
          names(qgis_result_args(result)))
    )
  expect_identical(qgis_result_status(result), 0L)
  expect_is(qgis_result_stderr(result), "character")
  expect_is(qgis_result_stdout(result), "character")
  expect_error(qgis_result_single(result, "numeric"), "zero outputs of type")
  expect_identical(
    qgis_result_single(result, "qgis_outputVector"),
    result$OUTPUT
    )

  result$.processx_result$stdout <- ""
  expect_error(qgis_check_stdout(result), "output could not be captured")
})
