test_that("qgis_extract_output_by_name() works", {
  expect_identical(qgis_extract_output_by_name(list(a = 1), "a"), 1)
  expect_error(qgis_extract_output_by_name(list(a = 1), "b"), "Result has no output")
  expect_identical(qgis_extract_output_by_name(list(a = 1, output = 5)), 5)
  expect_identical(qgis_extract_output_by_name(list(a = 1, notoutput = 8)), 1)

  expect_error(qgis_extract_output_by_name(list(a = 1, notoutput = 8), single = FALSE), "Result has no output")
  expect_error(qgis_extract_output_by_name(list(a = 1, notoutput = 8), "b", single = FALSE), "Result has no output")
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

  expect_s3_class(result, "qgis_result")
  expect_true(is_qgis_result(result))
  expect_output(print(result), "^<Result")
  expect_true(
    all(c("INPUT", "FIELD", "OPERATOR", "VALUE", "OUTPUT", "FAIL_OUTPUT") %in%
      names(qgis_result_args(result)))
  )
  expect_identical(qgis_result_status(result), 0L)
  expect_type(qgis_result_stderr(result), "character")
  expect_type(qgis_result_stdout(result), "character")
  expect_error(qgis_extract_output_by_class(result, "numeric"), "zero outputs of type")

  result$.processx_result$stdout <- ""
  expect_error(qgis_check_stdout(result), "output could not be captured")
})

test_that("qgis_extract_output_by_name() functions work", {
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

  expect_identical(
    qgis_extract_output_by_name(result, "OUTPUT"),
    result$OUTPUT
  )
  expect_identical(
    qgis_extract_output_by_name(result, "FAIL_OUTPUT"),
    result$FAIL_OUTPUT
  )
  expect_identical(
    qgis_extract_output_by_name(result),
    result$OUTPUT
  )

  expect_error(qgis_extract_output_by_name(result, "A" ), "Result has no output")
  expect_error(qgis_extract_output_by_name(result, 1), "Result has no output")
})

test_that("qgis_extract_output_by_position() functions work", {
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

  expect_identical(
    qgis_extract_output_by_position(result, 1),
    result$FAIL_OUTPUT
  )
  expect_identical(
    qgis_extract_output_by_position(result, 2),
    result$OUTPUT
  )

  expect_error(qgis_extract_output_by_position(result, "A"), "Result has no output")
  expect_error(qgis_extract_output_by_position(result, 8), "Result has no output")
})

test_that("qgis_extract_output_by_class() functions work", {
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

  expect_identical(
    qgis_extract_output_by_class(result, "qgis_outputVector"),
    result$OUTPUT
  )

  expect_error(qgis_extract_output_by_class(result, "A"), "Can't extract object")
  expect_error(qgis_extract_output_by_class(result, 1), "must be a character vector")
})
