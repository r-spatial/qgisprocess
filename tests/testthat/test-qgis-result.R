
test_that("qgis_output() works", {
  expect_identical(qgis_output(list(a = 1), "a"), 1)
  expect_identical(qgis_output(list(a = 1), "b", default = 2), 2)
  expect_error(qgis_output(list(a = 1), "b"), "Result has no output")
})

test_that("qgis_result_*() functions work", {
  skip_if_not(has_qgis())
  skip_if_offline()

  tmp_gpkg <- qgis_tmp_vector(".gpkg")

  result <- qgis_run_algorithm(
    "native:reprojectlayer",
    INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
    TARGET_CRS = "EPSG:4326",
    OUTPUT = tmp_gpkg,
    .quiet = TRUE
  )

  expect_true(file.exists(tmp_gpkg))
  qgis_result_clean(result)
  expect_false(file.exists(tmp_gpkg))

  expect_is(result, "qgis_result")
  expect_true(is_qgis_result(result))
  expect_output(print(result), "^<Result")
  expect_true(
    all(c("INPUT", "TARGET_CRS", "OUTPUT") %in%
          names(qgis_result_args(result)))
    )
  expect_is(qgis_result_stderr(result), "character")
  expect_is(qgis_result_stdout(result), "character")
  expect_error(qgis_result_single(result, "numeric"), "zero outputs of type")
  expect_identical(
    qgis_result_single(result, "qgis_outputVector"),
    result$OUTPUT
    )
})
