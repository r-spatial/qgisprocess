
test_that("qgis_run_algorithm() works", {
  skip_if_not(has_qgis())
  tmp_json <- tempfile()
  expect_output(
    qgis_run_algorithm("native:filedownloader", URL = "https://httpbin.org/get", OUTPUT = tmp_json, .quiet = FALSE),
    "^Running\\s+"
  )
  expect_true(file.exists(tmp_json))

  unlink(tmp_json)
  expect_silent(
    qgis_run_algorithm("native:filedownloader", URL = "https://httpbin.org/get", OUTPUT = tmp_json, .quiet = TRUE)
  )
  expect_true(file.exists(tmp_json))


})

test_that("qgis_has_algorithm() works", {
  skip_if_not(has_qgis())
  expect_true(qgis_has_algorithm("native:filedownloader"))
  expect_false(qgis_has_algorithm("notanalgorithm"))
})

test_that("qgis_has_provider() works", {
  skip_if_not(has_qgis())
  expect_true(qgis_has_provider("native"))
  expect_false(qgis_has_provider("notaprovider"))
})

test_that("qgis_providers() works", {
  skip_if_not(has_qgis())
  expect_true("native" %in% qgis_providers()$provider)
  expect_false("notaprovider" %in% qgis_providers()$provider)
})

test_that("is_qgis_model_file() works", {
  file <- tempfile()
  expect_false(is_qgis_model_file(file))
  dir.create(file)
  expect_false(is_qgis_model_file(file))
  unlink(file, recursive = TRUE)
  file.create(file)
  expect_true(is_qgis_model_file(file))
  unlink(file)
})

test_that("assert_algorithm_or_model_file() works", {
  file <- tempfile()
  file.create(file)
  expect_identical(assert_qgis_algorithm_or_model_file(file), file)
  expect_error(assert_qgis_algorithm_or_model_file(NULL), "`algorithm` must be")

  skip_if_not(has_qgis())
  expect_error(assert_qgis_algorithm_or_model_file("notanalgorithm"))
  expect_identical(assert_qgis_algorithm_or_model_file("native:filedownloader"), "native:filedownloader")
})
