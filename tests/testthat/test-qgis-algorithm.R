
test_that("qgis_run_algorithm() works", {
  skip_if_not(has_qgis())
  skip_if_offline()

  tmp_json <- tempfile()
  expect_output(
    qgis_run_algorithm("native:filedownloader", URL = "https://httpbin.org/get", OUTPUT = tmp_json, .quiet = FALSE),
    "^Running\\s+"
  )
  expect_true(file.exists(tmp_json))

  unlink(tmp_json)
  result <- expect_silent(
    qgis_run_algorithm("native:filedownloader", URL = "https://httpbin.org/get", OUTPUT = tmp_json, .quiet = TRUE)
  )
  expect_true(file.exists(tmp_json))

  expect_is(result, "qgis_result")
  expect_true(is_qgis_result(result))
  expect_output(print(result), "^<Result")
  expect_named(qgis_result_args(result), c("URL", "OUTPUT"))
  expect_is(qgis_result_stderr(result), "character")
  expect_is(qgis_result_stdout(result), "character")
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

test_that("assert_qgis_algorithm() works", {
  skip_if_not(has_qgis())
  expect_error(assert_qgis_algorithm("notanalgorithm"))
  expect_identical(assert_qgis_algorithm("native:filedownloader"), "native:filedownloader")
})
