
test_that("qgis_run_algorithm() works", {
  skip_if_not(has_qgis())
  skip_if_offline()

  tmp_json <- qgis_tmp_file(".json")
  result <- expect_output(
    qgis_run_algorithm("native:filedownloader", URL = "https://httpbin.org/get", OUTPUT = tmp_json, .quiet = FALSE),
    "^Running\\s+"
  )
  expect_true(file.exists(tmp_json))
  qgis_result_clean(result)

  unlink(tmp_json)
  result <- expect_silent(
    qgis_run_algorithm("native:filedownloader", URL = "https://httpbin.org/get", OUTPUT = tmp_json, .quiet = TRUE)
  )
  expect_true(file.exists(tmp_json))
  qgis_result_clean(result)
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
