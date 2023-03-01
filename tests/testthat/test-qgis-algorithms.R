test_that("qgis_algorithms() works", {
  skip_if_not(has_qgis())
  expect_s3_class(qgis_algorithms(), "data.frame")
  expect_gt(nrow(qgis_algorithms()), 200)
  expect_gt(ncol(qgis_algorithms()), 20)
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
  expect_s3_class(qgis_providers(), "data.frame")
  expect_true("native" %in% qgis_providers()$provider)
  expect_false("notaprovider" %in% qgis_providers()$provider)
  expect_identical(ncol(qgis_providers()), 3L)
})

test_that("assert_qgis_algorithm() works", {
  skip_if_not(has_qgis())
  expect_error(assert_qgis_algorithm("notanalgorithm"))
  expect_identical(assert_qgis_algorithm("native:filedownloader"), "native:filedownloader")
})
