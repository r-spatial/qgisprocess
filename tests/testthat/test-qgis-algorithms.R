test_that("qgis_algorithms() works", {
  skip_if_not(has_qgis())
  algs <- qgis_algorithms()
  expect_true(tibble::is_tibble(algs))
  expect_gt(nrow(algs), 200)
  expect_gt(ncol(algs), 20)
  old_names <- c(
    "provider", "provider_title", "algorithm",
    "algorithm_id", "algorithm_title"
  )
  # check that the 'old_names' columns have complete data (no NAs):
  expect_true(all(vapply(algs[old_names], function(x) all(!is.na(x)), logical(1))))
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
  expect_named(
    qgis_providers(),
    c("provider", "provider_title", "algorithm_count")
  )
})

test_that("assert_qgis_algorithm() works", {
  skip_if_not(has_qgis())
  expect_error(assert_qgis_algorithm("notanalgorithm"))
  expect_identical(assert_qgis_algorithm("native:filedownloader"), "native:filedownloader")
})
