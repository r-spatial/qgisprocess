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

test_that("Internal function assert_qgis_algorithm() works", {
  skip_if_not(has_qgis())
  expect_error(assert_qgis_algorithm("notanalgorithm"))
  expect_identical(assert_qgis_algorithm("native:filedownloader"), "native:filedownloader")
})

test_that("qgis_search_algorithms() works", {
  skip_if_not(has_qgis())
  expect_error(qgis_search_algorithms(), "at least one of the arguments")
  expect_error(qgis_search_algorithms(algorithm = 3))
  res1 <- qgis_search_algorithms(
    algorithm = "point.*line",
    provider = "^native$",
    group = "geometry"
  )
  expect_s3_class(res1, "data.frame")
  expect_identical(
    colnames(res1),
    c("provider", "provider_title", "group", "algorithm", "algorithm_title")
  )
  expect_gt(nrow(res1), 0L)
  res2 <- qgis_search_algorithms(algorithm = "point.*line")
  expect_gt(nrow(res2), nrow(res1))
})
