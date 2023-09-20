test_that("qgis_run() has a successful fallback if path is NULL", {
  skip_if_not(has_qgis())

  qgisprocess_cache$path <- NULL
  qgisprocess_cache$version <- NULL

  expect_message({v <- qgis_version(query = TRUE)}, "on the fly")
  expect_length(v, 1L)

  # both qgis_version(query = TRUE) and qgis_path(query = TRUE) restore
  # the cache values:
  expect_false(is.null(qgisprocess_cache$version))
  expect_false(is.null(qgisprocess_cache$path))
})
