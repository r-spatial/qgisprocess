
test_that("argument coercers work", {
  expect_error(as_qgis_argument(list(), "qgis_type"), "Don't know how to convert object of type")
  expect_identical(as_qgis_argument("chr value"), "chr value")
  expect_identical(as_qgis_argument(1), "1")
  expect_identical(as_qgis_argument(1L), "1")
})

test_that("argument cleaners work", {
  expect_null(qgis_clean_argument("some valule", "some type"))

  tmp <- structure(tempfile(), class = "qgis_tempfile")
  file.create(tmp)
  expect_true(file.exists(tmp))
  qgis_clean_argument(tmp)
  expect_false(file.exists(tmp))
})
