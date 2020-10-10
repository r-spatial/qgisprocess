
test_that("argument coercers work", {
  expect_error(as_qgis_argument(list()), "Don't know how to convert object of type")
  expect_identical(as_qgis_argument("chr value"), "chr value")
  expect_identical(as_qgis_argument(1), "1")
  expect_identical(as_qgis_argument(1L), "1")
  expect_identical(as_qgis_argument(TRUE), "TRUE")
})

test_that("default arguments are handled correctly", {
  expect_silent(as_qgis_argument(NULL))
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "sink")
    ),
    "Using `argname = qgis_tmp_vector"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "vectorDestination")
    ),
    "Using `argname = qgis_tmp_vector"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "rasterDestination")
    ),
    "Using `argname = qgis_tmp_raster"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "fileDestination")
    ),
    "Using `argname = qgis_tmp_file"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "folderDestination")
    ),
    "Using `argname = qgis_tmp_folder"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname")
    ),
    "Argument `argname` is unspecified"
  )
})

test_that("argument cleaners work", {
  expect_null(qgis_clean_argument("some valule"))

  tmp <- structure(tempfile(), class = "qgis_tempfile_arg")
  file.create(tmp)
  expect_true(file.exists(tmp))
  qgis_clean_argument(tmp)
  expect_false(file.exists(tmp))
})
