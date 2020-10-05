
test_that("argument coercers work", {
  expect_error(as_qgis_argument(list(), "qgis_type"), "Don't know how to convert object of type")
  expect_identical(as_qgis_argument("chr value"), "chr value")
  expect_identical(as_qgis_argument(1), "1")
  expect_identical(as_qgis_argument(1L), "1")
})

test_that("sf argument coercers work", {
  skip_if_not_installed("sf")
  sf_obj <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  expect_error(as_qgis_argument(sf_obj, "integer"), "Can't use 'sf' objects")

  tmp_file <- expect_match(as_qgis_argument(sf_obj, "source"), "\\.gpkg$")
  expect_is(tmp_file, "qgis_tempfile")
  unlink(tmp_file)
})

test_that("rasster argument coercers work", {
  skip_if_not_installed("raster")
  skip_if_not_installed("rgdal")

  obj <- raster::raster()
  expect_error(as_qgis_argument(obj, "integer"), "Can't use 'RasterLayer' objects")

  tmp_file <- expect_match(suppressWarnings(as_qgis_argument(obj, "raster")), "\\.tif$")
  expect_is(tmp_file, "qgis_tempfile")
  unlink(tmp_file)
})

test_that("argument cleaners work", {
  expect_null(qgis_clean_argument("some valule", "some type"))

  tmp <- structure(tempfile(), class = "qgis_tempfile")
  file.create(tmp)
  expect_true(file.exists(tmp))
  qgis_clean_argument(tmp)
  expect_false(file.exists(tmp))
})
