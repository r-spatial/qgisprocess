
test_that("raster argument coercers work", {
  skip_if_not_installed("raster")
  skip_if_not_installed("rgdal")

  obj <- raster::raster()
  expect_error(as_qgis_argument(obj, "integer"), "Can't use 'RasterLayer' objects")

  tmp_file <- expect_match(suppressWarnings(as_qgis_argument(obj, "raster")), "\\.tif$")
  expect_is(tmp_file, "qgis_tempfile")
  unlink(tmp_file)

  # also check rasters with embedded files
  obj <- raster::raster(system.file("longlake/longlake.tif", package = "qgisprocess"))
  expect_identical(as_qgis_argument(obj, "raster"), obj@file@name)
})
