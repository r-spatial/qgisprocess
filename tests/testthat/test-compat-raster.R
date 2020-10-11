
test_that("raster argument coercers work", {
  skip_if_not_installed("raster")
  skip_if_not_installed("rgdal")

  obj <- raster::raster()
  expect_error(
    as_qgis_argument(obj),
    "Can't convert 'RasterLayer' object"
  )

  tmp_file <- expect_match(
    suppressWarnings(as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer"))),
    "\\.tif$"
  )
  expect_is(tmp_file, "qgis_tempfile_arg")
  unlink(tmp_file)

  # also check rasters with embedded files
  obj <- raster::raster(system.file("longlake/longlake.tif", package = "qgisprocess"))
  expect_identical(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer")),
    obj@file@name
  )

  # check RasterBrick
  obj <- raster::brick(system.file("longlake/longlake.tif", package = "qgisprocess"))
  expect_identical(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer")),
    obj@file@name
  )
})
