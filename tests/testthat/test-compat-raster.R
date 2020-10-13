
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

test_that("raster result coercers work", {
  skip_if_not_installed("raster")
  skip_if_not_installed("rgdal")

  expect_is(
    qgis_as_raster(
      structure(
        system.file("longlake/longlake.tif", package = "qgisprocess"),
        class = "qgis_outputRaster"
      )
    ),
    "RasterLayer"
  )

  expect_is(
    qgis_as_brick(
      structure(
        system.file("longlake/longlake.tif", package = "qgisprocess"),
        class = "qgis_outputRaster"
      )
    ),
    "RasterBrick"
  )

  expect_is(
    qgis_as_raster(
      structure(
        system.file("longlake/longlake.tif", package = "qgisprocess"),
        class = "qgis_outputLayer"
      )
    ),
    "RasterLayer"
  )

  expect_is(
    qgis_as_brick(
      structure(
        system.file("longlake/longlake.tif", package = "qgisprocess"),
        class = "qgis_outputLayer"
      )
    ),
    "RasterBrick"
  )

  expect_is(
    qgis_as_raster(
      structure(
        list(
          OUTPUT = structure(
            system.file("longlake/longlake.tif", package = "qgisprocess"),
            class = "qgis_outputRaster"
          )
        ),
        class = "qgis_result"
      )
    ),
    "RasterLayer"
  )

  expect_is(
    qgis_as_brick(
      structure(
        list(
          OUTPUT = structure(
            system.file("longlake/longlake.tif", package = "qgisprocess"),
            class = "qgis_outputRaster"
          )
        ),
        class = "qgis_result"
      )
    ),
    "RasterBrick"
  )
})
