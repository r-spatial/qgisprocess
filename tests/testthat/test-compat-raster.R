
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

test_that("raster crs work", {
  skip_if_not_installed("raster")
  skip_if_not_installed("rgdal")

  obj <- raster::raster(system.file("longlake/longlake.tif", package = "qgisprocess"))

  crs_representation <- expect_match(
    as_qgis_argument(raster::crs(obj), qgis_argument_spec(qgis_type = "crs")),
    "UTM zone 20N"
  )

  expect_is(crs_representation, "character")
})

test_that("raster argument coercer for extent works", {
  skip_if_not_installed("raster")
  skip_if_not_installed("rgdal")

  obj <- raster::raster(system.file("longlake/longlake.tif", package = "qgisprocess"))


  bbox_representation <- expect_match(
    as_qgis_argument(raster::extent(obj), qgis_argument_spec(qgis_type = "extent")),
    "409891\\.446955431,411732\\.936955431,5083288\\.89932423,5084852\\.61932423"
  )

  expect_is(bbox_representation, "character")
})

test_that("raster argument coercer for crs works", {
  skip_if_not_installed("raster")
  skip_if_not_installed("rgdal")
  skip_if_not(has_qgis())

  obj <- raster::raster(system.file("longlake/longlake.tif", package = "qgisprocess"))

  result <- qgis_run_algorithm(
    "native:createconstantrasterlayer",
    EXTENT = raster::extent(obj),
    TARGET_CRS = raster::crs(obj),
    PIXEL_SIZE = 1000,
    OUTPUT_TYPE = "Byte",
    OUTPUT = qgis_tmp_raster(),
    NUMBER = 5,
    .quiet = TRUE
  )

  expect_true(file.exists(result$OUTPUT))
  qgis_result_clean(result)
})
