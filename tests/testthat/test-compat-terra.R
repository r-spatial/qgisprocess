test_that("terra argument coercers work", {
  skip_if_not_installed("terra")

  obj <- terra::rast(vals = 1:64800)
  expect_error(
    as_qgis_argument(obj),
    "Can't convert 'SpatRaster' object"
  )

  tmp_file <- expect_match(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer")),
    "\\.tif$"
  )
  expect_s3_class(tmp_file, "qgis_tempfile_arg")
  unlink(tmp_file)

  # also check rasters with embedded files
  obj <- terra::rast(system.file("longlake/longlake.tif", package = "qgisprocess"))

  # behaviour changed in a terra update
  sources <- terra::sources(obj)
  expect_identical(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer")),
    if (is.character(sources)) sources else sources$source
  )

  expect_warning(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "multilayer")),
    "extract the bands"
  )

  # check behaviour in case of band selection or reordering
  obj1 <- obj$longlake_2
  res <- expect_message(
    as_qgis_argument(obj1, qgis_argument_spec(qgis_type = "layer")),
    "Rewriting"
  )
  expect_s3_class(res, "qgis_tempfile_arg")

  obj2 <- obj[[3:1]]
  res <- expect_message(
    as_qgis_argument(obj2, qgis_argument_spec(qgis_type = "layer")),
    "Rewriting"
  )
  expect_s3_class(res, "qgis_tempfile_arg")

})

test_that("terra result coercers work", {
  skip_if_not_installed("terra")

  expect_s4_class(
    qgis_as_terra(
      structure(
        system.file("longlake/longlake.tif", package = "qgisprocess"),
        class = "qgis_outputRaster"
      )
    ),
    "SpatRaster"
  )

  expect_s4_class(
    qgis_as_terra(
      structure(
        system.file("longlake/longlake.tif", package = "qgisprocess"),
        class = "qgis_outputLayer"
      )
    ),
    "SpatRaster"
  )

  expect_s4_class(
    qgis_as_terra(
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
    "SpatRaster"
  )
})

test_that("terra argument coercer for extent works", {
  skip_if_not_installed("terra")

  obj <- terra::rast(system.file("longlake/longlake.tif", package = "qgisprocess"))

  bbox_representation <- expect_match(
    as_qgis_argument(terra::ext(obj), qgis_argument_spec(qgis_type = "extent")),
    "409891\\.446955431,411732\\.936955431,5083288\\.89932423,5084852\\.61932423"
  )

  expect_s3_class(bbox_representation, "character")
})
