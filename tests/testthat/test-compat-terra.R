test_that("terra argument coercers work for rasters", {
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

  tmp_file <- expect_match(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "raster")),
    "\\.tif$"
  )
  expect_s3_class(tmp_file, "qgis_tempfile_arg")
  unlink(tmp_file)
})

test_that("terra argument coercers work for SpatRaster referring to a file", {
  skip_if_not_installed("terra")

  obj <- terra::rast(system.file("longlake/longlake.tif", package = "qgisprocess"))
  sources <- terra::sources(obj)
  expect_identical(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer")),
    if (is.character(sources)) sources else sources$source
  )

  expect_identical(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "raster")),
    if (is.character(sources)) sources else sources$source
  )

  expect_warning(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "multilayer")),
    "extract the bands"
  )

  # check effect of resetting CRS
  obj2 <- obj
  terra::crs(obj2) <- NA
  res <- expect_message(
    as_qgis_argument(obj2, qgis_argument_spec(qgis_type = "raster")),
    "Rewriting.*since its CRS has been set to another value"
  )
  expect_s3_class(res, "qgis_tempfile_arg")

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





test_that("terra argument coercers work for locally created SpatVector", {
  skip_if_not_installed("terra")

  obj <- terra::vect(
    x = c("POINT (7e5 7e5)", "POINT (6e5 6.5e5)"),
    crs = "EPSG:3812"
  )
  expect_error(
    as_qgis_argument(obj),
    "Can't convert 'SpatVector' object"
  )

  tmp_file <- expect_match(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer")),
    "\\.gpkg$"
  )
  expect_s3_class(tmp_file, "qgis_tempfile_arg")
  unlink(tmp_file)

  tmp_file <- expect_match(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "vector")),
    "\\.gpkg$"
  )
  expect_s3_class(tmp_file, "qgis_tempfile_arg")
  unlink(tmp_file)

  expect_error(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "point")),
    "exactly one row and the geometry must be a point"
  )

  if (getRversion() < "4.3" && is_macos()) {
    expect_match(
      as_qgis_argument(obj[1, ], qgis_argument_spec(qgis_type = "point")),
      "^[\\de\\+]+,[\\de\\+]+(?:\\[\\w+:\\d+\\])?$",
      perl = TRUE
    )
  } else {
    expect_match(
      as_qgis_argument(obj[1, ], qgis_argument_spec(qgis_type = "point")),
      "^[\\de\\+]+,[\\de\\+]+\\[\\w+:\\d+\\]$",
      perl = TRUE
    )
  }

  terra::crs(obj) <- NA
  expect_match(
    as_qgis_argument(obj[1, ], qgis_argument_spec(qgis_type = "point")),
    "^[\\de\\+]+,[\\de\\+]+$",
    perl = TRUE
  )
})


test_that("terra argument coercers work for SpatVector referring to a file", {
  skip_if_not_installed("terra")

  tmp_file <- qgis_tmp_vector()
  expect_match(tmp_file, "\\.gpkg$")
  withr::local_file(tmp_file)

  suppressWarnings({
    # terra gives warning on this file: 'Z coordinates ignored'
    obj <- terra::vect(system.file("longlake/longlake.gpkg", package = "qgisprocess"))
  })
  terra::writeVector(obj, tmp_file)
  obj <- terra::vect(tmp_file)
  sources <- terra::sources(obj)
  expect_identical(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer")),
    if (is.character(sources)) sources else sources$source
  )

  expect_identical(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "vector")),
    if (is.character(sources)) sources else sources$source
  )

  expect_error(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "point")),
    "exactly one row and the geometry must be a point"
  )

  # check effect of resetting CRS
  obj2 <- obj
  terra::crs(obj2) <- NA
  res <- expect_message(
    as_qgis_argument(obj2, qgis_argument_spec(qgis_type = "vector")),
    "Rewriting.*since its CRS has been set to another value"
  )
  expect_s3_class(res, "qgis_tempfile_arg")

  # check effect of changed attribute names
  obj2 <- obj
  names(obj2) <- paste(names(obj), "new", sep = "_")
  res <- expect_message(
    as_qgis_argument(obj2, qgis_argument_spec(qgis_type = "vector")),
    "Rewriting.*since its attribute names"
  )
  expect_s3_class(res, "qgis_tempfile_arg")
})

test_that("terra argument coercers work for a SpatVector referring to a layer in a multi-layer file", {
  skip_if_not_installed("terra")

  tmp_file <- qgis_tmp_vector()
  expect_match(tmp_file, "\\.gpkg$")
  withr::local_file(tmp_file)

  suppressWarnings({
    # terra gives warning on this file: 'Z coordinates ignored'
    obj <- terra::vect(system.file("longlake/longlake.gpkg", package = "qgisprocess"))
  })
  terra::writeVector(obj, tmp_file)
  obj <- terra::vect(tmp_file)

  tmp_file2 <- qgis_tmp_vector()
  expect_match(tmp_file2, "\\.gpkg$")
  withr::local_file(tmp_file2)
  terra::writeVector(obj, tmp_file2)
  terra::writeVector(
    obj,
    tmp_file,
    layer = "layer2",
    insert = TRUE,
    overwrite = TRUE
  )
  expect_length(terra::vector_layers(tmp_file), 2)
  obj <- terra::vect(tmp_file, layer = "layer2")
  expect_match(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "vector")),
    "\\|layername="
  )
})





test_that("terra argument coercers work for SpatVectorProxy", {
  skip_if_not_installed("terra")

  tmp_file <- qgis_tmp_vector()
  expect_match(tmp_file, "\\.gpkg$")
  withr::local_file(tmp_file)

  suppressWarnings({
    # terra gives warning on this file: 'Z coordinates ignored'
    obj <- terra::vect(system.file("longlake/longlake.gpkg", package = "qgisprocess"))
  })
  terra::writeVector(obj, tmp_file)
  obj <- terra::vect(tmp_file, proxy = TRUE)
  sources <- terra::sources(obj)
  expect_identical(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer")),
    if (is.character(sources)) sources else sources$source
  )

  expect_identical(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "vector")),
    if (is.character(sources)) sources else sources$source
  )

  expect_error(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "point")),
    "exactly one row and the geometry must be a point"
  )

  # check behaviour for qgis_type = "point"
  tmp_file <- qgis_tmp_vector()
  withr::local_file(tmp_file)
  terra::writeVector(
    terra::vect(
      x = "POINT (7e5 7e5)",
      crs = "EPSG:3812"
    ),
    tmp_file
  )
  obj <- terra::vect(tmp_file, proxy = TRUE)
  if (getRversion() < "4.3" && is_macos()) {
    expect_match(
      as_qgis_argument(obj, qgis_argument_spec(qgis_type = "point")),
      "^[\\de\\+]+,[\\de\\+]+(?:\\[\\w+:\\d+\\])?$",
      perl = TRUE
    )
  } else {
    expect_match(
      as_qgis_argument(obj, qgis_argument_spec(qgis_type = "point")),
      "^[\\de\\+]+,[\\de\\+]+\\[\\w+:\\d+\\]$",
      perl = TRUE
    )
  }
})

test_that("terra argument coercers work for a SpatVectorProxy referring to a layer in a multi-layer file", {
  skip_if_not_installed("terra")

  tmp_file <- qgis_tmp_vector()
  expect_match(tmp_file, "\\.gpkg$")
  withr::local_file(tmp_file)

  suppressWarnings({
    # terra gives warning on this file: 'Z coordinates ignored'
    obj <- terra::vect(system.file("longlake/longlake.gpkg", package = "qgisprocess"))
  })
  terra::writeVector(obj, tmp_file)
  obj <- terra::vect(tmp_file)

  tmp_file2 <- qgis_tmp_vector()
  expect_match(tmp_file2, "\\.gpkg$")
  withr::local_file(tmp_file2)
  terra::writeVector(obj, tmp_file2)
  terra::writeVector(
    obj,
    tmp_file,
    layer = "layer2",
    insert = TRUE,
    overwrite = TRUE
  )
  obj <- terra::vect(tmp_file, layer = "layer2", proxy = TRUE)
  expect_match(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "vector")),
    "\\|layername="
  )
})



test_that("terra result coercers to SpatRaster work", {
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





test_that("terra result coercers to SpatVector work", {
  skip_if_not_installed("terra")

  tmp_file <- qgis_tmp_vector()
  expect_match(tmp_file, "\\.gpkg$")
  withr::local_file(tmp_file)

  suppressWarnings({
    # terra gives warning on this file: 'Z coordinates ignored'
    obj <- terra::vect(system.file("longlake/longlake.gpkg", package = "qgisprocess"))
  })
  terra::writeVector(obj, tmp_file)

  expect_s4_class(
    qgis_as_terra(structure(tmp_file, class = "qgis_outputVector")),
    "SpatVector"
  )

  expect_s4_class(
    qgis_as_terra(
      structure(tmp_file, class = "qgis_outputVector"),
      proxy = TRUE
    ),
    "SpatVectorProxy"
  )

  expect_s4_class(
    qgis_as_terra(structure(tmp_file, class = "qgis_outputLayer")),
    "SpatVector"
  )

  expect_s4_class(
    qgis_as_terra(
      structure(
        list(OUTPUT = structure(tmp_file, class = "qgis_outputVector")),
        class = "qgis_result"
      )
    ),
    "SpatVector"
  )

  expect_s4_class(
    qgis_as_terra(
      structure(
        list(OUTPUT = structure(tmp_file, class = "qgis_outputVector")),
        class = "qgis_result"
      ),
      proxy = TRUE
    ),
    "SpatVectorProxy"
  )

  # check acceptance of QGIS <dsn>|layername=<layer> format
  terra::writeVector(
    obj,
    tmp_file,
    layer = "layer2",
    insert = TRUE,
    overwrite = TRUE
  )
  expect_length(terra::vector_layers(tmp_file), 2)
  expect_s4_class(
    qgis_as_terra(
      structure(
        list(OUTPUT = structure(
          paste0(tmp_file, "|layername=layer2"),
          class = "qgis_outputVector"
        )),
        class = "qgis_result"
      )
    ),
    "SpatVector"
  )
})





test_that("terra argument coercer for SpatExtent works", {
  skip_if_not_installed("terra")

  obj <- terra::rast(system.file("longlake/longlake.tif", package = "qgisprocess"))

  expect_error(
    as_qgis_argument(terra::ext(obj)),
    "Can't convert"
  )

  bbox_representation <- expect_match(
    as_qgis_argument(terra::ext(obj), qgis_argument_spec(qgis_type = "extent")),
    "409891\\.446955431,411732\\.936955431,5083288\\.89932423,5084852\\.61932423"
  )

  expect_s3_class(bbox_representation, "character")
})
