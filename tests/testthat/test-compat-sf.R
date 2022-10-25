
test_that("sf argument coercers work", {
  skip_if_not_installed("sf")
  sf_obj <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  expect_error(
    as_qgis_argument(sf_obj),
    "Can't convert 'sf' object"
  )

  tmp_file <- expect_match(
    as_qgis_argument(sf_obj, qgis_argument_spec(qgis_type = "layer")),
    "\\.gpkg$"
  )
  expect_is(tmp_file, "qgis_tempfile_arg")
  unlink(tmp_file)
})

test_that("sf objects can be extracted from a qgis_result", {
  skip_if_not_installed("sf")
  skip_if_not(has_qgis())

  buffer_longlake <- function(OUTPUT, ...) {
    qgis_run_algorithm(
      "native:buffer",
      INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
      DISTANCE = 100,
      DISSOLVE = TRUE,
      MITER_LIMIT = 2,
      OUTPUT = OUTPUT,
      END_CAP_STYLE = 0,
      JOIN_STYLE = 0,
      .quiet = TRUE
    )
  }

  result <- buffer_longlake(OUTPUT = qgis_tmp_vector())
  result_alt <- buffer_longlake(OUTPUT = "ogr:dbname=llbuffer.gpkg table=llbuffer")

  result_sf <- sf::st_as_sf(result)
  expect_is(result_sf, "sf")

  result_sf_alt <- sf::st_as_sf(result_alt)
  expect_identical(result_sf, result_sf_alt)

  result$OUTPUT <- NULL
  unlink("llbuffer.gpkg")
  expect_error(sf::st_as_sf(result), "Can't extract object.")
})

test_that("sf crs work", {
  skip_if_not_installed("sf")
  sf_obj <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  crs_representation <- expect_match(
    as_qgis_argument(sf::st_crs(sf_obj), qgis_argument_spec(qgis_type = "crs")),
    "^GEOGCS"
  )

  expect_is(crs_representation, "character")
})

test_that("sf bbox work", {
  skip_if_not_installed("sf")
  sf_obj <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  bbox_representation <- expect_match(
    as_qgis_argument(sf::st_bbox(sf_obj), qgis_argument_spec(qgis_type = "extent")),
    "-84\\.3238525390625,-75\\.4569778442383,33\\.8819923400879,36\\.5896492004395\\[EPSG:4267\\]"
  )

  expect_is(bbox_representation, "character")
})

test_that("sf crs and bbox work", {
  skip_if_not_installed("sf")
  skip_if_not(has_qgis())

  sf_obj <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  result <- qgis_run_algorithm(
    "native:createconstantrasterlayer",
    EXTENT = sf::st_bbox(sf_obj),
    TARGET_CRS = sf::st_crs(5514),
    PIXEL_SIZE = 1000,
    OUTPUT_TYPE = "Byte",
    OUTPUT = qgis_tmp_raster(),
    NUMBER = 5,
    .quiet = TRUE
  )

  expect_true(file.exists(result$OUTPUT))
  qgis_result_clean(result)
})

test_that("sfc to QGIS point work", {
  skip_if_not_installed("sf")

  point <- sf::st_sfc(sf::st_point(c(1, 2)), crs = sf::st_crs("EPSG:5514"))

  point_representation <- expect_match(
    as_qgis_argument(point, qgis_argument_spec(qgis_type = "point")),
    "1,2\\[EPSG:5514\\]"
  )

  expect_is(point_representation, "character")

  point <- sf::st_sfc(sf::st_point(c(1, 2)))

  point_representation <- expect_match(
    as_qgis_argument(point, qgis_argument_spec(qgis_type = "point")),
    "1,2"
  )

  expect_is(point_representation, "character")
})

test_that("sfc to QGIS point rasises issues", {
  skip_if_not_installed("sf")

  points <- sf::st_sfc(list(sf::st_point(c(1, 2)),sf::st_point(c(1, 2))), crs = sf::st_crs("EPSG:5514"))

  expect_error(
    as_qgis_argument(
      points,
      qgis_argument_spec(qgis_type = "point")
    ),
    "Can't convert 'sfc' object to QGIS type 'point' as the length is not equal to 1"
  )

  points <- sf::st_sfc(sf::st_multipoint(matrix(1:15, ncol = 3)), crs = sf::st_crs("EPSG:5514"))

  expect_error(
    as_qgis_argument(
      points,
      qgis_argument_spec(qgis_type = "point")
    ),
    "Can't convert 'sfc' object to QGIS type 'point' as type is not 'POINT'"
  )
})

test_that("sf to QGIS point rasises issues", {
  skip_if_not_installed("sf")

  suppressWarnings(
    points <- sf::st_centroid(sf::read_sf(system.file("shape/nc.shp", package = "sf")))
  )

  expect_error(
    as_qgis_argument(
      points,
      qgis_argument_spec(qgis_type = "point")
    ),
    "Can't convert 'sfc' object to QGIS type 'point' as the length is not equal to 1"
  )

  points <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))[1,]

  expect_error(
    as_qgis_argument(
      points,
      qgis_argument_spec(qgis_type = "point")
    ),
    "Can't convert 'sfc' object to QGIS type 'point' as type is not 'POINT'"
  )
})

test_that("POINT to QGIS point work", {
  skip_if_not_installed("sf")

  point <- sf::st_point(c(1, 2))

  point_representation <- expect_match(
    as_qgis_argument(point, qgis_argument_spec(qgis_type = "point")),
    "1,2"
  )

  expect_is(point_representation, "character")
})

test_that("sf to QGIS point work", {
  skip_if_not_installed("sf")

  data <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  data <- sf::st_transform(data, sf::st_crs("EPSG:32019"))

  suppressWarnings(
    points <- sf::st_centroid(data[1,])
  )

  point_representation <- expect_match(
    as_qgis_argument(
      points,
      qgis_argument_spec(qgis_type = "point")
    ),
    "1265036\\.90059[0-9]+,985175\\.481905[0-9]+\\[EPSG:32019\\]")

  expect_is(point_representation, "character")
})
