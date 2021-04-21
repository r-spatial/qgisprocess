
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

  result <- qgis_run_algorithm(
    "native:buffer",
    INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
    DISTANCE = 100,
    DISSOLVE = TRUE,
    MITER_LIMIT = 2,
    OUTPUT = qgis_tmp_vector(),
    END_CAP_STYLE = 0,
    JOIN_STYLE = 0,
    .quiet = TRUE
  )

  result_sf <- sf::st_as_sf(result)
  expect_is(result_sf, "sf")

  result$OUTPUT <- NULL
  expect_error(sf::st_as_sf(result), "Can't extract sf object.")
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

  sf_obj <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

  result <- qgis_run_algorithm("native:createconstantrasterlayer",
                               EXTENT = sf::st_bbox(sf_obj),
                               TARGET_CRS = sf::st_crs(5514),
                               PIXEL_SIZE = 1000,
                               NUMBER=5,
                               .quiet = TRUE)
})
