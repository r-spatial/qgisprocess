
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
