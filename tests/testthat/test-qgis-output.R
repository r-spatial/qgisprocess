
test_that("characters are not incorrectly re-encoded from output", {
  skip_if_not(has_qgis())

  result <- qgis_run_algorithm(
    "native:simplifygeometries",
    INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
    METHOD = 0,
    TOLERANCE = 10,
    # e accent forward
    OUTPUT = qgis_tmp_vector("\U00E9.gpkg"),
    .quiet = TRUE
  )

  expect_identical(result$.args$OUTPUT, unclass(result$OUTPUT))
})

test_that("output parsing for string, numeric, and classed types works", {
  skip_if_not(has_qgis())
  skip_if_not_installed("stars")

  longlake_depth <- stars::read_stars(
    system.file("longlake/longlake_depth.tif", package = "qgisprocess")
  ) %>%
    sf::st_set_crs(sf::NA_crs_)
  longlake_mask <- longlake_depth > 0

  result <- qgis_run_algorithm(
    "native:rasterlayerzonalstats",
    INPUT = longlake_depth,
    BAND = 1,
    ZONES = longlake_mask,
    ZONES_BAND = 1,
    REF_LAYER = 0,
    OUTPUT_TABLE = tempfile(fileext = ".csv"),
    .quiet = TRUE
  )

  # string and numeric args should have no class
  expect_true(is.character(result$EXTENT))
  expect_identical(attr(result$EXTENT, "class"), NULL)
  expect_true(is.numeric(result$HEIGHT_IN_PIXELS))
  expect_identical(attr(result$HEIGHT_IN_PIXELS, "class"), NULL)

  # vector outputs should have a class
  expect_is(result$OUTPUT_TABLE, "qgis_outputVector")

  result$OUTPUT_TABLE
})

test_that("output parsing for multilayer outputs works", {
  skip_if_not(has_qgis())

  result_empty <- qgis_run_algorithm(
    "native:splitvectorlayer",
    INPUT = system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
    FIELD = "not_a_field_name",
    FILE_TYPE = 0,
    OUTPUT = qgis_tmp_file(""),
    .quiet = TRUE
  )

  expect_true(dir.exists(result_empty$OUTPUT))
  expect_identical(
    result_empty$OUTPUT_LAYERS,
    structure(character(0), class = "qgis_outputMultilayer")
  )

  result <- qgis_run_algorithm(
    "native:splitvectorlayer",
    INPUT = system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
    FIELD = "WAYPOINT_I",
    FILE_TYPE = 0,
    OUTPUT = qgis_tmp_file(""),
    .quiet = TRUE
  )

  expect_true(dir.exists(result$OUTPUT))
  expect_identical(length(list.files(result$OUTPUT)), length(result$OUTPUT_LAYERS))
  expect_is(result$OUTPUT_LAYERS, "qgis_outputMultilayer")
})
