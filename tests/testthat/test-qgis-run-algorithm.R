
test_that("qgis_run_algorithm() works", {
  skip_if_not(has_qgis())
  skip_if_offline()

  tmp_json <- qgis_tmp_file(".json")
  result <- expect_output(
    qgis_run_algorithm("native:filedownloader", URL = "https://httpbin.org/get", OUTPUT = tmp_json, .quiet = FALSE),
    "Running\\s+"
  )
  expect_true(file.exists(tmp_json))

  unlink(tmp_json)

  # arguments to native:filedownloader changed in recent nightly
  skip_if_not(identical(qgis_arguments("native:filedownloader")$name, c("URL", "OUTPUT")))

  result <- expect_silent(
    qgis_run_algorithm("native:filedownloader", URL = "https://httpbin.org/get", OUTPUT = tmp_json, .quiet = TRUE)
  )
  expect_true(file.exists(tmp_json))
  unlink(tmp_json)
})

test_that("qgis_run_algorithm() ignores unknown inputs", {
  skip_if_not(has_qgis())

  expect_message(
    qgis_run_algorithm(
      "native:buffer",
      NOT_AN_INPUT = "some value",
      INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
      DISTANCE = 100,
      .quiet = TRUE
    ),
    "Ignoring unknown input"
  )
})

test_that("qgis_run_algorithm accepts multiple input arguments", {
  skip_if_not(has_qgis())
  skip_if_not_installed("sf")

  v_1 <- sf::read_sf(system.file("longlake/longlake.gpkg", package = "qgisprocess"))
  v_2 <- v_3 <- v_1
  v_2$geom = v_2$geom + 1000
  sf::st_crs(v_2) <- sf::st_crs(v_1)
  v_3$geom <- v_3$geom - 1000
  sf::st_crs(v_3) <- sf::st_crs(v_1)
  out <- qgis_run_algorithm(
    "native:mergevectorlayers",
    LAYERS = v_1, LAYERS = v_2, LAYERS = v_3,
    .quiet = TRUE
  )
  tmp <- sf::read_sf(qgis_output(out, "OUTPUT"))
  expect_equal(nrow(tmp), 3)
})

test_that("qgis_run_algorithm runs with qgis:relief, for which the acceptable value of COLORS is NULL", {

  relief_args <- qgis_arguments("qgis:relief")
  expect_identical(relief_args["COLORS", ]$acceptable_values, list(NULL))

  result <- qgis_run_algorithm(
    "qgis:relief",
    INPUT=system.file("longlake/longlake_depth.tif", package = "qgisprocess"),
    Z_FACTOR=1,
    AUTO_COLORS=FALSE,
    COLORS="-0.5, 0, 170, 0, 0; 0, 0.5, 85, 255, 255; 0.5, 1, 0, 255, 0; 1, 2.5, 85, 85, 255"
    )

  expect_s3_class(result$OUTPUT, "qgis_outputRaster")
  expect_s3_class(result$FREQUENCY_DISTRIBUTION, "qgis_outputFile")
  expect_true(file.exists(result$OUTPUT))
  expect_true(file.exists(result$FREQUENCY_DISTRIBUTION))

})
