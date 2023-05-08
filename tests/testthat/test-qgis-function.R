test_that("qgis_function() works", {
  skip_if_not(has_qgis())

  expect_error(
    qgis_function("native:buffer", NOT_AN_ARG = "value"),
    "must be valid input names"
  )
  expect_error(
    qgis_function("native:buffer", "no name"),
    "must be named"
  )

  qgis_buffer <- qgis_function("native:buffer")
  expect_type(qgis_buffer, "closure")
  expect_identical(parent.env(environment(qgis_buffer)), baseenv())
  expect_true(rlang::is_call(body(qgis_buffer), "qgis_run_algorithm", ns = "qgisprocess"))

  buffer_args <- qgis_get_argument_specs("native:buffer")
  expect_identical(
    names(formals(qgis_buffer)),
    c(
      buffer_args$name,
      setdiff(names(formals(qgis_run_algorithm)), c("algorithm", "...", ".raw_json_input"))
    )
  )

  result <- qgis_buffer(
    system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
    DISTANCE = 100,
    DISSOLVE = TRUE,
    MITER_LIMIT = 2,
    OUTPUT = qgis_tmp_vector(),
    END_CAP_STYLE = 0,
    JOIN_STYLE = 0
  )

  expect_s3_class(result, "qgis_result")
})

test_that("qgis_pipe() works", {
  skip_if_not(has_qgis())

  result <- system.file("longlake/longlake_depth.gpkg", package = "qgisprocess") |>
    qgis_pipe(
      "native:buffer",
      DISTANCE = 100,
      DISSOLVE = TRUE,
      MITER_LIMIT = 2,
      OUTPUT = qgis_tmp_vector(),
      END_CAP_STYLE = 0,
      JOIN_STYLE = 0
    )

  expect_s3_class(result, "qgis_result")
  expect_true(file.exists(result$.args$OUTPUT))

  result2 <- result |>
    qgis_pipe("native:subdivide", MAX_NODES = 10, .clean = FALSE) |>
    qgis_pipe("native:dissolve", .clean = FALSE)

  expect_true(file.exists(result$.args$OUTPUT))

  expect_s3_class(result2, "qgis_result")
  expect_named(result2, c("OUTPUT", ".algorithm", ".args", ".raw_json_input", ".processx_result"))
  expect_equal(sf::st_area(sf::st_as_sf(result)), sf::st_area(sf::st_as_sf(result2)))

  result3 <- result |>
    qgis_extract_output_by_name("OUTPUT") |>
    qgis_pipe("native:subdivide", MAX_NODES = 10)
  expect_s3_class(result3, "qgis_result")
  expect_named(result3, c("OUTPUT", ".algorithm", ".args", ".raw_json_input", ".processx_result"))

  expect_error(
    result |>
      qgis_pipe(
        "native:subdivide",
        MAX_NODES = 10,
        .clean = FALSE,
        .select = "dummy"
      ),
    "The qgis_result object misses"
  )

  result4 <- result |>
    qgis_pipe("native:subdivide", MAX_NODES = 10)
  expect_false(file.exists(result$.args$OUTPUT))

  fake_result <- structure(result[".processx_result"], class = "qgis_result")
  expect_error(
    fake_result |> qgis_pipe("native:subdivide", MAX_NODES = 10),
    "The qgis_result object misses"
  )
})
