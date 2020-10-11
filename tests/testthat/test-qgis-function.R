
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
  expect_is(qgis_buffer, "function")
  expect_identical(parent.env(environment(qgis_buffer)), baseenv())
  expect_true(rlang::is_call(body(qgis_buffer), "qgis_run_algorithm", ns = "qgisprocess"))

  buffer_args <- qgis_arguments("native:buffer")
  expect_identical(
    names(formals(qgis_buffer)),
    c(buffer_args$name, setdiff(names(formals(qgis_run_algorithm)), c("algorithm", "...")))
  )

  result <- expect_silent(
    qgis_buffer(
      system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
      DISTANCE = 100,
      DISSOLVE = TRUE,
      MITER_LIMIT = 2,
      OUTPUT = qgis_tmp_vector(),
      END_CAP_STYLE = 0,
      JOIN_STYLE = 0
    )
  )

  expect_is(result, "qgis_result")
})

test_that("qgis_pipe() works", {
  skip_if_not(has_qgis())

  result <- system.file("longlake/longlake_depth.gpkg", package = "qgisprocess") %>%
    qgis_pipe(
      "native:buffer",
      DISTANCE = 100,
      DISSOLVE = TRUE,
      MITER_LIMIT = 2,
      OUTPUT = qgis_tmp_vector(),
      END_CAP_STYLE = 0,
      JOIN_STYLE = 0
    )

  expect_is(result, "qgis_result")
})

