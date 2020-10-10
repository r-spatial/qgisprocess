
test_that("qgis_function() works", {
  skip_if_not(has_qgis())

  qgis_buffer <- qgis_function("native:buffer")
  expect_is(qgis_buffer, "function")
  expect_identical(environment(qgis_buffer), baseenv())
  expect_true(rlang::is_call(body(qgis_buffer), "qgis_run_algorithm", ns = "qgisprocess"))

  buffer_args <- qgis_arguments("native:buffer")
  expect_identical(
    names(formals(qgis_buffer)),
    c(buffer_args$name, setdiff(names(formals(qgis_run_algorithm)), c("algorithm", "...")))
  )

  result <- qgis_buffer(
    system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
    DISTANCE = 10
  )

  expect_is(result, "qgis_result")
})

