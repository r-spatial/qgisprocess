test_that("qgis_output() works", {
  withr::local_options(warn = -1)
  expect_error(
    qgis_output(list(abcde = 1), "a"),
    "does not inherit from class qgis_result"
  )
  qres <- structure(list(a = 1, .args = "foo"), class = "qgis_result")
  expect_identical(qgis_output(qres, "a"), 1)
  expect_identical(qgis_output(qres, 1), 1)
  expect_error(qgis_output(qres, "b"), "Result has no output")
  expect_error(qgis_output(qres, ".args"), "Result has no output")
})
