
test_that("qgis_output() works", {
  expect_identical(qgis_output(list(a = 1), "a"), 1)
  expect_identical(qgis_output(list(a = 1), "b", default = 2), 2)
  expect_error(qgis_output(list(a = 1), "b"), "Result has no output")
})
