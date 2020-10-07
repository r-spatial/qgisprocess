
test_that("qgis_version() works", {
  skip_if_not(has_qgis())

  expect_match(qgis_version(), "^3\\.")
})

test_that("qgis_algorithms() works", {
  skip_if_not(has_qgis())

  algs <- qgis_algorithms()
  expect_true(tibble::is_tibble(algs))
  expect_true(nrow(algs) > 1)
  expect_true(all(vapply(algs, function(x) all(!is.na(x)), logical(1))))
})

test_that("qgis_configure() returns FALSE with no QGIS", {
  skip_if(has_qgis())
  expect_false(
    withr::with_options(
      list(qgisprocess.path = "notacommandatall"),
      qgis_configure(quiet = TRUE)
    )
  )
})

test_that("qgis_configure() works", {
  skip_if_not(has_qgis())
  expect_true(qgis_configure(quiet = TRUE))
})
