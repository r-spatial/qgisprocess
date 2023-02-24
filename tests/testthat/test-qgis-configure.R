
test_that("qgis_version() works", {
  skip_if_not(has_qgis())

  expect_match(qgis_version(), "^\\d{1,2}\\.\\d+.*-.+")

  capture.output({
    expect_message(qgis_version(debug = TRUE), "PROJ version")
    expect_message(qgis_version(debug = TRUE), "EPSG ")
  })
})

test_that("qgis_query_version() works", {
  skip_if_not(has_qgis())

  expect_match(qgis_query_version(), "^\\d{1,2}\\.\\d+.*-.+")
})

test_that("qgis_query_version() works for development versions of QGIS", {
  skip_if_not(has_qgis())
  qversion <- qgis_query_version()
  skip_if_not(
    stringr::str_detect(
      qversion,
      "^\\d{1,2}\\.\\d*[13579][\\.-]"
    ),
    paste("QGIS version", qversion, "is not a development version.")
  )

  expect_match(
    qversion,
    "^\\d{1,2}\\.\\d+.*-\\p{L}+, development state ([0-9a-f]{7,}|unclear:.+)",
    perl = TRUE
  )

  if (stringr::str_detect(qversion, ".+development state unclear:.+")) {
    expect_warning(qgis_query_version(), "version identifier")
  }
})

test_that("qgis_algorithms() works", {
  skip_if_not(has_qgis())

  algs <- qgis_algorithms()
  expect_true(tibble::is_tibble(algs))
  expect_true(nrow(algs) > 1)

  old_names <- c(
    "provider", "provider_title", "algorithm",
    "algorithm_id", "algorithm_title"
  )
  expect_true(all(vapply(algs[old_names], function(x) all(!is.na(x)), logical(1))))
})

test_that("qgis_configure() returns FALSE with no QGIS", {
  skip_if(has_qgis())
  expect_false(
    withr::with_options(
      list(qgisprocess.path = "notacommandatall"),
      qgis_configure(quiet = TRUE)
    )
  )
  expect_false(
    withr::with_envvar(
      list(R_QGISPROCESS_PATH = "notacommandatall"),
      qgis_configure(quiet = TRUE)
    )
  )
})

test_that("qgis_configure() works", {
  skip_if_not(has_qgis())
  expect_true(qgis_configure(quiet = TRUE))
})
