test_that("qgis_version() works", {
  skip_if_not(has_qgis())

  expect_match(qgis_version(), "^\\d{1,2}\\.\\d+.*-.+")
  expect_match(qgis_version(full = FALSE), "^\\d{1,2}\\.\\d+\\.\\d+$")
})

test_that("qgis_version(debug = TRUE) works", {
  skip_if_not(has_qgis())
  skip_if(
    package_version(qgis_version(full = FALSE)) < "3.22.0",
    "QGIS version is older than 3.22.0"
  )

  capture.output({
    expect_message(
      qgis_version(debug = TRUE),
      glue("Using.+{getNamespaceVersion('qgisprocess')}")
    )
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
