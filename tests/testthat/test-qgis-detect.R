test_that("qgis_detect_macos_paths() works", {
  if (is_macos()) {
    expect_type(qgis_detect_macos_paths(), "character")
  } else {
    expect_error(qgis_detect_macos_paths(), "non-MacOS")
  }
})

test_that("qgis_detect_windows_paths() works", {
  if (is_windows()) {
    expect_type(qgis_detect_windows_paths(), "character")
  } else {
    expect_error(qgis_detect_windows_paths(), "non-windows")
  }
})

test_that("qgis_detect_paths() works", {
  if (is_windows()) {
    expect_identical(qgis_detect_paths(), qgis_detect_windows_paths())
  } else if (is_macos()) {
    expect_identical(qgis_detect_paths(), qgis_detect_macos_paths())
  } else {
    expect_error(qgis_detect_paths(), "only")
  }
})

test_that("extract_version_from_paths() works", {
  expect_identical(extract_version_from_paths(character()), character())
  path <- "/QGIS 3.28.6/bin/qgis_process-qgis-ltr.bat"
  expect_identical(extract_version_from_paths(path), "3.28.6")
  paths <- c(
    "C:/OSGeo4W/bin/qgis_process-qgis-dev.bat",
    "/QGIS 3.30.0/bin/qgis_process-qgis-ltr.bat",
    "QGIS 3.30.0/bin/qgis_process-qgis-ltr.bat",
    "QGIS 3.30/bin/qgis_process-qgis-ltr.bat",
    "/QGIS 3.30.0",
    "QGIS 3.30.0aaa/bin/qgis_process-qgis-ltr.bat"
  )
  expect_identical(
    extract_version_from_paths(paths),
    c(NA, "3.30.0", "3.30.0", "3.30", NA, NA)
  )
})

test_that("sort_paths() works", {
  expect_identical(sort_paths(character()), character())
  paths <-
    c(
      "C:/OSGeo4W64/bin/qgis_process-qgis.bat",
      "C:/OSGeo4W/bin/qgis_process-qgis.bat",
      "C:/OSGeo4W64/bin/qgis_process-qgis-ltr.bat",
      "C:/OSGeo4W/bin/qgis_process-qgis-ltr.bat",
      "C:/OSGeo4W64/bin/qgis_process-qgis-dev.bat",
      "C:/OSGeo4W/bin/qgis_process-qgis-dev.bat",
      "C:/Program Files/QGIS 3.28.6/bin/qgis_process-qgis-ltr.bat",
      "C:/Program Files/QGIS 3.30.0/bin/qgis_process-qgis-ltr.bat",
      "C:/Program Files/QGIS 3.8/bin/qgis_process-qgis-ltr.bat",
      "C:/Program Files/QGIS 3.4.6/bin/qgis_process-qgis-ltr.bat",
      "C:/Program Files/QGIS 3.30.0aaa/bin/qgis_process-qgis-ltr.bat"
    )
  new_paths <- sort_paths(paths)
  expect_setequal(paths, new_paths)
  expect_identical(
    new_paths[1:4],
    c(
      "C:/Program Files/QGIS 3.30.0/bin/qgis_process-qgis-ltr.bat",
      "C:/Program Files/QGIS 3.28.6/bin/qgis_process-qgis-ltr.bat",
      "C:/Program Files/QGIS 3.8/bin/qgis_process-qgis-ltr.bat",
      "C:/Program Files/QGIS 3.4.6/bin/qgis_process-qgis-ltr.bat"
    )
  )
})
