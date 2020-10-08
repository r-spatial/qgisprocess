
test_that("qgis_detect_macos() works", {
  if (is_macos()) {
    expect_is(qgis_detect_macos(), "character")
  } else {
    expect_error(qgis_detect_macos(), "non-MacOS")
  }
})

test_that("qgis_detect_windows() works", {
  if (is_windows()) {
    expect_is(qgis_detect_windows(), "character")
  } else {
    expect_error(qgis_detect_windows(), "non-windows")
  }
})
