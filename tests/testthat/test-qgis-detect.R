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
