
test_that("tempfile generators work", {
  expect_true(dir.exists(qgis_tmp_base()))
  expect_match(qgis_tmp_file(ext = "extension"), "extension$")
  expect_match(qgis_tmp_vector(ext = "extension"), "extension$")
  expect_match(qgis_tmp_raster(ext = "extension"), "extension$")

  expect_match(
    withr::with_options(
      list(qgisprocess.tmp_vector_ext = "vectorext"),
      qgis_tmp_vector()
    ),
    "vectorext$"
  )

  expect_match(
    withr::with_options(
      list(qgisprocess.tmp_raster_ext = "rasterext"),
      qgis_tmp_raster()
    ),
    "rasterext$"
  )
})

test_that("tempfiles can be cleaned up", {
  expect_true(dir.exists(qgis_tmp_base()))
  file <- qgis_tmp_file()
  file.create(file)
  expect_true(file.exists(file))
  qgis_tmp_clean()
  expect_false(file.exists(file))
  expect_true(dir.exists(qgis_tmp_base()))
})
