
test_that("sf argument coercers work", {
  skip_if_not_installed("sf")
  sf_obj <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  expect_error(as_qgis_argument(sf_obj, "integer"), "Can't use 'sf' objects")

  tmp_file <- expect_match(as_qgis_argument(sf_obj, "source"), "\\.gpkg$")
  expect_is(tmp_file, "qgis_tempfile_arg")
  unlink(tmp_file)
})
