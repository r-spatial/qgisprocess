
test_that("stars argument coercers work", {
  skip_if_not_installed("stars")

  obj <- stars::st_as_stars(sf::st_bbox(c(xmin = 0, xmax = 1, ymin = 0, ymax = 1)))
  expect_error(
    as_qgis_argument(obj),
    "Can't convert 'stars' object"
  )

  tmp_file <- expect_match(
    suppressWarnings(as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer"))),
    "\\.tif$"
  )
  expect_is(tmp_file, "qgis_tempfile_arg")
  unlink(tmp_file)

  # also check stars_proxy
  obj <- stars::read_stars(
    system.file("longlake/longlake.tif", package = "qgisprocess"),
    proxy = TRUE
  )
  expect_identical(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer")),
    system.file("longlake/longlake.tif", package = "qgisprocess")
  )
})
