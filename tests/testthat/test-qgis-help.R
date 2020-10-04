
test_that("qgis_help_text()/show works", {
  skip_if_not(has_qgis())
  expect_match(qgis_help_text("native:filedownloader"), "native:filedownloader")
  expect_output(qgis_show_help("native:filedownloader"), "native:filedownloader")
})

test_that("qgis_description() works for all algorithms", {
  skip_if_not(has_qgis())

  for (algorithm in head(qgis_algorithms()$algorithm, 10)) {
    expect_length(qgis_description(!! algorithm), 1)
    expect_type(qgis_description(!! algorithm), "character")
  }

  for (algorithm in head(qgis_algorithms()$algorithm, 10)) {
    expect_is(qgis_arguments(!! algorithm), "data.frame")
  }

  for (algorithm in head(qgis_algorithms()$algorithm, 10)) {
    expect_is(qgis_outputs(!! algorithm), "data.frame")
  }
})
