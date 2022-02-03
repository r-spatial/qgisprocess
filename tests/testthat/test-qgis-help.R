
test_that("qgis_help_text()/show works", {
  skip_if_not(has_qgis())
  expect_match(qgis_help_text("native:filedownloader"), "native:filedownloader")
  expect_output(qgis_show_help("native:filedownloader"), "native:filedownloader")
})

test_that("qgis_description() works for algorithms", {
  skip_if_not(has_qgis())

  # buffer is one case where there is an extra bit of description
  # that makes parsing non-standard
  expect_true("SEGMENTS" %in% qgis_arguments("native:buffer")$name)

  for (algorithm in head(qgis_algorithms()$algorithm, 3)) {
    expect_length(qgis_description(!! algorithm), 1)
    expect_type(qgis_description(!! algorithm), "character")
  }

  for (algorithm in head(qgis_algorithms()$algorithm, 3)) {
    expect_is(qgis_arguments(!! algorithm), "data.frame")
    expect_false(any(is.na(qgis_arguments(!! algorithm)$name)))
    expect_false(any(is.na(qgis_arguments(!! algorithm)$qgis_type)))
    expect_false(any(is.na(qgis_arguments(!! algorithm)$description)))
  }

  for (algorithm in head(qgis_algorithms()$algorithm, 3)) {
    expect_is(qgis_outputs(!! algorithm), "data.frame")
    expect_false(any(is.na(qgis_outputs(!! algorithm)$name)))
    expect_false(any(is.na(qgis_outputs(!! algorithm)$qgis_output_type)))
  }
})

test_that("qgis_arguments() and qgis_outputs() work for selected algorithms", {
  skip_if_not(has_qgis())

  selected_algorithms <- c("native:buffer", "qgis:executesql")

  for (algorithm in selected_algorithms) {
    if (interactive()) message(algorithm)
    expect_is(qgis_arguments(!! algorithm), "data.frame")
    expect_false(any(is.na(qgis_arguments(!! algorithm)$name)))
    expect_false(any(is.na(qgis_arguments(!! algorithm)$qgis_type)))
    expect_false(any(is.na(qgis_arguments(!! algorithm)$description)))
  }

  for (algorithm in selected_algorithms) {
    expect_is(qgis_outputs(!! algorithm), "data.frame")
    expect_false(any(is.na(qgis_outputs(!! algorithm)$name)))
    expect_false(any(is.na(qgis_outputs(!! algorithm)$qgis_output_type)))
  }

})

test_that("qgis_arguments() and qgis_outputs() works for all algorithms", {
  skip("Test takes ~1 hr to run")
  for (algorithm in qgis_algorithms()$algorithm) {
    if (interactive()) message(algorithm)
    expect_is(qgis_arguments(!! algorithm), "data.frame")
    expect_false(any(is.na(qgis_arguments(!! algorithm)$name)))
    expect_false(any(is.na(qgis_arguments(!! algorithm)$qgis_type)))
    expect_false(any(is.na(qgis_arguments(!! algorithm)$description)))
  }

  for (algorithm in qgis_algorithms()$algorithm) {
    expect_is(qgis_outputs(!! algorithm), "data.frame")
    expect_false(any(is.na(qgis_outputs(!! algorithm)$name)))
    expect_false(any(is.na(qgis_outputs(!! algorithm)$qgis_output_type)))
  }
})

test_that("algorithms with no outputs have zero-row qgis_outputs()", {
  skip_if_not(has_qgis())
  expect_identical(nrow(qgis_outputs("native:spatialiteexecutesql")), 0L)
})
