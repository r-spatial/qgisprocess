test_that("qgis_help_text()/show works", {
  skip_if_not(has_qgis())
  expect_match(qgis_help_text("native:filedownloader"), "native:filedownloader")
  expect_output(qgis_show_help("native:filedownloader"), "native:filedownloader")
})

test_that("qgis_get_description() works for algorithms", {
  skip_if_not(has_qgis())

  # buffer is one case where there is an extra bit of description
  # that makes parsing non-standard
  expect_true("SEGMENTS" %in% qgis_get_argument_specs("native:buffer")$name)

  for (algorithm in head(qgis_algorithms()$algorithm, 3)) {
    expect_length(qgis_get_description(!!algorithm), 1)
    expect_type(qgis_get_description(!!algorithm), "character")
  }

  for (algorithm in head(qgis_algorithms()$algorithm, 3)) {
    expect_s3_class(qgis_get_argument_specs(!!algorithm), "data.frame")
    expect_false(any(is.na(qgis_get_argument_specs(!!algorithm)$name)))
    expect_false(any(is.na(qgis_get_argument_specs(!!algorithm)$qgis_type)))
    expect_false(any(is.na(qgis_get_argument_specs(!!algorithm)$description)))
  }

  for (algorithm in head(qgis_algorithms()$algorithm, 3)) {
    expect_s3_class(qgis_get_output_specs(!!algorithm), "data.frame")
    expect_false(any(is.na(qgis_get_output_specs(!!algorithm)$name)))
    expect_false(any(is.na(qgis_get_output_specs(!!algorithm)$qgis_output_type)))
  }
})

test_that("qgis_get_argument_specs() and qgis_get_output_specs() work for selected algorithms", {
  skip_if_not(has_qgis())

  selected_algorithms <- c("native:buffer", "qgis:executesql")

  for (algorithm in selected_algorithms) {
    expect_s3_class(qgis_get_argument_specs(!!algorithm), "data.frame")
    expect_false(any(is.na(qgis_get_argument_specs(!!algorithm)$name)))
    expect_false(any(is.na(qgis_get_argument_specs(!!algorithm)$qgis_type)))
    expect_false(any(is.na(qgis_get_argument_specs(!!algorithm)$description)))
  }

  for (algorithm in selected_algorithms) {
    expect_s3_class(qgis_get_output_specs(!!algorithm), "data.frame")
    expect_false(any(is.na(qgis_get_output_specs(!!algorithm)$name)))
    expect_false(any(is.na(qgis_get_output_specs(!!algorithm)$qgis_output_type)))
  }
})

test_that("qgis_get_argument_specs() and qgis_get_output_specs() works for all algorithms", {
  skip("Test takes ~1 hr to run")
  for (algorithm in qgis_algorithms()$algorithm) {
    if (interactive()) message(algorithm)
    expect_s3_class(qgis_get_argument_specs(!!algorithm), "data.frame")
    expect_false(any(is.na(qgis_get_argument_specs(!!algorithm)$name)))
    expect_false(any(is.na(qgis_get_argument_specs(!!algorithm)$qgis_type)))
    expect_false(any(is.na(qgis_get_argument_specs(!!algorithm)$description)))
  }

  for (algorithm in qgis_algorithms()$algorithm) {
    expect_s3_class(qgis_get_output_specs(!!algorithm), "data.frame")
    expect_false(any(is.na(qgis_get_output_specs(!!algorithm)$name)))
    expect_false(any(is.na(qgis_get_output_specs(!!algorithm)$qgis_output_type)))
  }
})

test_that("algorithms with no outputs have zero-row qgis_get_output_specs()", {
  skip_if_not(has_qgis())
  expect_identical(nrow(qgis_get_output_specs("native:spatialiteexecutesql")), 0L)
})

test_that("Help functions & qgis_function() yield a warning with deprecated algorithms only", {
  skip_if_not(has_qgis())
  skip_if_not(qgis_using_json_output())
  algs <- qgis_algorithms()
  skip_if_not(
    "deprecated" %in% colnames(algs) && sum(algs$deprecated) > 0,
    paste(
      "There are no deprecated algorithms available.",
      "Rewrite this test to simulate deprecated algorithms."
    )
  )
  local_edition(3)
  # if more than one warning pops up, it should be apparent from
  # testthat output (only the first warning is swallowed in the
  # third edition of testthat)

  alg_deprecated <- sample(algs$algorithm[algs$deprecated], 1)
  alg_non_deprecated <- sample(algs$algorithm[!algs$deprecated], 1)
  expect_warning(capture.output(qgis_show_help(alg_deprecated)))
  expect_no_warning(capture.output(qgis_show_help(alg_non_deprecated)))
  expect_warning(qgis_get_description(alg_deprecated))
  expect_no_warning(qgis_get_description(alg_non_deprecated))
  expect_warning(qgis_get_argument_specs(alg_deprecated))
  expect_no_warning(qgis_get_argument_specs(alg_non_deprecated))
  expect_warning(qgis_get_output_specs(alg_deprecated))
  expect_no_warning(qgis_get_output_specs(alg_non_deprecated))
  expect_warning(qgis_function(alg_deprecated))
  expect_no_warning(qgis_function(alg_non_deprecated))
})
