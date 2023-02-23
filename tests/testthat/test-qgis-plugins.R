test_that("qgis_plugins(which = \"all\", query = FALSE) returns sensible things", {
  skip_if_not(has_qgis())
  plugins <- qgis_plugins(which = "all", query = FALSE)
  expect_correct_plugins_format(plugins)
  expect_gte(sum(plugins$enabled), 1)
})

test_that("qgis_plugins(which = \"enabled\", query = FALSE) returns sensible things", {
  skip_if_not(has_qgis())
  plugins <- qgis_plugins(which = "enabled", query = FALSE)
  expect_correct_plugins_format(plugins)
  expect_gte(sum(plugins$enabled), 1)
})

test_that("qgis_plugins(which = \"disabled\", query = FALSE) returns sensible things", {
  skip_if_not(has_qgis())
  plugins <- qgis_plugins(which = "disabled", query = FALSE)
  expect_correct_plugins_format(plugins)
  expect_gte(sum(!plugins$enabled), 0)
})

test_that("qgis_plugins(query = FALSE, quiet = FALSE) messages are OK", {
  skip_if_not(has_qgis())
  expect_message(qgis_plugins(query = FALSE, quiet = FALSE), "Reading plugin list")
  expect_no_message(
    qgis_plugins(query = FALSE, quiet = FALSE, msg = FALSE),
    message = "Reading plugin list"
  )
  expect_message(
    qgis_plugins(query = FALSE, quiet = FALSE),
    "out of .+? plugins are enabled"
  )
})

test_that(glue("qgis_plugins(query = TRUE) works when using JSON output"), {
  skip_if_not(has_qgis())

  if (!qgis_use_json_output()) local_flipped_json_output()

  skip_if_not(qgis_use_json_output(), "Not using JSON output.")

  plugins <- qgis_plugins(query = TRUE)
  expect_correct_plugins_format(plugins)
  expect_gte(sum(plugins$enabled), 1)
})

test_that(glue("qgis_plugins(query = TRUE) works when NOT using JSON output"), {
  skip_if_not(has_qgis())

  if (qgis_use_json_output()) local_flipped_json_output()

  skip_if_not(!qgis_use_json_output(), "Using JSON output instead of 'not'.")

  plugins <- qgis_plugins(query = TRUE)
  expect_correct_plugins_format(plugins)
  expect_gte(sum(plugins$enabled), 1)
})


test_that("message_disabled_plugins() works", {
  plugins <- qgis_plugins()
  plugins$enabled <- FALSE
  expect_message(
    message_disabled_plugins(plugins),
    "Run `qgis_enable_plugins\\(\\)`.+access"
  )
})








