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

  if (!qgis_using_json_output()) local_flipped_json_output()

  skip_if_not(qgis_using_json_output(), "Not using JSON output.")

  plugins <- qgis_plugins(query = TRUE)
  expect_correct_plugins_format(plugins)
  expect_gte(sum(plugins$enabled), 1)
})

test_that(glue("qgis_plugins(query = TRUE) works when NOT using JSON output"), {
  skip_if_not(has_qgis())

  if (qgis_using_json_output()) local_flipped_json_output()

  skip_if_not(!qgis_using_json_output(), "Using JSON output instead of 'not'.")

  plugins <- qgis_plugins(query = TRUE)
  expect_correct_plugins_format(plugins)
  expect_gte(sum(plugins$enabled), 1)
})


test_that("message_disabled_plugins() works", {
  skip_if_not(has_qgis())
  plugins <- qgis_plugins()
  plugins$enabled <- FALSE
  expect_message(
    message_disabled_plugins(plugins),
    "Run `qgis_enable_plugins\\(\\)`.+access"
  )
})


test_that("qgis_enable_plugins() messages are OK", {
  skip_if_not(has_qgis())
  expect_message(qgis_enable_plugins(names = ""), "exiting")
  expect_message(
    qgis_enable_plugins(names = "processing"),
    "Ignoring.+processing"
  )
  expect_message(
    qgis_enable_plugins(names = "notaplugin"),
    "Ignoring unknown plugins: notaplugin"
  )
})


test_that("qgis_disable_plugins() messages are OK", {
  skip_if_not(has_qgis())
  expect_message(qgis_disable_plugins(names = ""), "exiting")
  expect_message(
    qgis_disable_plugins(names = "processing"),
    "Ignoring.+processing"
  )
  expect_message(
    qgis_disable_plugins(names = "notaplugin"),
    "Ignoring unknown plugins: notaplugin"
  )
})



test_that("qgis_enable_plugins() ignores an already enabled grassprovider plugin", {
  skip_if_not(has_qgis())
  skip_if_not(
    qgis_has_plugin("grassprovider"),
    "The 'grassprovider' plugin is missing."
  )
  skip_if(
    !subset(qgis_plugins(), name == "grassprovider")$enabled,
    "The 'grassprovider' plugin is disabled."
  )
  expect_message(
    qgis_enable_plugins("grassprovider"),
    "Ignoring plugins that are enabled already: grassprovider"
  )
})


test_that("qgis_disable_plugins() ignores an already disabled grassprovider plugin", {
  skip_if_not(has_qgis())
  skip_if_not(
    qgis_has_plugin("grassprovider"),
    "The 'grassprovider' plugin is missing."
  )
  skip_if(
    subset(qgis_plugins(), name == "grassprovider")$enabled,
    "The 'grassprovider' plugin is enabled."
  )
  expect_message(
    qgis_disable_plugins("grassprovider"),
    "Ignoring plugins that are disabled already: grassprovider"
  )
})





test_that("qgis_*able_plugins() works for a disabled grassprovider plugin", {
  skip_if_not(has_qgis())
  skip_if_not(
    qgis_has_plugin("grassprovider"),
    "The 'grassprovider' plugin is missing."
  )
  skip_if(
    subset(qgis_plugins(), name == "grassprovider")$enabled,
    "The 'grassprovider' plugin is enabled."
  )
  expect_message(
    qgis_enable_plugins("grassprovider"),
    "grassprovider successfully enabled!"
  )
  expect_true(subset(qgis_plugins(), name == "grassprovider")$enabled)
  expect_message(
    qgis_disable_plugins("grassprovider"),
    "grassprovider successfully disabled!"
  )
  expect_false(subset(qgis_plugins(), name == "grassprovider")$enabled)
})



test_that("qgis_*able_plugins() works for an enabled grassprovider plugin", {
  skip_if_not(has_qgis())
  skip_if_not(
    qgis_has_plugin("grassprovider"),
    "The 'grassprovider' plugin is missing."
  )
  skip_if(
    !subset(qgis_plugins(), name == "grassprovider")$enabled,
    "The 'grassprovider' plugin is disabled."
  )
  expect_message(
    qgis_disable_plugins("grassprovider"),
    "grassprovider successfully disabled!"
  )
  expect_false(subset(qgis_plugins(), name == "grassprovider")$enabled)
  expect_message(
    qgis_enable_plugins("grassprovider"),
    "grassprovider successfully enabled!"
  )
  expect_true(subset(qgis_plugins(), name == "grassprovider")$enabled)
})



test_that("Internal function arg_skip_loading_plugins() works", {
  expect_null(arg_skip_loading_plugins("grass:algorithm"))

  local_mocked_bindings(
    qgis_version = function(...) "3.34.0" # --skip-loading-plugins not supported
  )

  expect_null(arg_skip_loading_plugins("grass:algorithm"))
  expect_null(arg_skip_loading_plugins("native:algorithm"))
  expect_null(arg_skip_loading_plugins())

  local_mocked_bindings(
    qgis_version = function(...) "3.36.0" # --skip-loading-plugins supported
  )

  expect_null(arg_skip_loading_plugins("grass:algorithm"))
  expect_identical(
    arg_skip_loading_plugins("native:algorithm"),
    "--skip-loading-plugins"
  )
  expect_identical(arg_skip_loading_plugins(), "--skip-loading-plugins")
})
