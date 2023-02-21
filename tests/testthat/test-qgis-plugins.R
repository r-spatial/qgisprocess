test_that("qgis_plugins(which = \"all\", query = FALSE) returns sensible things", {
  skip_if_not(has_qgis())
  plugins <- qgis_plugins(which = "all", query = FALSE)
  expect_s3_class(plugins, "data.frame")
  expect_named(plugins, c("name", "enabled"))
  expect_type(plugins$name, "character")
  expect_type(plugins$enabled, "logical")
  expect_gte(sum(plugins$enabled), 1)
})

test_that("qgis_plugins(which = \"enabled\", query = FALSE) returns sensible things", {
  skip_if_not(has_qgis())
  plugins <- qgis_plugins(which = "enabled", query = FALSE)
  expect_s3_class(plugins, "data.frame")
  expect_named(plugins, c("name", "enabled"))
  expect_type(plugins$name, "character")
  expect_type(plugins$enabled, "logical")
  expect_gte(sum(plugins$enabled), 1)
})

test_that("qgis_plugins(which = \"disabled\", query = FALSE) returns sensible things", {
  skip_if_not(has_qgis())
  plugins <- qgis_plugins(which = "disabled", query = FALSE)
  expect_s3_class(plugins, "data.frame")
  expect_named(plugins, c("name", "enabled"))
  expect_type(plugins$name, "character")
  expect_type(plugins$enabled, "logical")
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

output <-
  if (has_qgis()) {
    if (qgis_use_json_output()) " (using JSON output)" else " (NOT using JSON output)"
  } else ""

test_that(glue("qgis_plugins(query = TRUE) works{output}"), {
  skip_if_not(has_qgis())
  plugins <- qgis_plugins(query = TRUE)
  expect_s3_class(plugins, "data.frame")
  expect_named(plugins, c("name", "enabled"))
  expect_type(plugins$name, "character")
  expect_type(plugins$enabled, "logical")
  expect_gte(sum(plugins$enabled), 1)
})

output2 <-
  if (has_qgis()) {
    if (!qgis_use_json_output()) " (using JSON output)" else " (NOT using JSON output)"
  } else ""

test_that(glue("qgis_plugins(query = TRUE) works{output2}"), {
  skip_if_not(has_qgis())

  # FLIPPING qgis_use_json_output() STATE
  withr::local_envvar(c(JSON_OUTPUT = qgis_use_json_output()))
  withr::local_options(qgisprocess.use_json_output = !qgis_use_json_output())
  qgis_use_json_output(query = TRUE) # updates cache environment
  # plan to restore this cache setting before exiting the test:
  withr::defer_parent(qgis_use_json_output(query = TRUE), priority = "last")

  # CHECK FLIPPED STATE
  expect_identical(
    as.logical(Sys.getenv("JSON_OUTPUT")),
    !qgis_use_json_output()
  )

  # EXPECTATIONS UNDER FLIPPED STATE
  plugins <- qgis_plugins(query = TRUE)
  expect_s3_class(plugins, "data.frame")
  expect_named(plugins, c("name", "enabled"))
  expect_type(plugins$name, "character")
  expect_type(plugins$enabled, "logical")
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








