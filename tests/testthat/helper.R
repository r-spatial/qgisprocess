local_flipped_json_output <- function() {
  # FLIPPING qgis_using_json_output() STATE
  withr::local_options(qgisprocess.use_json_output = !qgis_using_json_output())
  qgis_using_json_output(query = TRUE) # updates cache environment
  # plan to restore this cache setting before exiting the test:
  withr::defer_parent(qgis_using_json_output(query = TRUE), priority = "last")
}

expect_correct_plugins_format <- function(plugins) {
  expect_s3_class(plugins, "data.frame")
  expect_named(plugins, c("name", "enabled"))
  expect_type(plugins$name, "character")
  expect_type(plugins$enabled, "logical")
}
