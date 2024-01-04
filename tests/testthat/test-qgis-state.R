test_that("qgis_version() works", {
  skip_if_not(has_qgis())

  expect_match(qgis_version(), "^\\d{1,2}\\.\\d+.*-.+")
  expect_match(qgis_version(full = FALSE), "^\\d{1,2}\\.\\d+\\.\\d+$")
})

test_that("qgis_version(debug = TRUE) works", {
  skip_if_not(has_qgis())
  skip_if(
    package_version(qgis_version(full = FALSE)) < "3.22.0",
    "QGIS version is older than 3.22.0"
  )

  capture.output({
    expect_message(
      qgis_version(debug = TRUE),
      glue("Using.+{getNamespaceVersion('qgisprocess')}")
    )
    expect_message(qgis_version(debug = TRUE), "PROJ version")
    expect_message(qgis_version(debug = TRUE), "EPSG ")
  })
})

test_that("qgis_query_version() works", {
  skip_if_not(has_qgis())

  expect_match(qgis_query_version(), "^\\d{1,2}\\.\\d+.*-.+")
})

test_that("qgis_query_version() works for development versions of QGIS", {
  skip_if_not(has_qgis())
  qversion <- qgis_query_version()
  skip_if_not(
    stringr::str_detect(
      qversion,
      "^\\d{1,2}\\.\\d*[13579][\\.-]"
    ),
    paste("QGIS version", qversion, "is not a development version.")
  )

  expect_match(
    qversion,
    "^\\d{1,2}\\.\\d+.*-\\p{L}+, development state ([0-9a-f]{7,}|unclear:.+)",
    perl = TRUE
  )

  if (stringr::str_detect(qversion, ".+development state unclear:.+")) {
    expect_warning(qgis_query_version(), "version identifier")
  }
})




test_that("qgis_using_json_*() is determined by the QGIS version by default", {
  skip_if_not(has_qgis())
  original <- qgisprocess_cache$use_json_output

  local_mocked_bindings(
    qgis_version = function(...) "3.34.0" # supports JSON input
  )

  expect_true(qgis_using_json_input())
  expect_true(qgis_using_json_output())
  expect_message(qgis_using_json_output(quiet = FALSE), "Using JSON for output")

  local_mocked_bindings(
    qgis_version = function(...) "3.22.0" # does not support JSON input
  )

  expect_false(qgis_using_json_input())
  expect_true(qgis_using_json_output())
  expect_true(qgis_using_json_output(query = TRUE))

  qgisprocess_cache$use_json_output <- original
})



test_that("qgis_using_json_*() can be driven by user settings", {
  original <- qgisprocess_cache$use_json_output

  # use_json_input TRUE is validated against the QGIS version. If accepted,
  # qgis_using_json_output() takes its value
  withr::local_options(list(
    qgisprocess.use_json_input = TRUE
  ))
  local_mocked_bindings(
    qgis_version = function(...) "3.34.0" # supports JSON input
  )
  expect_true(qgis_using_json_input())
  expect_true(qgis_using_json_output())

  local_mocked_bindings(
    qgis_version = function(...) "3.22.0" # does not support JSON input
  )
  expect_warning(qgis_using_json_input(), "doesn't support JSON input")
  suppressWarnings(expect_false(qgis_using_json_input()))
  expect_true(qgis_using_json_output())

  # use_json_output FALSE can be honored if use_json_input is NOT acceptably set as TRUE
  withr::local_options(list(
    qgisprocess.use_json_output = FALSE
  ))
  suppressWarnings(expect_false(qgis_using_json_input()))
  expect_false(qgis_using_json_output())
  expect_message(qgis_using_json_output(quiet = FALSE), "Not using JSON for output")

  # use_json_output FALSE cannot be honored if use_json_input is acceptably set as TRUE
  local_mocked_bindings(
    qgis_version = function(...) "3.34.0" # supports JSON input
  )
  expect_warning(qgis_using_json_output(), "Conflicting user settings")
  suppressWarnings(expect_true(qgis_using_json_output()))
  expect_true(qgis_using_json_input())

  # if use_json_input is unset, qgis_using_json_input() takes the value from json_output
  withr::local_options(list(
    qgisprocess.use_json_input = NULL
  ))
  expect_false(qgis_using_json_output())
  expect_false(qgis_using_json_input())

  withr::local_options(list(
    qgisprocess.use_json_output = TRUE
  ))
  expect_true(qgis_using_json_input())
  expect_true(qgis_using_json_output())

  # use_json_input FALSE only affects qgis_using_json_input()
  withr::local_options(list(
    qgisprocess.use_json_input = FALSE
  ))
  expect_false(qgis_using_json_input())
  expect_true(qgis_using_json_output())

  qgisprocess_cache$use_json_output <- original
})




test_that("Internal function debug_json() works", {
  skip_if_not(has_qgis())
  expect_no_error(debug_json())
  expect_s3_class(debug_json(), "glue")
})






test_that("Internal function resolve_flag_opt() works", {
  expect_true(resolve_flag_opt(TRUE))
  expect_true(resolve_flag_opt("TRUE"))
  expect_true(resolve_flag_opt("true"))
  expect_false(resolve_flag_opt(""))
  expect_false(resolve_flag_opt(FALSE))
  expect_false(resolve_flag_opt("FALSE"))
  expect_false(resolve_flag_opt("false"))
  expect_identical(resolve_flag_opt("", keep_NA = TRUE), NA)
  expect_identical(resolve_flag_opt(NA, keep_NA = TRUE), NA)
  expect_error(resolve_flag_opt(NULL, keep_NA = TRUE), "must be")
  expect_error(resolve_flag_opt("maybe"), "must be")
  expect_error(resolve_flag_opt(c(TRUE, TRUE)), "must be")

  expect_false(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
  expect_identical(
    resolve_flag_opt(
      option_name = "test_option",
      envvar_name = "TEST_VAR",
      keep_NA = TRUE
    ),
    NA
  )

  expect_error(resolve_flag_opt(option_name = "test_option"), "Both")
  expect_error(resolve_flag_opt(envvar_name = "TEST_VAR"), "Both")

  withr::local_options(test_option = TRUE)
  expect_true(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
  expect_error(resolve_flag_opt(option_name = "test_option"), "Both")
  withr::local_options(test_option = "TRUE")
  expect_true(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
  withr::local_options(test_option = FALSE)
  expect_false(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
  withr::local_options(test_option = "FALSE")
  expect_false(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
  withr::local_options(test_option = 3)
  expect_error(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
  withr::local_options(test_option = NULL)
  expect_false(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))

  withr::local_envvar(TEST_VAR = "TRUE")
  expect_true(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
  expect_error(resolve_flag_opt(envvar_name = "TEST_VAR"), "Both")
  withr::local_envvar(TEST_VAR = "true")
  expect_true(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
  withr::local_envvar(TEST_VAR = "FALSE")
  expect_false(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
  withr::local_envvar(TEST_VAR = "false")
  expect_false(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
  withr::local_envvar(TEST_VAR = "3")
  expect_error(resolve_flag_opt(option_name = "test_option", envvar_name = "TEST_VAR"))
})
