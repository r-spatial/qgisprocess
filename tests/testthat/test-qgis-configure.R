test_that("qgis_version() works", {
  skip_if_not(has_qgis())

  expect_match(qgis_version(), "^\\d{1,2}\\.\\d+.*-.+")
  expect_match(qgis_version(full = FALSE), "^\\d{1,2}\\.\\d+.\\d+$")
})

test_that("qgis_version(debug = TRUE) works", {
  skip_if_not(has_qgis())
  skip_if(
    package_version(strsplit(qgis_version(), "-")[[1]][1]) < "3.22.0",
    "QGIS version is older than 3.22.0"
  )

  capture.output({
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

test_that("qgis_configure() returns FALSE with no QGIS", {
  skip_if(has_qgis())
  expect_false(
    withr::with_options(
      list(qgisprocess.path = "notacommandatall"),
      qgis_configure(quiet = TRUE)
    )
  )
  expect_false(
    withr::with_envvar(
      list(R_QGISPROCESS_PATH = "notacommandatall"),
      qgis_configure(quiet = TRUE)
    )
  )
})

test_that("qgis_configure() works", {
  skip_if_not(has_qgis())
  expect_true(qgis_configure(quiet = TRUE))
})


test_that("qgis_configure() messages are OK", {
  skip_if_not(has_qgis())

  res_nonquiet_nocache <-
    evaluate_promise(qgis_configure(quiet = FALSE))
  res_nonquiet_cache <-
    evaluate_promise(qgis_configure(use_cached_data = TRUE))

  expect_true(res_nonquiet_nocache$result)
  expect_true(res_nonquiet_cache$result)

  expect_true(all(stringi::stri_detect_regex(
    paste(res_nonquiet_nocache$messages, collapse = "\n"),
    c(
      "Success!",
      "Now using 'qgis_process'",
      "QGIS version is now set to",
      "JSON for output serialization",
      "JSON for input serialization",
      "[0-9]+ out of [0-9]+ available processing provider plugins are enabled",
      "You now have access to [0-9]+ algorithms from [0-9]+ QGIS processing providers",
      "Saving configuration to",
      "to inspect the cache environment"
    )
  )))

  expect_true(all(stringi::stri_detect_regex(
    paste(res_nonquiet_cache$messages, collapse = "\n"),
    c(
      "Checking configuration in cache file",
      "Checking cached QGIS version",
      "Checking cached QGIS plugins",
      "Restoring configuration from",
      "QGIS version:",
      "JSON for output serialization",
      "JSON for input serialization",
      "[0-9]+ out of [0-9]+ available processing provider plugins are enabled",
      "Having access to [0-9]+ algorithms from [0-9]+ QGIS processing providers",
      "to inspect the cache environment"
    )
  )))

  expect_message(
    qgis_configure(use_cached_data = TRUE, quiet = TRUE),
    "Success!"
  )
})

test_that("qgis_configure() works OK if cache conditions unmet", {
  skip_if_not(has_qgis())
  version <- as.character(utils::packageVersion("qgisprocess"))
  cache_data_file <- file.path(
    rappdirs::user_cache_dir("R-qgisprocess"),
    glue("cache-{version}.rds")
  )

  withr::defer(
    saveRDS(
      list(
        path = qgis_path(),
        version = qgis_version(),
        algorithms = qgis_algorithms(),
        plugins = qgis_plugins(),
        use_json_output = qgis_using_json_output()
      ),
      cache_data_file
    )
  )

  unlink(cache_data_file)
  expect_message(qgis_configure(use_cached_data = TRUE), "No cache found")

  saveRDS(list(version = qgis_version()), cache_data_file)
  expect_message(
    qgis_configure(use_cached_data = TRUE),
    "The cache does not contain all required elements"
  )

  expect_message(
    withr::with_options(
      list(qgisprocess.path = "wrong/path/to/qgis_process"),
      qgis_configure(use_cached_data = TRUE)
    ),
    "The user's qgisprocess.path option or the R_QGISPROCESS_PATH environment variable specify a different qgis_process path"
  )

  saveRDS(
    list(
      path = qgis_path(),
      version = qgis_version(),
      algorithms = "0",
      plugins = qgis_plugins(),
      use_json_output = qgis_using_json_output()
    ),
    cache_data_file
  )
  expect_message(
    qgis_configure(use_cached_data = TRUE),
    "The cache does not contain all required data"
  )

  saveRDS(
    list(
      path = "wrong/path/to/qgis_process",
      version = qgis_version(),
      algorithms = qgis_algorithms(),
      plugins = qgis_plugins(),
      use_json_output = qgis_using_json_output()
    ),
    cache_data_file
  )
  msg <- paste(capture.output(
    qgis_configure(use_cached_data = TRUE),
    type = "message"
  ), collapse = "\n")
  expect_true(stringr::str_detect(msg, "is not available anymore"))

  saveRDS(
    list(
      path = qgis_path(),
      version = "3.9999.9-Brussels",
      algorithms = qgis_algorithms(),
      plugins = qgis_plugins(),
      use_json_output = qgis_using_json_output()
    ),
    cache_data_file
  )
  expect_message(
    qgis_configure(use_cached_data = TRUE),
    "QGIS version change detected"
  )


  saveRDS(
    list(
      path = qgis_path(),
      version = qgis_version(),
      algorithms = qgis_algorithms(),
      plugins = data.frame(name = "XXX", enabled = FALSE),
      use_json_output = qgis_using_json_output()
    ),
    cache_data_file
  )
  expect_message(
    capture.output(qgis_configure(use_cached_data = TRUE)),
    "Change detected in \\(enabled\\) QGIS processing provider plugins!"
  )
})




test_that("abort_query_version() works", {
  lines <- c("aa", "bb")
  expect_error(
    abort_query_version(lines),
    "Output did not contain expected version information and was:"
  )
})
