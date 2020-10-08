
test_that("qgis_output() works", {
  expect_identical(qgis_output(list(a = 1), "a"), 1)
  expect_identical(qgis_output(list(a = 1), "b", default = 2), 2)
  expect_error(qgis_output(list(a = 1), "b"), "Result has no output")
})

test_that("qgis_result_*() functions work", {
  tmp_json <- qgis_tmp_file(".json")
  result <- qgis_run_algorithm(
    "native:filedownloader",
    URL = "https://httpbin.org/get",
    OUTPUT = tmp_json,
    .quiet = TRUE
  )
  expect_true(file.exists(tmp_json))
  qgis_result_clean(result)
  expect_false(file.exists(tmp_json))

  expect_is(result, "qgis_result")
  expect_true(is_qgis_result(result))
  expect_output(print(result), "^<Result")
  expect_named(qgis_result_args(result), c("URL", "OUTPUT"))
  expect_is(qgis_result_stderr(result), "character")
  expect_is(qgis_result_stdout(result), "character")
})
