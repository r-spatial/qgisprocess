
test_that("qgis_sanitize_arguments() ignores unknown inputs", {
  expect_message(
    expect_identical(
      qgis_sanitize_arguments(
        "some_algorithm",
        unknown_arg = 1,
        .algorithm_arguments = tibble::tibble(name = character())
      ),
      list()
    ),
    "Ignoring unknown input"
  )
})

test_that("qgis_sanitize_arguments() doesn't drop special arguments", {
  special <- list(PROJECT_PATH = "some_path", ELLIPSOID = "some_ellipse")
  for (i in 1:3) {
    if (i == 3) i <- 1:2
    expect_identical(
      qgis_sanitize_arguments(
        "some_algorithm",
        !!!special[i],
        .algorithm_arguments = tibble::tibble(name = character())
      ),
      !!special[i]
    )
  }
})

test_that("qgis_sanitize_arguments() accepts multiple input arguments", {
  sanitized <- qgis_sanitize_arguments(
    "some_algorithm",
    multi_arg = 1,
    multi_arg = 2,
    .algorithm_arguments = tibble::tibble(name = "multi_arg", qgis_type = NA_character_)
  )

  expect_identical(sanitized$multi_arg, qgis_list_input("1", "2"))
  expect_identical(qgis_serialize_arguments(sanitized), c("--multi_arg=1", "--multi_arg=2"))

  expect_identical(
    qgis_sanitize_arguments(
      "some_algorithm",
      multi_arg = qgis_list_input("1", "2"),
      .algorithm_arguments = tibble::tibble(name = "multi_arg", qgis_type = NA_character_)
    ),
    sanitized
  )
})

test_that("qgis_serialize_arguments() outputs correct JSON strings", {
  arguments <- list(
    LAYOUT = "Layout 1",
    TEXT_FORMAT = 0,
    OUTPUT = "output.pdf",
    PROJECT_PATH = "test.qgs",
    AGGREGATES = list(
      list(
        aggregate = "first_value",
        delimiter = ",",
        input = '"admin"',
        length = 36,
        name = "admin",
        precision = 0,
        type = 10
      ),
      list(
        aggregate = "concatenate",
        delimiter = ",",
        input = '"name"',
        length = 3000,
        name = "name",
        precision = 0,
        type = 10
      )
    )
  )
  json <- qgis_serialize_arguments(arguments = arguments, use_json_input = TRUE)
  expect_identical(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    list(
      inputs = list(
        LAYOUT = "Layout 1",
        TEXT_FORMAT = 0L,
        OUTPUT = "output.pdf",
        AGGREGATES = list(
          list(
            aggregate = "first_value",
            delimiter = ",",
            input = "\"admin\"",
            length = 36L,
            name = "admin",
            precision = 0L,
            type = 10L
          ),
          list(
            aggregate = "concatenate",
            delimiter = ",",
            input = "\"name\"",
            length = 3000L,
            name = "name",
            precision = 0L,
            type = 10L
            )
          )
        ),
      project_path = "test.qgs"
      )
    )
})

test_that("argument coercers work", {
  expect_error(as_qgis_argument(list()), "Don't know how to convert object of type")
  expect_identical(as_qgis_argument("chr value"), "chr value")
  expect_identical(as_qgis_argument(1), "1")
  expect_identical(as_qgis_argument(1L), "1")
  expect_identical(as_qgis_argument(TRUE), "TRUE")
  expect_identical(as_qgis_argument(qgis_list_input(1, 2)), qgis_list_input("1", "2"))
  expect_identical(
    as_qgis_argument(qgis_dict_input(a = 1, b = 2)),
    qgis_dict_input(a = "1", b = "2")
  )
})

test_that("character -> enum works", {
  expect_error(
    as_qgis_argument(
      "bad value",
      spec = qgis_argument_spec(
        name = "argname",
        qgis_type = "enum",
        available_values = c("good val1", "good val2")
      )
    ),
    "All values.*?must be one of the following"
  )

  expect_identical(
    as_qgis_argument(
      "good val2",
      spec = qgis_argument_spec(
        name = "argname",
        qgis_type = "enum",
        available_values = c("good val1", "good val2")
      )
    ),
    "1"
  )

  expect_identical(
    as_qgis_argument(
      c("good val2", "good val1"),
      spec = qgis_argument_spec(
        name = "argname",
        qgis_type = "enum",
        available_values = c("good val1", "good val2")
      )
    ),
    "1,0"
  )
})

test_that("default arguments are handled correctly", {
  expect_silent(as_qgis_argument(NULL))
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "sink")
    ),
    "Using `argname = qgis_tmp_vector"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "vectorDestination")
    ),
    "Using `argname = qgis_tmp_vector"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "rasterDestination")
    ),
    "Using `argname = qgis_tmp_raster"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "fileDestination")
    ),
    "Using `argname = qgis_tmp_file"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "folderDestination")
    ),
    "Using `argname = qgis_tmp_folder"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "enum", available_values = character(0))
    ),
    "is unspecified"
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname", qgis_type = "enum", available_values = "def_value")
    ),
    'Using.*?argname.*?"def_value"',
  )
  expect_message(
    as_qgis_argument(
      qgis_default_value(),
      qgis_argument_spec(name = "argname")
    ),
    "Argument `argname` is unspecified"
  )
})

test_that("argument cleaners work", {
  expect_null(qgis_clean_argument("some valule"))

  tmp <- structure(tempfile(), class = "qgis_tempfile_arg")
  file.create(tmp)
  expect_true(file.exists(tmp))
  qgis_clean_argument(tmp)
  expect_false(file.exists(tmp))
})
