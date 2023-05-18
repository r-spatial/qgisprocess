#' Create a wrapper function that runs one algorithm
#'
#' As opposed to [qgis_run_algorithm()], [qgis_function()] creates a callable
#' function based on the argument metadata provided by [qgis_get_argument_specs()].
#'
#' The logic of `qgis_function()` has been implemented in R package
#' [qgis](https://github.com/JanCaha/r_package_qgis).
#' This package also provides the QGIS documentation of each processing
#' algorithm as corresponding R function documentation.
#'
#' @inheritParams qgis_show_help
#' @param ... Algorithm arguments.
#'   These values are evaluated once and immediately, so you shouldn't
#'   call [qgis_tmp_file()] here.
#'
#' @export
#'
#' @examples
#' if (has_qgis()) {
#'   qgis_buffer <- qgis_function("native:buffer")
#'   qgis_buffer(
#'     system.file(
#'       "longlake/longlake_depth.gpkg",
#'       package = "qgisprocess"
#'     ),
#'     DISTANCE = 10
#'   )
#' }
#'
qgis_function <- function(algorithm, ...) {
  assert_qgis()
  assert_qgis_algorithm(algorithm)

  args <- qgis_get_argument_specs(algorithm)
  arg_names <- c(args$name, "PROJECT_PATH", "ELLIPSOID", ".quiet")

  # The dots are the default values and are not exposed as
  # function arguments, and become the environment for the function
  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_named(dots)) {
    abort("All default arguments specified in `qgis_function(...)` must be named.")
  }

  invalid_dot_names <- setdiff(names(dots), arg_names)
  if (length(invalid_dot_names) > 0) {
    abort(
      paste0(
        "All default arguments specified in `qgis_function(...)` must be valid input names.\n",
        "Invalid names: ",
        glue::glue_collapse(paste0("'", invalid_dot_names, "'"), ", ", last = " and "),
        "\nValid names: ",
        glue::glue_collapse(paste0("'", arg_names, "'"), ", ", last = " and ")
      )
    )
  }

  # this makes it possible to have prettier/shorter formals() that show up
  # in GUI complete helpers
  dots$qgis_default_value <- qgis_default_value

  # these become the arguments to qgis_run_algorithm()
  qgis_algorithm_args <- lapply(rlang::set_names(arg_names), rlang::sym)

  # these become the formals to the returned function
  # default values are not exposed
  qgis_fun_args <- lapply(
    rlang::set_names(setdiff(arg_names, names(dots))),
    function(x) rlang::call2("qgis_default_value")
  )

  # the non-algorithm args (but args to qgis_run_algorithm()) are not
  # qgis_default_value()
  default_run_alg_args <- list(PROJECT_PATH = NULL, ELLIPSOID = NULL, .quiet = TRUE)
  qgis_fun_args[intersect(names(default_run_alg_args), names(qgis_fun_args))] <-
    default_run_alg_args[intersect(names(default_run_alg_args), names(qgis_fun_args))]

  # generate the call to qgis_run_algorithm()
  qgis_algorithm_call <- rlang::call2(
    "qgis_run_algorithm",
    algorithm,
    !!!qgis_algorithm_args,
    .ns = "qgisprocess"
  )

  # set up the function
  fun <- function() {}
  formals(fun) <- qgis_fun_args
  body(fun) <- qgis_algorithm_call

  # set the function environment
  dots_env <- as.environment(dots)
  parent.env(dots_env) <- baseenv()
  environment(fun) <- dots_env

  fun
}





#' Run an algorithm using 'qgis_process': pipe-friendly wrapper
#'
#' [qgis_run_algorithm_p()] wraps [qgis_run_algorithm()], passing
#' its first argument to the first argument of the QGIS `algorithm`.
#' This makes it more convenient in a pipeline (hence '_p' in the name).
#'
#' Uses [qgis_function()] under the hood.
#'
#' @family functions to run one geoprocessing algorithm
#'
#' @inheritParams qgis_show_help
#' @inheritParams qgis_run_algorithm
#' @param .data Passed to the first input of `algorithm`.
#' If `.data` is a `qgis_result` (the result of a previous processing
#' step), `.data[[.select]]` is passed instead.
#' @param .select String.
#' The name of the element to select from `.data` if the latter is a
#' `qgis_result`.
#' Defaults to `"OUTPUT"`.
#' @param .clean Logical.
#' Should an incoming `qgis_result` be cleaned (using [qgis_clean_result()])
#' after processing?
#' @param ... Other algorithm arguments.
#'   These values are evaluated once and immediately, so you shouldn't
#'   call [qgis_tmp_file()] here.
#'
#' @export
#'
#' @examples
#' if (has_qgis()) {
#'   system.file(
#'     "longlake/longlake_depth.gpkg",
#'     package = "qgisprocess"
#'   ) |>
#'     qgis_run_algorithm_p(
#'       "native:buffer",
#'       DISTANCE = 10
#'     )
#' }
#'
qgis_run_algorithm_p <- function(
    .data,
    algorithm,
    ...,
    .select = "OUTPUT",
    .clean = TRUE,
    .quiet = TRUE) {
  UseMethod("qgis_run_algorithm_p")
}

#' @keywords internal
#' @export
qgis_run_algorithm_p.qgis_result <- function(
    .data,
    algorithm,
    ...,
    .select = "OUTPUT",
    .clean = TRUE,
    .quiet = TRUE) {
  assert_that(is.string(.select))
  withr::with_options(
    list(warning.length = 6e3),
    assert_that(
      .select %in% names(.data),
      msg = glue(
        "The qgis_result object misses a '{.select}' element.\n",
        "The included JSON-output was:\n",
        "{jsonlite::prettify(.data$.processx_result$stdout)}"
      )
    )
  )
  output <- unclass(.data[[.select]])
  fun <- qgis_function(algorithm)
  result <- fun(output, ..., .quiet = .quiet)
  if (.clean) qgis_clean_result(.data)
  result
}

#' @keywords internal
#' @export
qgis_run_algorithm_p.default <- function(
    .data,
    algorithm,
    ...,
    .select = "OUTPUT",
    .clean = TRUE,
    .quiet = TRUE) {
  if (stringr::str_detect(class(.data), "^qgis_output")) {
    .data <- unclass(.data)
  }
  fun <- qgis_function(algorithm)
  fun(.data, ..., .quiet = .quiet)
}
