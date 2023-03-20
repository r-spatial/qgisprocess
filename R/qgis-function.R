#' Create functions from QGIS algorithms
#'
#' As opposed to [qgis_run_algorithm()], [qgis_function()] creates a callable
#' function based on the argument metadata provided by [qgis_arguments()].
#' Unlike [qgis_run_algorithm()], [qgis_function()] sets the default value
#' of `.quiet` to `TRUE` to make the function more usable within other
#' R code. Similarly, [qgis_pipe()] wraps [qgis_run_algorithm()], passing
#' its first argument to the first input to `algorithm`.
#'
#' @inheritParams qgis_run_algorithm
#' @param .data Passed to the first input of `algorithm`.
#' @param ... Default values to set when using [qgis_function()].
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
#' if (has_qgis()) {
#'   qgis_pipe(
#'     system.file(
#'       "longlake/longlake_depth.gpkg",
#'       package = "qgisprocess"
#'     ),
#'     "native:buffer",
#'     DISTANCE = 10
#'   )
#' }
#'
qgis_function <- function(algorithm, ...) {
  assert_qgis()
  assert_qgis_algorithm(algorithm)

  args <- qgis_arguments(algorithm)
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

#' @rdname qgis_function
#' @export
qgis_pipe <- function(.data, algorithm, ..., .quiet = TRUE) {
  fun <- qgis_function(algorithm)
  fun(.data, ..., .quiet = .quiet)
}
