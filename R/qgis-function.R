
#' Create functions from QGIS algorithms
#'
#' As opposed to [qgis_run_algorithm()], [qgis_function()] creates a callable
#' function based on the argument metadata provided by [qgis_arguments()].
#' Unlike [qgis_run_algorithm()], [qgis_function()] sets the default value
#' of `.quiet` to `TRUE` to make the function more usable within other
#' R code.
#'
#' @inheritParams qgis_run_algorithm
#'
#' @return A function wrapping `qgis_run_algorithm(algorithm, ...)`.
#' @export
#'
#' @examples
#' if (has_qgis()) {
#'   qgis_buffer <- qgis_function("native:buffer")
#'   qgis_buffer(
#'     system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
#'     DISTANCE = 10
#'   )
#' }
#'
qgis_function <- function(algorithm, .quiet = TRUE) {
  assert_qgis()
  assert_qgis_algorithm(algorithm)

  dots <- rlang::list2(.quiet = .quiet)
  args <- qgis_arguments(algorithm)
  arg_names <- c(args$name, "PROJECT_PATH", "ELIPSOID")
  qgis_algorithm_args <- lapply(rlang::set_names(arg_names), rlang::sym)
  qgis_fun_args <- lapply(
    rlang::set_names(arg_names),
    function(x) rlang::call2("qgis_default_value", .ns = "qgisprocess")
  )
  qgis_fun_args[c("PROJECT_PATH", "ELIPSOID")] <- list(NULL)

  qgis_algorithm_call <- rlang::call2(
    "qgis_run_algorithm",
    algorithm,
    !!! qgis_algorithm_args,
    !!! dots,
    .ns = "qgisprocess"
  )

  fun <- function() {}
  formals(fun) <- c(qgis_fun_args, dots)
  body(fun) <- qgis_algorithm_call
  environment(fun) <- baseenv()

  fun
}
