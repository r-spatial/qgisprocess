
#' Run algorithms using 'qgis_process'
#'
#' Run QGIS algorithms.
#' See the [QGIS docs](https://docs.qgis.org/testing/en/docs/user_manual/processing_algs/qgis/index.html)
#' for a detailed description of the algorithms provided
#' 'out of the box' on QGIS (versions >= 3.14).
#'
#' @param algorithm A qualified algorithm name (e.g., "native:filedownloader") or
#'   a path to a QGIS model file.
#' @param PROJECT_PATH,ELLIPSOID Global values for QGIS project file and
#'   ellipsoid name for distance calculations.
#' @param ... Named key-value pairs as arguments for each algorithm. Features of
#'   [rlang::list2()] are supported. These arguments
#'   are converted to strings using [as_qgis_argument()].
#' @param .quiet Use `TRUE` to suppress output from processing algorithms.
#'
#' @export
#'
#' @examples
#' if (has_qgis()) {
#'   qgis_run_algorithm(
#'     "native:buffer",
#'     INPUT = system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
#'     DISTANCE = 10
#'   )
#' }
#'
qgis_run_algorithm <- function(algorithm, ..., PROJECT_PATH = NULL, ELLIPSOID = NULL, .quiet = FALSE) {
  assert_qgis()
  assert_qgis_algorithm(algorithm)

  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_named(dots)) {
    abort("All ... arguments to `qgis_run_algorithm()` must be named.")
  }

  # sanitize arguments and make sure they are cleaned up on exit
  args <- qgis_sanitize_arguments(
    algorithm,
    !!! dots,
    PROJECT_PATH = PROJECT_PATH,
    ELLIPSOID = ELLIPSOID
  )
  on.exit(qgis_clean_arguments(args))

  # generate command-line args
  args_str <- qgis_serialize_arguments(args)

  if (.quiet) {
    result <- qgis_run(
      args = c("run", algorithm, args_str)
    )
  } else {
    result <- qgis_run(
      args = c("run", algorithm, args_str),
      echo_cmd = TRUE,
      stdout_callback = function(x, ...) cat(x),
      stderr_callback = function(x, ...) message(x, appendLF = FALSE)
    )
    cat("\n")
  }

  # return a custom object to keep as much information as possible
  # about the output
  structure(
    rlang::list2(
      !!! qgis_parse_results(algorithm, result$stdout),
      .algorithm = algorithm,
      .args = args,
      .processx_result = result
    ),
    class = "qgis_result"
  )
}
