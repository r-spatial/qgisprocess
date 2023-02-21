
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
#' @param .raw_json_input The raw JSON to use as input in place of `...`.
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
qgis_run_algorithm <- function(algorithm, ..., PROJECT_PATH = NULL, ELLIPSOID = NULL,
                               .raw_json_input = NULL, .quiet = FALSE) {
  assert_qgis()
  assert_qgis_algorithm(algorithm)

  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_named(dots)) {
    abort("All ... arguments to `qgis_run_algorithm()` must be named.")
  }

  use_json_input <- !is.null(.raw_json_input) || qgis_use_json_input()
  use_json_output <- use_json_input || qgis_use_json_output()

  if (is.null(.raw_json_input)) {
    # sanitize arguments and make sure they are cleaned up on exit
    args <- qgis_sanitize_arguments(
      algorithm,
      !!!dots,
      PROJECT_PATH = PROJECT_PATH,
      ELLIPSOID = ELLIPSOID,
      .use_json_input = use_json_input
    )
    on.exit(qgis_clean_arguments(args))

    # generate command-line args or JSON input
    args_str <- qgis_serialize_arguments(args, use_json_input = use_json_input)
  } else {
    args_str <- .raw_json_input
    args <- list()
    if (length(list(...)) > 0) {
      abort("Can't use `.raw_json_input` with arguments in `qgis_run_algorithm()`")
    }
  }

  if (use_json_input) {
    stdin_file <- tempfile()
    on.exit(unlink(stdin_file), add = TRUE)
    writeLines(args_str, stdin_file, useBytes = TRUE)

    if (!.quiet) {
      cat("JSON input ----\n")
      cat(jsonlite::prettify(args_str, indent = 2))
      cat("\n")
    }
  }

  result <- qgis_run(
    args = c(
      if (use_json_output) "--json",
      "run",
      algorithm,
      if (use_json_input) "-" else args_str
    ),
    echo_cmd = !.quiet,
    stdout_callback = if (!.quiet && !use_json_output) function(x, ...) cat(x),
    stderr_callback = if (!.quiet) function(x, ...) message(x, appendLF = FALSE),
    stdin = if (use_json_input) stdin_file,
    encoding = if (use_json_output) "UTF-8" else ""
  )

  if (!.quiet) cat("\n")

  # return a custom object to keep as much information as possible
  # about the output
  result <- structure(
    rlang::list2(
      !!!qgis_parse_results(algorithm, result$stdout),
      .algorithm = algorithm,
      .args = args,
      .raw_json_input = if (use_json_input) args_str,
      .processx_result = result
    ),
    class = "qgis_result"
  )

  qgis_check_stdout(result)
  result
}
