#' Run an algorithm using 'qgis_process'
#'
#' Runs an algorithm using 'qgis_process'.
#' See the [QGIS docs](https://docs.qgis.org/latest/en/docs/user_manual/processing_algs/)
#' for a detailed description of the algorithms provided
#' 'out of the box' on QGIS.
#'
#' `qgis_run_algorithm()` accepts various R objects as algorithm arguments.
#' An overview is given by `vignette("qgis_arguments")`.
#' Examples include an R matrix or data frame for the
#' argument type 'matrix', R colors for the argument type 'color',
#' sf or terra (SpatVector) objects for the argument type 'vector' and
#' raster/terra/stars objects for the argument type 'raster', but there are many
#' more.
#' `qgis_run_algorithm()` preprocesses the provided objects into the format that
#' QGIS expects for a given argument.
#'
#' Providing R objects that cannot be converted to the applicable argument type
#' will lead to an error.
#'
#' @section Running QGIS models and Python scripts:
#' QGIS models and Python scripts can be added to the Processing Toolbox in the
#' QGIS GUI, by pointing at their corresponding file.
#' This will put the model or script below the provider 'Models' or
#' 'Scripts', respectively.
#' Next, it is necessary to run [qgis_configure()] in R in order to make the
#' model or script available to qgisprocess (even reloading the package won't
#' detect it, since these providers have dynamic content, not tied to a
#' plugin or to a QGIS version).
#' You can check the outcome with [qgis_providers()] and
#' [qgis_search_algorithms()].
#' Now, just as with other algorithms, you can provide the `model:<name>` or
#' `script:<name>` identifier to the `algorithm` argument of
#' `qgis_run_algorithm()`.
#'
#' As the output argument name of a QGIS model can have an R-unfriendly
#' syntax, you may need to take the JSON parameter string from the QGIS
#' processing dialog and feed the JSON string to the `.raw_json_input` argument
#' of `qgis_run_algorithm()` instead of providing separate arguments.
#'
#' Although the 'qgis_process' backend also supports replacing the 'algorithm'
#' parameter by the file path of a model file or a Python script, it is not
#' planned to implement this in qgisprocess, as it would bypass argument
#' preprocessing in R (including checks).
#'
#' @family functions to run one geoprocessing algorithm
#'
#' @seealso `vignette("qgis_arguments")`
#'
#' @param algorithm A qualified algorithm name (e.g., `"native:buffer"`) or
#'   a path to a QGIS model file.
#' @param PROJECT_PATH,ELLIPSOID Global values for QGIS project file and
#'   ellipsoid name for distance calculations.
#' @param ... Named key-value pairs as arguments for the algorithm. Features of
#'   [rlang::list2()] are supported. These arguments
#'   are converted to strings using [as_qgis_argument()].
#' @param .quiet Use `FALSE` to get extra output from 'qgis_process'.
#' This can be useful in debugging.
#' @param .raw_json_input The raw JSON to use as input in place of `...`.
#'
#' @returns A `qgis_result` object.
#'
#' @examplesIf has_qgis()
#' qgis_run_algorithm(
#'   "native:buffer",
#'   INPUT = system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
#'   DISTANCE = 10
#' )
#'
#' @export
qgis_run_algorithm <- function(algorithm, ..., PROJECT_PATH = NULL, ELLIPSOID = NULL,
                               .raw_json_input = NULL, .quiet = TRUE) {
  assert_qgis()
  assert_qgis_algorithm(algorithm)

  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_named(dots)) {
    abort("All ... arguments to `qgis_run_algorithm()` must be named.")
  }

  use_json_input <- !is.null(.raw_json_input) || qgis_using_json_input()
  use_json_output <- use_json_input || qgis_using_json_output()

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
      arg_skip_loading_plugins(algorithm),
      "run",
      algorithm,
      if (use_json_input) "-" else args_str
    ),
    echo_cmd = !.quiet,
    stdout_callback = if (!.quiet && !use_json_output) function(x, ...) cat(x),
    stderr_callback = function(x, ...) message(x, appendLF = FALSE),
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
