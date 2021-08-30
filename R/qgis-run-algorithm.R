
#' Run algorithms using 'qgis_process'
#'
#' Run QGIS algorithms.
#' See the [QGIS docs](https://docs.qgis.org/testing/en/docs/user_manual/processing_algs/qgis/index.html)
#' for a detailed description of the algorithms provided
#' 'out of the box' on QGIS (versions >= 3.14).
#'
#' @param algorithm A qualified algorithm name (e.g., "native:filedownloader") or
#'   a path to a QGIS model file.
#' @param PROJECT_PATH,ELIPSOID Global values for QGIS project file and
#'   elipsoid name for distance calculations.
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
qgis_run_algorithm <- function(algorithm, ..., PROJECT_PATH = NULL, ELIPSOID = NULL, .quiet = FALSE) {
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
    ELIPSOID = ELIPSOID
  )
  on.exit(qgis_clean_arguments(args))

  # turn sanitized arguments into command-line arguments
  # in the future this might be JSON to accommodate more input types

  # we can't deal with dict items yet
  args_dict <- vapply(args, inherits, logical(1), "qgis_dict_input")
  if (any(args_dict)) {
    labels <- names(args)[args_dict]
    abort("`qgis_run_algorithm()` can't generate command-line arguments from `qgis_dict_input()()`")
  }

  # otherwise, unlist() will flatten qgis_list_input() items
  args_flat <- unlist(args)
  arg_name_n <- vapply(args, length, integer(1))
  names(args_flat) <- unlist(Map(rep, names(args), arg_name_n))

  if (length(args_flat) > 0) {
    args_str <- paste0("--", names(args_flat), "=", vapply(args_flat, as.character, character(1)))
  } else {
    args_str <- character(0)
  }

  # To get around a bug in processx (#302), we need to use a stdout callback
  # to buffer stdout manually. For large outputs this would be slow, but
  # the size of the buffer seems to be large enough that this doesn't
  # matter.
  stdout_output <- ""

  if (.quiet) {
    result <- qgis_run(
      args = c("run", algorithm, args_str),
      stdout_callback = function(x, ...) {
        stdout_output <<- paste0(stdout_output, x)
      }
    )
  } else {
    result <- qgis_run(
      args = c("run", algorithm, args_str),
      echo_cmd = TRUE,
      stdout_callback = function(x, ...) {
        stdout_output <<- paste0(stdout_output, x)
        cat(x)
      },
      stderr_callback = function(x, ...) message(x, appendLF = FALSE)
    )
    cat("\n")
  }

  # return a custom object to keep as much information as possible
  # about the output
  structure(
    rlang::list2(
      !!! qgis_parse_results(algorithm, stdout_output),
      .algorithm = algorithm,
      .args = args,
      .processx_result = result
    ),
    class = "qgis_result"
  )
}
