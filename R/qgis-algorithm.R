#' Run algorithms using 'qgis_process'
#'
#' Run QGIS algorithms.
#' See the [QGIS docs](https://docs.qgis.org/testing/en/docs/user_manual/processing_algs/qgis/index.html)
#' for a detailed description of the algorithms provided
#' 'out of the box' on QGIS (versions >= 3.14).
#'
#' @param algorithm A qualified algorithm name (e.g., "native:filedownloader") or
#'   a path to a QGIS model file.
#' @param provider A provider identifier (e.g., "native")
#' @param PROJECT_PATH,ELIPSOID Global values for QGIS project file and
#'   elipsoid name for distance calculations.
#' @param ... Named key-value pairs as arguments for each algorithm. Features of
#'   [rlang::list2()] are supported.
#' @param .quiet Use `TRUE` to suppress output from processing algorithms.
#'
#' @export
#'
#' @examples
#' if (has_qgis()) qgis_has_algorithm("native:filedownloader")
#' if (has_qgis()) qgis_has_provider("native")
#' if (has_qgis()) qgis_providers()
qgis_run_algorithm <- function(algorithm, ..., PROJECT_PATH = rlang::zap(), ELIPSOID = rlang::zap(),
                               .quiet = FALSE) {
  assert_qgis()
  assert_qgis_algorithm_or_model_file(algorithm)

  # use list2 so that users can !!! argument lists
  # zap() means don't include (NULL may have meaning for some types)
  args <- rlang::list2(..., PROJECT_PATH = PROJECT_PATH, ELIPSOID = ELIPSOID)
  args <- args[!vapply(args, rlang::is_zap, logical(1))]

  if (length(args) > 0) {
    if (!rlang::is_named(args)) {
      stop("All arguments to `qgis_run_algorithm()` must be named.", call. = FALSE)
    }

    # get argument info for supplied args and run sanitizers
    arg_meta <- qgis_arguments(algorithm)
    arg_meta <- arg_meta[match(names(args), arg_meta$name), ]
    args <- Map(
      # have to do this omitting errors so that qgis_clean_argument()
      # is called on anything that succeeded regardless of other arg failures
      function(x, qgis_type) try(as_qgis_argument(x, qgis_type), silent = TRUE),
      args, arg_meta$qgis_type
    )

    # make sure cleanup is run on any temporary files created
    on.exit(Map(qgis_clean_argument, args, arg_meta$qgis_type))

    # look for sanitizer errors and stop() for them
    arg_errors <- vapply(args, inherits, "try-error", FUN.VALUE = logical(1))
    if (any(arg_errors)) {
      stop(args[arg_errors][[1]], call. = FALSE)
    }
  }

  args <- paste0("--", names(args), "=", vapply(args, as.character, character(1)))

  if (.quiet) {
    result <- qgis_run(args = c("run", algorithm, args))
  } else {
    result <- qgis_run(
      args = c("run", algorithm, args),
      echo_cmd = TRUE,
      stdout_callback = function(x, ...) cat(x)
    )
    cat("\n")
  }

  result
}

#' @rdname qgis_run_algorithm
#' @export
qgis_has_algorithm <- function(algorithm) {
  assert_qgis()
  as.character(algorithm) %in% qgis_algorithms()$algorithm
}

#' @rdname qgis_run_algorithm
#' @export
qgis_has_provider <- function(provider) {
  assert_qgis()
  as.character(provider) %in% unique(qgis_algorithms()$provider)
}

#' @rdname qgis_run_algorithm
#' @export
qgis_providers <- function(provider) {
  assert_qgis()
  algs <- qgis_algorithms()
  algs[!duplicated(algs$provider), c("provider", "provider_title")]
}

#' @rdname qgis_run_algorithm
#' @export
is_qgis_model_file <- function(algorithm) {
  file.exists(algorithm) && !dir.exists(algorithm)
}

#' @rdname qgis_run_algorithm
#' @export
assert_qgis_algorithm_or_model_file <- function(algorithm) {
  if (!is.character(algorithm) || length(algorithm) != 1) {
    stop("`algorithm` must be a character vector of length 1", call. = FALSE)
  } else if (!is_qgis_model_file(algorithm) && !qgis_has_algorithm(algorithm)) {
    stop(glue::glue("'{ algorithm }' is not a QGIS algorithm or path to model file."), call. = FALSE)
  }

  invisible(algorithm)
}
