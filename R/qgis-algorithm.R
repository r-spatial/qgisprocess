#' Run algorithms using 'qgis_process'
#'
#' Run QGIS algorithms.
#' See the [QGIS docs](https://docs.qgis.org/testing/en/docs/user_manual/processing_algs/qgis/index.html)
#' for a detailed description of the algorithms provided
#' 'out of the box' on QGIS (versions >= 3.14).
#'
#' @inheritParams qgis_run_algorithm
#' @inheritParams qgis_run
#'
#' @export
#'
#' @examples
#' if (has_qgis()) qgis_has_algorithm("native:filedownloader")
#' if (has_qgis()) qgis_algorithms()
#' if (has_qgis()) qgis_has_provider("native")
#' if (has_qgis()) qgis_providers()
#'
qgis_has_algorithm <- function(algorithm) {
  assert_qgis()
  as.character(algorithm) %in% qgis_algorithms()$algorithm
}

#' @rdname qgis_has_algorithm
#' @export
qgis_algorithms <- function(query = FALSE, quiet = TRUE) {
  if (query) {
    qgisprocess_cache$algorithms <- qgis_query_algorithms(quiet = quiet)
  }

  qgisprocess_cache$algorithms
}

#' @rdname qgis_has_algorithm
#' @export
qgis_has_provider <- function(provider, query = FALSE, quiet = TRUE) {
  assert_qgis()
  as.character(provider) %in% unique(qgis_algorithms(query, quiet)$provider)
}

#' @rdname qgis_has_algorithm
#' @export
qgis_providers <- function(provider) {
  assert_qgis()
  algs <- qgis_algorithms()
  algs[!duplicated(algs$provider), c("provider", "provider_title"), drop = FALSE]
}

#' @rdname qgis_has_algorithm
#' @export
assert_qgis_algorithm <- function(algorithm) {
  if (!is.character(algorithm) || length(algorithm) != 1) {
    abort("`algorithm` must be a character vector of length 1")
  } else if (!qgis_has_algorithm(algorithm)) {
    abort(
      glue(
        "Can't find QGIS algorithm '{ algorithm }'.\nRun `qgis_algorithms()` for a list of available algorithms."
      )
    )
  }

  invisible(algorithm)
}
