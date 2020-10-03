
#' Run algorithms using 'qgis_process'
#'
#' @param algorithm A qualified algorithm name (e.g., "native:filedownloader").
#' @param provider A provider identifier (e.g., "native")
#'
#' @export
#'
#' @examples
#' if (has_qgis()) qgis_has_algorithm("native:filedownloader")
#' if (has_qgis()) qgis_has_provider("native")
#' if (has_qgis()) qgis_providers()
#'
qgis_has_algorithm <- function(algorithm) {
  assert_qgis()
  as.character(algorithm) %in% qgis_algorithms()$algorithm
}

#' @rdname qgis_has_algorithm
#' @export
qgis_has_provider <- function(provider) {
  assert_qgis()
  as.character(provider) %in% unique(qgis_algorithms()$provider)
}

#' @rdname qgis_has_algorithm
#' @export
qgis_providers <- function(provider) {
  assert_qgis()
  algs <- qgis_algorithms()
  algs[!duplicated(algs$provider), c("provider", "provider_title")]
}

#' @rdname qgis_has_algorithm
#' @export
is_qgis_model_file <- function(algorithm) {
  file.exists(algorithm) && !dir.exists(algorithm)
}

#' @rdname qgis_has_algorithm
#' @export
assert_qgis_algorithm_or_model_file <- function(algorithm) {
  if (!is.character(algorithm) || length(algorithm) != 1) {
    stop("`algorithm` must be a character vector of length 1", call. = FALSE)
  } else if (!is_qgis_model_file(algorithm) && !qgis_has_algorithm(algorithm)) {
    stop(glue::glue("'{ algorithm }' is not a QGIS algorithm or path to model file."), call. = FALSE)
  }

  invisible(algorithm)
}
