#' Check availability of QGIS, a plugin, a provider or an algorithm
#'
#' `has_qgis()` checks whether the loaded qgisprocess cache is populated,
#' which means that a QGIS installation was accessible and responsive when
#' loading the package.
#' `qgis_has_plugin()`, `qgis_has_provider()` and `qgis_has_algorithm()` check
#' for the availability of one or several plugins, processing providers and
#' algorithms, respectively.
#' They are vectorized.
#'
#' @note
#' Only plugins that implement processing providers are supported.
#'
#' @return A logical, with length 1 in case of `has_qgis()`.
#'
#' @family topics about reporting the QGIS state
#' @concept functions to manage and explore QGIS and qgisprocess
#'
#' @param algorithm A qualified algorithm name
#' (e.g., `"native:buffer"`).
#' Can be a vector of names.
#' @param provider A provider name (e.g., `"native"`).
#' Can be a vector of names.
#' @param plugin A plugin name (e.g., `"native"`).
#' Can be a vector of names.
#' @inheritParams qgis_path
#'
#' @name has_qgis

#' @rdname has_qgis
#' @export
has_qgis <- function() {
  !is.null(qgisprocess_cache$path) &&
    !is.null(qgisprocess_cache$version) &&
    !is.null(qgisprocess_cache$algorithms) &&
    !is.null(qgisprocess_cache$plugins)
}

# @param action An action to take if the 'qgis_process' executable could not be
#   found.
#' @keywords internal
assert_qgis <- function(action = abort) {
  if (!has_qgis()) {
    action(
      paste0(
        "The QGIS processing utility ('qgis_process') is not installed or could not be found.\n",
        "Run `qgis_configure()` to configure this location.\n",
        "If 'qgis_process' is installed, set `options(qgisprocess.path = '/path/to/qgis_process')`\n",
        "and re-run `qgis_configure()`."
      )
    )
  }
}


#' @rdname has_qgis
#' @export
qgis_has_plugin <- function(plugin, query = FALSE, quiet = TRUE) {
  assert_that(is.character(plugin))
  plugin %in% qgis_plugins(query = query, quiet = quiet)$name
}

#' @rdname has_qgis
#' @export
qgis_has_provider <- function(provider, query = FALSE, quiet = TRUE) {
  assert_qgis()
  as.character(provider) %in% unique(qgis_algorithms(query, quiet)$provider)
}

#' @rdname has_qgis
#' @export
qgis_has_algorithm <- function(algorithm, query = FALSE, quiet = TRUE) {
  assert_qgis()
  as.character(algorithm) %in% qgis_algorithms(query, quiet)$algorithm
}
