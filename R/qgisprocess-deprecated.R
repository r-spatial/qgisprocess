#' Deprecated functions
#'
#' - Instead of `qgis_output()`, use [qgis_extract_output()] and related
#' functions.
#'
#'
#' @inheritParams is_qgis_result
#' @name qgisprocess-deprecated

#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_output <- function(x, which) {
  .Deprecated("qgis_extract_output")
  assert_that(inherits(x, "qgis_result"), length(which) == 1L)
  output_names <- setdiff(
    names(x),
    c(".algorithm", ".args", ".processx_result", ".raw_json_input")
  )
  x <- x[output_names]
  if (is.numeric(which) && (which %in% seq_along(x))) {
    x[[which]]
  } else if (which %in% names(x)) {
    x[[which]]
  } else {
    qgis_error_output_does_not_exist(x, which)
  }
}
