#' Deprecated functions
#'
#' - Instead of `qgis_output()`, use [qgis_extract_output()] and related
#' functions.
#' - Instead of `qgis_result_single()`, use [qgis_extract_output()] and related
#' functions.
#'
#'
#' @inheritParams is_qgis_result
#' @param what Character vector of classes.
#'
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



#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_result_single <- function(x, what) {
  .Deprecated("qgis_extract_output")
  # Limit result to elements that match class
  if (!missing(what)) {
    x <- x[vapply(x, inherits, what, FUN.VALUE = logical(1))]
    if (length(x) == 0L) {
      abort(
        paste(
          "Can't extract object from result: zero outputs of type",
          paste(what, collapse = " or ")
        )
      )
    }
  }

  # By default, take the first element named as output or OUTPUT.
  # Otherwise, take the first element that matches class.
  result <- x[grepl("^(output|OUTPUT)$", names(x))][1][[1]]
  if (is.null(result)) result <- x[[1]]
  result
}



