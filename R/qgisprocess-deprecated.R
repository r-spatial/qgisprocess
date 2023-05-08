#' Deprecated functions
#'
#' - Instead of `qgis_output()`, use [qgis_extract_output()] and related
#' functions.
#' - Instead of `qgis_result_single()`, use [qgis_extract_output()] and related
#' functions.
#' - Instead of `qgis_detect_windows()`, use [qgis_detect_windows_paths()].
#' - Instead of `qgis_detect_macos()`, use [qgis_detect_macos_paths()].
#' - Instead of `qgis_use_json_input()`, use [qgis_using_json_input()].
#' - Instead of `qgis_use_json_output()`, use [qgis_using_json_output()].
#' - Instead of `qgis_description()`, use [qgis_get_description()].
#' - Instead of `qgis_arguments()`, use [qgis_get_argument_specs()].
#' - Instead of `qgis_outputs()`, use [qgis_get_output_specs()].
#' - Instead of `qgis_pipe()`, use [qgis_run_algorithm_p()].
#' - Instead of `qgis_tmp_clean()`, use [qgis_clean_tmp()].
#' - Instead of `qgis_result_clean()`, use [qgis_clean_result()].
#'
#' @inheritParams is_qgis_result
#' @param what Character vector of classes.
#' @param ... Arguments passed to the new function.
#' This is done for functions where only the function name changed at
#' time of deprecation.
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


# nocov start


#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_detect_windows <- function(...) {
  .Deprecated("qgis_detect_windows_paths")
  qgis_detect_windows_paths(...)
}


#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_detect_macos <- function() {
  .Deprecated("qgis_detect_macos_paths")
  qgis_detect_macos_paths()
}


#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_use_json_input <- function() {
  .Deprecated("qgis_using_json_input")
  qgis_using_json_input()
}


#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_use_json_output <- function(...) {
  .Deprecated("qgis_using_json_output")
  qgis_using_json_output(...)
}


#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_description <- function(...) {
  .Deprecated("qgis_get_description")
  qgis_get_description(...)
}


#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_arguments <- function(...) {
  .Deprecated("qgis_get_argument_specs")
  qgis_get_argument_specs(...)
}


#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_outputs <- function(...) {
  .Deprecated("qgis_get_output_specs")
  qgis_get_output_specs(...)
}


#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_pipe <- function(...) {
  .Deprecated("qgis_run_algorithm_p")
  qgis_run_algorithm_p(...)
}


#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_tmp_clean <- function(...) {
  .Deprecated("qgis_clean_tmp")
  qgis_clean_tmp(...)
}


#' @rdname qgisprocess-deprecated
#' @export
#' @keywords internal
qgis_result_clean <- function(...) {
  .Deprecated("qgis_clean_result")
  qgis_clean_result(...)
}


# nocov end
