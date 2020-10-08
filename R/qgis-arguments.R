
#' Type coercion for arguments to QGIS processing algorithms
#'
#' @param x An object passed to a QGIS processing algorithm
#' @param qgis_type A character vector of length 1 (e.g., "source").
#' @param value The result of [as_qgis_argument()] after the QGIS processing
#'   algorithm has been run.
#'
#' @export
#'
as_qgis_argument <- function(x, qgis_type) {
  UseMethod("as_qgis_argument")
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.default <- function(x, qgis_type) {
  abort(
    glue(
      paste0(
        "Don't know how to convert object of type ",
        "'{ paste(class(x), collapse = \" / \") }' ",
        "to QGIS type '{ qgis_type }'"
      )
    )
  )
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.character <- function(x, qgis_type) {
  paste0(x, collapse = " ")
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.numeric <- function(x, qgis_type) {
  paste0(x, collapse = " ")
}

#' @rdname as_qgis_argument
#' @export
qgis_clean_argument <- function(value, qgis_type) {
  UseMethod("qgis_clean_argument")
}

#' @rdname as_qgis_argument
#' @export
qgis_clean_argument.default <- function(value, qgis_type) {
  # by default, do nothing!
}

#' @rdname as_qgis_argument
#' @export
qgis_clean_argument.qgis_tempfile_arg <- function(value, qgis_type) {
  unlink(value)
}
