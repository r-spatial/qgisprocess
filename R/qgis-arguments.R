
#' Type coercion for arguments to QGIS processing algorithms
#'
#' @param x An object passed to a QGIS processing algorithm
#' @param qgis_type A character vector of length 1 (e.g., "source").
#' @param value The result of [as_qgis_argument()] after the QGIS processing
#'   algorithm has been run.
#'
#' @export
#'
as_qgis_argument <- function(x, qgis_type = NA) {
  UseMethod("as_qgis_argument")
}

# All `qgis_type` values:
# c("source", "sink", "raster", "band", "boolean", "string", "rasterDestination",
#   "crs", "distance", "field", "vectorDestination", "multilayer",
#   "enum", "extent", "number", "file", "folderDestination", "fileDestination",
#   "vector", "point", "range", "expression", "aggregates", "layout",
#   "layer", "layoutitem", "maptheme", "matrix", "fields_mapping",
#   "coordinateoperation", "tininputlayers", "vectortilewriterlayers",
#   "execute_sql", "raster_calc_expression", "relief_colors", "color"
# )

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.default <- function(x, qgis_type = NA) {
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
as_qgis_argument.character <- function(x, qgis_type = NA) {
  switch(
    as.character(qgis_type),
    field = paste0(x, collapse = ";"),
    paste0(x, collapse = ",")
  )
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.logical <- function(x, qgis_type = NA) {
  paste0(x, collapse = ",")
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.numeric <- function(x, qgis_type = NA) {
  paste0(x, collapse = ",")
}

#' @rdname as_qgis_argument
#' @export
qgis_clean_argument <- function(value, qgis_type = NA) {
  UseMethod("qgis_clean_argument")
}

#' @rdname as_qgis_argument
#' @export
qgis_clean_argument.default <- function(value, qgis_type = NA) {
  # by default, do nothing!
}

#' @rdname as_qgis_argument
#' @export
qgis_clean_argument.qgis_tempfile_arg <- function(value, qgis_type = NA) {
  unlink(value)
}
