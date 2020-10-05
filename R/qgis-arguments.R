
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
  stop(
    glue::glue(
      paste0(
        "Don't know how to convert object of type ",
        "'{ paste(class(x), collapse = \" / \") }' ",
        "to QGIS type '{ qgis_type }'"
      )
    ),
    call. = FALSE
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
as_qgis_argument.sf <- function(x, qgis_type) {
  if (qgis_type != "source") {
    stop(glue::glue("Can't use 'sf' objects for QGIS arguments with type '{ qgis_type }'"), call. = FALSE)
  }

  path <- tempfile(fileext = ".gpkg")
  sf::write_sf(x, path)
  structure(path, class = "qgis_tempfile")
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.RasterLayer <- function(x, qgis_type) {
  if (qgis_type != "raster") {
    stop(glue::glue("Can't use 'RasterLayer' objects for QGIS arguments with type '{ qgis_type }'"), call. = FALSE)
  }

  if (x@file@name != ""){
    file_ext <- stringr::str_match(a,
                                   ".*\\.([a-zA-Z]+)$")[2]

    file_ext <- stringr::str_to_lower(file_ext)

    if (file_ext %in% c("grd", "asc", "sdat", "rst", "nc", "tif", "tiff", "gtiff", "envi", "bil", "img")){
      return(x@file@name)
    }
  }

  path <- tempfile(fileext = ".tif")
  raster::writeRaster(x, path)
  structure(path, class = "qgis_tempfile")
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
qgis_clean_argument.qgis_tempfile <- function(value, qgis_type) {
  unlink(value)
}
