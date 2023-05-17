#' Convert a qgis_result object or one of its elements to a raster object
#'
#' @note Just use `qgis_as_raster()` and `qgis_as_brick()` in R scripts;
#' it will use the correct method.
#'
#' @family topics about coercing processing output
#' @family topics about accessing or managing processing results
#'
#' @param output A result from [qgis_run_algorithm()] or one of the
#' [qgis_extract_output()] functions.
#' @param ... Arguments passed to [raster::raster()] or [raster::brick()].
#' @name qgis_as_raster

#' @rdname qgis_as_raster
#' @export
qgis_as_raster <- function(output, ...) {
  UseMethod("qgis_as_raster")
}

#' @rdname qgis_as_raster
#' @export
qgis_as_brick <- function(output, ...) {
  UseMethod("qgis_as_brick")
}

#' @rdname qgis_as_raster
#' @export
qgis_as_raster.qgis_outputRaster <- function(output, ...) {
  raster::raster(unclass(output), ...)
}

#' @rdname qgis_as_raster
#' @export
qgis_as_brick.qgis_outputRaster <- function(output, ...) {
  raster::brick(unclass(output), ...)
}

#' @rdname qgis_as_raster
#' @export
qgis_as_raster.qgis_outputLayer <- function(output, ...) {
  raster::raster(unclass(output), ...)
}

#' @rdname qgis_as_raster
#' @export
qgis_as_brick.qgis_outputLayer <- function(output, ...) {
  raster::brick(unclass(output), ...)
}

#' @rdname qgis_as_raster
#' @export
qgis_as_raster.qgis_result <- function(output, ...) {
  result <- qgis_extract_output_by_class(output, c("qgis_outputRaster", "qgis_outputLayer"))
  raster::raster(unclass(result), ...)
}

#' @rdname qgis_as_raster
#' @export
qgis_as_brick.qgis_result <- function(output, ...) {
  result <- qgis_extract_output_by_class(output, c("qgis_outputRaster", "qgis_outputLayer"))
  raster::brick(unclass(result), ...)
}

#' @keywords internal
#' @export
as_qgis_argument.RasterLayer <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  as_qgis_argument_raster(x, spec, use_json_input)
}

# @param x A [raster::raster()] or [raster::brick()].
#' @keywords internal
#' @export
as_qgis_argument.RasterBrick <- function(x, spec = qgis_argument_spec(),
                                         use_json_input = FALSE) {
  as_qgis_argument_raster(x, spec, use_json_input)
}

#' @keywords internal
as_qgis_argument_raster <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("raster", "layer", "multilayer"))) {
    abort(glue("Can't convert '{ class(x)[1] }' object to QGIS type '{ spec$qgis_type }'"))
  }

  # try to use a filename if present
  if (x@file@name != "") {
    file_ext <- stringr::str_to_lower(tools::file_ext(x@file@name))
    if (file_ext %in% c("grd", "asc", "sdat", "rst", "nc", "tif", "tiff", "gtiff", "envi", "bil", "img")) {
      return(x@file@name)
    }
  }

  path <- qgis_tmp_raster()
  raster::writeRaster(x, path)
  structure(path, class = "qgis_tempfile_arg")
}

#' @keywords internal
#' @export
as_qgis_argument.CRS <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("crs"))) {
    abort(glue("Can't convert 'crs' object to QGIS type '{ spec$qgis_type }'"))
  }

  raster::wkt(x)
}

#' @keywords internal
#' @export
as_qgis_argument.Extent <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("extent"))) {
    abort(glue("Can't convert 'Extent' object to QGIS type '{ spec$qgis_type }'"))
  }

  glue("{x@xmin},{x@xmax},{x@ymin},{x@ymax}")
}
