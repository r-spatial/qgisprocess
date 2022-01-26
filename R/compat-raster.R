
#' Convert raster objects to/from QGIS inputs/outputs
#'
#' @param x A [raster::raster()] or [raster::brick()].
#' @param output The result from [qgis_run_algorithm()] or [qgis_output()].
#' @param ... Passed to [raster::raster()] or [raster::brick()].
#' @inheritParams as_qgis_argument
#'
#' @export
#'
as_qgis_argument.RasterLayer <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  as_qgis_argument_raster(x, spec, use_json_input)
}

#' @rdname as_qgis_argument.RasterLayer
#' @export
as_qgis_argument.RasterBrick <- function(x, spec = qgis_argument_spec(),
                                         use_json_input = FALSE) {
  as_qgis_argument_raster(x, spec, use_json_input)
}

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

#' @rdname as_qgis_argument.RasterLayer
#' @export
qgis_as_raster <- function(output, ...) {
  UseMethod("qgis_as_raster")
}

#' @rdname as_qgis_argument.RasterLayer
#' @export
qgis_as_brick <- function(output, ...) {
  UseMethod("qgis_as_brick")
}

#' @rdname as_qgis_argument.RasterLayer
#' @export
qgis_as_raster.qgis_outputRaster <- function(output, ...) {
  raster::raster(unclass(output), ...)
}

#' @rdname as_qgis_argument.RasterLayer
#' @export
qgis_as_brick.qgis_outputRaster <- function(output, ...) {
  raster::brick(unclass(output), ...)
}

#' @rdname as_qgis_argument.RasterLayer
#' @export
qgis_as_raster.qgis_outputLayer <- function(output, ...) {
  raster::raster(unclass(output), ...)
}

#' @rdname as_qgis_argument.RasterLayer
#' @export
qgis_as_brick.qgis_outputLayer <- function(output, ...) {
  raster::brick(unclass(output), ...)
}

#' @rdname as_qgis_argument.RasterLayer
#' @export
qgis_as_raster.qgis_result <- function(output, ...) {
  result <- qgis_result_single(output, c("qgis_outputRaster", "qgis_outputLayer"))
  raster::raster(unclass(result), ...)
}

#' @rdname as_qgis_argument.RasterLayer
#' @export
qgis_as_brick.qgis_result <- function(output, ...) {
  result <- qgis_result_single(output, c("qgis_outputRaster", "qgis_outputLayer"))
  raster::brick(unclass(result), ...)
}

#' @export
as_qgis_argument.CRS <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("crs"))) {
    abort(glue("Can't convert 'crs' object to QGIS type '{ spec$qgis_type }'"))
  }

  raster::wkt(x)
}

#' @export
as_qgis_argument.Extent <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("extent"))) {
    abort(glue("Can't convert 'Extent' object to QGIS type '{ spec$qgis_type }'"))
  }

  glue("{x@xmin},{x@xmax},{x@ymin},{x@ymax}")
}
