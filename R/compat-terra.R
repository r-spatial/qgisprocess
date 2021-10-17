
#' Convert raster objects to/from QGIS inputs/outputs
#'
#' @param x A [terra::rast()].
#' @param output The result from [qgis_run_algorithm()] or [qgis_output()].
#' @param ... Passed to [terra::rast()].
#' @inheritParams as_qgis_argument
#'
#' @export
#'
as_qgis_argument.SpatRaster <- function(x, spec = qgis_argument_spec()) {
  as_qgis_argument_terra(x, spec)
}

as_qgis_argument_terra <- function(x, spec = qgis_argument_spec()) {
  if (!isTRUE(spec$qgis_type %in% c("raster", "layer", "multilayer"))) {
    abort(glue("Can't convert '{ class(x)[1] }' object to QGIS type '{ spec$qgis_type }'"))
  }

  # try to use a filename if present
  if (terra::sources(x)$source != "") {
    file_ext <- stringr::str_to_lower(tools::file_ext(terra::sources(x)$source))
    if (file_ext %in% c("grd", "asc", "sdat", "rst", "nc", "tif", "tiff", "gtiff", "envi", "bil", "img")) {
      return(terra::sources(x)$source)
    }
  }

  path <- qgis_tmp_raster()
  terra::writeRaster(x, path)
  structure(path, class = "qgis_tempfile_arg")
}

#' @rdname as_qgis_argument.SpatRaster
#' @export
qgis_as_terra <- function(output, ...) {
  UseMethod("qgis_as_terra")
}

#' @rdname as_qgis_argument.SpatRaster
#' @export
qgis_as_terra.qgis_outputRaster <- function(output, ...) {
  terra::rast(unclass(output), ...)
}

#' @rdname as_qgis_argument.SpatRaster
#' @export
qgis_as_terra.qgis_outputLayer <- function(output, ...) {
  terra::rast(unclass(output), ...)
}

#' @rdname as_qgis_argument.SpatRaster
#' @export
qgis_as_terra.qgis_result <- function(output, ...) {
  # find the first raster output and read it
  for (result in output) {
    if (inherits(result, "qgis_outputRaster") || inherits(result, "qgis_outputLayer")) {
      return(terra::rast(unclass(result), ...))
    }
  }

  abort("Can't extract 'terra' raster from result: zero outputs of type 'outputRaster' or 'outputLayer'.")
}

#' @export
as_qgis_argument.SpatExtent <- function(x, spec = qgis_argument_spec()) {
  if (!isTRUE(spec$qgis_type %in% c("extent"))) {
    abort(glue("Can't convert 'SpatExtent' object to QGIS type '{ spec$qgis_type }'"))
  }

  ex <- as.vector(terra::ext(x))

  glue("{ex[['xmin']]},{ex[['xmax']]},{ex[['ymin']]},{ex[['ymax']]}")
}
