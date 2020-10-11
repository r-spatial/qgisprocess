
#' @rdname as_qgis_argument
#' @export
as_qgis_argument.RasterLayer <- function(x, spec = qgis_argument_spec()) {
  as_qgis_argument_raster(x, spec)
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.RasterBrick <- function(x, spec = qgis_argument_spec()) {
  as_qgis_argument_raster(x, spec)
}

as_qgis_argument_raster <- function(x, spec = qgis_argument_spec()) {
  if (!isTRUE(spec$qgis_type %in% c("raster", "layer"))) {
    abort(glue("Can't convert '{ class(x)[1] }' object to QGIS type '{ spec$qgis_type }'"))
  }

  # try to use a filename if present
  if (x@file@name != ""){
    file_ext <- stringr::str_to_lower(tools::file_ext(x@file@name))
    if (file_ext %in% c("grd", "asc", "sdat", "rst", "nc", "tif", "tiff", "gtiff", "envi", "bil", "img")) {
      return(x@file@name)
    }
  }

  path <- qgis_tmp_raster()
  raster::writeRaster(x, path)
  structure(path, class = "qgis_tempfile_arg")
}
