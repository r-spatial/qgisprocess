
#' @rdname as_qgis_argument
#' @export
as_qgis_argument.RasterLayer <- function(x, qgis_type) {
  if (qgis_type != "raster") {
    abort(glue("Can't use 'RasterLayer' objects for QGIS arguments with type '{ qgis_type }'"))
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
