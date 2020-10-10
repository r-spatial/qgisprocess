
#' @rdname as_qgis_argument
#' @export
as_qgis_argument.sf <- function(x, spec = qgis_argument_spec()) {
  if (!isTRUE(spec$qgis_type %in% c("source", "layer", "vector"))) {
    abort(glue("Can't convert 'sf' object to QGIS type '{ spec$qgis_type }'"))
  }

  path <- qgis_tmp_vector()
  sf::write_sf(x, path)
  structure(path, class = "qgis_tempfile_arg")
}
