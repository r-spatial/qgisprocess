
#' Convert sf objects to/from QGIS inputs/outputs
#'
#' @inheritParams as_qgis_argument
#'
#' @export
#'
as_qgis_argument.sf <- function(x, spec = qgis_argument_spec()) {
  if (!isTRUE(spec$qgis_type %in% c("source", "layer", "vector"))) {
    abort(glue("Can't convert 'sf' object to QGIS type '{ spec$qgis_type }'"))
  }

  path <- qgis_tmp_vector()
  sf::write_sf(x, path)
  structure(path, class = "qgis_tempfile_arg")
}

# dynamically registered in zzz.R
st_as_sf.qgis_result <- function(x, ...) {
  # find the first vector output and read it
  for (result in x) {
    if (inherits(result, "qgis_outputVector") || inherits(result, "qgis_outputLayer")) {
      return(sf::read_sf(result, ...))
    }
  }

  abort("Can't extract sf object from result: zero outputs of type 'outputVector' or 'outputLayer'.")
}
