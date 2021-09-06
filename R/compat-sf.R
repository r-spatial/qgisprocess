
#' Convert sf objects to/from QGIS inputs/outputs
#'
#' @inheritParams as_qgis_argument
#'
#' @export
#'
as_qgis_argument.sf <- function(x, spec = qgis_argument_spec()) {
  if (!isTRUE(spec$qgis_type %in% c("source", "layer", "vector", "multilayer", "point"))) {
    abort(glue("Can't convert 'sf' object to QGIS type '{ spec$qgis_type }'"))
  }

  if (spec$qgis_type == "point") {
    as_qgis_argument(sf::st_geometry(x), spec=spec)
  } else {
    path <- qgis_tmp_vector()
    sf::write_sf(x, path)
    structure(path, class = "qgis_tempfile_arg")
  }
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

#'
#' @inheritParams as_qgis_argument
#'
#' @export
#'
as_qgis_argument.crs <- function(x, spec = qgis_argument_spec()) {
  if (!isTRUE(spec$qgis_type %in% c("crs"))) {
    abort(glue("Can't convert 'crs' object to QGIS type '{ spec$qgis_type }'"))
  }

  x$Wkt
}

#'
#' @inheritParams as_qgis_argument
#'
#' @export
#'
as_qgis_argument.bbox <- function(x, spec = qgis_argument_spec()) {
  if (!isTRUE(spec$qgis_type %in% c("extent"))) {
    abort(glue("Can't convert 'bbox' object to QGIS type '{ spec$qgis_type }'"))
  }

  if (!is.na(sf::st_crs(x)$epsg)){
    glue("{x$xmin},{x$xmax},{x$ymin},{x$ymax}[EPSG:{sf::st_crs(x)$epsg}]")
  }else{
    glue("{x$xmin},{x$xmax},{x$ymin},{x$ymax}")
  }
}

#'
#' @inheritParams as_qgis_argument
#'
#' @export
#'
as_qgis_argument.sfc <- function(x, spec = qgis_argument_spec()) {

  if (!isTRUE(spec$qgis_type %in% c("point"))) {
    abort(glue("Can't convert 'sfc' object to QGIS type '{ spec$qgis_type }'"))
  }

  if (isTRUE(length(x) != 1)){
    abort(glue("Can't convert 'sfc' object to QGIS type '{ spec$qgis_type }' as the length is not equal to 1"))
  }

  if (isTRUE((sf::st_geometry_type(x) != "POINT"))){
    abort(glue("Can't convert 'sfc' object to QGIS type '{ spec$qgis_type }' as type is not 'POINT'"))
  }

  if (!is.na(sf::st_crs(x)$epsg)){
    glue("{x[[1]][1]},{x[[1]][2]}[EPSG:{sf::st_crs(x)$epsg}]")
  }else{
    glue("{x[[1]][1]},{x[[1]][2]}")
  }
}

#'
#' @inheritParams as_qgis_argument
#'
#' @export
#'
as_qgis_argument.POINT <- function(x, spec = qgis_argument_spec()) {

  if (!isTRUE(spec$qgis_type %in% c("point"))) {
    abort(glue("Can't convert 'POINT' object to QGIS type '{ spec$qgis_type }'"))
  }

  glue("{x[1]},{x[2]}")
}
