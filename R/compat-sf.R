#' Convert a qgis_result object or one of its elements to an sf object
#'
#' @details
#' The sf package must be loaded explicitly to use these methods.
#'
#' @note Just use `st_as_sf()` in R scripts, it will use the correct
#' method.
#'
#' @family topics about coercing processing output
#' @family topics about accessing or managing processing results
#'
#' @param ... Arguments passed to [sf::read_sf()].
#' @inheritParams qgis_as_raster
#'
#' @returns An `sf` object.
#'
#' @examplesIf has_qgis() && requireNamespace("sf", quietly = TRUE)
#' \donttest{
#' # not running below examples in R CMD check to save time
#' result <- qgis_run_algorithm(
#'   "native:buffer",
#'   INPUT = system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
#'   DISTANCE = 10
#' )
#'
#' # most direct approach, autoselecting a `qgis_outputVector` type
#' # output from the `result` object and reading as sf object:
#' sf::st_as_sf(result)
#'
#' # if you need more control, extract the needed output element first:
#' output_vector <- qgis_extract_output(result, "OUTPUT")
#' sf::st_as_sf(output_vector)
#' }
#'
#' @name st_as_sf


#' @rdname st_as_sf
# dynamically registered in zzz.R
st_as_sf.qgis_result <- function(x, ...) {
  output <- qgis_extract_output_by_class(x, c("qgis_outputVector", "qgis_outputLayer"))
  sf::st_as_sf(output, ...)
}


#' @rdname st_as_sf
# dynamically registered in zzz.R
st_as_sf.qgis_outputVector <- function(x, ...) {
  if (grepl("\\|layer", x)) {
    output_splitted <- strsplit(x, "\\|layer.*=")[[1]]
    sf::read_sf(output_splitted[1], output_splitted[2], ...)
  } else {
    sf::read_sf(x, ...)
  }
}

#' @rdname st_as_sf
# dynamically registered in zzz.R
st_as_sf.qgis_outputLayer <- function(x, ...) {
  if (grepl("\\|layer", x)) {
    output_splitted <- strsplit(x, "\\|layer.*=")[[1]]
    sf::read_sf(output_splitted[1], output_splitted[2], ...)
  } else {
    sf::read_sf(x, ...)
  }
}

#' @keywords internal
#' @export
as_qgis_argument.sf <- function(x, spec = qgis_argument_spec(),
                                use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("source", "layer", "vector", "multilayer", "point"))) {
    abort(glue("Can't convert 'sf' object to QGIS type '{ spec$qgis_type }'"))
  }

  if (spec$qgis_type == "point") {
    as_qgis_argument(sf::st_geometry(x), spec = spec)
  } else {
    path <- qgis_tmp_vector()
    sf::write_sf(x, path)
    structure(path, class = "qgis_tempfile_arg")
  }
}


#' @keywords internal
#' @export
as_qgis_argument.crs <- function(x, spec = qgis_argument_spec(),
                                 use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("crs"))) {
    abort(glue("Can't convert 'crs' object to QGIS type '{ spec$qgis_type }'"))
  }

  x$Wkt
}

#' @keywords internal
#' @export
as_qgis_argument.bbox <- function(x, spec = qgis_argument_spec(),
                                  use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("extent"))) {
    abort(glue("Can't convert 'bbox' object to QGIS type '{ spec$qgis_type }'"))
  }

  if (!is.na(sf::st_crs(x)$epsg)) {
    glue("{x$xmin},{x$xmax},{x$ymin},{x$ymax}[EPSG:{sf::st_crs(x)$epsg}]")
  } else {
    glue("{x$xmin},{x$xmax},{x$ymin},{x$ymax}")
  }
}

#' @keywords internal
#' @export
as_qgis_argument.sfc <- function(x, spec = qgis_argument_spec(),
                                 use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("point"))) {
    abort(glue("Can't convert 'sfc' object to QGIS type '{ spec$qgis_type }'"))
  }

  if (isTRUE(length(x) != 1)) {
    abort(glue("Can't convert 'sfc' object to QGIS type '{ spec$qgis_type }' as the length is not equal to 1"))
  }

  if (isTRUE((sf::st_geometry_type(x) != "POINT"))) {
    abort(glue("Can't convert 'sfc' object to QGIS type '{ spec$qgis_type }' as type is not 'POINT'"))
  }

  if (!is.na(sf::st_crs(x)$epsg)) {
    glue("{x[[1]][1]},{x[[1]][2]}[EPSG:{sf::st_crs(x)$epsg}]")
  } else {
    glue("{x[[1]][1]},{x[[1]][2]}")
  }
}

#' @keywords internal
#' @export
as_qgis_argument.POINT <- function(x, spec = qgis_argument_spec(),
                                   use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("point"))) {
    abort(glue("Can't convert 'POINT' object to QGIS type '{ spec$qgis_type }'"))
  }

  glue("{x[1]},{x[2]}")
}
