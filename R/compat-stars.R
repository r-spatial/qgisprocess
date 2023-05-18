#' Convert a qgis_result object or one of its elements to a stars object
#'
#' @note Just use `st_as_stars()` in R scripts, it will use the correct
#' method.
#'
#' @family topics about coercing processing output
#' @family topics about accessing or managing processing results
#'
#' @param .x A result from [qgis_run_algorithm()] or one of the
#' [qgis_extract_output()] functions.
#' @param ... Arguments passed to [stars::read_stars()].
#'
#' @name st_as_stars

#' @rdname st_as_stars
# dynamically registered in zzz.R
st_as_stars.qgis_outputRaster <- function(.x, ...) {
  stars::read_stars(unclass(.x), ...)
}

#' @rdname st_as_stars
# dynamically registered in zzz.R
st_as_stars.qgis_outputLayer <- function(.x, ...) {
  stars::read_stars(unclass(.x), ...)
}

#' @rdname st_as_stars
# dynamically registered in zzz.R
st_as_stars.qgis_result <- function(.x, ...) {
  result <- qgis_extract_output_by_class(.x, c("qgis_outputRaster", "qgis_outputLayer"))
  stars::read_stars(unclass(result), ...)
}


# @param x A stars or stars_proxy object.
#' @keywords internal
#' @export
as_qgis_argument.stars <- function(x, spec = qgis_argument_spec(),
                                   use_json_input = FALSE) {
  as_qgis_argument_stars(x, spec, use_json_input)
}

#' @keywords internal
#' @export
as_qgis_argument.stars_proxy <- function(x, spec = qgis_argument_spec(),
                                         use_json_input = FALSE) {
  as_qgis_argument_stars(x, spec, use_json_input)
}

#' @keywords internal
as_qgis_argument_stars <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("raster", "layer", "multilayer"))) {
    abort(glue("Can't convert '{ class(x)[1] }' object to QGIS type '{ spec$qgis_type }'"))
  }

  # try to use a filename if present
  if (inherits(x, "stars_proxy") && (length(x) == 1)) {
    file <- unclass(x)[[1]]
    file_ext <- stringr::str_to_lower(tools::file_ext(file))
    if (file_ext %in% c("grd", "asc", "sdat", "rst", "nc", "tif", "tiff", "gtiff", "envi", "bil", "img")) {
      return(file)
    }
  }

  path <- qgis_tmp_raster()
  stars::write_stars(x, path)
  structure(path, class = "qgis_tempfile_arg")
}
