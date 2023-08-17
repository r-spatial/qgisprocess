#' Convert a qgis_result object or one of its elements to a raster object
#'
#' @family topics about coercing processing output
#' @family topics about accessing or managing processing results
#'
#' @param x A `qgis_result` object from [qgis_run_algorithm()] or a
#' `qgis_output*` object from one of the [qgis_extract_output()] functions.
#' @param ... Arguments passed to [raster::raster()] or [raster::brick()].
#'
#' @returns A `RasterLayer` or a `RasterBrick` object.
#'
#' @examplesIf has_qgis() && requireNamespace("raster", quietly = TRUE)
#' \donttest{
#' # not running below examples in R CMD check to save time
#' result <- qgis_run_algorithm(
#'   "native:slope",
#'   INPUT = system.file("longlake/longlake_depth.tif", package = "qgisprocess")
#' )
#'
#' # most direct approach, autoselecting a `qgis_outputRaster` type
#' # output from the `result` object and reading as RasterLayer:
#' qgis_as_raster(result)
#'
#' # if you need more control, extract the needed output element first:
#' output_raster <- qgis_extract_output(result, "OUTPUT")
#' qgis_as_raster(output_raster)
#' }
#'
#' @name qgis_as_raster

#' @rdname qgis_as_raster
#' @export
qgis_as_raster <- function(x, ...) {
  UseMethod("qgis_as_raster")
}

#' @rdname qgis_as_raster
#' @export
qgis_as_brick <- function(x, ...) {
  UseMethod("qgis_as_brick")
}

#' @rdname qgis_as_raster
#' @export
qgis_as_raster.qgis_outputRaster <- function(x, ...) {
  raster::raster(unclass(x), ...)
}

#' @rdname qgis_as_raster
#' @export
qgis_as_brick.qgis_outputRaster <- function(x, ...) {
  raster::brick(unclass(x), ...)
}

#' @rdname qgis_as_raster
#' @export
qgis_as_raster.qgis_outputLayer <- function(x, ...) {
  raster::raster(unclass(x), ...)
}

#' @rdname qgis_as_raster
#' @export
qgis_as_brick.qgis_outputLayer <- function(x, ...) {
  raster::brick(unclass(x), ...)
}

#' @rdname qgis_as_raster
#' @export
qgis_as_raster.qgis_result <- function(x, ...) {
  result <- qgis_extract_output_by_class(x, c("qgis_outputRaster", "qgis_outputLayer"))
  raster::raster(unclass(result), ...)
}

#' @rdname qgis_as_raster
#' @export
qgis_as_brick.qgis_result <- function(x, ...) {
  result <- qgis_extract_output_by_class(x, c("qgis_outputRaster", "qgis_outputLayer"))
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

  if (raster::nlayers(x) > 1L && spec$qgis_type == "multilayer") {
    warning("You passed a multiband RasterBrick object as one of the layers for a multilayer argument.\n",
      "It is expected that only the first band will be used by QGIS!\n",
      "If you need each band to be processed, you need to extract the bands and pass them as ",
      "separate layers to the algorithm (either by repeating the argument, or by wrapping ",
      "in qgis_list_input()).",
      call. = FALSE
    )
  }

  # try to use a filename if present
  file <- raster::filename(x)
  if (file != "") {
    accepted_ext <- c("grd", "asc", "sdat", "rst", "nc", "tif", "tiff", "gtiff", "envi", "bil", "img")
    file_ext <- stringr::str_to_lower(tools::file_ext(file))
    if (file_ext %in% accepted_ext) {
      names_match <- identical(names(x), names(raster::brick(file)))
      if (names_match) {
        return(file)
      } else if (raster::nlayers(x) > 1L) {
        message(glue(
          "Rewriting the multi-band RasterBrick object as a temporary file before passing to QGIS, since ",
          "its bands (names, order, selection) differ from those in the source file '{ file }'."
        ))
      } else {
        message(glue(
          "Rewriting the '{ names(x) }' band of '{ file }' as a temporary file, otherwise ",
          "QGIS may use another or all bands of the source file if ",
          "passing its filepath."
        ))
      }
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
