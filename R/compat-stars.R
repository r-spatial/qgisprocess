#' Convert a qgis_result object or one of its elements to a stars object
#'
#' @details
#' The stars package must be loaded explicitly to use these methods.
#'
#' @note Just use `st_as_stars()` in R scripts, it will use the correct
#' method.
#'
#' @family topics about coercing processing output
#' @family topics about accessing or managing processing results
#'
#' @param ... Arguments passed to [stars::read_stars()].
#' @inheritParams qgis_as_raster
#'
#' @returns A `stars` or a `stars_proxy` object.
#'
#' @examplesIf has_qgis() && requireNamespace("stars", quietly = TRUE)
#' \donttest{
#' # not running below examples in R CMD check to save time
#' result <- qgis_run_algorithm(
#'   "native:slope",
#'   INPUT = system.file("longlake/longlake_depth.tif", package = "qgisprocess")
#' )
#'
#' # most direct approach, autoselecting a `qgis_outputRaster` type
#' # output from the `result` object and reading as stars or stars_proxy:
#' stars::st_as_stars(result)
#' stars::st_as_stars(result, proxy = TRUE)
#'
#' # if you need more control, extract the needed output element first:
#' output_raster <- qgis_extract_output(result, "OUTPUT")
#' stars::st_as_stars(output_raster)
#' }
#'
#' @name st_as_stars

#' @rdname st_as_stars
#' @exportS3Method stars::st_as_stars
st_as_stars.qgis_outputRaster <- function(x, ...) {
  stars::read_stars(unclass(x), ...)
}

#' @rdname st_as_stars
#' @exportS3Method stars::st_as_stars
st_as_stars.qgis_outputLayer <- function(x, ...) {
  stars::read_stars(unclass(x), ...)
}

#' @rdname st_as_stars
#' @exportS3Method stars::st_as_stars
st_as_stars.qgis_result <- function(x, ...) {
  result <- qgis_extract_output_by_class(x, c("qgis_outputRaster", "qgis_outputLayer"))
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

  if (!is.na(dim(x)["band"]) &&
    dim(x)["band"] > 1L &&
    spec$qgis_type == "multilayer") {
    warning("You passed a multiband stars object as one of the layers for a multilayer argument.\n",
      "It is expected that only the first band will be used by QGIS!\n",
      "If you need each band to be processed, you need to extract the bands and pass them as ",
      "separate layers to the algorithm (either by repeating the argument, or by wrapping ",
      "in qgis_list_input()).",
      call. = FALSE
    )
  }

  # try to use a filename if present
  if (inherits(x, "stars_proxy") && (length(x) == 1)) {
    file <- unclass(x)[[1]]
    accepted_ext <- c("grd", "asc", "sdat", "rst", "nc", "tif", "tiff", "gtiff", "envi", "bil", "img")
    file_ext <- stringr::str_to_lower(tools::file_ext(file))
    if (file_ext %in% accepted_ext) {
      # single-band case that normally originates from single-band data source:
      if (is.na(dim(x)["band"])) {
        if (is.na(dim(stars::read_stars(file, proxy = TRUE))["band"]) ||
          dim(stars::read_stars(file, proxy = TRUE))["band"] == 1L) {
          return(file)
          # non-matching bands:
        } else {
          message(glue(
            "Rewriting the single-band stars object as a temporary file before passing to QGIS, since ",
            "the number of bands is larger in the source file '{ file }'."
          ))
          return(write_stars_as_tempfile_arg(x))
        }
      }
      # we can only check for total band number; they have no names in stars
      stars_ok <- utils::packageVersion("stars") >= as.package_version("0.6-0")
      nrbands_match <- identical(
        dim(x)["band"],
        dim(stars::read_stars(file, proxy = TRUE))["band"]
      ) && # stars < 0.6-0 has different implementation of band selection in stars_proxy
        (stars_ok || is.null(attr(x, "call_list")) || dim(x)["band"] == 1L)
      if (nrbands_match) {
        return(file)
      } else {
        message(glue(
          "Rewriting the stars object as a temporary file before passing to QGIS, since ",
          "the number of bands { ifelse(stars_ok, 'differs', 'may differ') } ",
          "from those in the source file '{ file }'."
        ))
        return(write_stars_as_tempfile_arg(x))
      }
    }
  }
  write_stars_as_tempfile_arg(x)
}

#' @keywords internal
write_stars_as_tempfile_arg <- function(x) {
  path <- qgis_tmp_raster()
  stars::write_stars(x, path)
  structure(path, class = "qgis_tempfile_arg")
}
