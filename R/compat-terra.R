#' Convert a qgis_result object or one of its elements to a terra object
#'
#' This function performs coercion to one of the terra classes
#' `SpatRaster`, `SpatVector` or `SpatVectorProxy` (add `proxy = TRUE` for the
#' latter).
#' The distinction between `SpatRaster` and `SpatVector` is based on the
#' output type.
#'
#' @family topics about coercing processing output
#' @family topics about accessing or managing processing results
#'
#' @param ... Arguments passed to [terra::rast()] or [terra::vect()], depending
#' on the output type of `x` (or one of its elements, if `x` is a
#' `qgis_result`).
#' @inheritParams qgis_as_raster
#'
#' @returns A `SpatRaster`, `SpatVector` or `SpatVectorProxy` object.
#'
#' @examplesIf has_qgis() && requireNamespace("terra", quietly = TRUE)
#' \donttest{
#' # not running below examples in R CMD check to save time
#' result <- qgis_run_algorithm(
#'   "native:slope",
#'   INPUT = system.file("longlake/longlake_depth.tif", package = "qgisprocess")
#' )
#'
#' # most direct approach, autoselecting a `qgis_outputRaster` type
#' # output from the `result` object and reading as SpatRaster:
#' qgis_as_terra(result)
#'
#' # if you need more control, extract the needed output element first:
#' output_raster <- qgis_extract_output(result, "OUTPUT")
#' qgis_as_terra(output_raster)
#'
#' # Same holds for coercion to SpatVector
#' result2 <- qgis_run_algorithm(
#'   "native:buffer",
#'   INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
#'   DISTANCE = 100
#' )
#'
#' qgis_as_terra(result2)
#' output_vector <- qgis_extract_output(result2, "OUTPUT")
#' qgis_as_terra(output_vector)
#'
#' # SpatVectorProxy:
#' qgis_as_terra(result2, proxy = TRUE)
#' }
#'
#' @name qgis_as_terra

#' @rdname qgis_as_terra
#' @export
qgis_as_terra <- function(x, ...) {
  UseMethod("qgis_as_terra")
}

#' @rdname qgis_as_terra
#' @export
qgis_as_terra.qgis_outputRaster <- function(x, ...) {
  terra::rast(unclass(x), ...)
}

#' @rdname qgis_as_terra
#' @export
qgis_as_terra.qgis_outputLayer <- function(x, ...) {
  tryCatch(
    terra::rast(unclass(x), ...),
    error = function(e) {
      qgis_as_spatvector(x, ...)
    },
    warning = function(w) {
      if (!grepl("not recognized as a supported file format", w)) {
        warning(w)
      }
      qgis_as_spatvector(x, ...)
    }
  )
}

#' @rdname qgis_as_terra
#' @export
qgis_as_terra.qgis_outputVector <- function(x, ...) {
  qgis_as_spatvector(x, ...)
}

#' @rdname qgis_as_terra
#' @export
qgis_as_terra.qgis_result <- function(x, ...) {
  result <- qgis_extract_output_by_class(
    x,
    c("qgis_outputRaster", "qgis_outputVector", "qgis_outputLayer")
  )
  qgis_as_terra(result, ...)
}

#' @keywords internal
qgis_as_spatvector <- function(x, ...) {
  if (grepl("\\|layer", unclass(x))) {
    output_splitted <- strsplit(unclass(x), "\\|layer.*=")[[1]]
    terra::vect(output_splitted[1], output_splitted[2], ...)
  } else {
    terra::vect(unclass(x), ...)
  }
}


# @param x A [terra::rast()].
#' @keywords internal
#' @export
as_qgis_argument.SpatRaster <- function(x, spec = qgis_argument_spec(),
                                        use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("raster", "layer", "multilayer"))) {
    abort(glue("Can't convert '{ class(x)[1] }' object to QGIS type '{ spec$qgis_type }'"))
  }

  if (terra::nlyr(x) > 1L && spec$qgis_type == "multilayer") {
    warning("You passed a multiband SpatRaster object as one of the layers for a multilayer argument.\n",
      "It is expected that only the first band will be used by QGIS!\n",
      "If you need each band to be processed, you need to extract the bands and pass them as ",
      "separate layers to the algorithm (either by repeating the argument, or by wrapping ",
      "in qgis_list_input()).",
      call. = FALSE
    )
  }

  # try to use a filename if present (behaviour changed around terra 1.5.12)
  sources <- terra::sources(x)
  if (!is.character(sources)) {
    sources <- sources$source
  }

  if (!identical(sources, "") && identical(length(sources), 1L)) {
    accepted_ext <- c("grd", "asc", "sdat", "rst", "nc", "tif", "tiff", "gtiff", "envi", "bil", "img")
    file_ext <- stringr::str_to_lower(tools::file_ext(sources))
    if (file_ext %in% accepted_ext) {
      reread <- terra::rast(sources)
      names_match <- identical(names(x), names(reread))
      crs_match <- identical(terra::crs(x), terra::crs(reread))
      if (names_match && crs_match) {
        return(sources)
      } else if (!crs_match) {
        message(glue(
          "Rewriting the SpatRaster object as a temporary file before passing to QGIS, since ",
          "its CRS has been set to another value than that in the source file '{ sources }'."
        ))
      } else if (terra::nlyr(x) > 1L) {
        message(glue(
          "Rewriting the multi-band SpatRaster object as a temporary file before passing to QGIS, since ",
          "its bands (names, order, selection) differ from those in the source file '{ sources }'."
        ))
      } else {
        message(glue(
          "Rewriting the '{ names(x) }' band of '{ sources }' as a temporary file, otherwise ",
          "QGIS may use another or all bands of the source file if ",
          "passing its filepath."
        ))
      }
    }
  }

  path <- qgis_tmp_raster()
  terra::writeRaster(x, path)
  structure(path, class = "qgis_tempfile_arg")
}



#' @keywords internal
#' @export
as_qgis_argument.SpatVector <- function(x, spec = qgis_argument_spec(),
                                        use_json_input = FALSE) {
  as_qgis_argument_terra_vector(x, spec, use_json_input)
}


#' @keywords internal
#' @export
as_qgis_argument.SpatVectorProxy <- function(x, spec = qgis_argument_spec(),
                                             use_json_input = FALSE) {
  as_qgis_argument_terra_vector(x, spec, use_json_input)
}


#' @keywords internal
as_qgis_argument_terra_vector <- function(x,
                                          spec = qgis_argument_spec(),
                                          use_json_input = FALSE) {
  class <- class(x)[1]

  if (!isTRUE(spec$qgis_type %in% c("source", "layer", "vector", "multilayer", "point"))) {
    abort(glue("Can't convert '{ class }' object to QGIS type '{ spec$qgis_type }'"))
  }

  # try to use a filename if present
  sources <- terra::sources(x)
  if (!is.character(sources)) {
    sources <- sources$source
  }
  if (
    !identical(sources, "") &&
      identical(length(sources), 1L) &&
      !identical(spec$qgis_type, "point")
  ) {
    # rewrite if attribute names differ from source:
    if (grepl("::", sources)) {
      chunks <- strsplit(sources, "::")[[1]]
      proxy <- terra::vect(chunks[1], chunks[2], proxy = TRUE)
      source_names <- names(proxy)
    } else {
      proxy <- terra::vect(sources, proxy = TRUE)
      source_names <- names(proxy)
    }
    if (!identical(names(x), source_names)) {
      message(glue(
        "Rewriting the {class} object as a temporary file before passing to QGIS, since ",
        "its attribute names (including order, selection) differ from those in the source file '{ sources }'."
      ))
      # rewrite if CRS differs (terra source reference is kept if CRS is reset,
      # not if data is transformed):
    } else if (!identical(terra::crs(x), terra::crs(proxy))) {
      message(glue(
        "Rewriting the {class} object as a temporary file before passing to QGIS, since ",
        "its CRS has been set to another value than that in the source file '{ sources }'."
      ))
    } else {
      return(sub("::", "|layername=", sources))
    }
  }

  if (identical(spec$qgis_type, "point")) {
    assert_that(
      identical(terra::geomtype(x), "points"), # is.points() not defined for proxy
      identical(nrow(x), 1),
      msg = glue(
        "QGIS argument type 'point' can take a {class} object, but it must ",
        "have exactly one row and the geometry must be a point."
      )
    )
    crs_code <- as.character(terra::crs(x, describe = TRUE)[, c("authority", "code")])
    if (inherits(x, "SpatVectorProxy")) {
      x <- terra::query(x, n = 1)
    }
    coord <- terra::geom(x)[1, c("x", "y")]
    if (!any(is.na(crs_code))) {
      return(glue("{coord[1]},{coord[2]}[{crs_code[1]}:{crs_code[2]}]"))
    } else {
      return(glue("{coord[1]},{coord[2]}"))
    }
  }

  # (re)write to file
  if (inherits(x, "SpatVectorProxy")) {
    x <- terra::query(x)
  }
  path <- qgis_tmp_vector()
  terra::writeVector(x, path)
  structure(path, class = "qgis_tempfile_arg")
}



#' @keywords internal
#' @export
as_qgis_argument.SpatExtent <- function(x, spec = qgis_argument_spec(),
                                        use_json_input = FALSE) {
  if (!isTRUE(spec$qgis_type %in% c("extent"))) {
    abort(glue("Can't convert 'SpatExtent' object to QGIS type '{ spec$qgis_type }'"))
  }

  ex <- as.vector(terra::ext(x))

  glue("{ex[['xmin']]},{ex[['xmax']]},{ex[['ymin']]},{ex[['ymax']]}")
}
