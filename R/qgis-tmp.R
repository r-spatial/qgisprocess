#' Manage temporary files
#'
#' These functions create temporary files that can be used
#' in calls to [qgis_run_algorithm()] or elsewhere. These
#' files are created in a special temporary directory
#' ([qgis_tmp_base()]) that should be periodically cleaned up
#' using [qgis_clean_tmp()]. You can set your preferred
#' vector and/or raster file extension using
#' `options(qgisprocess.tmp_vector_ext = "...")` and/or
#' `options(qgisprocess.tmp_raster_ext = "...")`, respectively.
#'
#' @family topics about programming or debugging utilities
#'
#' @param ext The file extension to be used.
#'
#' @returns A character vector indicating the location of a
#'   (not yet created) temporary file.
#' @export
#'
#' @examples
#' qgis_tmp_base()
#' qgis_tmp_file(".csv")
#' qgis_tmp_vector()
#' qgis_tmp_raster()
#'
qgis_tmp_file <- function(ext) {
  tempfile(tmpdir = qgis_tmp_base(), fileext = ext)
}

# @param x A character vector of file names.
#' @keywords internal
is_qgis_tmp_file <- function(x) {
  startsWith(x, qgis_tmp_base()) & (x != qgis_tmp_base())
}

#' @rdname qgis_tmp_file
#' @export
qgis_tmp_folder <- function() {
  qgis_tmp_file("")
}

#' @rdname qgis_tmp_file
#' @export
qgis_tmp_vector <- function(ext = getOption("qgisprocess.tmp_vector_ext", ".gpkg")) {
  qgis_tmp_file(ext)
}

#' @rdname qgis_tmp_file
#' @export
qgis_tmp_raster <- function(ext = getOption("qgisprocess.tmp_raster_ext", ".tif")) {
  qgis_tmp_file(ext)
}

#' @rdname qgis_tmp_file
#' @export
qgis_tmp_base <- function() {
  qgisprocess_internal_obj$qgis_tmp_dir_location
}

#' @rdname qgis_tmp_file
#' @export
qgis_clean_tmp <- function() {
  unlink(qgisprocess_internal_obj$qgis_tmp_dir_location, recursive = TRUE)
  dir.create(qgisprocess_internal_obj$qgis_tmp_dir_location)
}

# Create environment to store temporary directory path of qgisprocess.
# Its value is set at load time in zzz.R
qgisprocess_internal_obj <- new.env(parent = emptyenv())
qgisprocess_internal_obj$qgis_tmp_dir_location <- NULL
