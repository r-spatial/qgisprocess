#' Detect QGIS installations with 'qgis_process'
#'
#' @param drive_letter The drive letter on which to search. By default,
#'   this is the same drive letter as the R executable.
#'
#' @return A character vector of possible paths to the QGIS executable.
#' @export
#'
#' @examples
#' if (is_windows()) qgis_detect_windows()
#' if (is_macos()) qgis_detect_macos()
#'
qgis_detect_windows <- function(drive_letter = strsplit(R.home(), ":")[[1]][1]) {
  if (!is_windows()) {
    abort("Can't use `qgis_detect_windows()` on a non-windows platform.")
  }

  bat_files <- c(
    "qgis_process-qgis.bat",
    "qgis_process-qgis-ltr.bat",
    "qgis_process-qgis-dev.bat"
  )

  posssible_locs_win_df <- expand.grid(
    qgis = c(
      list.files(glue("{ drive_letter }:/Program Files"), "QGIS*", full.names = TRUE),
      file.path(glue("{ drive_letter }:/"), "OSGeo4W64"),
      file.path(glue("{ drive_letter }:/"), "OSGeo4W")
    ),
    file = file.path("bin", bat_files)
  )

  possible_locs_win <- file.path(posssible_locs_win_df$qgis, posssible_locs_win_df$file)
  possible_locs_win[file.exists(possible_locs_win)]
}

#' @rdname qgis_detect_windows
#' @export
qgis_detect_macos <- function() {
  if (!is_macos()) {
    abort("Can't use `qgis_detect_macos()` on a non-MacOS platform.")
  }

  possible_locs_mac <- file.path(
    list.files("/Applications", "QGIS*", full.names = TRUE),
    "Contents/MacOS/bin/qgis_process"
  )

  possible_locs_mac[file.exists(possible_locs_mac)]
}

#' @rdname qgis_detect_windows
#' @export
is_macos <- function() {
  (.Platform$OS.type == "unix") &&
    identical(unname(Sys.info()["sysname"]), "Darwin")
}

#' @rdname qgis_detect_windows
#' @export
is_windows <- function() {
  .Platform$OS.type == "windows"
}
