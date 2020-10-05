#' @export
qgis_detect_win <- function(quiet = FALSE,
                            drive_letter = stringr::str_split(R.home(), ":")[[1]][1],
                            .prefer_dev = FALSE) {


  bat_files <- c("qgis_process-qgis.bat", "qgis_process-qgis-dev.bat")

  if (.prefer_dev) {
    bat_files <- rev(bat_files)
  }

  posssible_locs_win_df <- expand.grid(
    qgis = c(
      list.files(glue::glue("{ drive_letter }:/Program Files"), "QGIS*", full.names = TRUE),
      file.path(glue::glue("{ drive_letter }:/"), "OSGeo4W64"),
      file.path(glue::glue("{ drive_letter }:/"), "OSGeo4W")
    ),
    file = file.path("bin", bat_files)
  )

  possible_locs_win <- file.path(posssible_locs_win_df$qgis, posssible_locs_win_df$file)
  possible_locs_win <- possible_locs_win[file.exists(possible_locs_win)]

  if (length(possible_locs_win) == 0) {
    stop("No QGIS installation containing 'qgis_process' found!", call. = FALSE)
  }

  for (path in possible_locs_win) {
    if (!quiet) message(glue::glue("Trying '{ path }'"))
    result <- try(qgisprocess::qgis_run(path = path), silent = TRUE)
    if (!inherits(result, "try-error")) {
      return(path)
    }
  }

  stop("QGIS installation found, but all candidate paths failed to execute.", call. = FALSE)
}
