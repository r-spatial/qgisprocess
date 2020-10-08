
.onLoad <- function(...) {
  qgis_configure(quiet = TRUE)

  # create package temporary directory
  qgis_tmp_dir_location <<- tempfile()
  dir.create(qgis_tmp_dir_location)
}

.onUnload <- function(...) {
  # cleanup package temporary directory
  unlink(qgis_tmp_dir_location, recursive = TRUE)
}

.onAttach <- function(...) {
  if (has_qgis()) {
    packageStartupMessage(
      glue::glue("Using 'qgis_process' at '{ qgis_path() }'.\nRun `qgis_configure()` for details.")
    )
  } else {
    packageStartupMessage(
      "The 'qgis_process' command-line utility was not found.\nRun `qgis_configure()` for details."
    )
  }
}
