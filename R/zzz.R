
.onLoad <- function(...) {
  qgis_configure(quiet = TRUE)
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
