
# nocov start

.onLoad <- function(...) {
  qgis_configure(quiet = TRUE)

  vctrs::s3_register("sf::st_as_sf", "qgis_result")
  vctrs::s3_register("stars::st_as_stars", "qgis_result")
  vctrs::s3_register("stars::st_as_stars", "qgis_outputLayer")
  vctrs::s3_register("stars::st_as_stars", "qgis_outputRaster")

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
      glue::glue(
        "Using 'qgis_process' at '{ qgis_path() }'.",
        "QGIS version: { qgis_version() }",
        "Metadata of { nrow(qgis_algorithms()) } algorithms successfully cached.",
        "Run `qgis_configure()` for details.",
        .sep = "\n"
      )
    )
  } else {
    packageStartupMessage(
      "The 'qgis_process' command-line utility was not found.\nRun `qgis_configure()` for details."
    )
  }
}

# nocov end
