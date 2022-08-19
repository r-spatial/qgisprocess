
# nocov start

.onLoad <- function(...) {
  qgis_configure(quiet = TRUE, use_cached_data = TRUE)

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
        "Using 'qgis_process' at '{ qgis_path() }'.\n",
        "QGIS version: { qgis_version() }\n",
        if (is.null(qgisprocess_cache$loaded_from)) {
          paste0(
            "Metadata of { nrow(qgis_algorithms()) } algorithms successfully cached.\n",
            "Run `qgis_algorithms()` to see them.\n"
          )
        } else {
          paste0(
            "Configuration loaded from '{ qgisprocess_cache$loaded_from }'\n",
            "Run `qgis_configure(use_cached_data = TRUE)` to reload cache and get more details.\n"
          )
        },
        ">>> If you need another installed QGIS version, run `qgis_configure()`; see ",
        "its documentation if you need to preset the path of qgis_process.\n",
        if (qgis_use_json_input()) "- Using JSON for input serialization.\n" else "",
        if (qgis_use_json_output()) "- Using JSON for output serialization.\n" else "",
        .sep = ""
      )
    )
  } else {
    packageStartupMessage(
      "The 'qgis_process' command-line utility was not found.\nRun `qgis_configure()` for details."
    )
  }
}

# nocov end
