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
      glue(
        "QGIS version: { qgis_version() }\n",
        "Having access to { nrow(qgis_algorithms()) } algorithms ",
        "from { nrow(qgis_providers()) } QGIS processing providers.\n",
        "Run `qgis_configure(use_cached_data = TRUE)` to reload cache and get more details.\n",
        .sep = ""
      )
    )
    message_disabled_plugins(qgisprocess_cache$plugins, startup = TRUE)
  } else {
    packageStartupMessage(
      "\n>>> PROBLEM encountered: couldn't build package cache! <<<\n",
      "The 'qgis_process' command-line utility was either not found or\n",
      "did not fulfil the needs to build the package cache.\n",
      "Please run `qgis_configure()` to fix this and rebuild the cache.\n",
      "See its documentation if you need to preset the path of qgis_process.\n",
      "If the problem persists, make sure that you correctly installed QGIS\n",
      "for your operating system using the instructions at\n",
      "https://download.qgis.org."
    )
  }
}

# nocov end
