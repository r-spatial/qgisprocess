#' List, enable or disable QGIS plugins
#'
#' `qgis_plugins()` lists the available plugins that implement Processing
#' providers.
#' `qgis_enable_plugins()` enables plugins while `qgis_disable_plugins()`
#' does the reverse.
#'
#' The cache is immediately updated upon enabling or disabling plugins from R.
#'
#' If you installed, removed, enabled or disabled plugins in the QGIS GUI, then
#' run `qgis_configure()` to make those changes available in R.
#'
#' @note
#' Only plugins that implement Processing providers are supported.
#' Installing or removing plugins is not supported.
#'
#' @inheritParams qgis_run
#' @param which String defining which plugins to enlist, based on their
#' status in QGIS (enabled or disabled).
#' Must be one of: `"all"`, `"enabled"`, `"disabled"`.
#'
#' @return
#' A tibble of plugins and their status.
#' @export
qgis_plugins <- function(
    which = "all",
    query = FALSE,
    quiet = TRUE
) {

  stopifnot(which %in% c("all", "enabled", "disabled"))

  if (query) {
    qgisprocess_cache$plugins <- qgis_query_plugins(quiet = quiet)
  }

  plugins <- qgisprocess_cache$plugins

  if (!quiet && query) message(glue(
    "{ nrow(plugins[plugins$enabled, ]) } out of { nrow(plugins) } ",
    "available processing provider plugins are enabled."
  ))

  if (!quiet && !query) message(
    "Reading plugin list from the qgisprocess cache.\n",
    "  If you changed plugin availability or status in the QGIS GUI\n",
    "  since you loaded the qgisprocess package,\n",
    "  then you must run `qgis_configure()` or reload the package\n",
    "  to capture these changes."
  )

  switch(
    which,
    "all" = plugins,
    "enabled" = plugins[plugins$enabled, ],
    "disabled" = plugins[!plugins$enabled, ]
  )
}



#' @keywords internal
qgis_query_plugins <- function(quiet = FALSE) {

  if (qgis_use_json_output()) {

    out <- qgis_run(args = c("plugins", "--json"))$stdout
    pluginlist <- jsonlite::fromJSON(out)$plugins
    plugins <- tibble::enframe(pluginlist)
    plugins$value <- unlist(plugins$value, use.names = FALSE)
    colnames(plugins) <- c("name", "enabled")

  } else {

    lines <- readLines(textConnection(qgis_run("plugins")$stdout))
    pluginvec <- stringr::str_extract(lines, "^\\*?\\s+\\w+")
    pluginvec <- pluginvec[!is.na(pluginvec)]
    plugins <- tibble::tibble(
      name = stringr::str_match(pluginvec, "^\\*?\\s+(\\w+)")[, 2],
      enabled = stringr::str_detect(pluginvec, "^\\*")
    )

  }

  return(plugins)
}





#' @keywords internal
message_disabled_plugins <- function(plugins, prepend_newline = FALSE) {
  if (!identical(nrow(plugins[plugins$enabled, ]), nrow(plugins))) {
    if(prepend_newline) message()
    message(glue(
      '==> Run `qgis_enable_plugins()` to enable ',
      '{ nrow(plugins[!plugins$enabled, ]) } disabled ',
      'plugins and access their algorithms: ',
      '{ paste(plugins$name[!plugins$enabled], collapse = ", ") }'
    ))
  }
}



