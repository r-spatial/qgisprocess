#' Enable or disable QGIS plugins
#'
#' Processing plugins, installed in QGIS, can be in an 'enabled' or 'disabled'
#' state in QGIS.
#' The plugin state can be controlled from R.
#' `qgis_enable_plugins()` enables plugins while `qgis_disable_plugins()`
#' does the reverse.
#'
#' The cache is immediately updated upon enabling or disabling plugins from R.
#'
#' Run [qgis_plugins()] to list the available plugins that implement processing
#' providers.
#'
#' If you installed, removed, enabled or disabled plugins in the QGIS GUI, then
#' run [qgis_configure()] to make those changes available in R.
#'
#' If `names` is not provided to `qgis_enable_plugins()`, it is assumed that all
#' _disabled_ plugins are to be enabled.
#' If `names` is not provided to `qgis_disable_plugins()`, it is assumed that all
#' _enabled_ plugins are to be disabled.
#' Note that the 'processing' plugin is ignored, because it is always available
#' to 'qgis_process' (not QGIS though).
#'
#' @note
#' Only plugins that implement processing providers are supported.
#' Installing or removing plugins is not supported.
#'
#' @seealso [qgis_plugins()]
#' @family topics about configuring QGIS and qgisprocess
#' @concept functions to manage and explore QGIS and qgisprocess
#'
#' @inheritParams qgis_path
#' @param names Optional character vector of plugin names.
#'
#' @returns A tibble of plugins, invisibly.
#'
#' @examplesIf has_qgis()
#' qgis_enable_plugins("name_of_plugin")
#'
#' @name qgis_enable_plugins


#' @rdname qgis_enable_plugins
#' @export
qgis_enable_plugins <- function(names = NULL, quiet = FALSE) {
  handle_plugins(names = names, quiet = quiet, mode = "enable")
}


#' @rdname qgis_enable_plugins
#' @export
qgis_disable_plugins <- function(names = NULL, quiet = FALSE) {
  handle_plugins(names = names, quiet = quiet, mode = "disable")
}


#' @export
#' @rdname qgis_algorithms
qgis_plugins <- function(
    which = "all",
    query = FALSE,
    quiet = TRUE,
    ...) {
  assert_that(which %in% c("all", "enabled", "disabled"))
  assert_that(is.flag(query), !is.na(query))
  assert_that(is.flag(quiet), !is.na(quiet))

  if (!("msg" %in% names(list(...)))) msg <- TRUE else msg <- list(...)$msg[[1]]
  assert_that(is.flag(msg), !is.na(msg))

  if (query) {
    qgisprocess_cache$plugins <- qgis_query_plugins(quiet = quiet)
  }

  plugins <- qgisprocess_cache$plugins

  if (!quiet && msg && !query) message(
    "Reading plugin list from the qgisprocess cache.\n",
    "  If you changed plugin availability or status in the QGIS GUI ",
    "since you loaded the\n",
    "  qgisprocess package, then you must run `qgis_configure()` or reload ",
    "the package\n  to capture these changes."
  )

  if (!quiet) message(glue(
    "{ sum(plugins$enabled) } out of { nrow(plugins) } ",
    "available processing provider plugins are enabled."
  ))

  switch(which,
    "all" = plugins,
    "enabled" = plugins[plugins$enabled, ],
    "disabled" = plugins[!plugins$enabled, ]
  )
}



#' @keywords internal
qgis_query_plugins <- function(quiet = FALSE) {
  arg_skip_loading <- arg_skip_loading_plugins()
  if (qgis_using_json_output()) {
    result <- qgis_run(args = c("plugins", "--json", arg_skip_loading))
    if (nchar(result$stderr) > 0L) {
      message(
        "\nStandard error message from 'qgis_process':\n",
        result$stderr,
        "\n"
      )
    }
    pluginlist <- jsonlite::fromJSON(result$stdout)$plugins
    plugins <- tibble::enframe(pluginlist)
    plugins$value <- unlist(plugins$value, use.names = FALSE)
    colnames(plugins) <- c("name", "enabled")
  } else {
    result <- qgis_run(args = c("plugins", arg_skip_loading))
    if (nchar(result$stderr) > 0L) {
      message(
        "\nStandard error message from 'qgis_process':\n",
        result$stderr,
        "\n"
      )
    }
    lines <- readLines(textConnection(result$stdout))
    pluginvec <- stringr::str_extract(lines, "^\\*?\\s+\\w+")
    pluginvec <- pluginvec[!is.na(pluginvec)]
    plugins <- tibble::tibble(
      name = stringr::str_match(pluginvec, "^\\*?\\s+(\\w+)")[, 2],
      enabled = stringr::str_detect(pluginvec, "^\\*")
    )
  }

  plugins
}




#' @keywords internal
arg_skip_loading_plugins <- function(algorithm = NULL) {
  if (!is.null(algorithm) && !algorithm_is_native(algorithm)) {
    return(NULL)
  }
  if (package_version(qgis_version(full = FALSE)) >= "3.36.0") {
    "--skip-loading-plugins"
  } else NULL
}




#' @keywords internal
message_disabled_plugins <- function(
    plugins,
    prepend_newline = FALSE,
    startup = FALSE) {
  if (!identical(sum(plugins$enabled), nrow(plugins))) {
    if (prepend_newline && !startup) message()
    n_dis <- sum(!plugins$enabled)
    msg <- glue(
      'Run `qgis_enable_plugins()` to enable ',
      '{n_dis} disabled ',
      'plugin{ ifelse(n_dis > 1, "s", "") } ',
      'and access { ifelse(n_dis > 1, "their", "its") } algorithms: ',
      '{ paste(plugins$name[!plugins$enabled], collapse = ", ") }'
    )
    msg <- paste0(
      strwrap(msg, prefix = "    ", initial = ">>> "),
      collapse = "\n"
    )
    if (!startup) message(msg) else packageStartupMessage(msg)
  }
}




#' @keywords internal
handle_plugins <- function(names = NULL, quiet = FALSE, mode) {
  assert_that(is.flag(quiet), !is.na(quiet))
  assert_that(mode %in% c("enable", "disable"))

  moded <- paste0(mode, "d")
  moded_rev <- ifelse(moded == "enabled", "disabled", "enabled")

  if (is.null(names)) {
    names <- qgis_plugins(which = moded_rev)$name
    if (mode == "disable") {
      names <- names[names != "processing"]
    }
  } else {
    assert_that(is.character(names))
    names_old <- names
    names_unavailable <- names_old[!names_old %in% c(qgis_plugins(which = "all")$name, "")]
    names_skip <- names_old[names_old %in% qgis_plugins(which = moded)$name]
    if (!quiet && length(names_unavailable) > 0L) message(
      "Ignoring unknown plugins: ",
      paste(names_unavailable, collapse = ", ")
    )
    if (!quiet && length(names_skip) > 0L) message(glue(
      "Ignoring plugins that are {moded} already: ",
      "{paste(names_skip, collapse = ', ')}"
    ))
    names <- names_old[names_old %in% qgis_plugins(which = moded_rev)$name]
    if (!quiet && "processing" %in% names && mode == "disable") message(
      "Ignoring the 'processing' plugin, because it is always available to ",
      "'qgis_process' (not QGIS though)."
    )
    if (mode == "disable") {
      names <- names[names != "processing"]
    }
  }

  if (length(names) == 0L) {
    if (!quiet) message("No QGIS plugins to be handled; exiting.")
    return(invisible(NULL))
  }

  if (!quiet) message(glue(
    "Trying to {mode} following plugin(s): ",
    "{paste(names, collapse = ', ')}"
  ))

  counter <- 0L
  if (mode == "enable") for (p in names) {
    counter <- enable_plugin(p, quiet = quiet) + counter
  }
  if (mode == "disable") for (p in names) {
    counter <- disable_plugin(p, quiet = quiet) + counter
  }

  if (counter > 0L) {
    if (!quiet) {
      message("\nRebuilding cache to reflect current plugin state ...\n")
    }
    qgis_configure(use_cached_data = FALSE, quiet = quiet)
  }

  invisible(qgis_plugins())
}



#' @keywords internal
enable_plugin <- function(name, quiet = FALSE) {
  error_detected <- FALSE
  tryCatch(
    {
      qgis_run(args = c("plugins", "enable", name))
      if (!quiet) message(name, " successfully enabled!")
    },
    error = function(e) {
      message(glue(
        "'{name}' was not successfully enabled. Error message was:\n\n{e}\n",
        ifelse("stderr" %in% names(e) && nchar(e$stderr) > 0, e$stderr, "")
      ))
      assign("error_detected", TRUE, envir = parent.env(environment()))
    }
  )
  if (error_detected) invisible(FALSE) else invisible(TRUE)
}



#' @keywords internal
disable_plugin <- function(name, quiet = FALSE) {
  error_detected <- FALSE
  tryCatch(
    {
      qgis_run(args = c("plugins", "disable", name))
      if (!quiet) message(name, " successfully disabled!")
    },
    error = function(e) {
      message(glue(
        "'{name}' was not successfully disabled. Error message was:\n\n{e}\n",
        ifelse("stderr" %in% names(e) && nchar(e$stderr) > 0, e$stderr, "")
      ))
      assign("error_detected", TRUE, envir = parent.env(environment()))
    }
  )
  if (error_detected) invisible(FALSE) else invisible(TRUE)
}
