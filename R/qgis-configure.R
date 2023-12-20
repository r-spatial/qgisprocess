#' Configure qgisprocess
#'
#' Run `qgis_configure()` to bring the package configuration in line with
#' QGIS and to save this configuration to a persistent cache.
#' See the _Details_ section for more information about setting the path of
#' the 'qgis_process' command line tool.
#'
#' The qgisprocess package is a wrapper around the 'qgis_process' command line
#' tool distributed with QGIS (>=3.14). Several functions use heuristics to
#' detect the location of the 'qgis_process' executable.
#'
#' When loading the package, the configuration is automatically read from the
#' cache with `qgis_configure(use_cached_data = TRUE, quiet = TRUE)` in order
#' to save time.
#' Run `qgis_configure(use_cached_data = TRUE)` manually to get more details.
#'
#' Use `qgis_algorithms()`, `qgis_providers()`, `qgis_plugins()`,
#' `qgis_using_json_output()`, `qgis_path()` and `qgis_version()` to inspect cache
#' contents.
#'
#' If the configuration
#' fails or you have more than one QGIS installation, you can set
#' `options(qgisprocess.path = "path/to/qgis_process")` or the
#' `R_QGISPROCESS_PATH` environment variable (useful on CI). On Linux the
#' 'qgis_process' executable is generally available on the user's PATH,
#' on MacOS the executable is within the QGIS*.app/Contents/MacOS/bin folder,
#' and on Windows the executable is named qgis_process-qgis.bat or
#' qgis_process-qgis-dev.bat and is located in Program Files/QGIS*/bin or
#' OSGeo4W(64)/bin.
#'
#' @family topics about configuring QGIS and qgisprocess
#' @concept functions to manage and explore QGIS and qgisprocess
#' @seealso [qgis_unconfigure()]
#' @seealso [qgis_path()], [qgis_version()]
#'
#' @param use_cached_data Use the cached algorithm list and `path` found when
#'   configuring qgisprocess during the last session. This saves some time
#'   loading the package.
#' @inheritParams qgis_run
#' @inheritParams qgis_path
#'
#' @returns The result of [processx::run()].
#'
#' @examples
#' \donttest{
#' # not running in R CMD check to save time
#' qgis_configure(use_cached_data = TRUE)
#' }
#'
#' \dontrun{
#' # package reconfiguration
#' # (not run in example() as it rewrites the package cache file)
#' qgis_configure()
#' }
#'
#' @export
qgis_configure <- function(quiet = FALSE, use_cached_data = FALSE) {
  tryCatch(
    {
      qgis_unconfigure()

      cache_data_file <- qgis_pkgcache_file()

      # Practically all code of this function now consists of handling
      # use_cached_data = TRUE. This includes cases where qgis_reconfigure() must
      # be called, when a condition about the cache is not met. Note that
      # qgis_configure() by default (use_cached_data = FALSE) calls
      # qgis_reconfigure() straight away.


      # below message is usually relevant when loading the package, and serves to
      # announce some waiting time (checking and reading the cache) in case this
      # process will happen silently (as is the case when loading the pkg). Put
      # here, since packagestartupmessages in .onLoad are not good practice and
      # trigger a note during R CMD check. Same applies to the Success! message
      # further down.
      # Note: more messages have been turned into startupmessages since CRAN
      # wants to be able to suppress them.
      if (use_cached_data && quiet) {
        packageStartupMessage(
          "Attempting to load the package cache ... ",
          appendLF = FALSE
        )
      }

      if (use_cached_data && file.exists(cache_data_file)) {
        try({
          cached_data <- readRDS(cache_data_file)

          if (!quiet) message(glue("Checking configuration in cache file ({cache_data_file})"))

          # CACHE CONDITION: contains minimum required elements (i.e. objects; the
          # cache is an environment)
          if (
            !all(
              c("path", "version", "algorithms", "plugins", "use_json_output")
              %in% names(cached_data)
            )
          ) {
            if (quiet) packageStartupMessage()
            packageStartupMessage(
              "The cache does not contain all required elements.\n",
              "Will try to reconfigure qgisprocess and build new cache ..."
            )
            qgis_reconfigure(cache_data_file = cache_data_file, quiet = quiet)
            return(invisible(has_qgis()))
          }

          # CACHE CONDITION: the path element does not contradict the environment
          # variable/option for the qgis_process path
          option_path <- getOption(
            "qgisprocess.path",
            Sys.getenv("R_QGISPROCESS_PATH")
          )

          if (!identical(option_path, "") && !identical(option_path, cached_data$path)) {
            if (quiet) packageStartupMessage()
            packageStartupMessage(glue(
              "The user's qgisprocess.path option or the R_QGISPROCESS_PATH environment ",
              "variable specify a different qgis_process path ({option_path}) ",
              "than the cache did ({cached_data$path}).\n",
              "Hence rebuilding cache to reflect this change ..."
            ))
            qgis_reconfigure(cache_data_file = cache_data_file, quiet = quiet)
            return(invisible(has_qgis()))
          }

          # CACHE CONDITION: the elements checked by has_qgis() look OK

          condition <-
            is.string(cached_data$version) && nchar(cached_data$version) > 3L &&
              is.string(cached_data$path) && nchar(cached_data$path) > 10L &&
              inherits(cached_data$algorithms, "data.frame") &&
              inherits(cached_data$plugins, "data.frame")

          if (!condition) {
            if (quiet) packageStartupMessage()
            packageStartupMessage(
              "The cache does not contain all required data.\n",
              "Will try to reconfigure qgisprocess and build new cache ..."
            )
            qgis_reconfigure(cache_data_file = cache_data_file, quiet = quiet)
            return(invisible(has_qgis()))
          }

          # CACHE CONDITION: qgis_process is indeed available in the cached path

          outcome <- try(qgis_run(path = cached_data$path), silent = TRUE)
          if (inherits(outcome, "try-error")) {
            if (quiet) packageStartupMessage()
            packageStartupMessage(
              glue(
                "'{cached_data$path}' (cached path) is not available anymore.\n",
                "Will try to reconfigure qgisprocess and build new cache ..."
              )
            )
            qgis_reconfigure(cache_data_file = cache_data_file, quiet = quiet)
            return(invisible(has_qgis()))
          }

          # CACHE CONDITION: the path element does not contradict the
          # environment variable/option to automatically switch to a newer
          # available QGIS version
          if (is_windows() || is_macos()) {
            opt <- getOption(
              "qgisprocess.detect_newer_qgis",
              Sys.getenv("R_QGISPROCESS_DETECT_NEWER_QGIS")
            )
            assert_that(
              assertthat::is.flag(opt) ||
                (assertthat::is.string(opt) && opt %in% c("", "TRUE", "FALSE", "true", "false")),
              msg = "Option 'qgisprocess.detect_newer_qgis' must be 'TRUE' or 'FALSE'."
            )
            if (identical(opt, "")) opt <- NA
            opt || grepl("TRUE|true", opt)

            first_qgis <- qgis_detect_paths()[1]
            newer_available <- !is.na(extract_version_from_paths(first_qgis)) &&
              !identical(cached_data$path, first_qgis)

            if (isTRUE(opt) && isTRUE(newer_available) && interactive()) {
              packageStartupMessage()
              packageStartupMessage(glue(
                "A newer QGIS installation seems to be available: ",
                "{extract_version_from_paths(first_qgis)}."
              ))
              answer <- ""
              while (!grepl("^[Yy](?:[Ee][Ss])?$|^[Nn](?:[Oo])?$", answer)) {
                answer <- readline("Do you want to try it and rebuild the cache? (y/n) ")
              }
              if (grepl("^[Yy]", answer)) {
                newer_ok <- FALSE
                tryCatch(
                  {
                    qgis_run(path = first_qgis)
                    newer_ok <- TRUE
                  },
                  error = function(e) {
                    packageStartupMessage(
                      glue(
                        "'{first_qgis}' does not work as expected.\n",
                        "So will not try it further."
                      )
                    )
                  }
                )
                if (newer_ok) {
                  packageStartupMessage(
                    "Will try to reconfigure qgisprocess and build new cache ..."
                  )
                  qgis_reconfigure(cache_data_file = cache_data_file, quiet = quiet)
                  return(invisible(has_qgis()))
                }
              } else if (!quiet) {
                packageStartupMessage(
                  "\nNOTE: if you don't want to autodetect QGIS version updates ",
                  "in the future, unset the qgisprocess.detect_newer_qgis ",
                  "option or the R_QGISPROCESS_DETECT_NEWER_QGIS environment ",
                  "variable."
                )
              }
            }
          }

          # CACHE CONDITION: the cached QGIS version equals the one reported by
          # qgis_process

          if (!quiet) message(glue(
            "Checking cached QGIS version with version reported by '{cached_data$path}' ..."
          ))

          qgisprocess_cache$path <- cached_data$path
          qversion <- qgis_query_version(quiet = quiet)
          qgisprocess_cache$path <- NULL

          if (!identical(qversion, cached_data$version)) {
            if (quiet) packageStartupMessage()
            packageStartupMessage(glue(
              "QGIS version change detected:\n",
              "- in the qgisprocess cache it was: {cached_data$version}\n",
              "- while '{cached_data$path}' is at {qversion}\n",
              "Hence rebuilding cache to reflect this change ..."
            ))
            qgis_reconfigure(cache_data_file = cache_data_file, quiet = quiet)
            return(invisible(has_qgis()))
          }

          if (!quiet) message(glue("QGIS versions match! ({qversion})"))

          # CACHE CONDITION: the cached QGIS plugins equal the ones reported by
          # qgis_process, including their state

          if (!quiet) message(glue(
            "Checking cached QGIS plugins (and state) with those reported by '{cached_data$path}' ..."
          ))

          qgisprocess_cache$path <- cached_data$path
          qgisprocess_cache$use_json_output <- cached_data$use_json_output
          qplugins <- qgis_query_plugins(quiet = quiet)
          qgisprocess_cache$path <- NULL
          qgisprocess_cache$use_json_output <- NULL

          if (!identical(qplugins, cached_data$plugins)) {
            if (quiet) packageStartupMessage()
            packageStartupMessage(
              "Change detected in (enabled) QGIS processing provider plugins!\n",
              "- in the qgisprocess cache it was:"
            )
            print(cached_data$plugins)
            packageStartupMessage(glue(
              "- while '{cached_data$path}' currently returns:"
            ))
            print(qplugins)
            packageStartupMessage(
              "Hence rebuilding cache to reflect this change ..."
            )
            qgis_reconfigure(cache_data_file = cache_data_file, quiet = quiet)
            return(invisible(has_qgis()))
          }

          if (!quiet) {
            message(glue(
              "QGIS plugins match! ({ sum(qplugins$enabled) } ",
              "processing provider plugin(s) enabled)"
            ))
            message_disabled_plugins(qplugins, prepend_newline = TRUE)
          }

          # ASSIGNING CACHE OBJECTS

          if (!quiet) message(glue(
            "\n\nRestoring configuration from '{cache_data_file}'"
          ))

          qgisprocess_cache$path <- cached_data$path
          qgisprocess_cache$version <- cached_data$version
          qgisprocess_cache$use_json_output <- cached_data$use_json_output
          qgisprocess_cache$algorithms <- cached_data$algorithms
          qgisprocess_cache$plugins <- cached_data$plugins
          qgisprocess_cache$loaded_from <- cache_data_file

          # FINAL HANDLING OF SUCCESSFUL use_cached_data = TRUE

          if (!quiet) { # triggers messages
            invisible(qgis_version(query = FALSE, quiet = FALSE))
            invisible(qgis_path(query = FALSE, quiet = FALSE))
            invisible(qgis_using_json_output(query = FALSE, quiet = FALSE))
            message(
              ifelse(qgis_using_json_input(), "Using ", "Not using "),
              "JSON for input serialization."
            )
            invisible(qgis_plugins(query = FALSE, quiet = FALSE, msg = FALSE))
            invisible(qgis_algorithms(query = FALSE, quiet = FALSE))
            message_inspect_cache()
          }

          # usually applicable only on loading the package:
          if (quiet && !is.null(qgisprocess_cache$loaded_from)) {
            (packageStartupMessage("Success!"))
          }

          return(invisible(has_qgis()))
        })
      }

      if (use_cached_data && !file.exists(cache_data_file)) {
        packageStartupMessage(
          "No cache found.\n",
          "Will try to reconfigure qgisprocess and build new cache ..."
        )
      }

      # use_cached_data = FALSE or cache is missing:

      qgis_reconfigure(cache_data_file = cache_data_file, quiet = quiet)
    },
    error = function(e) {
      qgis_unconfigure()
      if (!quiet) message(e)
    }
  )

  invisible(has_qgis())
}




#' @keywords internal
qgis_reconfigure <- function(cache_data_file, quiet = FALSE) {
  path <- qgis_path(query = TRUE, quiet = quiet)
  if (!quiet) message()

  tryCatch(
    {
      version <- qgis_version(query = TRUE, quiet = quiet)
    },
    error = function(e) {
      message(glue(
        "\n\nATTENTION: the QGIS version could not be queried. ",
        "You will loose some functionality.\n",
        "You may want to run `qgis_version(query = TRUE)` followed by ",
        "`qgis_configure()`; see its documentation.\n",
        "Error message was:\n\n{e}\n",
        ifelse("stderr" %in% names(e) && nchar(e$stderr) > 0, e$stderr, "")
      ))
    }
  )

  use_json_output <- qgis_using_json_output(query = TRUE, quiet = quiet)

  if (!quiet) message(
    ifelse(qgis_using_json_input(), "Using ", "Not using "),
    "JSON for input serialization."
  )

  tryCatch(
    {
      plugins <- qgis_plugins(query = TRUE, quiet = quiet, msg = FALSE)
    },
    error = function(e) {
      message(glue(
        "\n\nATTENTION: the QGIS plugins could not be queried. ",
        "You will loose some functionality.\n",
        "You may want to (re)run `qgis_configure()`; see its documentation.\n",
        "Error message was:\n\n{e}\n",
        ifelse("stderr" %in% names(e) && nchar(e$stderr) > 0, e$stderr, "")
      ))
    }
  )

  tryCatch(
    {
      algorithms <- qgis_algorithms(query = TRUE, quiet = quiet)
    },
    error = function(e) {
      message(glue(
        "\n\nATTENTION: the QGIS algorithms could not be queried. ",
        "You will loose some functionality.\n",
        "You may want to (re)run `qgis_configure()`; see its documentation.\n",
        "Error message was:\n\n{e}\n",
        ifelse("stderr" %in% names(e) && nchar(e$stderr) > 0, e$stderr, "")
      ))
    }
  )

  if (!quiet && exists("plugins")) message_disabled_plugins(plugins, prepend_newline = TRUE)

  if (has_qgis()) {
    if (!quiet) message(glue("\n\nSaving configuration to '{cache_data_file}'"))

    try({
      if (!dir.exists(dirname(cache_data_file))) {
        dir.create(dirname(cache_data_file), recursive = TRUE)
      }

      saveRDS(
        list(
          path = path,
          version = version,
          algorithms = algorithms,
          plugins = plugins,
          use_json_output = use_json_output
        ),
        cache_data_file
      )
    })

    if (!quiet) message_inspect_cache()
  } else if (!quiet) {
    message(config_problem_msg)
  }
}



#' @keywords internal
message_inspect_cache <- function() {
  message(
    "Use qgis_algorithms(), qgis_providers(), qgis_plugins(), ",
    "qgis_using_json_output(),\nqgis_path() and qgis_version() ",
    "to inspect the cache environment."
  )
}





#' @keywords internal
qgis_env <- function() {
  getOption(
    "qgisprocess.env",
    list(QT_QPA_PLATFORM = "offscreen")
  )
}





#' Clean the package cache
#'
#' Empties the qgisprocess cache environment.
#'
#' @family topics about programming or debugging utilities
#'
#' @returns `NULL`, invisibly.
#'
#' @examples
#' \dontrun{
#' # not running this function in example() as it clears the cache environment.
#' qgis_unconfigure()
#' }
#'
#' # undoing qgis_unconfigure() by repopulating the cache environment from file:
#' \donttest{
#' # not running in R CMD check to save time
#' qgis_configure(use_cached_data = TRUE)
#' }
#'
#' @export
qgis_unconfigure <- function() {
  qgisprocess_cache$path <- NULL
  qgisprocess_cache$version <- NULL
  qgisprocess_cache$algorithms <- NULL
  qgisprocess_cache$plugins <- NULL
  qgisprocess_cache$loaded_from <- NULL
  qgisprocess_cache$use_json_output <- NULL
  invisible(NULL)
}
