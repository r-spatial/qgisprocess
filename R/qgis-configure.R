#' Configure and run 'qgis_process'
#'
#' Run `qgis_configure()` to bring the package configuration in line with
#' QGIS and to save this configuration to a persistent cache.
#' See the _Details_ section for more information about setting the path of
#' the 'qgis_process' command line tool.
#' `qgis_run()` is meant for directly calling this tool, but should normally not
#' be needed.
#'
#' The qgisprocess package is a wapper around the 'qgis_process' command line
#' tool distributed with QGIS (>=3.14). Several functions use heuristics to
#' detect the location of the 'qgis_process' executable.
#'
#' When loading the package, the configuration is automatically read from the
#' cache with `qgis_configure(use_cached_data = TRUE, quiet = TRUE)` in order
#' to save time.
#' Run `qgis_configure(use_cached_data = TRUE)` manually to get more details.
#'
#' Use `qgis_algorithms()`, `qgis_providers()`, `qgis_plugins()`,
#' `qgis_use_json_output()`, `qgis_path()` and `qgis_version()` to inspect cache
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
#' @param ... Passed to [processx::run()].
#' @param args Command-line arguments
#' @param quiet Use `FALSE` to display more information about the command, possibly
#'   useful for debugging.
#' @param query Use `TRUE` to refresh the cached value.
#' @param action An action to take if the 'qgis_process' executable could not be
#'   found.
#' @param env A [list()] of environment variables. Defaults to [qgis_env()].
#' @param path A path to the 'qgis_process' executable. Defaults to [qgis_path()].
#' @param use_cached_data Use the cached algorithm list and `path` found when
#'   configuring qgisprocess during the last session. This saves some time
#'   loading the package.
#' @param debug Logical.
#' If `TRUE`, also output the version of QGIS, the operating system and all
#' relevant libraries, as reported by the 'qgis_process' command.
#'
#' @return The result of [processx::run()].
#' @export
#'
#' @examples
#' if (has_qgis()) qgis_path()
#' if (has_qgis()) qgis_version()
#' if (has_qgis()) qgis_algorithms()
#' if (has_qgis()) qgis_providers()
#' if (has_qgis()) qgis_plugins()
#' qgis_configure(use_cached_data = TRUE)
#' qgis_configure()
#'
qgis_run <- function(args = character(), ..., env = qgis_env(), path = qgis_path()) {
  # workaround for running Windows batch files where arguments have spaces
  # see https://github.com/r-lib/processx/issues/301
  if (is_windows()) {
    withr::with_envvar(
      env,
      processx::run("cmd.exe", c("/c", "call", path, args), ...),
    )
  } else {
    withr::with_envvar(
      env,
      processx::run(path, args, ...),
    )
  }
}

#' @rdname qgis_run
#' @export
has_qgis <- function() {
  !is.null(qgisprocess_cache$path) &&
    !is.null(qgisprocess_cache$version) &&
    !is.null(qgisprocess_cache$algorithms) &&
    !is.null(qgisprocess_cache$plugins)
}

#' @rdname qgis_run
#' @export
assert_qgis <- function(action = abort) {
  if (!has_qgis()) {
    action(
      paste0(
        "The QGIS processing utility ('qgis_process') is not installed or could not be found.\n",
        "Run `qgis_configure()` to configure this location.\n",
        "If 'qgis_process' is installed, set `options(qgisprocess.path = '/path/to/qgis_process')`\n",
        "and re-run `qgis_configure()`."
      )
    )
  }
}

#' @rdname qgis_run
#' @export
qgis_configure <- function(quiet = FALSE, use_cached_data = FALSE) {
  tryCatch(
    {
      qgis_unconfigure()

      version <- as.character(utils::packageVersion("qgisprocess"))

      cache_data_file <- file.path(
        rappdirs::user_cache_dir("R-qgisprocess"),
        glue("cache-{version}.rds")
      )

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
      if (use_cached_data && quiet) {
        packageStartupMessage(
          "Attempting to load the cache ... ",
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
            if (quiet) message()
            message(
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
            if (quiet) message()
            message(glue(
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
            is.string(cached_data$version) && nchar(cached_data$version) > 0L &&
              is.string(cached_data$path) && nchar(cached_data$path) > 0L &&
              inherits(cached_data$algorithms, "data.frame") &&
              inherits(cached_data$plugins, "data.frame")

          if (!condition) {
            if (quiet) message()
            message(
              "The cache does not contain all required data.\n",
              "Will try to reconfigure qgisprocess and build new cache ..."
            )
            qgis_reconfigure(cache_data_file = cache_data_file, quiet = quiet)
            return(invisible(has_qgis()))
          }

          # CACHE CONDITION: qgis_process is indeed available in the cached path

          tryCatch(
            {
              qgis_run(path = cached_data$path)
            },
            error = function(e) {
              if (quiet) message()
              abort(
                glue(
                  "'{cached_data$path}' (cached path) is not available anymore.\n",
                  "Will try to reconfigure qgisprocess and build new cache ..."
                )
              )
              qgis_reconfigure(cache_data_file = cache_data_file, quiet = quiet)
              return(invisible(has_qgis()))
            }
          )

          # CACHE CONDITION: the cached QGIS version equals the one reported by
          # qgis_process

          if (!quiet) message(glue(
            "Checking cached QGIS version with version reported by '{cached_data$path}' ..."
          ))

          qgisprocess_cache$path <- cached_data$path
          qversion <- qgis_query_version(quiet = quiet)
          qgisprocess_cache$path <- NULL

          if (!identical(qversion, cached_data$version)) {
            if (quiet) message()
            message(glue(
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
            if (quiet) message()
            message(
              "Change detected in (enabled) QGIS processing provider plugins!\n",
              "- in the qgisprocess cache it was:"
            )
            print(cached_data$plugins)
            message(glue(
              "- while '{cached_data$path}' currently returns:"
            ))
            print(qplugins)
            message(
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
            invisible(qgis_use_json_output(query = FALSE, quiet = FALSE))
            message(
              ifelse(qgis_use_json_input(), "Using ", "Not using "),
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
        message(
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

  version <- qgis_version(query = TRUE, quiet = quiet)

  use_json_output <- qgis_use_json_output(query = TRUE, quiet = quiet)

  if (!quiet) message(
    ifelse(qgis_use_json_input(), "Using ", "Not using "),
    "JSON for input serialization."
  )

  plugins <- qgis_plugins(query = TRUE, quiet = quiet, msg = FALSE)

  algorithms <- qgis_algorithms(query = TRUE, quiet = quiet)

  if (!quiet) message_disabled_plugins(plugins, prepend_newline = TRUE)

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
}



#' @keywords internal
message_inspect_cache <- function() {
  message(
    "Use qgis_algorithms(), qgis_providers(), qgis_plugins(), ",
    "qgis_use_json_output(),\nqgis_path() and qgis_version() ",
    "to inspect the cache environment."
  )
}



#' @rdname qgis_run
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

#' @rdname qgis_run
#' @export
qgis_version <- function(query = FALSE, quiet = TRUE, debug = FALSE) {
  if (query) qgisprocess_cache$version <- qgis_query_version(quiet = quiet)

  if (!quiet) {
    message(
      "QGIS version",
      ifelse(query, " is now set to: ", ": "),
      qgisprocess_cache$version
    )
  }

  if (debug) {
    print(qgisprocess_cache$version)
    message()
    message("Versions reported by 'qgis_process':")
    message("------------------------------------")
    message(qgis_run(args = "--version")$stdout)
    return(invisible(qgisprocess_cache$version))
  }

  qgisprocess_cache$version
}

#' @rdname qgis_run
#' @export
qgis_path <- function(query = FALSE, quiet = TRUE) {
  if (query) qgisprocess_cache$path <- qgis_query_path(quiet = quiet)
  if (!quiet) message_path(query = query)
  qgisprocess_cache$path
}


#' @keywords internal
message_path <- function(query = FALSE) {
  pathstring <-
    if (qgisprocess_cache$path == "qgis_process") {
      "in the system PATH"
    } else {
      glue("at '{qgisprocess_cache$path}'")
    }
  message(
    ifelse(query, "Now using ", "Using "),
    glue("'qgis_process' {pathstring}.")
  )
  message(
    ">>> If you need another installed QGIS instance, run `qgis_configure()`;\n",
    "    see `?qgis_configure` if you need to preset the path of 'qgis_process'."
  )
}


#' @keywords internal
qgis_query_path <- function(quiet = FALSE) {
  if (!is.null(getOption("qgisprocess.path"))) {
    path <- getOption("qgisprocess.path", "qgis_process")
    if (!quiet) message(glue("Trying getOption('qgisprocess.path'): '{ path }'"))
    tryCatch(
      {
        qgis_run(path = path)
        if (!quiet) message("Success!")
        return(path)
      },
      error = function(e) {
        if (!quiet) message(as.character(e))
      }
    )
  } else {
    if (!quiet) message("getOption('qgisprocess.path') was not found.")
  }

  if (Sys.getenv("R_QGISPROCESS_PATH", "") != "") {
    path <- Sys.getenv("R_QGISPROCESS_PATH")
    if (!quiet) message(glue("Trying Sys.getenv('R_QGISPROCESS_PATH'): '{ path }'"))
    tryCatch(
      {
        qgis_run(path = path)
        if (!quiet) message("Success!")
        return(path)
      },
      error = function(e) {
        if (!quiet) message(as.character(e))
      }
    )
  } else {
    if (!quiet) message("Sys.getenv('R_QGISPROCESS_PATH') was not found.")
  }

  if (!quiet) message(glue("Trying 'qgis_process' on PATH..."))
  tryCatch(
    {
      qgis_run(path = "qgis_process")
      if (!quiet) message("Success!")
      return("qgis_process")
    },
    error = function(e) {
      if (!quiet) message("'qgis_process' is not available on PATH.")
    }
  )

  possible_locs <- if (is_macos()) {
    qgis_detect_macos()
  } else if (is_windows()) {
    qgis_detect_windows()
  }

  if (length(possible_locs) == 0) {
    abort("No QGIS installation containing 'qgis_process' found!")
  }

  if (!quiet) {
    message(
      sprintf(
        "Found %s QGIS installation%s containing 'qgis_process':\n %s",
        length(possible_locs),
        if (length(possible_locs) == 1) "" else "s",
        paste(possible_locs, collapse = "\n")
      )
    )
  }

  for (path in possible_locs) {
    if (!quiet) message(glue("Trying command '{ path }'"))
    tryCatch(
      {
        qgis_run(path = path)
        if (!quiet) message("Success!")
        return(path)
      },
      error = function(e) {}
    )
  }

  abort("QGIS installation found, but all candidate paths failed to execute.")
}

#' @rdname qgis_run
#' @export
qgis_use_json_output <- function(query = FALSE, quiet = TRUE) {
  if (query) {
    opt <- getOption(
      "qgisprocess.use_json_output",
      Sys.getenv(
        "R_QGISPROCESS_USE_JSON_OUTPUT",
        ""
      )
    )

    if (identical(opt, "")) {
      # This doesn't work on the default GHA runner for Ubuntu and
      # maybe can't be guaranteed to work on Linux. On Linux, we try
      # to list algorithms with --json and check if the command fails
      qgisprocess_cache$use_json_output <- is_windows() ||
        is_macos() ||
        (qgis_run(c("--json", "list"), error_on_status = FALSE)$status == 0)
    } else {
      qgisprocess_cache$use_json_output <- isTRUE(opt) || identical(opt, "true")
    }
  }

  if (!quiet) message(
    ifelse(qgisprocess_cache$use_json_output, "Using ", "Not using "),
    "JSON for output serialization."
  )

  qgisprocess_cache$use_json_output
}

#' @rdname qgis_run
#' @export
qgis_use_json_input <- function() {
  opt <- getOption(
    "qgisprocess.use_json_input",
    Sys.getenv(
      "R_QGISPROCESS_USE_JSON_INPUT",
      ""
    )
  )

  if (identical(opt, "")) {
    qgis_use_json_output() &&
      (package_version(strsplit(qgis_version(), "-")[[1]][1]) >= "3.23.0")
  } else {
    isTRUE(opt) || identical(opt, "true")
  }
}

#' @rdname qgis_run
#' @export
qgis_env <- function() {
  getOption(
    "qgisprocess.env",
    list(QT_QPA_PLATFORM = "offscreen")
  )
}

#' @keywords internal
qgis_query_version <- function(quiet = FALSE) {
  result <- qgis_run(args = "--version")
  lines <- readLines(textConnection(result$stdout))
  match <- stringr::str_match(
    lines,
    "QGIS\\s(\\d{1,2}\\.\\d+.*-\\p{L}+)\\s.*\\((.+)\\)"
  )[, 2:3, drop = TRUE]
  match <- match[!is.na(match)]
  if (length(match) == 0L) abort_query_version(lines = lines)
  if (
    !stringr::str_detect(match[1], "-[Mm]a(ster|in)$") &&
      !stringr::str_detect(match[1], "^\\d{1,2}\\.\\d*[13579][\\.-]")
  ) {
    return(match[1])
  } else {
    if (length(match) < 2L) abort_query_version(lines = lines)
    if (!stringr::str_detect(match[2], "^[0-9a-f]{7,}$")) {
      warning("Please consider building the QGIS development version from ",
        "within the QGIS git repository, in order to have a unique ",
        "version identifier of QGIS, or propose the people making the ",
        "QGIS build to do so. ",
        "Currently the specific version identifier is '",
        match[2],
        "'.",
        call. = TRUE
      )
      match[2] <- paste("unclear:", match[2])
    }
    return(paste0(match[1], ", development state ", match[2]))
  }
}

#' @keywords internal
abort_query_version <- function(lines) {
  abort(
    paste0(
      "Output did not contain expected version information and was:\n\n",
      paste(lines, collapse = "\n")
    )
  )
}


# environment for cache
qgisprocess_cache <- new.env(parent = emptyenv())
qgisprocess_cache$path <- NULL
qgisprocess_cache$version <- NULL
qgisprocess_cache$algorithms <- NULL
qgisprocess_cache$plugins <- NULL
qgisprocess_cache$use_json_output <- NULL
qgisprocess_cache$loaded_from <- NULL
