#' Get metadata about the used 'qgis_process' command
#'
#' `qgis_path()` returns the filepath of the 'qgis_process' command, while
#' `qgis_version()` returns the QGIS version.
#'
#' @family topics about reporting the QGIS state
#' @concept functions to manage and explore QGIS and qgisprocess
#' @seealso [qgis_configure()]
#'
#' @param full Logical.
#' If `FALSE`, only return the `"x.y.z"` version string instead of the full
#' version string that includes the name.
#' Defaults to `TRUE`; ignored if `debug = TRUE`.
#' @param debug Logical.
#' If `TRUE`, also output the version of QGIS, the operating system and all
#' relevant libraries, as reported by the 'qgis_process' command.
#' @param quiet Use `FALSE` to display more information,
#' possibly useful for debugging.
#' @param query Use `TRUE` to refresh the cached value.
#'
#' @returns A string.
#'
#' @examplesIf has_qgis()
#' qgis_path()
#' qgis_path(quiet = FALSE)
#' qgis_version()
#' qgis_version(full = FALSE)
#' qgis_version(debug = TRUE)
#'
#' @name qgis_path

#' @rdname qgis_path
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
        if (!quiet) message(glue(
          "Running '{ path }' was not successful. ",
          "Please check the qgisprocess.path option.\n",
          "Error message was:\n\n{e}\n",
          ifelse("stderr" %in% names(e) && nchar(e$stderr) > 0, e$stderr, "")
        ))
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
        if (!quiet) message(glue(
          "Running '{ path }' was not successful. ",
          "Please check the R_QGISPROCESS_PATH environment variable.\n",
          "Error message was:\n\n{e}\n",
          ifelse("stderr" %in% names(e) && nchar(e$stderr) > 0, e$stderr, "")
        ))
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
    qgis_detect_macos_paths()
  } else if (is_windows()) {
    qgis_detect_windows_paths()
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
      error = function(e) {
        message(glue("'{ path }' failed to execute."))
      }
    )
  }

  abort("QGIS installation found, but all candidate paths failed to execute.")
}



#' @rdname qgis_path
#' @export
qgis_version <- function(query = FALSE, quiet = TRUE, full = TRUE, debug = FALSE) {
  if (query) qgisprocess_cache$version <- qgis_query_version(quiet = quiet)

  if (!quiet) {
    message(
      "QGIS version",
      ifelse(query, " is now set to: ", ": "),
      qgisprocess_cache$version
    )
  }

  if (!full || debug) short <- strsplit(qgisprocess_cache$version, "-")[[1]][1]

  if (debug) {
    if (package_version(short) < "3.22.0") {
      warning("'debug = TRUE' is not supported for QGIS versions < 3.22")
      return(qgisprocess_cache$version)
    }
    print(qgisprocess_cache$version)
    message()
    message("Using qgisprocess ", getNamespaceVersion("qgisprocess"))
    message()
    message("Versions reported by 'qgis_process':")
    message("------------------------------------")
    message(qgis_run(args = "--version")$stdout)
    return(invisible(qgisprocess_cache$version))
  }

  if (full) qgisprocess_cache$version else short
}



#' @keywords internal
qgis_query_version <- function(quiet = FALSE) {
  tryCatch(
    {
      result <- qgis_run(args = "--version")
      lines <- readLines(textConnection(result$stdout))
      match <- stringr::str_match(
        lines,
        "QGIS\\s(\\d{1,2}\\.\\d+.*-\\p{L}+)\\s.*\\((.+)\\)"
      )[, 2:3, drop = TRUE]
    },
    error = function(e) {
      # QGIS < 3.22 does not support '--version'
      result <- qgis_run(args = character(0))
      lines <- readLines(textConnection(result$stdout))
      match <- stringr::str_match(
        lines,
        "\\((\\d{1,2}\\.\\d+.*-.+)\\)"
      )[, 2, drop = TRUE]
      assign("match", match, envir = parent.env(environment()))
      assign("lines", lines, envir = parent.env(environment()))
    }
  )
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




#' Report if JSON objects are used for input to and output from 'qgis_process'
#'
#' Returns a logical that reveals whether the JSON input and output methods are
#' used, respectively.
#'
#' Since QGIS 3.24 the JSON input method of 'qgis_process' is used by default
#' when calling the command.
#' It allows for more complex input argument types in certain algorithms that
#' require a more complex input argument, e.g. a list of lists (see
#' [qgis_list_input()]).
#'
#' Likewise, JSON output is the default output format requested from
#' 'qgis_process'.
#' Using the JSON input method of 'qgis_process' automatically implies
#' using the JSON output method; when _not_ using the JSON input method it is
#' possible (but not the default) to also not use the JSON output method.
#'
#' The defaults can be overruled with the options
#' `qgisprocess.use_json_input` or `qgisprocess.use_json_output`, and with the
#' environment variables `R_QGISPROCESS_USE_JSON_INPUT` or
#' `R_QGISPROCESS_USE_JSON_OUTPUT`.
#'
#' The JSON output method returned is always
#' cached for the current session by `qgis_using_json_output()`.
#' Given that `qgis_using_json_output()` is called by various functions
#' in the package, having a user setting 'use_json_output' in place (see above)
#' will have effect during subsequent use of the package.
#' To cache the value between sessions, [qgis_configure()] needs to be called
#' (stores the value in a cache file).
#'
#' The JSON input method is not cached but simply determined on the fly, based
#' on QGIS version, the JSON output method and the user setting if present.
#'
#' @param query Logical.
#' Should the outcome of `qgis_using_json_output()` ignore the cached value?
#' If set as `TRUE` and no user setting 'use_json_output' is in place (see
#' Details), the function returns `TRUE` and the cached value for the
#' current session is set as `TRUE`.
#' If set as `FALSE`, the function returns the cached value if no user setting
#' 'use_json_output' is in place _and_ on condition the cached value does not
#' conflict with a 'use_json_input' user setting.
#' @inheritParams qgis_path
#'
#' @family topics about programming or debugging utilities
#' @family topics about reporting the QGIS state
#' @concept functions to manage and explore QGIS and qgisprocess
#'
#' @returns A logical of length 1.
#'
#' @examplesIf has_qgis()
#' qgis_using_json_input()
#  qgis_using_json_output()
#'
#' @export
qgis_using_json_input <- function() {
  opt <- readopt_json_input()

  if (identical(opt, "")) {
    qgis_using_json_output() &&
      !is.null(qgis_version()) &&
      (package_version(qgis_version(full = FALSE)) >= "3.23.0")
  } else {
    json_input_is_set <- isTRUE(opt) || identical(opt, "true") || identical(opt, "TRUE")
    if (
      json_input_is_set &&
        !is.null(qgis_version()) &&
        package_version(qgis_version(full = FALSE)) < "3.23.0"
    ) {
      warning(glue(
        "QGIS version {qgis_version(full = FALSE)} doesn't support JSON input. ",
        "Please use a currently supported QGIS version."
      ))
      return(FALSE)
    }
    json_input_is_set
  }
}


#' @rdname qgis_using_json_input
#' @export
qgis_using_json_output <- function(query = FALSE, quiet = TRUE) {
  opt <- readopt_json_output()

  if (identical(opt, "")) {
    if (
      # query is kept in order to, when set as FALSE (the default), consistently
      # return 'FALSE' if that is the value stored in the cache file
      query ||
        # never allow a pre-existing cache value 'FALSE' to survive if JSON INput
        # is EXPLICITLY and ACCEPTABLY set as TRUE
        json_input_set_and_acceptable(qgis_version(full = FALSE))
    ) {
      qgisprocess_cache$use_json_output <- TRUE
    } # no 'else return(qgisprocess_cache$use_json_output)' here since we want to get at the message
  } else { # explicit JSON output setting!
    # resolving conflicts with explicit JSON INput setting:
    qgisprocess_cache$use_json_output <- resolve_explicit_json_output(
      json_output_setting = opt,
      qgis_version(full = FALSE)
    )
  }

  if (!quiet) message(
    ifelse(qgisprocess_cache$use_json_output, "Using ", "Not using "),
    "JSON for output serialization."
  )

  qgisprocess_cache$use_json_output
}



#' @keywords internal
readopt_json_input <- function() {
  readopt("qgisprocess.use_json_input", "R_QGISPROCESS_USE_JSON_INPUT")
}



#' @keywords internal
readopt_json_output <- function() {
  readopt("qgisprocess.use_json_output", "R_QGISPROCESS_USE_JSON_OUTPUT")
}




#' @keywords internal
readopt <- function(option_name, envvar_name) {
  getOption(
    option_name,
    Sys.getenv(
      envvar_name,
      ""
    )
  )
}


#' Handle an explicitly set 'use_json_output'
#'
#' The `qgisprocess.use_json_output` option or the
#' `R_QGISPROCESS_USE_JSON_OUTPUT` environment variable
#' can be set explicitly by the user. The function determines whether the user
#' setting can be accepted, depending on the presence of an explicit user
#' setting of the `qgisprocess.use_json_input` option or the
#' `R_QGISPROCESS_USE_JSON_INPUT` environment variable.
#'
#' This helper already assumes that `qgisprocess.use_json_output` or
#' `R_QGISPROCESS_USE_JSON_OUTPUT` have an
#' explicit setting; the empty case is not being handled here!
#'
#' This function intercepts the potential conflict of json_input being
#' explicitly set as `TRUE` and json_output being explicitly set as `FALSE`.
#' In such case, the result will still be set as `TRUE` on condition that
#' the json_input setting is valid (see [qgis_using_json_input()]).
#'
#' @noRd
#' @keywords internal
resolve_explicit_json_output <- function(json_output_setting, qgis_version) {
  json_output_is_set <- isTRUE(json_output_setting) ||
    identical(json_output_setting, "true") ||
    identical(json_output_setting, "TRUE")
  # with JSON INput EXPLICITLY set as TRUE, always use JSON output if the
  # version requirement is met (it is how 'qgis_process run' works, so
  # better do that throughout the package)
  json_input_is_acceptably_set <- json_input_set_and_acceptable(qgis_version)
  if (json_input_is_acceptably_set && !json_output_is_set) {
    warning(
      "Conflicting user settings: 'use JSON output' was set to FALSE, ",
      "but 'use JSON input' is set to TRUE (and granted).\n",
      "Will use JSON output!"
    )
  }
  json_input_is_acceptably_set || json_output_is_set
}







#' @keywords internal
json_input_set_and_acceptable <- function(qgis_version) {
  opt_json_input <- readopt_json_input()
  (isTRUE(opt_json_input) ||
    identical(opt_json_input, "true") ||
    identical(opt_json_input, "TRUE")) &&
    !is.null(qgis_version) &&
    package_version(qgis_version) >= "3.23.0"
}




#' JSON state debugging while developing
#'
#' @noRd
#' @keywords internal
debug_json <- function() {
  assert_qgis()
  cache_data_file <- qgis_pkgcache_file()
  assert_that(file.exists(cache_data_file))
  cached_data <- readRDS(cache_data_file)
  glue(
    "cached_data$use_json_output = {cached_data$use_json_output}\n",
    "qgisprocess_cache$use_json_output = {qgisprocess_cache$use_json_output}\n",
    "readopt_json_input() = {readopt_json_input()}\n",
    "readopt_json_output() = {readopt_json_output()}\n",
    "qgis_using_json_input() = {qgis_using_json_input()}\n",
    "qgis_using_json_output() = {qgis_using_json_output()}\n",
    "qgisprocess_cache$use_json_output = {qgisprocess_cache$use_json_output}\n",
    "cached_data$use_json_output = {cached_data$use_json_output}"
  )
}
