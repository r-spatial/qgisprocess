#' Get metadata about the used 'qgis_process' command
#'
#' `qgis_path()` returns the filepath of the 'qgis_process' command, while
#' `qgis_version()` returns the QGIS version.
#'
#' @family topics about reporting the QGIS state
#' @concept functions to manage and explore QGIS and qgisprocess
#' @seealso [qgis_configure()]
#'
#' @param debug Logical.
#' If `TRUE`, also output the version of QGIS, the operating system and all
#' relevant libraries, as reported by the 'qgis_process' command.
#' @param quiet Use `FALSE` to display more information,
#' possibly useful for debugging.
#' @param query Use `TRUE` to refresh the cached value.
#'
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
      error = function(e) {}
    )
  }

  abort("QGIS installation found, but all candidate paths failed to execute.")
}



#' @rdname qgis_path
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
    if (package_version(strsplit(qgis_version(), "-")[[1]][1]) < "3.22.0") {
      warning("'debug = TRUE' is not supported for QGIS versions < 3.22")
      return(qgisprocess_cache$version)
    }
    print(qgisprocess_cache$version)
    message()
    message("Versions reported by 'qgis_process':")
    message("------------------------------------")
    message(qgis_run(args = "--version")$stdout)
    return(invisible(qgisprocess_cache$version))
  }

  qgisprocess_cache$version
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
#'
#' The settings can be overruled with the options
#' `qgisprocess.use_json_input` or `qgisprocess.use_json_output`, and with the
#' environment variables `R_QGISPROCESS_USE_JSON_INPUT` or
#' `R_QGISPROCESS_USE_JSON_OUTPUT.`
#' Since the JSON output method is cached by the package,
#' `qgis_using_json_output(query = TRUE)` is needed for these settings to take
#' effect if the package was loaded already.
#'
#'
#' @inheritParams qgis_path
#'
#'
#' @family topics about programming or debugging utilities
#' @family topics about reporting the QGIS state
#' @concept functions to manage and explore QGIS and qgisprocess
#'
#' @export
qgis_using_json_input <- function() {
  opt <- getOption(
    "qgisprocess.use_json_input",
    Sys.getenv(
      "R_QGISPROCESS_USE_JSON_INPUT",
      ""
    )
  )

  if (identical(opt, "")) {
    qgis_using_json_output() &&
      (package_version(strsplit(qgis_version(), "-")[[1]][1]) >= "3.23.0")
  } else {
    isTRUE(opt) || identical(opt, "true")
  }
}


#' @rdname qgis_using_json_input
#' @export
qgis_using_json_output <- function(query = FALSE, quiet = TRUE) {
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
