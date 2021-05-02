
#' Configure and run 'qgis_process'
#'
#' The qgisprocess package is a wapper around the 'qgis_process' command line
#' tool distributed with QGIS (>=3.14). These functions use heuristics to
#' detect the location of the 'qgis_process' executable. If the configuration
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
#'
#' @return The result of [processx::run()].
#' @export
#'
#' @examples
#' if (has_qgis()) qgis_path()
#' if (has_qgis()) qgis_version()
#' if (has_qgis()) qgis_algorithms()
#' qgis_configure()
#'
qgis_run <- function(args = character(), ..., env = qgis_env(), path = qgis_path()) {
  # workaround for running Windows batch files where arguments have spaces
  # or special characters (here, notably the newline and the quote)
  # see https://github.com/r-lib/processx/issues/301
  if (is_windows()) {
    cmd_args <- c("/c", "call", path, args)

    # need to shell-escape special characters using the cmd.exe shell escape
    cmd_args_esc <- gsub('(["\n])', "^\\1", cmd_args)

    withr::with_envvar(
      env,
      processx::run("cmd.exe", cmd_args_esc, ...),
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
    !is.null(qgisprocess_cache$algorithms)
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
qgis_configure <- function(quiet = FALSE) {
  tryCatch({
    qgis_unconfigure()

    qgis_path(query = TRUE, quiet = quiet)
    qgis_version(query = TRUE, quiet = quiet)
    qgis_algorithms(query = TRUE, quiet = quiet)
  }, error = function(e) {
    qgis_unconfigure()
    if (!quiet) message(e)
  })

  invisible(has_qgis())
}

#' @rdname qgis_run
#' @export
qgis_unconfigure <- function() {
  qgisprocess_cache$path <- NULL
  qgisprocess_cache$version <- NULL
  qgisprocess_cache$algorithms <- NULL
  qgisprocess_cache$help_text <- new.env(parent = emptyenv())
  invisible(NULL)
}

#' @rdname qgis_run
#' @export
qgis_version <- function(query = FALSE, quiet = TRUE) {
  if (query) {
    qgisprocess_cache$version <- qgis_query_version(quiet = quiet)
  }

  qgisprocess_cache$version
}

#' @rdname qgis_run
#' @export
qgis_algorithms <- function(query = FALSE, quiet = TRUE) {
  if (query) {
    qgisprocess_cache$algorithms <- qgis_query_algorithms(quiet = quiet)
  }

  qgisprocess_cache$algorithms
}

#' @rdname qgis_run
#' @export
qgis_path <- function(query = FALSE, quiet = TRUE) {
  if (query) {
    qgisprocess_cache$path <- qgis_query_path(quiet = quiet)
  }

  qgisprocess_cache$path
}

#' @rdname qgis_run
#' @export
qgis_query_path <- function(quiet = FALSE) {
  if (!is.null(getOption("qgisprocess.path"))) {
    path <- getOption("qgisprocess.path", "qgis_process")
    if (!quiet) message(glue::glue("Trying getOption('qgisprocess.path'): '{ path }'"))
    tryCatch({
      qgis_run(path = path)
      if (!quiet) message("Success!")
      return(path)
    }, error = function(e) {
      if (!quiet) message(as.character(e))
    })
  } else {
    if (!quiet) message("getOption('qgisprocess.path') was not found.")
  }

  if (Sys.getenv("R_QGISPROCESS_PATH", "") != "") {
    path <- Sys.getenv("R_QGISPROCESS_PATH")
    if (!quiet) message(glue::glue("Trying Sys.getenv('R_QGISPROCESS_PATH'): '{ path }'"))
    tryCatch({
      qgis_run(path = path)
      if (!quiet) message("Success!")
      return(path)
    }, error = function(e) {
      if (!quiet) message(as.character(e))
    })
  } else {
    if (!quiet) message("Sys.getenv('R_QGISPROCESS_PATH') was not found.")
  }

  if (!quiet) message(glue::glue("Trying 'qgis_process' on PATH"))
  tryCatch({
    qgis_run(path = "qgis_process")
    if (!quiet) message("Success!")
    return("qgis_process")
  }, error = function(e) {
    if (!quiet) message(as.character(e))
  })

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
    if (!quiet) message(glue::glue("Trying command '{ path }'"))
    tryCatch({
      qgis_run(path = path)
      if (!quiet) message("Success!")
      return(path)
    }, error = function(e) {})
  }

  abort("QGIS installation found, but all candidate paths failed to execute.")
}

#' @rdname qgis_run
#' @export
qgis_env <- function() {
  getOption("qgisprocess.env", list(QT_QPA_PLATFORM = 'offscreen'))
}

#' @rdname qgis_run
#' @export
qgis_query_version <- function(quiet = FALSE) {
  result <- qgis_run(args = character(0))
  lines <- readLines(textConnection(result$stdout))
  match <- stringr::str_match(lines, "\\(([0-9.]+[[:cntrl:][:alnum:].-]*)\\)")[, 2, drop = TRUE]
  if (all(is.na(match))) {
    abort(
      message(
        "Output did not contain expected version information and was:\n\n",
        paste(lines, collapse = "\n"))
    )
  }
  match[!is.na(match)]
}

#' @rdname qgis_run
#' @export
qgis_query_algorithms <- function(quiet = FALSE) {
  result <- qgis_run(args = "list")
  lines <- trimws(readLines(textConnection(trimws(result$stdout))))

  which_lines_blank <- which(lines == "")
  provider_title <- lines[which_lines_blank + 1]
  alg_start <- which_lines_blank + 2
  alg_end <- c(which_lines_blank[-1] - 1, length(lines))
  alg_indices_lst <- Map(seq, alg_start, alg_end)
  alg_indices <- unlist(alg_indices_lst)

  alg_split <- stringr::str_split(lines, "\\s+", n = 2)
  alg_full_id <- vapply(alg_split, "[", 1, FUN.VALUE = character(1))
  alg_title <- vapply(alg_split, "[", 2, FUN.VALUE = character(1))

  alg_id_split <- strsplit(alg_full_id, ":", fixed = TRUE)
  provider <- vapply(alg_id_split, "[", 1, FUN.VALUE = character(1))
  alg_id <- vapply(alg_id_split, "[", 2, FUN.VALUE = character(1))

  algorithms <- tibble::tibble(
    provider = do.call("[", list(provider, alg_indices)),
    provider_title = unlist(Map(rep, provider_title, each = vapply(alg_indices_lst, length, integer(1)))),
    algorithm = do.call("[", list(alg_full_id, alg_indices)),
    algorithm_id = do.call("[", list(alg_id, alg_indices)),
    algorithm_title = do.call("[", list(alg_title, alg_indices))
  )

  # sometimes items such as 'Models' don't have algorithm IDs listed
  algorithms[!is.na(algorithms$algorithm_id), ]
}

# environment for cache
qgisprocess_cache <- new.env(parent = emptyenv())
qgisprocess_cache$path <- NULL
qgisprocess_cache$version <- NULL
qgisprocess_cache$algorithms <- NULL
qgisprocess_cache$help_text <- NULL
