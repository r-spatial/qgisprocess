
#' Configure and run 'qgis_process'
#'
#' @param ... Passed to [processx::run()].
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
#'
qgis_run <- function(..., env = qgis_env(), quiet = qgis_quiet(), path = qgis_path()) {
  result <- withr::with_envvar(
    env,
    processx::run(path, ...),
  )

  result
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
assert_qgis <- function(action = stop) {
  if (!has_qgis()) {
    action(
      paste0(
        "The QGIS processing utility ('qgis_process') is not installed or could not be found.\n",
        "Run `qgis_configure()` to configure this location.\n",
        "If 'qgis_process' is installed, set `options(qgisprocess.path = '/path/to/qgis_process')`\n",
        "and re-run `qgis_configure()`."
      ),
      call. = FALSE
    )
  }
}

#' @rdname qgis_run
#' @export
qgis_configure <- function(quiet = FALSE) {
  tryCatch({
    qgisprocess_cache$path <- NULL
    qgisprocess_cache$version <- NULL
    qgisprocess_cache$algorithms <- NULL

    qgis_path(query = TRUE, quiet = quiet)
    qgis_version(query = TRUE, quiet = quiet)
    qgis_algorithms(query = TRUE, quiet = quiet)
  }, error = function(e) {
    # failed config!
    qgisprocess_cache$path <- NULL
    qgisprocess_cache$version <- NULL
    qgisprocess_cache$algorithms <- NULL

    if (!quiet) message(e)
  })

  invisible(has_qgis())
}

#' @rdname qgis_run
#' @export
qgis_version <- function(query = FALSE, quiet = qgis_quiet()) {
  if (query) {
    qgisprocess_cache$version <- qgis_query_version(quiet = quiet)
  }

  qgisprocess_cache$version
}

#' @rdname qgis_run
#' @export
qgis_algorithms <- function(query = FALSE, quiet = qgis_quiet()) {
  if (query) {
    qgisprocess_cache$algorithms <- qgis_query_algorithms(quiet = quiet)
  }

  qgisprocess_cache$algorithms
}

#' @rdname qgis_run
#' @export
qgis_path <- function(query = FALSE, quiet = qgis_quiet()) {
  if (query) {
    qgisprocess_cache$path <- qgis_query_path(quiet = quiet)
  }

  qgisprocess_cache$path
}

#' @rdname qgis_run
#' @export
qgis_query_path <- function(quiet = FALSE) {
  if (is.null(getOption("qgisprocess.path"))) {
    if (!quiet) message(glue::glue("Using 'qgis_process' on PATH"))
    qgis_run(path = "qgis_process", quiet = quiet)
    "qgis_process"
  } else {
    path <- getOption("qgisprocess.path", "qgis_process")
    if (!quiet) message(glue::glue("Using getOption('qgisprocess.path'): '{ path }'"))
    qgis_run(path = path, quiet = quiet)
    path
  }
}

#' @rdname qgis_run
#' @export
qgis_quiet <- function() {
  getOption("qgisprocess.quiet", TRUE)
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
  match <- stringr::str_match(lines[1], "\\(([0-9.]+[A-Za-z0-9.-]*)\\)")[, 2, drop = TRUE]
  if (identical(match, NA_character_)) {
    stop(
      paste0(
        "First line of output did not contain expected version information.\n",
        glue::glue("First line of output was '{ lines[1] }'")
      ),
      call. = FALSE
    )
  }

  match
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

  tibble::tibble(
    provider = do.call("[", list(provider, alg_indices)),
    provider_title = unlist(Map(rep, provider_title, each = vapply(alg_indices_lst, length, integer(1)))),
    algorithm = do.call("[", list(alg_full_id, alg_indices)),
    algorithm_id = do.call("[", list(alg_id, alg_indices)),
    algorithm_title = do.call("[", list(alg_title, alg_indices))
  )
}

# environment for cache
qgisprocess_cache <- new.env(parent = emptyenv())
qgisprocess_cache$path <- NULL
qgisprocess_cache$version <- NULL
qgisprocess_cache$algorithms <- NULL
