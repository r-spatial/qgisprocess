
#' Configure and run 'qgis_process'
#'
#' @param ...,timeout Passed to [processx::run()].
#' @param quiet Use `FALSE` to display more information about the command, possibly
#'   useful for debugging. Defaults to the value of [qgs_quiet()].
#' @param env A [list()] of environment variables.
#'
#' @return The result of [processx::run()].
#' @export
#'
#' @examples
#'
qgs_run <- function(..., timeout = 10, env = list(QT_QPA_PLATFORM='offscreen'), quiet = qgs_quiet()) {
  path <- qgs_path()
  result <- withr::with_envvar(
    env,
    processx::run(path, ..., timeout = timeout),
  )

  if (result$timeout) {
    stop(glue::glue("Command '{ path }' timed out ({ timeout } seconds)"), call. = FALSE)
  }

  result
}

#' @rdname qgs_run
#' @export
qgs_query_version <- function(timeout = 10) {
  result <- qgs_run(args = character(0), timeout = timeout)
  lines <- readLines(textConnection(result$stdout))
  stringr::str_match(lines[1], "\\(([0-9.]+[A-Za-z0-9.-]*)\\)")[, 2, drop = TRUE]
}

#' @rdname qgs_run
#' @export
qgs_query_algorithms <- function(timeout = 10) {
  result <- qgs_run(args = "list", timeout = timeout)
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

#' @rdname qgs_run
#' @export
qgs_path <- function() {
  getOption("qgisprocess.path", "qgis_process")
}

#' @rdname qgs_run
#' @export
qgs_quiet <- function() {
  getOption("qgisprocess.quiet", TRUE)
}

# environment for cache
qgisprocess_cache <- new.env(parent = emptyenv())
qgisprocess_cache$path <- NULL
qgisprocess_cache$algorithms <- NULL
