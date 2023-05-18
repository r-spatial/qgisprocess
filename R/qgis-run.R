#' Call the 'qgis_process' command directly
#'
#' `qgis_run()` offers full access to 'qgis_process'.
#' Run `cat(qgis_run("--help")$stdout)` to get the command's help.
#'
#' @return
#' A [processx::run()] return value, i.e. a list with `status`, `stdout`,
#'  `stderr` and `timeout` elements.
#'
#' @param ... Passed to [processx::run()].
#' @param args Command-line arguments
#' @param env A [list()] of environment variables.
#' Defaults to
#' `getOption("qgisprocess.env", list(QT_QPA_PLATFORM = "offscreen"))`.
#' @param path A path to the 'qgis_process' executable. Defaults to
#' [qgis_path()].
#'
#' @family topics about programming or debugging utilities
#' @family topics about configuring QGIS and qgisprocess
#'
#' @export
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
