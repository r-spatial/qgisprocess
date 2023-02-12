
#' Access algorithm results
#'
#' @param x An object returned by [qgis_run_algorithm()].
#' @param which The name or index of an output.
#' @param default A default value if the output does not exist.
#' @param what Character vector of classes.
#' At least one class must be inherited by an element of `x` for that element
#' to be selected.
#'
#' @details
#' `qgis_result_single()` tries to extract the single most useful element
#' from a `qgis_result` object.
#'
#' @export
#'
is_qgis_result <- function(x) {
  inherits(x, "qgis_result")
}

#' @rdname is_qgis_result
#' @export
qgis_result_clean <- function(x) {
  args_chr <- as.character(x$.args[vapply(x$.args, is.character, logical(1))])
  unlink(args_chr[is_qgis_tmp_file(args_chr)], recursive = TRUE)
  invisible(x)
}

#' @rdname is_qgis_result
#' @export
qgis_output <- function(x, which, default = qgis_error_output_does_not_exist(x, which)) {
  if (is.numeric(which) && (which %in% seq_along(x))) {
    x[[which]]
  } else if (which %in% names(x)) {
    x[[which]]
  } else {
    default
  }
}

#' @rdname is_qgis_result
#' @export
qgis_error_output_does_not_exist <- function(x, which) {
  available_outputs <- glue::glue_collapse(
    paste0("'", setdiff(names(x), c(".algorithm", ".args", ".processx_result")), "'"),
    sep = ", ", last = " and "
  )

  abort(glue("Result has no output '{ which }'.\nAvailable outputs are { available_outputs }"))
}


#' @keywords internal
qgis_check_stdout <- function(x) {
  if (qgis_result_status(x) == 0L && qgis_result_stdout(x) == "") {
    stop(
      "The algorithm appears to have run without error, ",
      "but the output could not be captured in R.\n",
      "Please try again after running:\noptions(qgisprocess.use_json_output = FALSE); qgis_configure()\n",
      call. = FALSE
    )
  }
}


#' @rdname is_qgis_result
#' @export
qgis_result_single <- function(x, what) {

  # Limit result to elements that match class
  x <- x[vapply(x, inherits, what, FUN.VALUE = logical(1))]
  if (length(x) == 0L) {
    abort(
      paste(
        "Can't extract object from result: zero outputs of type",
        paste(what, collapse = " or ")
      )
    )
  }

  # By default, take the first element named as output or OUTPUT.
  # Otherwise, take the first element that matches class.
  result <- x[grepl("^(output|OUTPUT)$", names(x))][1][[1]]
  if (is.null(result)) result <- x[[1]]
  result

}



#' @rdname is_qgis_result
#' @export
qgis_result_status <- function(x) {
  x$.processx_result$status
}

#' @rdname is_qgis_result
#' @export
qgis_result_stdout <- function(x) {
  x$.processx_result$stdout
}

#' @rdname is_qgis_result
#' @export
qgis_result_stderr <- function(x) {
  x$.processx_result$stderr
}

#' @rdname is_qgis_result
#' @export
qgis_result_args <- function(x) {
  x$.args
}

#' @export
print.qgis_result <- function(x, ...) {
  cat(glue("<Result of `qgis_run_algorithm(\"{ x$.algorithm }\", ...)`>\n\n"))
  utils::str(x[!(names(x) %in% c(".algorithm", ".args", ".processx_result", ".raw_json_input"))], ...)
  invisible(x)
}
