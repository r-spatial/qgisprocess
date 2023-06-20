#' Access processing output
#'
#' @description
#' These functions extract one output element from the result of
#' [qgis_run_algorithm()], potentially more than one in the case of
#' `qgis_extract_output_by_class()`.
#' An output element can be extracted based on its name, its position in the
#' printed `qgis_result` object returned by [qgis_run_algorithm()], or its
#' class.
#'
#' `qgis_extract_output()` is an alias to `qgis_extract_output_by_name()`.
#'
#' @concept main functions to access or manage processing results
#' @family topics about accessing or managing processing results
#'
#' @param x An object returned by [qgis_run_algorithm()].
#' @param which The index of an output.
#' @param name The name of an output.
#' @param class Character vector of classes.
#' At least one class must be inherited by an element of `x` for that element
#' to be selected.
#' @param single Logical.
#' Ensures the selection of a single output in `qgis_extract_output_by_class()`.
#' The `OUTPUT` or `output` element is taken if available and on condition that
#' it inherits a specified class; otherwise falls back
#' to the first element that inherits a specified class.
#' @param first Logical.
#' Should `qgis_extract_output_by_name()` fall back to the first
#' output element if the default `OUTPUT` or `output` element is not available?
#' Only takes effect if `name` is equal to `OUTPUT` or `output`, but not found.
#'
#' @return A `qgis_output*` object.
#'
#' @examples
#' if (has_qgis()) {
#'   result <- qgis_run_algorithm(
#'     "native:buffer",
#'     INPUT = system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
#'     DISTANCE = 10
#'   )
#'   qgis_extract_output(result)
#' }
#'
#' @name qgis_extract_output

#' @rdname qgis_extract_output
#' @export
qgis_extract_output_by_name <- function(x, name = "OUTPUT", first = TRUE) {
  assert_that(is.string(name))
  x <- qgis_leave_only_results(x)
  if (name %in% names(x)) {
    x[[name]]
  } else {
    default_name = grepl("^(output|OUTPUT)$", name)
    result <- x[grepl("^(output|OUTPUT)$", names(x))][1][[1]]
    if (default_name && !is.null(result)) {
      return(result)
    } else if (default_name && is.null(result) && first) {
      return(x[[1]])
    } else {
      qgis_error_output_does_not_exist(x, name)
    }
  }
}



#' @rdname qgis_extract_output
#' @export
qgis_extract_output <- qgis_extract_output_by_name



#' @keywords internal
qgis_leave_only_results <- function(x) {
  assert_that(inherits(x, "qgis_result"))
  output_names <- setdiff(
    names(x),
    c(".algorithm", ".args", ".processx_result", ".raw_json_input")
  )
  x[output_names]
}



#' @rdname qgis_extract_output
#' @export
qgis_extract_output_by_position <- function(x, which) {
  assert_that(is.number(which))
  x <- qgis_leave_only_results(x)
  if (is.numeric(which) && (which %in% seq_along(x))) {
    x[[which]]
  } else {
    qgis_error_output_does_not_exist(x, which)
  }
}


#' @rdname qgis_extract_output
#' @export
qgis_extract_output_by_class <- function(x, class, single = TRUE) {
  assert_that(is.character(class))
  x <- qgis_leave_only_results(x)
  # Limit result to elements that match class
  x <- x[vapply(x, inherits, class, FUN.VALUE = logical(1))]
  if (length(x) == 0L) {
    abort(
      paste(
        "Can't extract object from result: zero outputs of type",
        paste(class, collapse = " or ")
      )
    )
  }

  # By default, take the first element named as output or OUTPUT.
  # Otherwise, take the first element that matches class.
  if (single) {
    result <- x[grepl("^(output|OUTPUT)$", names(x))][1][[1]]
    if (is.null(result)) result <- x[[1]]
  } else {
    result <- x
  }

  result
}


#' @keywords internal
qgis_error_output_does_not_exist <- function(x, which) {
  assert_that(
    !any(names(x) %in%
      c(".algorithm", ".args", ".processx_result", ".raw_json_input")),
    inherits(x, "list")
  )
  available_outputs <- glue::glue_collapse(
    paste0("'", names(x), "' (", seq_along(names(x)), ")"),
    sep = ", ", last = " and "
  )

  abort(glue("Result has no output { which }.\nAvailable outputs are { available_outputs }"))
}



#' Clean processing results
#'
#' Deletes any temporary files that are defined in a
#' `qgis_result` object.
#' These may comprise both input and output files.
#'
#' @concept main functions to access or manage processing results
#' @family topics about accessing or managing processing results
#'
#' @inheritParams qgis_extract_output
#'
#' @export
qgis_clean_result <- function(x) {
  args_chr <- as.character(x$.args[vapply(x$.args, is.character, logical(1))])
  unlink(args_chr[is_qgis_tmp_file(args_chr)], recursive = TRUE)
  invisible(x)
}


#' Access processing results: extra tools
#'
#' A `qgis_result` object is a list that, next to the output elements,
#' also contains other elements that can be useful in scripting.
#' Several of these can be extracted with convenience functions:
#' the exit status of the process, standard output and standard error of
#' 'qgis_process', arguments passed to 'qgis_process'.
#'
#' @family topics about programming or debugging utilities
#' @family topics about accessing or managing processing results
#'
#' @inheritParams qgis_extract_output
#'
#' @name qgis_result_status

#' @rdname qgis_result_status
#' @export
qgis_result_status <- function(x) {
  x$.processx_result$status
}

#' @rdname qgis_result_status
#' @export
qgis_result_stdout <- function(x) {
  x$.processx_result$stdout
}

#' @rdname qgis_result_status
#' @export
qgis_result_stderr <- function(x) {
  x$.processx_result$stderr
}

#' @rdname qgis_result_status
#' @export
qgis_result_args <- function(x) {
  x$.args
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


#' @keywords internal
is_qgis_result <- function(x) {
  inherits(x, "qgis_result")
}


#' @keywords internal
#' @export
print.qgis_result <- function(x, ...) {
  cat(glue("<Result of `qgis_run_algorithm(\"{ x$.algorithm }\", ...)`>\n\n"))
  utils::str(x[!(names(x) %in% c(".algorithm", ".args", ".processx_result", ".raw_json_input"))], ...)
  invisible(x)
}
