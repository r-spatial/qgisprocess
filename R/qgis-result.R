#' Access algorithm results
#'
#' @param x An object returned by [qgis_run_algorithm()].
#' @param which The name or index of an output.
#' @param what Character vector of classes.
#' At least one class must be inherited by an element of `x` for that element
#' to be selected.
#'
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

qgis_leave_only_results <- function(x) {
  assert_that(inherits(x, "qgis_result"), length(which) == 1L)
  output_names <- setdiff(
    names(x),
    c(".algorithm", ".args", ".processx_result", ".raw_json_input")
  )
  x <- x[output_names]
}


#' @rdname is_qgis_result
#' @export
#'
qgis_extract_output_by_name <- function(x, name = "OUTPUT", single = TRUE) {
  x <- qgis_leave_only_results(x)
  if (name %in% names(x)) {
    x[[name]]
  } else {
    default_name = grepl("^(output|OUTPUT)$", name)
    result <- x[grepl("^(output|OUTPUT)$", names(x))][1][[1]]
    if (default_name && !is.null(result) && single) {
      return(result)
    } else if (default_name && is.null(result) && single) {
      return(x[[1]])
    }else {
      abort(
        qgis_error_output_does_not_exist(x, name)
      )
    }
  }
}


#' @rdname is_qgis_result
#' @export
#'
qgis_extract_output_by_position <- function(x, which) {
  x <- qgis_leave_only_results(x)
  if (is.numeric(which) && (which %in% seq_along(x))) {
    x[[which]]
  } else {
    abort(
      qgis_error_output_does_not_exist(x, which)
    )
  }
}


#' @rdname is_qgis_result
#' @export
qgis_extract_output_by_class <- function(x, class, single = TRUE) {
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


#' @rdname is_qgis_result
#' @export
#' @keywords internal

qgis_error_output_does_not_exist <- function(x, which) {
  assert_that(
    !any(names(x) %in%
      c(".algorithm", ".args", ".processx_result", ".raw_json_input")),
    inherits(x, "list")
  )
  available_outputs <- glue::glue_collapse(
    paste0("'", names(x), "'"),
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
