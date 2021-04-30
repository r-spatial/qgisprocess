#' Run algorithms using 'qgis_process'
#'
#' Run QGIS algorithms.
#' See the [QGIS docs](https://docs.qgis.org/testing/en/docs/user_manual/processing_algs/qgis/index.html)
#' for a detailed description of the algorithms provided
#' 'out of the box' on QGIS (versions >= 3.14).
#'
#' @param algorithm A qualified algorithm name (e.g., "native:filedownloader") or
#'   a path to a QGIS model file.
#' @param provider A provider identifier (e.g., "native")
#' @param PROJECT_PATH,ELIPSOID Global values for QGIS project file and
#'   elipsoid name for distance calculations.
#' @param ... Named key-value pairs as arguments for each algorithm. Features of
#'   [rlang::list2()] are supported. These arguments
#'   are converted to strings using [as_qgis_argument()].
#' @param .quiet Use `TRUE` to suppress output from processing algorithms.
#'
#' @export
#'
#' @examples
#' if (has_qgis()) qgis_has_algorithm("native:filedownloader")
#' if (has_qgis()) qgis_has_provider("native")
#' if (has_qgis()) qgis_providers()
#'
#' if (has_qgis()) {
#'   qgis_run_algorithm(
#'     "native:buffer",
#'     INPUT = system.file("longlake/longlake_depth.gpkg", package = "qgisprocess"),
#'     DISTANCE = 10
#'   )
#' }
#'
qgis_run_algorithm <- function(algorithm, ..., PROJECT_PATH = NULL, ELIPSOID = NULL, .quiet = FALSE) {
  assert_qgis()
  assert_qgis_algorithm(algorithm)
  # use list2 so that users can !!! argument lists
  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_named(dots)) {
    abort("All ... arguments to `qgis_run_algorithm()` must be named.")
  }

  # generate an argument template and fill in provided arguments
  arg_meta <- qgis_arguments(algorithm)
  # take care of multiple input arguments
  # so far this works only if only one argument is duplicated (but I can't
  # remember that a QGIS algorithm has more than 1 argument of type multiple
  # input)
  dups <- dots[duplicated(names(dots)) |
                 duplicated(names(dots), fromLast = TRUE)]
  ind = `if`(length(dups) > 0, arg_meta$name == names(dups[1]), NA)
  ro <- 1:nrow(arg_meta)  # row order
  r <- rep(1, nrow(arg_meta))  # number of times to be repeated
  r[ind] <- length(dups)
  arg_meta <- arg_meta[rep(ro, times = r), ]
  args = rlang::set_names(c(arg_meta$name, "PROJECT_PATH", "ELIPSOID"))
  # we need to write it like this due to duplicated names
  ind = names(args) %in% names(dots)
  args[ind] <- dots[names(args)[ind]]
  args[!ind] <-
    lapply(args[!ind], function(x) qgis_default_value())
  args["PROJECT_PATH"] <- list(PROJECT_PATH)
  args["ELIPSOID"] <- list(ELIPSOID)

  # warn about unspecified arguments (don't error so that users can
  # write code for more than one QGIS install if args are added)
  unknown_args <- setdiff(names(dots), names(args))
  if (length(unknown_args) > 0){
    for (arg_name in unknown_args) {
      message(glue("Ignoring unknown input '{ arg_name }'"))
    }
  }

  # get argument info for supplied args and run sanitizers
  arg_spec <- Map(qgis_argument_spec_by_name, list(algorithm), names(args), list(arg_meta))
  args <- Map(
    # have to do this omitting errors so that qgis_clean_argument()
    # is called on anything that succeeded regardless of other arg failures
    function(x, spec) try(as_qgis_argument(x, spec), silent = TRUE),
    args,
    arg_spec
  )

  # remove instances of qgis_default_value()
  is_default_value <- vapply(args, is_qgis_default_value, logical(1))
  args <- args[!is_default_value]
  arg_spec <- arg_spec[!is_default_value]

  # make sure cleanup is run on any temporary files created
  on.exit(Map(qgis_clean_argument, args, arg_spec))

  # look for sanitizer errors and stop() for them
  arg_errors <- vapply(args, inherits, "try-error", FUN.VALUE = logical(1))
  if (any(arg_errors)) {
    abort(args[arg_errors][[1]])
  }

  if (length(args) > 0) {
    args_str <- paste0("--", names(args), "=", vapply(args, as.character, character(1)))
  } else {
    args_str <- character(0)
  }

  # To get around a bug in processx (#302), we need to use a stdout callback
  # to buffer stdout manually. For large outputs this would be slow, but
  # the size of the buffer seems to be large enough that this doesn't
  # matter.
  stdout_output <- ""

  if (.quiet) {
    result <- qgis_run(
      args = c("run", algorithm, args_str),
      stdout_callback = function(x, ...) {
        stdout_output <<- paste0(stdout_output, x)
      }
    )
  } else {
    result <- qgis_run(
      args = c("run", algorithm, args_str),
      echo_cmd = TRUE,
      stdout_callback = function(x, ...) {
        stdout_output <<- paste0(stdout_output, x)
        cat(x)
      },
      stderr_callback = function(x, ...) message(x, appendLF = FALSE)
    )
    cat("\n")
  }

  # return a custom object to keep as much information as possible
  # about the output
  structure(
    rlang::list2(
      # ... eventually, this will contain the parsed output values
      !!! qgis_parse_results(algorithm, stdout_output),
      .algorithm = algorithm,
      .args = args,
      .processx_result = result
    ),
    class = "qgis_result"
  )
}

#' @rdname qgis_run_algorithm
#' @export
qgis_has_algorithm <- function(algorithm) {
  assert_qgis()
  as.character(algorithm) %in% qgis_algorithms()$algorithm
}

#' @rdname qgis_run_algorithm
#' @export
qgis_has_provider <- function(provider) {
  assert_qgis()
  as.character(provider) %in% unique(qgis_algorithms()$provider)
}

#' @rdname qgis_run_algorithm
#' @export
qgis_providers <- function(provider) {
  assert_qgis()
  algs <- qgis_algorithms()
  algs[!duplicated(algs$provider), c("provider", "provider_title")]
}

#' @rdname qgis_run_algorithm
#' @export
assert_qgis_algorithm <- function(algorithm) {
  if (!is.character(algorithm) || length(algorithm) != 1) {
    abort("`algorithm` must be a character vector of length 1")
  } else if (!qgis_has_algorithm(algorithm)) {
    abort(
      glue(
        "Can't find QGIS algorithm '{ algorithm }'.\nRun `qgis_algorithms()` for a list of available algorithms."
      )
    )
  }

  invisible(algorithm)
}
