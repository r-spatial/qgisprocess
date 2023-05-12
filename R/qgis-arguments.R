#' Type coercion for arguments to QGIS processing algorithms
#'
#' Calls to [qgis_run_algorithm()] can and should contain R objects that
#' need to be serialized before they are passed to the command line. In
#' some cases (e.g., sf objects), temporary files need to be written and
#' cleaned up. The `as_qgis_argument()` and `qgis_clean_argument()` S3
#' generics provide hooks for argument values to be serialized correctly.
#'
#' @param x An object passed to a QGIS processing algorithm
#' @param value The result of [as_qgis_argument()] after the QGIS processing
#'   algorithm has been run.
#' @param spec A `list()` with values for `algorithm`, `name`,
#'   `description`, and `qgis_type`. See [qgis_argument_spec()] to
#'   create a blank `spec` for testing.
#' @param .use_json_input,use_json_input TRUE if the arguments will be
#'   serialized as JSON instead of serialized as a command-line argument.
#' @name as_qgis_argument


#' @rdname as_qgis_argument
#' @keywords internal
#' @export
as_qgis_argument <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  UseMethod("as_qgis_argument")
}


# @param .algorithm_arguments The result of [qgis_get_argument_specs()]
#' @keywords internal
qgis_sanitize_arguments <- function(algorithm, ..., .algorithm_arguments = qgis_get_argument_specs(algorithm),
                                    .use_json_input = FALSE) {
  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_named(dots)) {
    abort("All ... arguments to `qgis_sanitize_arguments()` must be named.")
  }

  # get QGIS types, values, and names for this algorithm
  arg_meta <- .algorithm_arguments

  # specifying an argument twice is the command-line equivalent
  # of passing multiple values. Here, we generate a qgis_list_input()
  # and let qgis_serialize_arguments() take care of the details
  dot_names <- names(dots)
  duplicated_dot_names <- unique(dot_names[duplicated(dot_names)])
  regular_dot_names <- setdiff(dot_names, duplicated_dot_names)

  user_args <- vector("list", length(unique(dot_names)))
  names(user_args) <- unique(dot_names)
  user_args[regular_dot_names] <- dots[regular_dot_names]
  for (arg_name in duplicated_dot_names) {
    items <- unname(dots[dot_names == arg_name])
    user_args[[arg_name]] <- qgis_list_input(!!!items)
  }

  # warn about unspecified arguments (don't error so that users can
  # write code for more than one QGIS install if args are added)
  unknown_args <- setdiff(names(dots), c("PROJECT_PATH", "ELLIPSOID", arg_meta$name))
  if (length(unknown_args) > 0) {
    for (arg_name in unknown_args) {
      message(glue("Ignoring unknown input '{ arg_name }'"))
    }
  }
  special_args <- user_args[c("PROJECT_PATH", "ELLIPSOID")]
  special_args <- special_args[!sapply(special_args, is.null)]

  # generate argument list template and populate user-supplied values
  args <- rep(list(qgis_default_value()), nrow(arg_meta))
  names(args) <- arg_meta$name
  args[intersect(names(args), names(user_args))] <- user_args[intersect(names(args), names(user_args))]
  args <- c(args, special_args)

  # get argument specs to pass to as_qgis_argument()
  arg_spec <- Map(
    qgis_argument_spec_by_name,
    rep_len(list(algorithm), length(args)),
    names(args),
    rep_len(list(arg_meta), length(args))
  )
  names(arg_spec) <- names(args)

  # sanitize arguments but make sure to clean up everything if one of the sanitizers errors
  args_sanitized <- vector("list", length(args))
  names(args_sanitized) <- names(args)

  # on.exit() works because it evaluates in the execution environment
  # (so `all_args_sanitized` can be set to TRUE)
  all_args_sanitized <- FALSE
  on.exit(if (!all_args_sanitized) qgis_clean_arguments(args_sanitized))

  for (name in names(args)) {
    args_sanitized[[name]] <-
      as_qgis_argument(args[[name]], arg_spec[[name]], use_json_input = .use_json_input)
  }
  all_args_sanitized <- TRUE

  # remove instances of qgis_default_value()
  is_default_value <- vapply(args_sanitized, is_qgis_default_value, logical(1))
  args_sanitized <- args_sanitized[!is_default_value]

  args_sanitized
}


#' @keywords internal
unclass_recursive <- function(x) {
  is_list <- vapply(x, is.list, logical(1))
  x[is_list] <- lapply(x[is_list], unclass_recursive)
  lapply(x, unclass)
}


#' @keywords internal
# turn sanitized arguments into command-line arguments
qgis_serialize_arguments <- function(arguments, use_json_input = FALSE) {
  if (use_json_input) {
    arguments <- unclass_recursive(arguments)
    arglist <-
      list(
        inputs = arguments[!(names(arguments) %in% c("ELLIPSOID", "PROJECT_PATH"))],
        ellipsoid = arguments$ELLIPSOID,
        project_path = arguments$PROJECT_PATH
      )
    arglist <- arglist[!vapply(arglist, is.null, logical(1))]
    jsonlite::toJSON(arglist, auto_unbox = TRUE)
  } else {
    args_dict <- vapply(arguments, inherits, logical(1), "qgis_dict_input")
    if (any(args_dict)) {
      labels <- names(arguments)[args_dict]
      abort("`qgis_run_algorithm()` can't generate command-line arguments from `qgis_dict_input()`")
    }

    # otherwise, unlist() will flatten qgis_list_input() items
    args_flat <- unlist(arguments)
    arg_name_n <- vapply(arguments, length, integer(1))
    names(args_flat) <- unlist(Map(rep, names(arguments), arg_name_n))

    if (length(args_flat) > 0) {
      paste0("--", names(args_flat), "=", vapply(args_flat, as.character, character(1)))
    } else {
      character(0)
    }
  }
}

#' @keywords internal
qgis_clean_arguments <- function(arguments) {
  lapply(arguments, qgis_clean_argument)
  invisible(NULL)
}

#' @keywords internal
qgis_default_value <- function() {
  structure(list(), class = "qgis_default_value")
}

#' @keywords internal
is_qgis_default_value <- function(x) {
  inherits(x, "qgis_default_value")
}

# All `qgis_type` values:
# c("source", "sink", "raster", "band", "boolean", "string", "rasterDestination",
#   "crs", "distance", "field", "vectorDestination", "multilayer",
#   "enum", "extent", "number", "file", "folderDestination", "fileDestination",
#   "vector", "point", "range", "expression", "aggregates", "layout",
#   "layer", "layoutitem", "maptheme", "matrix", "fields_mapping",
#   "coordinateoperation", "tininputlayers", "vectortilewriterlayers",
#   "execute_sql", "raster_calc_expression", "relief_colors", "color"
# )

#' @keywords internal
#' @export
as_qgis_argument.default <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  abort(
    glue(
      paste0(
        "Don't know how to convert object of type ",
        "'{ paste(class(x), collapse = \" / \") }' ",
        "to QGIS type '{ spec$qgis_type }'"
      )
    )
  )
}

#' @keywords internal
#' @export
as_qgis_argument.list <- function(x, spec = qgis_argument_spec(), use_json_input = FALSE) {
  if (use_json_input) {
    return(x)
  }

  NextMethod()
}

#' @keywords internal
#' @export
as_qgis_argument.qgis_default_value <- function(x, spec = qgis_argument_spec(),
                                                use_json_input = FALSE) {
  # This is an opportunity to fill in a missing value based on the
  # information provided in `spec` (or `qgis_default_value()` to keep missingness).

  if (isTRUE(spec$qgis_type %in% c("sink", "vectorDestination"))) {
    message(glue("Using `{ spec$name } = qgis_tmp_vector()`"))
    qgis_tmp_vector()
  } else if (isTRUE(spec$qgis_type == "rasterDestination")) {
    message(glue("Using `{ spec$name } = qgis_tmp_raster()`"))
    qgis_tmp_raster()
  } else if (isTRUE(spec$qgis_type == "folderDestination")) {
    message(glue("Using `{ spec$name } = qgis_tmp_folder()`"))
    qgis_tmp_folder()
  } else if (isTRUE(spec$qgis_type == "fileDestination")) {
    # these are various types of files (pdf, raster stats, etc.)
    message(glue("Using `{ spec$name } = qgis_tmp_file(\"\")`"))
    qgis_tmp_file("")
  } else if (isTRUE(spec$qgis_type == "enum") && length(spec$available_values) > 0) {
    default_enum_value <- rlang::as_label(spec$available_values[1])
    message(glue("Using `{ spec$name } = { default_enum_value }`"))
    if (use_json_input) 0 else "0"
  } else {
    # We don't know the actual default values here as far as I can tell
    message(glue("Argument `{ spec$name }` is unspecified (using QGIS default value)."))
    qgis_default_value()
  }
}

#' @keywords internal
#' @export
as_qgis_argument.NULL <- function(x, spec = qgis_argument_spec(),
                                  use_json_input = FALSE) {
  # NULL is similar to qgis_default_value() except it (1) never fills in
  # a default value at the R level and (2) never generates any messages.
  # It returns qgis_default_value() because this is the sentinel for removing
  # an item from the system call to `qgis_process`
  qgis_default_value()
}

#' @keywords internal
#' @export
as_qgis_argument.character <- function(x, spec = qgis_argument_spec(),
                                       use_json_input = FALSE) {
  result <- switch(as.character(spec$qgis_type),
    field = if (use_json_input) x else paste0(x, collapse = ";"),
    enum = {
      x_int <- match(x, spec$available_values)
      invalid_values <- x[is.na(x_int)]

      if (length(invalid_values) > 0) {
        abort(
          paste0(
            glue("All values for input '{ spec$name }' must be one of the following:\n\n"),
            glue::glue_collapse(
              paste0('"', spec$available_values, '"'),
              ", ",
              last = " or "
            )
          )
        )
      }

      x_int - 1
    },
    x
  )

  if (use_json_input) result else paste(result, collapse = ",")
}

#' @keywords internal
#' @export
as_qgis_argument.logical <- function(x, spec = qgis_argument_spec(),
                                     use_json_input = FALSE) {
  if (use_json_input) x else paste0(x, collapse = ",")
}

#' @keywords internal
#' @export
as_qgis_argument.numeric <- function(x, spec = qgis_argument_spec(),
                                     use_json_input = FALSE) {
  if (use_json_input) x else paste0(x, collapse = ",")
}

#' @rdname as_qgis_argument
#' @keywords internal
#' @export
qgis_clean_argument <- function(value) {
  UseMethod("qgis_clean_argument")
}

#' @keywords internal
#' @export
qgis_clean_argument.default <- function(value) {
  # by default, do nothing!
}

#' @keywords internal
#' @export
qgis_clean_argument.qgis_tempfile_arg <- function(value) {
  unlink(value)
}


#' Specify QGIS argument types
#'
#' @param name,description,qgis_type,available_values,acceptable_values
#'   Column values of `arguments` denoting
#'   the argument name, description, and acceptable values.
#' @inheritParams qgis_run_algorithm
#'
#' @return A [list()] with an element for each input argument.
#'
#' @examples
#' # These became internal functions!
#' qgisprocess:::qgis_argument_spec()
#' if (has_qgis()) qgisprocess:::qgis_argument_spec_by_name("native:filedownloader", "URL")
#' @name qgis_argument_spec


#' @keywords internal
qgis_argument_spec <- function(algorithm = NA_character_, name = NA_character_,
                               description = NA_character_, qgis_type = NA_character_,
                               available_values = character(0), acceptable_values = character(0)) {
  list(
    algorithm = algorithm,
    name = name,
    description = description,
    qgis_type = qgis_type,
    available_values = available_values,
    acceptable_values = acceptable_values
  )
}

#' @keywords internal
qgis_argument_spec_by_name <- function(algorithm, name,
                                       .algorithm_arguments = qgis_get_argument_specs(algorithm)) {
  # These are special-cased at the command-line level, so they don't have
  # types defined in the help file. Here, we create two special types
  # ELLIPSOID and PROJECT_PATH.
  if (isTRUE(name %in% c("ELLIPSOID", "PROJECT_PATH"))) {
    return(qgis_argument_spec(algorithm, name, name))
  }

  position <- match(name, .algorithm_arguments$name)
  if (is.na(position)) {
    abort(
      glue(
        paste0(
          "'{ name }' is not an argument for algorithm '{ algorithm }'.",
          "\nSee `qgis_show_help(\"{ algorithm }\")` for a list of supported arguments."
        )
      )
    )
  }

  c(list(algorithm = algorithm), lapply(.algorithm_arguments, "[[", position))
}


#' Prepare a compound input argument
#'
#' Some algorithm arguments require a compound object, consisting of
#' several layers or elements.
#' These functions apply strict validation rules when generating this object and
#' are recommended.
#'
#' `qgis_list_input()` generates an unnamed list of class `qgis_list_input`.
#' The use of `qgis_list_input()` instead of list() is _required_ for compound
#' arguments _in case of no-JSON input_ (see [qgis_using_json_input()]).
#' Since it applies strict validation rules, it is recommended in all cases
#' though.
#'
#' `qgis_dict_input()` generates an named list of class `qgis_dict_input`.
#' `qgis_dict_input()` is only supported when the JSON input method applies
#' (see [qgis_using_json_input()]), where it can be interchanged with a named `list()`.
#' It can only be used for arguments requiring _named_ lists.
#' Since it applies strict validation rules, it is recommended above `list()`.
#'
#' @concept topics about preparing input values
#'
#' @param ... Named values for `qgis_dict_input()` or
#'   unnamed values for `qgis_list_input()`.
#'
#' @return
#'   - `qgis_list_input()`: An object of class 'qgis_list_input'
#'   - `qgis_dict_input()`: An object of class 'qgis_dict_input'
#' @export
#'
#' @examples
#' qgis_list_input(1, 2, 3)
#' qgis_dict_input(a = 1, b = 2, c = 3)
#'
qgis_list_input <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) > 0 && rlang::is_named(dots)) {
    abort("All ... arguments to `qgis_list_input()` must be unnamed.")
  }

  structure(dots, class = "qgis_list_input")
}

#' @rdname qgis_list_input
#' @export
qgis_dict_input <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_dictionaryish(dots)) {
    abort("All ... arguments to `qgis_dict_input()` must have unique names.")
  }

  structure(dots, class = "qgis_dict_input")
}

#' @keywords internal
#' @export
as_qgis_argument.qgis_list_input <- function(x, spec = qgis_argument_spec(),
                                             use_json_input = FALSE) {
  qgis_list_input(!!!lapply(x, as_qgis_argument, spec = spec, use_json_input = use_json_input))
}

#' @keywords internal
#' @export
as_qgis_argument.qgis_dict_input <- function(x, spec = qgis_argument_spec(),
                                             use_json_input = FALSE) {
  qgis_dict_input(!!!lapply(x, as_qgis_argument, spec = spec, use_json_input = use_json_input))
}

#' @keywords internal
#' @export
qgis_clean_argument.qgis_list_input <- function(value) {
  lapply(value, qgis_clean_argument)
}

#' @keywords internal
#' @export
qgis_clean_argument.qgis_dict_input <- function(value) {
  lapply(value, qgis_clean_argument)
}
