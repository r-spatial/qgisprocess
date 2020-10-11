
#' Type coercion for arguments to QGIS processing algorithms
#'
#' Calls to [qgis_run_algorithm()] can and should contain R objects that
#' need to be serialized before they are passed to the command line. In
#' some cases (e.g., sf objects), temporary files need to be written and
#' cleaned up. The [as_qgis_argument()] and [qgis_clean_argument()] S3
#' generics provide hooks for argument values to be serialized correctly.
#'
#' @param x An object passed to a QGIS processing algorithm
#' @param value The result of [as_qgis_argument()] after the QGIS processing
#'   algorithm has been run.
#' @param spec A `list()` with values for `algorithm`, `name`,
#'   `description`, and `qgis_type`. See [qgis_argument_spec()] to
#'   create a blank `spec` for testing.
#' @param name,description,qgis_type Column values of `arguments` denoting
#'   the argument name, its description, and its QGIS type name.
#' @param arguments The result of [qgis_arguments()].
#' @inheritParams qgis_run_algorithm
#'
#' @export
#'
as_qgis_argument <- function(x, spec = qgis_argument_spec()) {
  UseMethod("as_qgis_argument")
}

#' @rdname as_qgis_argument
#' @export
qgis_default_value <- function() {
  structure(list(), class = "qgis_default_value")
}

#' @rdname as_qgis_argument
#' @export
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

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.default <- function(x, spec = qgis_argument_spec()) {
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

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.qgis_default_value <- function(x, spec = qgis_argument_spec()) {
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
    "0"

  } else {
    # We don't know the actual default values here as far as I can tell
    message(glue("Argument `{ spec$name }` is unspecified (using QGIS default value)."))
    qgis_default_value()
  }
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.NULL <- function(x, spec = qgis_argument_spec()) {
  # NULL is similar to qgis_default_value() except it (1) never fills in
  # a default value at the R level and (2) never generates any messages.
  # It returns qgis_default_value() because this is the sentinel for removing
  # an item from the system call to `qgis_process`
  qgis_default_value()
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.character <- function(x, spec = qgis_argument_spec()) {
  switch(
    as.character(spec$qgis_type),
    field = paste0(x, collapse = ";"),
    enum = {
      x_int <- match(x, spec$available_values)
      invalid_values <- x[is.na(x_int)]

      if (length(invalid_values) > 0) {
        abort(
          paste0(
            glue("All values for input '{ spec$name }' must be one of the following:\n\n"),
            glue::glue_collapse(
              paste0('"', spec$available_values, '"'),
              ", ", last = " or "
            )
          )
        )
      }

      paste0(x_int - 1, collapse = ",")
    },
    paste0(x, collapse = ",")
  )
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.logical <- function(x, spec = qgis_argument_spec()) {
  paste0(x, collapse = ",")
}

#' @rdname as_qgis_argument
#' @export
as_qgis_argument.numeric <- function(x, spec = qgis_argument_spec()) {
  paste0(x, collapse = ",")
}

#' @rdname as_qgis_argument
#' @export
qgis_clean_argument <- function(value, spec = qgis_argument_spec()) {
  UseMethod("qgis_clean_argument")
}

#' @rdname as_qgis_argument
#' @export
qgis_clean_argument.default <- function(value, spec = qgis_argument_spec()) {
  # by default, do nothing!
}

#' @rdname as_qgis_argument
#' @export
qgis_clean_argument.qgis_tempfile_arg <- function(value, spec = qgis_argument_spec()) {
  unlink(value)
}

#' @rdname as_qgis_argument
#' @export
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

#' @rdname as_qgis_argument
#' @export
qgis_argument_spec_by_name <- function(algorithm, name, arguments = qgis_arguments(algorithm)) {
  # These are special-cased at the command-line level, so they don't have
  # types defined in the help file. Here, we create two special types
  # ELIPSOID and PROJECT_PATH.
  if (isTRUE(name %in% c("ELIPSOID", "PROJECT_PATH"))) {
    return(qgis_argument_spec(algorithm, name, name))
  }

  position <- match(name, arguments$name)
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

  c(list(algorithm = algorithm), lapply(arguments, "[[", position))
}
