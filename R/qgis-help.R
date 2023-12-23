#' Get detailed information about one algorithm
#'
#' @family topics about information on algorithms & processing providers
#'
#' @param algorithm A qualified algorithm name
#' (e.g., `"native:buffer"`).
#'
#' @returns
#' - `qgis_get_description()`: a string.
#' - `qgis_get_argument_specs()`, `qgis_get_output_specs()`: a tibble.
#' - `qgis_show_help()`: the algorithm name, invisibly.
#'
#' @examplesIf has_qgis()
#' qgis_get_description("native:filedownloader")
#' \donttest{
#' # not running below examples in R CMD check to save time
#' qgis_get_argument_specs("native:filedownloader")
#' qgis_get_output_specs("native:filedownloader")
#' qgis_show_help("native:filedownloader")
#' }
#'
#' @export
qgis_show_help <- function(algorithm) {
  cat(qgis_help_text(algorithm))
  cat("\n")
  invisible(algorithm)
}

#' @rdname qgis_show_help
#' @export
qgis_get_description <- function(algorithm) {
  vapply(
    algorithm,
    function(x) qgis_parse_help(algorithm)$description,
    character(1)
  )
}

#' @keywords internal
extract_type_component <- function(param_element, component) {
  type <- param_element[["type"]]
  if (length(type) == 1L && inherits(type, "character")) {
    if (component == "id") return(type) else return(NULL)
  }
  type[[component]]
}

#' @rdname qgis_show_help
#' @export
qgis_get_argument_specs <- function(algorithm) {
  if (qgis_using_json_output()) {
    help <- qgis_help_json(algorithm)
    out <- tibble::tibble(
      name = names(help$parameters),
      description = vapply(help$parameters, "[[", character(1), "description"),
      qgis_type = vapply(help$parameters, extract_type_component, character(1), "id"),
      default_value = lapply(help$parameters, "[[", "default_value"),
      available_values = lapply(help$parameters, "[[", c("raw_definition", "options")),
      acceptable_values = lapply(help$parameters, extract_type_component, "acceptable_values")
    )

    out[] <- lapply(out, unname)

    # The order of the parameters is alphabetized in JSON but has a
    # natural ordering in the parsed help text (which we need for backward
    # compatibility)
    out_legacy <- qgis_parse_help(algorithm)$arguments

    out[match(out_legacy$name, out$name), ]
  } else {
    qgis_parse_help(algorithm)$arguments
  }
}

#' @rdname qgis_show_help
#' @export
qgis_get_output_specs <- function(algorithm) {
  if (qgis_using_json_output()) {
    help <- qgis_help_json(algorithm)
    out <- tibble::tibble(
      name = names(help$outputs),
      description = vapply(help$outputs, "[[", character(1), "description"),
      qgis_output_type = vapply(help$outputs, "[[", character(1), "type")
    )

    out[] <- lapply(out, unname)
    out
  } else {
    qgis_parse_help(algorithm)$outputs
  }
}

#' @keywords internal
qgis_help_json <- function(algorithm) {
  cached <- help_cache_file(algorithm, json = TRUE)
  if (qgis_using_cached_help() && file.exists(cached)) {
    try(return(jsonlite::fromJSON(readRDS(cached))))
  }

  assert_qgis()
  assert_qgis_algorithm(algorithm)

  result <- qgis_run(
    args = c("--json", "help", algorithm),
    encoding = "UTF-8"
  )

  try({
    if (!dir.exists(dirname(cached))) dir.create(dirname(cached))
    saveRDS(result$stdout, cached)
  })

  jsonlite::fromJSON(result$stdout)
}

qgis_help_text <- function(algorithm) {
  cached <- help_cache_file(algorithm, json = FALSE)
  if (qgis_using_cached_help() && file.exists(cached)) {
    try(return(readRDS(cached)))
  }

  assert_qgis()
  assert_qgis_algorithm(algorithm)

  result <- qgis_run(
    args = c("help", algorithm)
  )

  try({
    if (!dir.exists(dirname(cached))) dir.create(dirname(cached))
    saveRDS(result$stdout, cached)
  })

  result$stdout
}

qgis_parse_help <- function(algorithm) {
  help_text <- trimws(qgis_help_text(algorithm))

  sec_description <- stringr::str_match(
    help_text,
    stringr::regex(
      "-+\\s+Description\\s+-+\\s+(.*?)\\s+-+\\s+(Arguments|Outputs)",
      dotall = TRUE, multiline = TRUE
    )
  )[, 2, drop = TRUE]

  sec_args <- stringr::str_match(
    help_text,
    stringr::regex(
      "-+\\s+Arguments\\s+-+\\s+(.*?)\\s+-+\\s+Outputs",
      dotall = TRUE, multiline = TRUE
    )
  )[, 2, drop = TRUE]

  sec_args_lines <- stringr::str_trim(readLines(textConnection(sec_args)), side = "right")
  arg_start <- stringr::str_which(sec_args_lines, "^[^\\s]")
  arg_end <- if (length(arg_start) == 0) character(0) else c(arg_start[-1] - 1, length(sec_args_lines))
  arg_text <- unlist(
    Map(
      function(a, b) paste(sec_args_lines[a:b], collapse = "\n"),
      arg_start + 1,
      arg_end
    )
  )

  arg_info <- stringr::str_split(sec_args_lines[arg_start], "\\s*:\\s*", n = 2)
  arg_type <- stringr::str_match(arg_text, "Argument type:\\s*(.+)")[, 2, drop = TRUE]
  arg_sec_available <- stringr::str_match(
    arg_text,
    stringr::regex(
      "Available values:\\s*\\n\\s*-\\s*[0-9]+\\s*:\\s*(.+?)\\s*Acceptable values",
      multiline = TRUE, dotall = TRUE
    )
  )[, 2, drop = TRUE]
  arg_available <- stringr::str_split(arg_sec_available, "\\n\\s*-\\s*[0-9]+\\s*:\\s*")
  arg_available[is.na(arg_sec_available)] <- list(character(0))

  arg_sec_acceptable <- stringr::str_match(
    arg_text,
    stringr::regex("Acceptable values:\\s*\\n\\s*-\\s*(.+)", multiline = TRUE, dotall = TRUE)
  )[, 2, drop = TRUE]
  arg_acceptable <- stringr::str_split(arg_sec_acceptable, "\\n\\s*-\\s*")
  arg_acceptable[is.na(arg_sec_acceptable)] <- list(character(0))

  sec_outputs <- stringr::str_match(
    help_text,
    stringr::regex(
      "-+\\s+Outputs\\s+-+\\s+(.*)",
      dotall = TRUE, multiline = TRUE
    )
  )[, 2, drop = TRUE]

  outputs <- stringr::str_match_all(
    sec_outputs,
    stringr::regex(
      paste0(
        "^([A-Za-z0-9_]+):\\s+<([A-Za-z0-9_ .-]+)>\n\\s([A-Za-z0-9_ .]+)\\s*"
      ),
      dotall = TRUE, multiline = TRUE
    )
  )[[1]]

  # if there are no outputs, there won't be a match here
  outputs <- outputs[!is.na(outputs[, 1, drop = TRUE]), , drop = FALSE]

  list(
    description = sec_description,
    arguments = tibble::tibble(
      name = vapply(arg_info, "[[", 1, FUN.VALUE = character(1)),
      description = vapply(arg_info, "[[", 2, FUN.VALUE = character(1)),
      qgis_type = arg_type,
      available_values = arg_available,
      acceptable_values = arg_acceptable
    ),
    outputs = tibble::tibble(
      name = outputs[, 2, drop = TRUE],
      description = outputs[, 4, drop = TRUE],
      qgis_output_type = outputs[, 3, drop = TRUE]
    )
  )
}

qgis_using_cached_help <- function() {
  opt <- getOption(
    "qgisprocess.use_cached_help",
    Sys.getenv("R_QGISPROCESS_USE_CACHED_HELP", "true")
  )

  isTRUE(opt) || identical(opt, "true") || identical(opt, "TRUE")
}

help_cache_file <- function(algorithm, json) {
  hash <- rlang::hash(
    list(
      qgisprocess_cache$path,
      qgis_version(),
      utils::packageVersion("qgisprocess"),
      json
    )
  )

  alg <- gsub(":", "_", algorithm)

  file.path(
    qgis_cache_dir(),
    glue("help-{ alg }-{ hash }.rds")
  )
}
