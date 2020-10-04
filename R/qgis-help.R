
#' Show algorithm help
#'
#' @inheritParams qgis_run_algorithm
#'
#' @export
#'
#' @examples
#' if (has_qgis()) qgis_show_help("native:filedownloader")
#' if (has_qgis()) qgis_description("native:filedownloader")
#' if (has_qgis()) qgis_arguments("native:filedownloader")
#'
qgis_show_help <- function(algorithm) {
  cat(qgis_help_text(algorithm))
  cat("\n")
  invisible(algorithm)
}

#' @rdname qgis_show_help
#' @export
qgis_description <- function(algorithm) {
  vapply(
    algorithm,
    function(x) qgis_parsed_help(algorithm)$description,
    character(1)
  )
}

#' @rdname qgis_show_help
#' @export
qgis_arguments <- function(algorithm) {
  qgis_parsed_help(algorithm)$arguments
}

#' @rdname qgis_show_help
#' @export
qgis_outputs <- function(algorithm) {
  qgis_parsed_help(algorithm)$outputs
}

qgis_help_text <- function(algorithm) {
  if (algorithm %in% names(qgisprocess_cache$help_text)) {
    return(qgisprocess_cache$help_text[[algorithm]])
  }

  assert_qgis()
  assert_qgis_algorithm_or_model_file(algorithm)

  result <- qgis_run(
    args = c("help", algorithm)
  )

  qgisprocess_cache$help_text[[algorithm]] <- result$stdout
  result$stdout
}

qgis_parsed_help <- function(algorithm) {
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

  args <- stringr::str_match_all(
    sec_args,
    stringr::regex(
      paste0(
        "^([A-Za-z0-9_]+):\\s+([A-Za-z0-9_ .-]+)\n\\s*",
        "Argument type:\\s+([A-Za-z0-9_]+)"
      ),
      dotall = TRUE, multiline = TRUE
    )
  )[[1]]

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
        "^([A-Za-z0-9_]+):\\s+<([A-Za-z0-9_ .-]+)>\n\\s([A-Za-z0-9_ .]+)\\s*"),
      dotall = TRUE, multiline = TRUE
    )
  )[[1]]

  list(
    description = sec_description,
    arguments = tibble::tibble(
      name = args[, 2, drop = TRUE],
      description = args[, 3, drop = TRUE],
      qgis_type = args[, 4, drop = TRUE]
    ),
    outputs = tibble::tibble(
      name = outputs[, 2, drop = TRUE],
      description = outputs[, 4, drop = TRUE],
      qgis_output_type = outputs[, 3, drop = TRUE]
    )
  )
}

