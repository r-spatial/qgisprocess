qgis_parse_results <- function(algorithm, output) {
  if (stringr::str_detect(output, "^\\s*\\{")) {
    output_parsed <- jsonlite::fromJSON(output)
    outputs_list <- output_parsed$results
    output_names <- names(outputs_list)

    algorithm_outputs <- qgis_outputs(algorithm)

    Map(
      qgis_result_output,
      outputs_list,
      algorithm_outputs$qgis_output_type[match(output_names, algorithm_outputs$name)]
    )
  } else {
    sec_results <- stringr::str_match(
      output,
      stringr::regex(
        "-+\\s+Results\\s+-+\\s+(.*)",
        dotall = TRUE, multiline = TRUE
      )
    )[, 2, drop = TRUE]

    output_lines <- readLines(textConnection(trimws(sec_results)))
    outputs <- stringr::str_split(output_lines, "\\s*:\\s*", n = 2)
    outputs_list <- lapply(outputs, "[", 2)
    output_names <- vapply(outputs, "[", 1, FUN.VALUE = character(1))

    algorithm_outputs <- qgis_outputs(algorithm)

    outputs_list <- Map(
      qgis_parse_result_output,
      outputs_list,
      algorithm_outputs$qgis_output_type[match(output_names, algorithm_outputs$name)]
    )

    names(outputs_list) <- output_names
    outputs_list
  }
}

# All values of `qgis_output_type`
# c("outputVector", "outputRaster", "outputString", "outputFile",
#   "outputFolder", "outputHtml", "outputNumber", "outputMultilayer",
#   "outputLayer"
# )
qgis_parse_result_output <- function(value, qgis_output_type) {
  switch(qgis_output_type,

    # numbers and strings have clear mappings to R types
    outputNumber = as.numeric(value),
    outputString = value,

    # e.g., native::splitvectorlayer
    # a comma-separated list of values (hopefully without commas in
    # the filenames...)
    outputMultilayer = if (trimws(value) == "") {
      structure(character(0), class = "qgis_outputMultilayer")
    } else {
      structure(stringr::str_split(value, "\\s*,\\s*")[[1]], class = "qgis_outputMultilayer")
    },

    # by default, a classed string that can be reinterpreted
    structure(value, class = paste0("qgis_", qgis_output_type))
  )
}

qgis_result_output <- function(value, qgis_output_type) {
  switch(qgis_output_type,

    # numbers and strings have clear mappings to R types
    outputNumber = as.numeric(value %||% NA_character_),
    outputString = as.character(value %||% NA_character_),
    outputMultilayer = structure(as.character(value), class = paste0("qgis_", qgis_output_type)),

    # by default, a classed string that can be reinterpreted
    structure(value %||% NA, class = paste0("qgis_", qgis_output_type))
  )
}
