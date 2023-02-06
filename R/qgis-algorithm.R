#' Run algorithms using 'qgis_process'
#'
#' Run QGIS algorithms.
#' See the [QGIS docs](https://docs.qgis.org/testing/en/docs/user_manual/processing_algs/qgis/index.html)
#' for a detailed description of the algorithms provided
#' 'out of the box' on QGIS (versions >= 3.14).
#'
#' @param provider A provider identifier (e.g., "native")
#' @inheritParams qgis_run_algorithm
#' @inheritParams qgis_run
#'
#' @export
#'
#' @examples
#' if (has_qgis()) qgis_has_algorithm("native:filedownloader")
#' if (has_qgis()) qgis_algorithms()
#' if (has_qgis()) qgis_has_provider("native")
#' if (has_qgis()) qgis_providers()
#'
qgis_has_algorithm <- function(algorithm) {
  assert_qgis()
  as.character(algorithm) %in% qgis_algorithms()$algorithm
}

#' @rdname qgis_has_algorithm
#' @export
qgis_algorithms <- function(query = FALSE, quiet = TRUE) {
  if (query) {
    qgisprocess_cache$algorithms <- qgis_query_algorithms(quiet = quiet)
    if (!quiet) message(glue(
      "You now have access to { nrow(qgisprocess_cache$algorithms) } ",
      "algorithms from { nrow(qgis_providers()) } QGIS processing providers."
    ))
  }

  qgisprocess_cache$algorithms
}

#' @rdname qgis_has_algorithm
#' @export
qgis_has_provider <- function(provider, query = FALSE, quiet = TRUE) {
  assert_qgis()
  as.character(provider) %in% unique(qgis_algorithms(query, quiet)$provider)
}

#' @rdname qgis_has_algorithm
#' @export
qgis_providers <- function() {
  assert_qgis()
  algs <- qgis_algorithms()
  algs[!duplicated(algs$provider), c("provider", "provider_title"), drop = FALSE]
}

#' @rdname qgis_has_algorithm
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



#' @keywords internal
qgis_query_algorithms <- function(quiet = FALSE) {
  if (qgis_use_json_output()) {
    result <- qgis_run(args = c("list", "--json"), encoding = "UTF-8")
    result_parsed <- jsonlite::fromJSON(result$stdout)

    providers_ptype <- tibble::tibble(
      provider_can_be_activated = logical(),
      default_raster_file_extension = character(),
      default_vector_file_extension = character(),
      provider_is_active = logical(),
      provider_long_name = character(),
      provider_name = character(),
      supported_output_raster_extensions = list(),
      supported_output_table_extensions = list(),
      supported_output_vector_extensions = list(),
      supports_non_file_based_output = logical(),
      provider_version = character(),
      provider_warning = character()
    )

    provider_mod_names <- c(
      "can_be_activated", "is_active", "long_name", "name",
      "version", "warning"
    )

    providers <- lapply(result_parsed$providers, function(p) {
      p_tbl <- providers_ptype[NA_integer_, ]

      p$algorithms <- NULL
      p <- p[!vapply(p, is.null, logical(1))]
      mod_names <- names(p) %in% provider_mod_names
      names(p)[mod_names] <- paste0("provider_", names(p)[mod_names])

      field_needs_wrap <- vapply(p_tbl[names(p)], is.list, logical(1))
      p[field_needs_wrap] <- lapply(p[field_needs_wrap], list)
      p_tbl[names(p)] <- p
      p_tbl
    })

    providers <- vctrs::vec_rbind(!!! providers, .ptype = providers_ptype, .names_to = "provider_id")

    fields_ptype <- tibble::tibble(
      can_cancel = logical(),
      deprecated = logical(),
      group = character(),
      has_known_issues = logical(),
      help_url = character(),
      name = character(),
      requires_matching_crs = logical(),
      short_description = character(),
      tags = list()
    )

    algs <- lapply(result_parsed$providers, function(p) {
      algs_p <- lapply(p$algorithms, function(alg) {
        alg_tbl <- fields_ptype[NA_integer_, ]

        alg <- alg[!vapply(alg, is.null, logical(1))]
        alg <- alg[intersect(names(alg), names(alg_tbl))]

        field_needs_wrap <- vapply(alg_tbl[names(alg)], is.list, logical(1))
        alg[field_needs_wrap] <- lapply(alg[field_needs_wrap], list)

        alg_tbl[names(alg)] <- alg
        alg_tbl
      })

      vctrs::vec_rbind(!!! algs_p, ptype = fields_ptype, .names_to = "algorithm")
    })

    fields_ptype$algorithm <- character()
    algs <- vctrs::vec_rbind(!!! algs, ptype = fields_ptype, .names_to = "provider_id")
    algs <- vctrs::vec_cbind(
      algs,
      providers[match(algs$provider_id, providers$provider_id), setdiff(names(providers), "provider_id")]
    )

    # for compatibility with old output
    algs$algorithm_id <- stringr::str_remove(algs$algorithm, "^.*?:")
    algs$algorithm_title <- algs$name
    algs$provider_title <- algs$provider_name
    algs$provider <- algs$provider_id

    first_cols <- c("provider", "provider_title", "algorithm", "algorithm_id", "algorithm_title")
    algs[c(first_cols, setdiff(names(algs), first_cols))]
  } else {
    result <- qgis_run(args = "list")
    lines <- trimws(readLines(textConnection(trimws(result$stdout))))

    which_lines_blank <- which(lines == "")
    provider_title <- lines[which_lines_blank + 1]
    alg_start <- which_lines_blank + 2
    alg_end <- c(which_lines_blank[-1] - 1, length(lines))
    alg_indices_lst <- Map(seq, alg_start, alg_end)
    alg_indices <- unlist(alg_indices_lst)

    alg_split <- stringr::str_split(lines, "\\s+", n = 2)
    alg_full_id <- vapply(alg_split, "[", 1, FUN.VALUE = character(1))
    alg_title <- vapply(alg_split, "[", 2, FUN.VALUE = character(1))

    alg_id_split <- strsplit(alg_full_id, ":", fixed = TRUE)
    provider <- vapply(alg_id_split, "[", 1, FUN.VALUE = character(1))
    alg_id <- vapply(alg_id_split, "[", 2, FUN.VALUE = character(1))

    algorithms <- tibble::tibble(
      provider = do.call("[", list(provider, alg_indices)),
      provider_title = unlist(Map(rep, provider_title, each = vapply(alg_indices_lst, length, integer(1)))),
      algorithm = do.call("[", list(alg_full_id, alg_indices)),
      algorithm_id = do.call("[", list(alg_id, alg_indices)),
      algorithm_title = do.call("[", list(alg_title, alg_indices))
    )

    # sometimes items such as 'Models' don't have algorithm IDs listed
    algorithms[!is.na(algorithms$algorithm_id), ]
  }
}

