#' List algorithms, processing providers or plugins
#'
#' Functions that return metadata about the installed and enabled algorithms or
#' processing providers, or about the installed plugins that implement
#' processing providers.
#' See the [QGIS docs](https://docs.qgis.org/latest/en/docs/user_manual/processing_algs/qgis/index.html)
#' for a detailed description of the algorithms provided
#' 'out of the box' on QGIS.
#'
#' @family topics about information on algorithms & processing providers
#' @family topics about reporting the QGIS state
#' @concept functions to manage and explore QGIS and qgisprocess
#' @seealso [qgis_enable_plugins()], [qgis_disable_plugins()]
#'
#' @param which String defining which plugins to select, based on their
#' status in QGIS (enabled or disabled).
#' Must be one of: `"all"`, `"enabled"`, `"disabled"`.
#' @param ... Only used by other functions calling this function.
#' @inheritParams qgis_path
#'
#' @return
#' A tibble of algorithms, processing providers or plugins, with metadata.
#'
#' @export
#'
#' @examples
#' if (has_qgis()) qgis_has_algorithm("native:filedownloader")
#' if (has_qgis()) qgis_algorithms()
#' if (has_qgis()) qgis_has_provider("native")
#' if (has_qgis()) qgis_providers()
#'
qgis_algorithms <- function(query = FALSE, quiet = TRUE) {
  if (query) {
    qgisprocess_cache$algorithms <- qgis_query_algorithms(quiet = quiet)
  }
  if (!quiet) message(glue(
    "{ifelse(query, 'You now have ', 'Having ')}",
    "access to { nrow(qgisprocess_cache$algorithms) } algorithms ",
    "from { nrow(qgis_providers()) } QGIS processing providers."
  ))
  qgisprocess_cache$algorithms
}

#' @rdname qgis_algorithms
#' @export
qgis_providers <- function(query = FALSE, quiet = TRUE) {
  assert_qgis()
  algs <- qgis_algorithms(query = query, quiet = quiet)
  counted <- stats::aggregate(
    algs[[1]],
    by = list(algs$provider, algs$provider_title),
    FUN = length
  )
  tibble::as_tibble(
    rlang::set_names(
      counted,
      c("provider", "provider_title", "algorithm_count")
    )
  )
}

#' @keywords internal
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
  if (qgis_using_json_output()) {
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

    providers <- vctrs::vec_rbind(!!!providers, .ptype = providers_ptype, .names_to = "provider_id")

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

      vctrs::vec_rbind(!!!algs_p, ptype = fields_ptype, .names_to = "algorithm")
    })

    fields_ptype$algorithm <- character()
    algs <- vctrs::vec_rbind(!!!algs, ptype = fields_ptype, .names_to = "provider_id")
    algs <- vctrs::vec_cbind(
      algs,
      providers[match(algs$provider_id, providers$provider_id), setdiff(names(providers), "provider_id")]
    )

    # for compatibility with old output
    algs$algorithm_id <- stringr::str_remove(algs$algorithm, "^.*?:")
    colnames(algs)[colnames(algs) == "name"] <- "algorithm_title"
    colnames(algs)[colnames(algs) == "provider_name"] <- "provider_title"
    colnames(algs)[colnames(algs) == "provider_id"] <- "provider"

    first_cols <- c("provider", "provider_title", "algorithm", "algorithm_id", "algorithm_title")
    second_cols <- setdiff(
      names(algs)[grepl("provider", names(algs))],
      first_cols
    )
    algs[c(first_cols, second_cols, setdiff(names(algs), c(first_cols, second_cols)))]
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



#' Search geoprocessing algorithms
#'
#' Searches for algorithms using a regular expression.
#' In its simplest form
#' that is just a string that must match part of a character value.
#'
#' When using multiple arguments in combination, only the algorithms are
#' returned that fulfill all conditions.
#'
#' All regular expressions that [stringr::str_detect()] can handle, are
#' accepted.
#' Have a look at [stringi::search_regex()] to get a nice overview.
#'
#' @family topics about information on algorithms & processing providers
#' @concept functions to manage and explore QGIS and qgisprocess
#'
#' @param algorithm Regular expression to match the `algorithm` or
#' `algorithm_title` value from the output of [qgis_algorithms()].
#' @param provider Regular expression to match the `provider` or
#' `provider_title` value from the output of [qgis_algorithms()].
#' @param group Regular expression to match the `group` value
#' from the output of [qgis_algorithms()].
#'
#' @return A tibble.
#'
#' @examples
#' if (has_qgis()) {
#'   qgis_search_algorithms(
#'     algorithm = "point.*line",
#'     provider = "^native$"
#'   )
#' }
#'
#'
#' @export
qgis_search_algorithms <- function(
    algorithm = NULL,
    provider = NULL,
    group = NULL
) {
  assert_that(
    !is.null(algorithm) || !is.null(provider) || !is.null(group),
    msg = "You must provide at least one of the arguments."
    )
  result <- qgis_algorithms(query = FALSE, quiet = TRUE)
  assert_that(inherits(result, "data.frame"))
  assert_that(
    nrow(result) > 0L,
    msg = "qgis_algorithms() returns an empty dataframe; no searching done."
  )
  result <- result[, c(
    "provider",
    "provider_title",
    "group",
    "algorithm",
    "algorithm_title"
  )]
  if (!is.null(algorithm)) {
    assert_that(is.string(algorithm))
    result <- result[
      stringr::str_detect(result$algorithm, algorithm) |
        stringr::str_detect(result$algorithm_title, algorithm),
      ]
  }
  if (!is.null(provider)) {
    assert_that(is.string(provider))
    result <- result[
      stringr::str_detect(result$provider, provider) |
        stringr::str_detect(result$provider_title, provider),
    ]
  }
  if (!is.null(group)) {
    assert_that(is.string(group))
    result <- result[stringr::str_detect(result$group, group), ]
  }
  result
  }

