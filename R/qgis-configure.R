
#' Configure and run 'qgis_process'
#'
#' The qgisprocess package is a wapper around the 'qgis_process' command line
#' tool distributed with QGIS (>=3.14). These functions use heuristics to
#' detect the location of the 'qgis_process' executable. If the configuration
#' fails or you have more than one QGIS installation, you can set
#' `options(qgisprocess.path = "path/to/qgis_process")` or the
#' `R_QGISPROCESS_PATH` environment variable (useful on CI). On Linux the
#' 'qgis_process' executable is generally available on the user's PATH,
#' on MacOS the executable is within the QGIS*.app/Contents/MacOS/bin folder,
#' and on Windows the executable is named qgis_process-qgis.bat or
#' qgis_process-qgis-dev.bat and is located in Program Files/QGIS*/bin or
#' OSGeo4W(64)/bin.
#'
#' @param ... Passed to [processx::run()].
#' @param args Command-line arguments
#' @param quiet Use `FALSE` to display more information about the command, possibly
#'   useful for debugging.
#' @param query Use `TRUE` to refresh the cached value.
#' @param action An action to take if the 'qgis_process' executable could not be
#'   found.
#' @param env A [list()] of environment variables. Defaults to [qgis_env()].
#' @param path A path to the 'qgis_process' executable. Defaults to [qgis_path()].
#' @param use_cached_data Use the cached algorithm list and `path` found when
#'   configuring qgisprocess during the last session. This saves some time
#'   loading the package.
#'
#' @return The result of [processx::run()].
#' @export
#'
#' @examples
#' if (has_qgis()) qgis_path()
#' if (has_qgis()) qgis_version()
#' if (has_qgis()) qgis_algorithms()
#' qgis_configure()
#'
qgis_run <- function(args = character(), ..., env = qgis_env(), path = qgis_path()) {
  # workaround for running Windows batch files where arguments have spaces
  # see https://github.com/r-lib/processx/issues/301
  if (is_windows()) {
    withr::with_envvar(
      env,
      processx::run("cmd.exe", c("/c", "call", path, args), ...),
    )
  } else {
    withr::with_envvar(
      env,
      processx::run(path, args, ...),
    )
  }
}

#' @rdname qgis_run
#' @export
has_qgis <- function() {
  !is.null(qgisprocess_cache$path) &&
    !is.null(qgisprocess_cache$version) &&
    !is.null(qgisprocess_cache$algorithms)
}

#' @rdname qgis_run
#' @export
assert_qgis <- function(action = abort) {
  if (!has_qgis()) {
    action(
      paste0(
        "The QGIS processing utility ('qgis_process') is not installed or could not be found.\n",
        "Run `qgis_configure()` to configure this location.\n",
        "If 'qgis_process' is installed, set `options(qgisprocess.path = '/path/to/qgis_process')`\n",
        "and re-run `qgis_configure()`."
      )
    )
  }
}

#' @rdname qgis_run
#' @export
qgis_configure <- function(quiet = FALSE, use_cached_data = FALSE) {
  tryCatch({
    qgis_unconfigure()

    version <- as.character(utils::packageVersion("qgisprocess"))

    cache_data_file <- file.path(
      rappdirs::user_cache_dir("R-qgisprocess"),
      glue("cache-{version}.rds")
    )

    if (use_cached_data && file.exists(cache_data_file)) {
      try({
        cached_data <- readRDS(cache_data_file)

        if (!quiet) message(glue("Checking configuration from '{cache_data_file}'"))

        # respect environment variable/option for path
        option_path <- getOption(
          "qgisprocess.path",
          Sys.getenv("R_QGISPROCESS_PATH")
        )

        if (identical(option_path, "") || identical(option_path, cached_data$path)) {

          if (!quiet) message(glue(
            "Checking cached QGIS version with version reported by '{cached_data$path}' ..."
          ))

          # since we will query the program, first check that it still works

          tryCatch({
            qgis_run(path = cached_data$path)
          }, error = function(e) {
            abort(
              glue(
                "'{cached_data$path}' (cached path) is not available anymore.\n",
                "Will try to reconfigure qgisprocess and build new cache ..."
              )
            )
          })

          # note the difference with the further qgis_version() statement,
          # where it will also respect the outcome of qgis_path(query = TRUE);
          # below it uses qgis_path(query = FALSE), hence takes cached_data$path
          qgisprocess_cache$path <- cached_data$path
          qversion <- qgis_version(query = TRUE, quiet = quiet)
          qgisprocess_cache$path <- NULL

          if (identical(qversion, cached_data$version)) {
            if (!quiet) message(glue("QGIS versions match! ({qversion})"))
            if (!quiet) message(glue("Restoring configuration from '{cache_data_file}'"))

            qgisprocess_cache$path <- cached_data$path
            qgisprocess_cache$version <- cached_data$version
            qgisprocess_cache$use_json_output <- cached_data$use_json_output
            qgisprocess_cache$algorithms <- cached_data$algorithms
            qgisprocess_cache$loaded_from <- cache_data_file

            if (!quiet) {
              message(
                glue::glue(
                  "Metadata of { nrow(cached_data$algorithms) } algorithms are present in cache.\n",
                  "Run `qgis_algorithms()` to see them."
                )
              )
              if (qgis_use_json_input()) {
                message("- Using JSON for input serialization.")
              }
              if (qgis_use_json_output()) {
                message("- Using JSON for output serialization.")
              }
            }

            return(invisible(TRUE))

          } else {
            message(glue(
              "QGIS version change detected:\n",
              "- in the qgisprocess cache it was: {cached_data$version}\n",
              "- while '{cached_data$path}' is at {qversion}"
            ))
          }
        } else {
          message(glue(
            "The user's qgisprocess.path option or the R_QGISPROCESS_PATH environment ",
            "variable specify a different qgis_process path ({option_path}) ",
            "than the cache did ({cached_data$path})."))
        }

        message("Hence rebuilding cache to reflect this change ...")
      })
    }

    path <- qgis_path(query = TRUE, quiet = quiet)

    version <- qgis_version(query = TRUE, quiet = quiet)
    if (!quiet) message(glue("QGIS version: { version }"))

    use_json_output <- qgis_use_json_output(query = TRUE)
    algorithms <- qgis_algorithms(query = TRUE, quiet = quiet)

    if (!quiet) message(glue("Saving configuration to '{cache_data_file}'"))

    try({
      if (!dir.exists(dirname(cache_data_file))) {
        dir.create(dirname(cache_data_file), recursive = TRUE)
      }

      saveRDS(
        list(
          path = path,
          version = version,
          algorithms = algorithms,
          use_json_output = use_json_output
        ),
        cache_data_file
      )
    })

    if (!quiet) {
      message(
        glue(
          "Metadata of { nrow(algorithms) } algorithms queried and stored in cache.\n",
          "Run `qgis_algorithms()` to see them."
        )
      )

      if (qgis_use_json_input()) {
        message("- Using JSON for input serialization.")
      }

      if (qgis_use_json_output()) {
        message("- Using JSON for output serialization.")
      }
    }
  }, error = function(e) {
    qgis_unconfigure()
    if (!quiet) message(e)
  })

  invisible(has_qgis())
}

#' @rdname qgis_run
#' @export
qgis_unconfigure <- function() {
  qgisprocess_cache$path <- NULL
  qgisprocess_cache$version <- NULL
  qgisprocess_cache$algorithms <- NULL
  qgisprocess_cache$loaded_from <- NULL
  invisible(NULL)
}

#' @rdname qgis_run
#' @export
qgis_version <- function(query = FALSE, quiet = TRUE) {
  if (query) {
    qgisprocess_cache$version <- qgis_query_version(quiet = quiet)
  }

  qgisprocess_cache$version
}

#' @rdname qgis_run
#' @export
qgis_path <- function(query = FALSE, quiet = TRUE) {
  if (query) {
    qgisprocess_cache$path <- qgis_query_path(quiet = quiet)
  }

  qgisprocess_cache$path
}

#' @rdname qgis_run
#' @export
qgis_query_path <- function(quiet = FALSE) {
  if (!is.null(getOption("qgisprocess.path"))) {
    path <- getOption("qgisprocess.path", "qgis_process")
    if (!quiet) message(glue("Trying getOption('qgisprocess.path'): '{ path }'"))
    tryCatch({
      qgis_run(path = path)
      if (!quiet) message("Success!")
      return(path)
    }, error = function(e) {
      if (!quiet) message(as.character(e))
    })
  } else {
    if (!quiet) message("getOption('qgisprocess.path') was not found.")
  }

  if (Sys.getenv("R_QGISPROCESS_PATH", "") != "") {
    path <- Sys.getenv("R_QGISPROCESS_PATH")
    if (!quiet) message(glue("Trying Sys.getenv('R_QGISPROCESS_PATH'): '{ path }'"))
    tryCatch({
      qgis_run(path = path)
      if (!quiet) message("Success!")
      return(path)
    }, error = function(e) {
      if (!quiet) message(as.character(e))
    })
  } else {
    if (!quiet) message("Sys.getenv('R_QGISPROCESS_PATH') was not found.")
  }

  if (!quiet) message(glue("Trying 'qgis_process' on PATH..."))
  tryCatch({
    qgis_run(path = "qgis_process")
    if (!quiet) message("Success!")
    return("qgis_process")
  }, error = function(e) {
    if (!quiet) message("'qgis_process' is not available on PATH.")
  })

  possible_locs <- if (is_macos()) {
    qgis_detect_macos()
  } else if (is_windows()) {
    qgis_detect_windows()
  }

  if (length(possible_locs) == 0) {
    abort("No QGIS installation containing 'qgis_process' found!")
  }

  if (!quiet) {
    message(
      sprintf(
        "Found %s QGIS installation%s containing 'qgis_process':\n %s",
        length(possible_locs),
        if (length(possible_locs) == 1) "" else "s",
        paste(possible_locs, collapse = "\n")
      )
    )
  }

  for (path in possible_locs) {
    if (!quiet) message(glue("Trying command '{ path }'"))
    tryCatch({
      qgis_run(path = path)
      if (!quiet) message("Success!")
      return(path)
    }, error = function(e) {})
  }

  abort("QGIS installation found, but all candidate paths failed to execute.")
}

#' @rdname qgis_run
#' @export
qgis_use_json_output <- function(query = FALSE) {
  if (query) {
    opt <- getOption(
      "qgisprocess.use_json_output",
      Sys.getenv(
        "R_QGISPROCESS_USE_JSON_OUTPUT",
        ""
      )
    )

    if (identical(opt, "")) {
      # This doesn't work on the default GHA runner for Ubuntu and
      # maybe can't be guaranteed to work on Linux. On Linux, we try
      # to list algorithms with --json and check if the command fails
      qgisprocess_cache$use_json_output <- is_windows() ||
        is_macos() ||
        (qgis_run(c("--json", "list"), error_on_status = FALSE)$status == 0)
    } else {
      qgisprocess_cache$use_json_output <- isTRUE(opt) || identical(opt, "true")
    }
  }

  qgisprocess_cache$use_json_output
}

#' @rdname qgis_run
#' @export
qgis_use_json_input <- function() {
  opt <- getOption(
    "qgisprocess.use_json_input",
    Sys.getenv(
      "R_QGISPROCESS_USE_JSON_INPUT",
      ""
    )
  )

  if (identical(opt, "")) {
    qgis_use_json_output() &&
      (package_version(strsplit(qgis_version(), "-")[[1]][1]) >= "3.23.0")
  } else {
    isTRUE(opt) || identical(opt, "true")
  }
}

#' @rdname qgis_run
#' @export
qgis_env <- function() {
  getOption(
    "qgisprocess.env",
    list(QT_QPA_PLATFORM = "offscreen")
  )
}

#' @rdname qgis_run
#' @export
qgis_query_version <- function(quiet = FALSE) {
  result <- qgis_run(args = character(0))
  lines <- readLines(textConnection(result$stdout))
  match <- stringr::str_match(lines, "\\((\\d{1,2}\\.\\d+.*-.+)\\)")[, 2, drop = TRUE]
  if (all(is.na(match))) {
    abort(
      paste0(
        "Output did not contain expected version information and was:\n\n",
        paste(lines, collapse = "\n")
      )
    )
  }
  match[!is.na(match)]
}

#' @rdname qgis_run
#' @export
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

# environment for cache
qgisprocess_cache <- new.env(parent = emptyenv())
qgisprocess_cache$path <- NULL
qgisprocess_cache$version <- NULL
qgisprocess_cache$algorithms <- NULL
qgisprocess_cache$use_json_output <- NULL
qgisprocess_cache$loaded_from <- NULL
