# The algorithm list is always available, but the help text
# associated with all algorithms can take up to an hour to load
# because it takes so many calls to qgis_process. This builds
# a cache of some relevant info and saves it (mostly for development
# purposes).

library(qgisprocess)

algorithms <- qgis_algorithms()
all_outputs <- lapply(qgis_algorithms()$algorithm, qgis_get_output_specs)
all_args <- lapply(qgis_algorithms()$algorithm, qgis_get_argument_specs)

algorithms$outputs <- all_outputs
algorithms$arguments <- all_args
algorithms$description <- vapply(algorithms$algorithm, qgis_get_description, character(1))

alg_outputs <- tidyr::unnest(algorithms[c("algorithm", "outputs")], outputs)
alg_arguments <- tidyr::unnest(algorithms[c("algorithm", "arguments")], arguments)

readr::write_csv(qgis_algorithms(), "inst/algorithms/algorithms.csv")
readr::write_csv(alg_outputs, "inst/algorithms/outputs.csv")
readr::write_csv(alg_arguments, "inst/algorithms/arguments.csv")
