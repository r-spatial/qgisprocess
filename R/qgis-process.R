
qgs_path <- function() {
  getOption("qgisprocess.path", "qgis_process")
}

qgs_version <- function(timeout = 10) {
  processx::run(qgs_path(), args = "--version", timeout = 10)
}
