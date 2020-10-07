
is_macos <- function() {
  (.Platform$OS.type == "unix") &&
    (Sys.info()["sysname"] == "Darwin")
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}
