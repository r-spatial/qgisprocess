test_that("qgis_cache_dir() works", {
  expect_match(qgis_cache_dir(), "R-qgisprocess")
})

test_that("qgis_pkgcache_file() works", {
  expect_match(qgis_pkgcache_file(), "cache-.+\\.rds$")
})

test_that("qgis_delete_old_cachefiles() works", {
  skip_if_not(has_qgis())

  oldfiles <- file.path(
    qgis_cache_dir(),
    c(paste0("oldfile_", 1:9), "cache-old.rds")
  )
  withr::local_file(oldfiles)

  create_oldfiles <- function(filepaths) {
    file.create(filepaths)
    Sys.setFileTime(filepaths, as.POSIXct("2022-12-25 12:00:00", tz = "CET"))
  }

  create_oldfiles(oldfiles)
  expect_true(all(file.exists(oldfiles)))
  expect_lt(max(file.mtime(oldfiles)), Sys.time())
  expect_message(
    qgis_delete_old_cachefiles(age_days = 1e7, quiet = FALSE),
    "no cache files older than"
  )
  expect_true(all(file.exists(oldfiles)))
  expect_message(
    qgis_delete_old_cachefiles(quiet = FALSE),
    "Deleted 10 cache files"
  )
  expect_true(all(!file.exists(oldfiles)))

  create_oldfiles(oldfiles)
  expect_message(
    qgis_delete_old_cachefiles(type = "package", quiet = FALSE),
    "Deleted 1 cache file"
  )
  qgis_delete_old_cachefiles(quiet = TRUE)

  create_oldfiles(oldfiles)
  expect_message(
    qgis_delete_old_cachefiles(type = "help", quiet = FALSE),
    "Deleted 9 cache files"
  )
  qgis_delete_old_cachefiles(type = "package", quiet = TRUE)
  expect_true(all(!file.exists(oldfiles)))
})
