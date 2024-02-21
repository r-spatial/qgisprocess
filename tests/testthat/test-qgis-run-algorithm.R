input <-
  if (has_qgis()) {
    if (qgis_using_json_input()) " (using JSON input)" else " (NOT using JSON input)"
  } else ""

test_that(glue("qgis_run_algorithm() works{input}"), {
  skip_if_not(has_qgis())

  tmp_gpkg <- qgis_tmp_vector(".gpkg")

  result <- expect_output(
    qgis_run_algorithm(
      "native:reprojectlayer",
      INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
      TARGET_CRS = "EPSG:4326",
      OUTPUT = tmp_gpkg,
      .quiet = FALSE
    ),
    "Running\\s+"
  )
  expect_true(file.exists(tmp_gpkg))

  unlink(tmp_gpkg)

  result <- expect_output(
    qgis_run_algorithm(
      "native:reprojectlayer",
      INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
      TARGET_CRS = "EPSG:4326",
      OUTPUT = tmp_gpkg,
      .quiet = TRUE
    ),
    NA
  )
  expect_true(file.exists(tmp_gpkg))
  unlink(tmp_gpkg)
})

test_that(glue("qgis_run_algorithm() ignores unknown inputs{input}"), {
  skip_if_not(has_qgis())

  expect_message(
    qgis_run_algorithm(
      "native:buffer",
      NOT_AN_INPUT = "some value",
      INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
      DISTANCE = 100,
      .quiet = TRUE
    ),
    "Ignoring unknown input"
  )
})

test_that(glue("qgis_run_algorithm() accepts multiple input arguments{input}"), {
  skip_if_not(has_qgis())
  skip_if_not_installed("sf")

  v_1 <- sf::read_sf(system.file("longlake/longlake.gpkg", package = "qgisprocess"))
  v_2 <- v_3 <- v_1
  v_2$geom <- v_2$geom + 1000
  sf::st_crs(v_2) <- sf::st_crs(v_1)
  v_3$geom <- v_3$geom - 1000
  sf::st_crs(v_3) <- sf::st_crs(v_1)
  out <- qgis_run_algorithm(
    "native:mergevectorlayers",
    LAYERS = v_1, LAYERS = v_2, LAYERS = v_3
  )
  tmp <- sf::read_sf(qgis_extract_output_by_name(out, "OUTPUT"))
  expect_equal(nrow(tmp), 3)
})

test_that(glue("qgis_run_algorithm() accepts a qgis_list_input argument{input}"), {
  skip_if_not(has_qgis())
  skip_if_not_installed("sf")

  v_1 <- sf::read_sf(system.file("longlake/longlake.gpkg", package = "qgisprocess"))
  v_2 <- v_3 <- v_1
  v_2$geom <- v_2$geom + 1000
  sf::st_crs(v_2) <- sf::st_crs(v_1)
  v_3$geom <- v_3$geom - 1000
  sf::st_crs(v_3) <- sf::st_crs(v_1)
  out <- qgis_run_algorithm(
    "native:mergevectorlayers",
    LAYERS = qgis_list_input(v_1, v_2, v_3)
  )
  tmp <- sf::read_sf(qgis_extract_output_by_name(out, "OUTPUT"))
  expect_equal(nrow(tmp), 3)
})

test_that(glue("qgis_run_algorithm() works when passing a numeric vector to a range input type{input}"), {
  skip_if_not(has_qgis())
  skip_if_not_installed("terra")
  skip_if(!("GRASS" %in% qgis_providers()$provider_title), "GRASS is not available")

  obj <- terra::rast(system.file("longlake/longlake_depth.tif", package = "qgisprocess"))
  out <- qgis_run_algorithm("grass:r.rescale", input = obj, to = c(0, 1))
  tmp <- qgis_as_terra(out)
  expect_identical(max(terra::values(tmp), na.rm = TRUE), 1L)
  out2 <- qgis_run_algorithm("grass:r.rescale", input = obj, to = "0,1")
  tmp2 <- qgis_as_terra(out2)
  expect_identical(terra::values(tmp), terra::values(tmp2))
})

test_that(glue("qgis_run_algorithm() supports the matrix input type{input}"), {
  skip_if_not(has_qgis())
  skip_if_not_installed("terra")

  path <- system.file("longlake/longlake.tif", package = "qgisprocess")
  vec <- c(25, 100, 1, 101, 175, 2, 176, 255, 3)
  mat <- matrix(vec, ncol = 3, byrow = TRUE)
  df <- data.frame(min = mat[, 1], max = mat[, 2], val = mat[, 3])

  res_vec <- qgis_run_algorithm("native:reclassifybytable",
    INPUT_RASTER = path,
    RASTER_BAND = 1,
    TABLE = vec,
    RANGE_BOUNDARIES = 2
  )
  res_mat <- qgis_run_algorithm("native:reclassifybytable",
    INPUT_RASTER = path,
    RASTER_BAND = 1,
    TABLE = mat,
    RANGE_BOUNDARIES = 2
  )
  res_df <- qgis_run_algorithm("native:reclassifybytable",
    INPUT_RASTER = path,
    RASTER_BAND = 1,
    TABLE = df,
    RANGE_BOUNDARIES = 2
  )

  expect_s3_class(res_vec$OUTPUT, "qgis_outputRaster")
  expect_s3_class(res_mat$OUTPUT, "qgis_outputRaster")
  expect_s3_class(res_df$OUTPUT, "qgis_outputRaster")
  vals_vec <- terra::values(qgis_as_terra(res_vec))
  vals_mat <- terra::values(qgis_as_terra(res_mat))
  vals_df <- terra::values(qgis_as_terra(res_df))
  attr(vals_vec, "dimnames") <- NULL
  attr(vals_mat, "dimnames") <- NULL
  attr(vals_df, "dimnames") <- NULL
  expect_identical(vals_vec, vals_mat)
  expect_identical(vals_vec, vals_df)
})

test_that(glue("qgis_run_algorithm() runs with qgis:relief, for which the acceptable value of COLORS is NULL{input}"), {
  skip_if_not(has_qgis())
  skip_if_not_installed("terra")

  relief_args <- qgis_get_argument_specs("qgis:relief")
  expect_identical(relief_args["COLORS", ]$acceptable_values, list(NULL))

  result <- qgis_run_algorithm(
    "qgis:relief",
    INPUT = system.file("longlake/longlake_depth.tif", package = "qgisprocess"),
    Z_FACTOR = 1,
    AUTO_COLORS = FALSE,
    COLORS = "-0.5, 0, 170, 0, 0; 0, 0.5, 85, 255, 255; 0.5, 1, 0, 255, 0; 1, 2.5, 85, 85, 255"
  )

  expect_s3_class(result$OUTPUT, "qgis_outputRaster")
  expect_s3_class(result$FREQUENCY_DISTRIBUTION, "qgis_outputFile")
  expect_true(file.exists(result$OUTPUT))
  expect_true(file.exists(result$FREQUENCY_DISTRIBUTION))

  mat <- matrix(
    c(-0.5, 0, 170, 0, 0, 0, 0.5, 85, 255, 255, 0.5, 1, 0, 255, 0, 1, 2.5, 85, 85, 255),
    ncol = 5,
    byrow = TRUE,
    dimnames = list(NULL, c("min", "max", "r", "g", "b"))
  )
  df <- as.data.frame(mat)
  df$col <- rgb(df$r, df$g, df$b, alpha = 255, maxColorValue = 255)
  df <- df[, c("min", "max", "col")]

  res_df <- qgis_run_algorithm(
    "qgis:relief",
    INPUT = system.file("longlake/longlake_depth.tif", package = "qgisprocess"),
    Z_FACTOR = 1,
    AUTO_COLORS = FALSE,
    COLORS = df
  )

  expect_s3_class(res_df$OUTPUT, "qgis_outputRaster")
  vals_string <- terra::values(qgis_as_terra(result))
  vals_df <- terra::values(qgis_as_terra(res_df))
  attr(vals_string, "dimnames") <- NULL
  attr(vals_df, "dimnames") <- NULL
  expect_identical(vals_string, vals_df)
})


test_that(glue("qgis_run_algorithm() succeeds when it needs a QGIS project{input}"), {
  skip_if_not(has_qgis())
  # Until Issue #68 is resolved (native:printlayouttopdf segfaults on MacOS):
  skip_on_os("mac")
  # QGIS 3.28.2 (and a series of QGIS 3.29 builds) always segfault
  # see https://github.com/qgis/QGIS/issues/51383
  qversion <- qgis_version()
  skip_if(
    stringr::str_detect(qversion, "^3\\.28\\.2-"),
    paste(
      "QGIS version",
      qversion,
      "is reported to segfault with 'native:printlayouttopdf'."
    )
  )

  tmp_pdf <- qgis_tmp_file(".pdf")

  result <- qgis_run_algorithm(
    "native:printlayouttopdf",
    LAYOUT = "Layout 1",
    OUTPUT = tmp_pdf,
    PROJECT_PATH = system.file("extdata/longlake.qgs", package = "qgisprocess")
  )

  expect_true(file.exists(tmp_pdf))
  expect_identical(tmp_pdf, as.character(result$OUTPUT))
  unlink(tmp_pdf)
})



test_that(glue("qgis_run_algorithm() succeeds when it uses an aggregates input argument{input}"), {
  skip_if_not(has_qgis())
  skip_if_not_installed("sf")
  skip_if_not(qgis_using_json_input())

  # preparing a layer of multiple polygons and some attributes
  ll_res <- qgis_run_algorithm(
    "native:subdivide",
    INPUT = system.file("longlake/longlake.gpkg", package = "qgisprocess"),
    MAX_NODES = 40
  )
  ll <- sf::st_as_sf(ll_res)
  ll <- sf::st_cast(sf::st_set_agr(ll, "constant"), "POLYGON")
  skip_if_not(nrow(ll) == 25L, "Intermediate result does not have the expected 25 polygons")
  depth_vals <- c("very deep", "deep", "shallow", "very shallow", "dry")
  ll$depth <- rep(depth_vals, 5)
  ll$name <- letters[1:25]

  result <- qgis_run_algorithm(
    "native:aggregate",
    AGGREGATES = qgis_list_input(
      qgis_dict_input(
        aggregate = "first_value",
        delimiter = ",",
        input = '"depth"',
        length = 20,
        name = "depth",
        precision = 0,
        type = 10
      ),
      qgis_dict_input(
        aggregate = "concatenate",
        delimiter = ",",
        input = '"name"',
        length = 20,
        name = "name",
        precision = 0,
        type = 10
      )
    ),
    INPUT = ll,
    GROUP_BY = "depth"
  )

  expect_s3_class(result$OUTPUT, "qgis_outputVector")

  ll_res <- sf::st_as_sf(result)

  expect_identical(ll_res$depth, depth_vals)
  expect_identical(ll_res$name[1], "a,f,k,p,u")
})

test_that("qgis_run_algorithm() yields a warning with a deprecated algorithm", {
  skip_if_not(has_qgis())
  skip_if_not(qgis_using_json_output())
  algs <- qgis_algorithms()
  skip_if_not(
    "deprecated" %in% colnames(algs) && sum(algs$deprecated) > 0,
    "There are no deprecated algorithms available."
  )
  skip_if_not(
    "native:raisewarning" %in% algs$algorithm[algs$deprecated],
    "'native:raisewarning' is not an available deprecated algorithm."
  )
  local_edition(3)
  # if more than one warning pops up, it should be apparent from
  # testthat output (only the first warning is swallowed in the
  # third edition of testthat)

  suppressMessages(
    expect_warning(
      qgis_run_algorithm(
        "native:raisewarning",
        MESSAGE = "Some text than won't come back though."
      )
    )
  )
})
