library(testthat)
library(sf)
library(geos)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

testthat::test_that("get_transects creates a single cross section at ~50% along a line", {
  line <- sf::st_geometry(sf::st_linestring(matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ,10, 11, 12, 13, 14, 15, 16, 17), ncol = 2, byrow = TRUE)))
  line <- sf::st_as_sf(line)
  line$hy_id = "my_hy_id"
  line <- dplyr::rename(line, "geometry" = "x")
  
  NUMBER_OF_TRANSECTS <- 1
  
  # Test for a single transect at the midpoint
  transects_single <- hydrofabric3D::get_transects(line, bf_width = 3, n = NUMBER_OF_TRANSECTS)
  
  testthat::expect_s3_class(transects_single, "sf")
  testthat::expect_equal(nrow(transects_single), NUMBER_OF_TRANSECTS)
  testthat::expect_true(
    dplyr::between(ceiling(transects_single$cs_measure[1]), 49, 51)
  )
  
  
})

testthat::test_that("get_transects creates 2 cross sections at appropriate distances from eachother", {
  line <- sf::st_geometry(sf::st_linestring(matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ,10, 11, 12, 13, 14, 15, 16, 17), ncol = 2, byrow = TRUE)))
  line <- sf::st_as_sf(line)
  line$hy_id = "my_hy_id"
  line <- dplyr::rename(line, "geometry" = "x")
  
  NUMBER_OF_TRANSECTS <- 2
  
  transects <- hydrofabric3D::get_transects(line, bf_width = 3, n = NUMBER_OF_TRANSECTS)
  
  testthat::expect_s3_class(transects, "sf")
  testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
  testthat::expect_true(
    dplyr::between(ceiling(transects$cs_measure[1]), 24, 26)
  )
  testthat::expect_true(
    dplyr::between(ceiling(transects$cs_measure[2]), 86, 88)
  )
  
})


testthat::test_that("get_transects won't create a transect if it will intersect with an already created transect line", {
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  
  vlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = 4326
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070) %>% 
    smoothr::densify(20)
    
  NUMBER_OF_TRANSECTS <- 10
  
  # At this width, one of the transect lines would intersect a previously created one, therefore, its not created
  # You'd expect to get 10 transects, but only 9 are possible at this width
  transects <- hydrofabric3D::get_transects(vlines, bf_width = 25, n = NUMBER_OF_TRANSECTS)
  
  testthat::expect_s3_class(transects, "sf")
  testthat::expect_equal(nrow(transects), 9)
  measure_diff_where_transect_removed <- transects$cs_measure[6] - transects$cs_measure[5]
  testthat::expect_true(
    dplyr::between(measure_diff_where_transect_removed, 19, 21)
  )
})

testthat::test_that("get 2 transects using get_transects() on each SF MULTILINESTRING flowline in test flowlines dataset", {
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
  ACCEPTABLE_PCT_DIFF <- 1
  NUMBER_OF_TRANSECTS <- 2
  
  for (i in seq_along(flowlines)) {
    # message(i)
    transects <- hydrofabric3D::get_transects(flowlines$geom[i], bf_width = 50, n = NUMBER_OF_TRANSECTS)
    
    # plot(flowlines$geom[i])
    # plot(transects$geometry, add = T)
   
    testthat::expect_s3_class(transects, "sf")
    testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
   
    # full flowline length 
    total_flowline_length <- flowlines$lengthkm[i] * 1000
    
    diff_between_ds_distance_and_cs_measure <- ((transects$ds_distance / total_flowline_length) * 100 ) - transects$cs_measure
    
    # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
    ds_distance_lines_up_with_cs_measure_percents <- !any(diff_between_ds_distance_and_cs_measure > ACCEPTABLE_PCT_DIFF)
    
    testthat::expect_true(ds_distance_lines_up_with_cs_measure_percents)
    
  }
  
})

testthat::test_that("get 2 transects using get_transects() on each SF LINESTRING flowline in test flowlines dataset", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    sf::st_cast("LINESTRING")
  
  # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
  ACCEPTABLE_PCT_DIFF <- 1
  NUMBER_OF_TRANSECTS <- 2
  
  for (i in seq_along(1:nrow(flowlines))) {
    # message(i)
    transects <- hydrofabric3D::get_transects(flowlines$geom[i], bf_width = 50, n = NUMBER_OF_TRANSECTS)
    
    # plot(flowlines$geom[i])
    # plot(transects$geometry, add = T)
    
    testthat::expect_s3_class(transects, "sf")
    testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
    
    # full flowline length 
    total_flowline_length <- flowlines$lengthkm[i] * 1000
    
    diff_between_ds_distance_and_cs_measure <- ((transects$ds_distance / total_flowline_length) * 100 ) - transects$cs_measure
    
    # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
    ds_distance_lines_up_with_cs_measure_percents <- !any(diff_between_ds_distance_and_cs_measure > ACCEPTABLE_PCT_DIFF)
    
    testthat::expect_true(ds_distance_lines_up_with_cs_measure_percents)
    
  }
  
})

testthat::test_that("get 10 transects using get_transects() on each SF MULTILINESTRING", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  
  # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
  ACCEPTABLE_PCT_DIFF <- 1
  NUMBER_OF_TRANSECTS <- 10
  
  for (i in seq_along(1:nrow(flowlines))) {
    # message(i)
    transects <- hydrofabric3D::get_transects(flowlines$geom[i], bf_width = 50, n = NUMBER_OF_TRANSECTS)
    
    # plot(flowlines$geom[i])
    # plot(transects$geometry, add = T)
    
    testthat::expect_s3_class(transects, "sf")
    testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
    
    # full flowline length 
    total_flowline_length <- flowlines$lengthkm[i] * 1000
    
    diff_between_ds_distance_and_cs_measure <- ((transects$ds_distance / total_flowline_length) * 100 ) - transects$cs_measure
    
    # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
    ds_distance_lines_up_with_cs_measure_percents <- !any(diff_between_ds_distance_and_cs_measure > ACCEPTABLE_PCT_DIFF)
    
    testthat::expect_true(ds_distance_lines_up_with_cs_measure_percents)
    
  }
  
})
testthat::test_that("get 10 transects using get_transects() on each geos_geometry MULTILINESTRING", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  flowlines_geos <- flowlines %>% geos::as_geos_geometry()
  # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
  ACCEPTABLE_PCT_DIFF <- 1
  NUMBER_OF_TRANSECTS <- 10
  
  for (i in seq_along(flowlines_geos)) {
    # message(i)
    transects <- hydrofabric3D::get_transects(flowlines_geos[i], bf_width = 50, n = NUMBER_OF_TRANSECTS)
    
    # plot(flowlines$geom[i])
    # plot(transects$geometry, add = T)
    
    testthat::expect_s3_class(transects, "sf")
    testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
    
    # full flowline length 
    total_flowline_length <- geos::geos_length(flowlines_geos[i])
    diff_between_ds_distance_and_cs_measure <- ((transects$ds_distance / total_flowline_length) * 100 ) - transects$cs_measure
    
    # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
    ds_distance_lines_up_with_cs_measure_percents <- !any(diff_between_ds_distance_and_cs_measure > ACCEPTABLE_PCT_DIFF)
    
    testthat::expect_true(ds_distance_lines_up_with_cs_measure_percents)
    
  }
  
})

testthat::test_that("really high number of transect lines results in less transects being generated because of intersection removals (SF MULTILINESTRING)", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  
  # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
  ACCEPTABLE_PCT_DIFF <- 1
  NUMBER_OF_TRANSECTS <- 1000
  
  for (i in seq_along(1:nrow(flowlines))) {
    message(i)
    transects <- hydrofabric3D::get_transects(flowlines$geom[i], bf_width = 50, n = NUMBER_OF_TRANSECTS)
    
    # plot(flowlines$geom[i])
    # plot(transects$geometry, add = T)
    
    testthat::expect_s3_class(transects, "sf")
    testthat::expect_false(nrow(transects) == NUMBER_OF_TRANSECTS)
    
    # full flowline length 
    total_flowline_length <- flowlines$lengthkm[i] * 1000
    
    diff_between_ds_distance_and_cs_measure <- ((transects$ds_distance / total_flowline_length) * 100 ) - transects$cs_measure
    
    # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
    ds_distance_lines_up_with_cs_measure_percents <- !any(diff_between_ds_distance_and_cs_measure > ACCEPTABLE_PCT_DIFF)
    
    testthat::expect_true(ds_distance_lines_up_with_cs_measure_percents)
    
  }
  
})

testthat::test_that("Error from SF POLYGON)", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  polys     <- sf::st_buffer(flowlines, 1) 
  poly      <- polys$geom[1]
  
  testthat::expect_error(hydrofabric3D::get_transects(poly, 4, 3))
})

testthat::test_that("Error from SF MULTIPOLYGON)", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  polys     <- sf::st_buffer(flowlines, 1) 
  poly      <- polys$geom[1] %>% 
    sf::st_cast("MULTIPOLYGON")
  
  testthat::expect_error(hydrofabric3D::get_transects(poly, 4, 3))
  
})

testthat::test_that("Error from character bf_width", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  testthat::expect_error(hydrofabric3D::get_transects(flowlines$geom[1], " dsgfd" , 3))
  
})

testthat::test_that("Error from logical bf_width", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  testthat::expect_error(hydrofabric3D::get_transects(flowlines$geom[1], TRUE , 3))
  
})
testthat::test_that("Error from list bf_width", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  testthat::expect_error(hydrofabric3D::get_transects(flowlines$geom[1], list(x = 43) , 3))
  
})

testthat::test_that("Error from NA bf_width", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  testthat::expect_error(hydrofabric3D::get_transects(flowlines$geom[1], NA , 3))
  
})

testthat::test_that("Error from NULL bf_width", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  testthat::expect_error(hydrofabric3D::get_transects(flowlines$geom[1], NULL , 3))
  
})