library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::extend_transects_both_sides() ----
# -------------------------------------------------------------------
testthat::test_that("extend_transects_both_sides() test that both sides of a transect will be extended if they're both valid extensions", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST   <- 25
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(cs_id == 4) %>% 
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
    )
  
  extensions <- extend_transects_both_sides(transects    = transects, 
                                          flowlines    = flowlines, 
                                          crosswalk_id = CROSSWALK_ID, 
                                          cs_id        = CS_ID,
                                          grouping_id  = CROSSWALK_ID
  )
  
  # plot(flowlines$geometry)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  # # 
  
  # mapview::mapview(flowlines) +
  #   mapview::mapview(extensions, color = "red") +
  # #   mapview::mapview(extensions_any, color = "red") +
  # #   mapview::mapview(extensions_both, color = "red") +
  #   mapview::mapview(transects, color = "green")
  
  left_is_extended  <- extensions$left_is_extended
  right_is_extended <- extensions$right_is_extended
  
  # both sides get extended as both are valid
  testthat::expect_true(left_is_extended)
  testthat::expect_true(right_is_extended)
  
})

testthat::test_that("extend_transects_both_sides() test that both neither side of a transect will be extended if they're both INVALID extensions", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 500
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(cs_id == 4) %>% 
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
    )
  
  extensions <- extend_transects_both_sides(transects    = transects, 
                                          flowlines    = flowlines, 
                                          crosswalk_id = CROSSWALK_ID, 
                                          cs_id        = CS_ID,
                                          grouping_id  = CROSSWALK_ID
  )
  
  # plot(flowlines$geometry)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  # 
  
  # mapview::mapview(flowlines) +
  #   mapview::mapview(extensions, color = "red") +
  # #   mapview::mapview(extensions_any, color = "red") +
  # #   mapview::mapview(extensions_both, color = "red") +
  #   mapview::mapview(transects, color = "green")
  
  left_is_extended  <- extensions$left_is_extended
  right_is_extended <- extensions$right_is_extended
  
  # Neither side gets extended because the right side hits a flowline twice
  testthat::expect_false(left_is_extended)
  testthat::expect_false(right_is_extended)
  
})

testthat::test_that("extend_transects_both_sides() test extensions will only go out to the maximum specified distance and no further", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 500
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(cs_id == 1) %>%
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
    )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)

  
  extensions <- extend_transects_both_sides(transects    = transects, 
                                            flowlines    = flowlines, 
                                            crosswalk_id = CROSSWALK_ID, 
                                            cs_id        = CS_ID,
                                            grouping_id  = CROSSWALK_ID
  )
  
  # plot(flowlines$geometry)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  # 
  
  # mapview::mapview(flowlines) +
  #   mapview::mapview(extensions, color = "red") +
  # #   mapview::mapview(extensions_any, color = "red") +
  # #   mapview::mapview(extensions_both, color = "red") +
  #   mapview::mapview(transects, color = "green")
  
  # has_correct_left_extension_dist_value   <- extensions$left_distance == EXT_DIST
  # has_correct_right_extension_dist_value  <- extensions$right_distance == EXT_DIST
  
  # both the left and right side have the correct extension distance 
  testthat::expect_equal(extensions$left_distance, EXT_DIST)
  testthat::expect_equal( extensions$right_distance, EXT_DIST)
  
  starting_length   <- as.numeric(sf::st_length(transects))
  new_total_length  <- as.numeric(sf::st_length(extensions))
  testthat::expect_equal(starting_length + (EXT_DIST * 2), new_total_length)
  
  # (starting_length + (EXT_DIST * 2)) == new_total_length
  
})