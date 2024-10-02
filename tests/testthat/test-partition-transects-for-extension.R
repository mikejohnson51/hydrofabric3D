
library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D:::partition_transects_for_extension() ----
# -------------------------------------------------------------------

testthat::test_that("partition_transects_for_extension() simple case of a single flowline with a single polygon that fully encompasses both sides of all transects, each transect has a polygon index ID of 1 (single polygon)", {
  # transect_lines, 
  # polygons, 
  # flowlines, 
  # crosswalk_id = 'hy_id',  
  # grouping_id = 'mainstem',
  # max_extension_distance = 3000
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  
  
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  # buff <- 
  #   flowlines %>% 
  #   sf::st_buffer(200) 
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  # LENGTH_COL_NAME    <- "cs_lengthm"
  # PCT <- 0.5 
  NUM_OF_TRANSECTS  <- 3
  MAX_EXT_DIST      <- 3000
  BUFF_DIST         <- 200 
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      # tot_drainage_areasqkm,
      # bf_width,
      geom
    ) 
  
  # create testiung polygons by buffering the flowlines
  polygons <- 
    flowlines %>%
    sf::st_buffer(BUFF_DIST)
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )  
  
  # mapview::mapview(transects) + polygons
  left_partition <- partition_transects_for_extension(
    transects = transects,
    polygons_subset = polygons,
    dir = "left"
  ) 
  
  right_partition <- partition_transects_for_extension(
    transects = transects,
    polygons_subset = polygons,
    dir = "right"
  ) 
  
  left_poly_idxs   <- c(1, 1, 1)
  right_poly_idxs  <- c(1, 1, 1)
  
  left_half_of_transects_have_correct_poly_idxs <- all(left_poly_idxs == unlist(left_partition$polygon_index))
  right_half_of_transects_have_correct_poly_idxs <- all(right_poly_idxs == unlist(right_partition$polygon_index))
  
  testthat::expect_true(left_half_of_transects_have_correct_poly_idxs)
  testthat::expect_true(right_half_of_transects_have_correct_poly_idxs)
  
  
})

testthat::test_that("partition_transects_for_extension() correct output columns - single flowline, polygon, and set of transects", {
  # transect_lines, 
  # polygons, 
  # flowlines, 
  # crosswalk_id = 'hy_id',  
  # grouping_id = 'mainstem',
  # max_extension_distance = 3000
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  
  
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  # buff <- 
  #   flowlines %>% 
  #   sf::st_buffer(200) 
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  # LENGTH_COL_NAME    <- "cs_lengthm"
  # PCT <- 0.5 
  NUM_OF_TRANSECTS  <- 3
  MAX_EXT_DIST      <- 3000
  BUFF_DIST         <- 200 
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      # tot_drainage_areasqkm,
      # bf_width,
      geom
    ) 
  
  # create testiung polygons by buffering the flowlines
  polygons <- 
    flowlines %>%
    sf::st_buffer(BUFF_DIST)
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )  
  
  # mapview::mapview(transects) + polygons
  left_partition <- partition_transects_for_extension(
    transects = transects,
    polygons_subset = polygons,
    dir = "left"
  ) 
  
  right_partition <- partition_transects_for_extension(
    transects = transects,
    polygons_subset = polygons,
    dir = "right"
  ) 
   
  # minimum expected cols
  expected_cols <- c(CROSSWALK_ID, 
                     "cs_id", 
                     "geometry",
                     "polygon_index"
                     )
                     
  
  has_left_required_output_cols  <- check_required_cols(left_partition, expected_cols = expected_cols)
  has_right_required_output_cols <- check_required_cols(right_partition, expected_cols = expected_cols)
  
  testthat::expect_true(has_left_required_output_cols)
  testthat::expect_true(has_right_required_output_cols)
  
})


testthat::test_that("partition_transects_for_extension() 1 polygon with a set of transects where NONE of the transects are within the polygon, results in empty list values for the polygon index for all transects", {
  # transect_lines, 
  # polygons, 
  # flowlines, 
  # crosswalk_id = 'hy_id',  
  # grouping_id = 'mainstem',
  # max_extension_distance = 3000
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  CROSSWALK_ID       <- "id"
  NUM_OF_TRANSECTS  <- 3
  BUFF_DIST         <- 200 
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1:2) %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      # tot_drainage_areasqkm,
      # bf_width,
      geom
    ) 
  
  # create testiung polygons by buffering the flowlines
  polygons <- 
    flowlines %>%
    dplyr::slice(1) %>% 
    sf::st_buffer(BUFF_DIST)
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    ) %>%  
    dplyr::filter(!id %in% polygons$id)
  
  # mapview::mapview(transects) + polygons
  
  left_partition <- partition_transects_for_extension(
    transects = transects,
    polygons_subset = polygons,
    dir = "left"
  ) 
  
  right_partition <- partition_transects_for_extension(
    transects = transects,
    polygons_subset = polygons,
    dir = "right"
  ) 
  
  left_half_of_transects_is_empty_list   <- inherits(left_partition$polygon_index, "list") && length(left_partition$polygon_index) == 0 
  right_half_of_transects_is_empty_list  <- inherits(right_partition$polygon_index, "list") && length(right_partition$polygon_index) == 0 

  testthat::expect_true(left_half_of_transects_is_empty_list)
  testthat::expect_true(right_half_of_transects_is_empty_list)
  
  
})