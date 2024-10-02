
library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()
# 
# -------------------------------------------------------------------
# ---- hydrofabric3D::is_valid_transect_line() ----
# -------------------------------------------------------------------

# TODO: move each test in here to its own test
testthat::test_that("is_valid_transect_line() - testing standard easy cases (both valid and invalid transects) - using test data", {
  # transect_lines, 
  # polygons, 
  # flowlines, 
  # crosswalk_id = 'hy_id',  
  # grouping_id = 'mainstem',
  # max_extension_distance = 3000
  
  # flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
  #   dplyr::slice(10)
  # 
  # # flowlines$geom %>% plot()
  # 
  # CROSSWALK_ID       <- "id"
  # NUM_OF_TRANSECTS  <- 5
  # CS_WIDTH <- 300
  # flowlines <-
  #   flowlines %>% 
  #   # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
  #   dplyr::select(
  #     dplyr::any_of(c(CROSSWALK_ID)), 
  #     # tot_drainage_areasqkm,
  #     # bf_width,
  #     geom
  #   ) 
  # 
  # # generate transects
  # transects <- cut_cross_sections(
  #   net = flowlines,
  #   id  = CROSSWALK_ID,  
  #   num = NUM_OF_TRANSECTS,
  #   cs_widths = CS_WIDTH
  # ) %>% 
  #   dplyr::select(
  #     dplyr::any_of(c(CROSSWALK_ID)), cs_id
  #   )
  # 
  # plot(flowlines$geom) 
  # plot(transects$geometry, col = 'red', add = T)
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans     <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type == "flowline")
  
  # plot(flowlines$x)
  # plot(trans$x[1], add= T)
  # plot(trans$x[2], add= T)
  # plot(trans$x[3], add= T)
  # plot(trans$x[4], add= T)
  # plot(trans$x[5], add= T)
    
  # transect line 1 only intersects flowline 1 time and does NOT interesect transects 3 and 4
  is_valid <- hydrofabric3D:::is_valid_transect_line(
    geos::as_geos_geometry(trans$x[1]),
    geos::as_geos_geometry(trans$x[c(1, 3, 4)]),
    geos::as_geos_geometry(flowlines)
  )
  testthat::expect_true(
    is_valid
  )
  
  # TODO: put this in its own test
  # transect line #2 now intersects with transect line 1, NOT VALID
  is_valid <- hydrofabric3D:::is_valid_transect_line(
    geos::as_geos_geometry(trans$x[1]),
    geos::as_geos_geometry(trans$x[c(1, 2, 3, 4)]),
    geos::as_geos_geometry(flowlines)
  )
  
  testthat::expect_false(
    is_valid 
  )
  
  # TODO: put this in its own test
  # transect line #5 does NOT intersect other transects but it DOES intersect flowline 2 times (NOT VALID) 
  is_valid <- hydrofabric3D:::is_valid_transect_line(
    geos::as_geos_geometry(trans$x[5]),
    geos::as_geos_geometry(trans$x[c(5)]),
    geos::as_geos_geometry(flowlines)
  )
  
  testthat::expect_false(
    is_valid 
  )
  
  # TODO: put this in its own test
  # transect line #5 DOES intersects transects 2 and 3 AND it DOES intersect flowline 2 times (NOT VALID) 
  is_valid <- hydrofabric3D:::is_valid_transect_line(
    geos::as_geos_geometry(trans$x[5]),
    geos::as_geos_geometry(trans$x[c(2, 3)]),
    geos::as_geos_geometry(flowlines)
  )
  
  testthat::expect_false(
    is_valid 
  )
  
})
