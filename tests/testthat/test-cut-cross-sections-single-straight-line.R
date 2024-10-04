library(testthat)
library(dplyr)
library(sf)
# library(hydrofabric3D)

source("testing_utils.R")

# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::cut_cross_sections() ----
# -------------------------------------------------------------------

testthat::test_that("cut_cross_sections(), cut transects 1-500 transects and make sure num input is equal to number of transects in output", {
  CROSSWALK_ID = "id"
  
  flowline <- 
    make_single_straight_line() %>% 
    dplyr::mutate(
      id = "1"
    )
  
  for (NUM_OF_TRANSECTS in seq(1, 500, 25)) {
    # message("cutting ", NUM_OF_TRANSECTS, " (densifying to ", NUM_OF_TRANSECTS + 1, ")")
    
    transects <- cut_cross_sections(
      net      = flowline,
      num      = NUM_OF_TRANSECTS,
      densify  = NUM_OF_TRANSECTS + 1
    )
    
    has_expected_number_of_transects <- nrow(transects) == NUM_OF_TRANSECTS
    testthat::expect_true(has_expected_number_of_transects)
  }
  
  # plot(flowline$x)
  # plot(transects$geometry, add = T)
 
})

# TODO: deal with if there is a duplicate geometry and/or CROSSWALK_ID in the set of given flowlines
testthat::test_that("cut_cross_sections() - 2 duplicate flowlines and IDs  ", {
  # CROSSWALK_ID = "id"
  # NUM_OF_TRANSECTS <- 3
  # 
  # flowlines <-
  #     dplyr::bind_rows(
  #       make_single_straight_line() %>% 
  #       dplyr::mutate(
  #         id = "1"
  #       ),
  #       make_single_straight_line() %>% 
  #       dplyr::mutate(
  #         id = "1"
  #       )
  #     )
  # 
  # # sf:::distinct.sf(flowlines)
  # 
  # # flowlines$id
  # 
  #   transects <- cut_cross_sections(
  #     net      = flowlines,
  #     num      = NUM_OF_TRANSECTS,
  #     densify  = NUM_OF_TRANSECTS + 1
  #   )
  #   
  #   has_expected_number_of_transects <- nrow(transects) == NUM_OF_TRANSECTS
  #   testthat::expect_true(has_expected_number_of_transects)
  # # plot(flowline$x)
  # # plot(transects$geometry, add = T)
  
})
