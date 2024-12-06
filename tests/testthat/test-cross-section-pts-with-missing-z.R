
library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")

# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::cross_section_pts() with missing Z values from DEM ----
# -------------------------------------------------------------------

testthat::test_that("check that missing NA value is identified and added as a NA Z value for the correct transect", {
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_IDS_MISSING_Z    <- c(66)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) 
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  ) 
  
  invalid_cs <-     
    cs_pts %>% 
    dplyr::filter(
      cs_id %in% CS_IDS_MISSING_Z
    ) 
  
  correct_transects_missing_z <- 
    invalid_cs  %>% 
    dplyr::pull(Z) %>% 
    is.na() %>% 
    any()
  
  testthat::expect_true(correct_transects_missing_z)
  
  all_cs_have_atleast_min_num_pts <- 
    all(
      cs_pts %>% 
        sf::st_drop_geometry() %>% 
        add_tmp_id(x = ID_COL) %>% 
        dplyr::group_by(tmp_id) %>% 
        dplyr::count() %>% 
        dplyr::pull(n) >= MIN_PTS_PER_CS
    )
  
  testthat::expect_true(all_cs_have_atleast_min_num_pts)
  
})