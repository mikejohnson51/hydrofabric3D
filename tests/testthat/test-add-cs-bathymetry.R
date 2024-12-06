library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::add_cs_bathymetry() ----
# -------------------------------------------------------------------

testthat::test_that("add_cs_bathymetry() - check that the bottom depth gets moved down according to the prescribed depths array", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  DEPTHS <- c(10, 20, 30, 40, 50)
  
  for (depth in DEPTHS) {
    # message(depth)
    
    cs_pts <- 
      flowlines %>% 
      dplyr::slice(1)  %>%  
      cut_cross_sections(
        crosswalk_id  = "id",  
        num = 1
      ) %>% 
      cross_section_pts(crosswalk_id = "id", 
                        points_per_cs = 10,
                        min_pts_per_cs = 10, 
                        dem = DEM_PATH
      ) %>% 
      classify_points("id") %>% 
      dplyr::mutate(
        TW = 30,
        DEPTH = depth ,
        DINGMAN_R = 1
      ) %>% 
      pts_to_XY()
    
    cs_bathy <- 
      cs_pts %>% 
      add_cs_bathymetry("id") %>% 
      classify_points("id")
    
    # cs_pts %>%
    #   plot_cs_pts("id", size = 4)
    # cs_bathy %>%
    #   plot_cs_pts("id", size = 4)
    
    old_bottomZ <- 
      cs_pts %>% 
      dplyr::filter(point_type == "bottom") %>% 
      dplyr::slice_min(Z, with_ties = FALSE) %>% 
      dplyr::pull(Z)
    
    new_bottomZ <- 
      cs_bathy %>% 
      dplyr::filter(point_type == "bottom") %>% 
      dplyr::slice_min(Z, with_ties = FALSE) %>% 
      dplyr::pull(Z)
    
    change_in_bottomZ <- old_bottomZ - new_bottomZ
    
    testthat::expect_true(
      dplyr::between(
        change_in_bottomZ,
        depth - 5,
        depth + 5
      )
    )
  }
  
})