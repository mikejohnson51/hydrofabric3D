library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::transects_to_cs_pts() ----
# -------------------------------------------------------------------
testthat::test_that("transects_to_cs_pts creates correct number of points for 3 transects", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 10
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>%
    # dplyr::slice(1:3) %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::rename(!!sym(ID_COL) := id) %>% 
    dplyr::select(
      dplyr::any_of(ID_COL), 
      tot_drainage_areasqkm,
      bf_width,
      geom
    ) 
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <- dplyr::select(transects,
                             dplyr::any_of(ID_COL),
                             cs_id,
                             cs_lengthm
  )
  
  cs <- hydrofabric3D:::add_points_per_cs(
    cs              = transects,
    points_per_cs   = NULL,
    min_pts_per_cs  = 10,
    dem             = DEM_PATH
  )

  cs_pts <- transects_to_cs_pts(
    cs,
    cs$points_per_cs
  )
  
  expected_num_pts <- sum(cs$points_per_cs)
  actual_num_pts   <- nrow(cs_pts)
  
  testthat::expect_true(expected_num_pts == actual_num_pts)
  
  testthat::expect_error(
     transects_to_cs_pts(
        cs,
        NA
    ) 
  )
  
  testthat::expect_error(
     transects_to_cs_pts(
        cs,
        NULL
    ) 
  )
  
  testthat::expect_error(
    transects_to_cs_pts(
      cs,
      "AFGDSA"
    ) 
  )
})