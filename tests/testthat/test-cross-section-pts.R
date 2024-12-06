library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::cross_section_pts() ----
# -------------------------------------------------------------------

testthat::test_that("check CS points default columns, from basic single flowline transects data (using 'hy_id')", {
  
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
    crosswalk_id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <- dplyr::select(transects,
                dplyr::any_of(ID_COL),
                cs_id,
                cs_lengthm
  )
  
  cs_pts = hydrofabric3D::cross_section_pts(
    transects              = transects,
    crosswalk_id    = ID_COL,
    points_per_cs   = POINTS_PER_CS,
    min_pts_per_cs  = MIN_PTS_PER_CS,
    dem             = DEM_PATH
  )
 
  # cross section points should have the minimum number of columns, 
  # should have transect columns in addition to the default cs pts cols 
  # Which in this case, the transect cols + cs_pt cols ALL overlap
  testthat::expect_true(
    cs_pts_has_min_output_cols(cs_pts, crosswalk_id = ID_COL)
  ) 
  
  # # make sure that the cs_pts have the default cs_pts columns AND the columns from the input transects 
  # testthat::expect_true(
  #   cs_pts_has_correct_cols_from_transects(cs_pts, transects, crosswalk_id = ID_COL)
  # )
  
  # check has minimum required output columns
  testthat::expect_true(
    check_cs_pts_has_required_cols(cs_pts, crosswalk_id = ID_COL)
  ) 
  
  # atleast the minimum number of points were extracted 
  testthat::expect_true(nrow(cs_pts) >= nrow(transects) * MIN_PTS_PER_CS)
  
})

testthat::test_that("error when incorrect columns are provided", {
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
    crosswalk_id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  )
  
   testthat::expect_error(
    hydrofabric3D::cross_section_pts(
      transects             = dplyr::select(transects,
                                     cs_id
      ),
      crosswalk_id   = ID_COL,
      points_per_cs  = POINTS_PER_CS,
      min_pts_per_cs = MIN_PTS_PER_CS,
      dem            = DEM_PATH
    )
  )
  testthat::expect_error(
    hydrofabric3D::cross_section_pts(
      transects             = dplyr::select(transects,
                                     dplyr::any_of(ID_COL),
                                     cs_lengthm
      ),
      crosswalk_id             = ID_COL,
      points_per_cs  = POINTS_PER_CS,
      min_pts_per_cs = MIN_PTS_PER_CS,
      dem            = DEM_PATH
    )
  )
  
   testthat::expect_error(
    hydrofabric3D::cross_section_pts(
      transects             = dplyr::select(transects,
                                     dplyr::any_of(ID_COL),
                                     cs_id
      ),
      crosswalk_id             = ID_COL,
      points_per_cs  = POINTS_PER_CS,
      min_pts_per_cs = MIN_PTS_PER_CS,
      dem            = DEM_PATH
    )
  )
    
   # incorrecct 'crosswalk_id' value
   testthat::expect_error(
    hydrofabric3D::cross_section_pts(
      transects             = dplyr::select(transects,
                                     dplyr::any_of(ID_COL),
                                     cs_id,
                                     cs_lengthm
      ),
      crosswalk_id             = "132365764",
      points_per_cs  = POINTS_PER_CS,
      min_pts_per_cs = MIN_PTS_PER_CS,
      dem            = DEM_PATH
    )
  )
  
  cs_pts = hydrofabric3D::cross_section_pts(
    transects             = dplyr::select(transects,
                                   dplyr::any_of(ID_COL),
                                   cs_id,
                                   cs_lengthm
    ),
    crosswalk_id             = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  )
  
})

testthat::test_that("default transects columns from single flowline, using specified 'hy_id' column", {
  
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
    crosswalk_id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  )
  
  cs_pts = hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  )
  
  testthat::expect_true(
    cs_pts_has_min_output_cols(cs_pts, crosswalk_id = ID_COL)
   ) 
  
  # testthat::expect_true(
  #   cs_pts_has_correct_cols_from_transects(cs_pts, transects, ID_COL)
  # )
 
  # check has minimum required output columns
  testthat::expect_true(
    check_cs_pts_has_required_cols(cs_pts, crosswalk_id = ID_COL)
  ) 
  
  # atleast the minimum number of points were extracted 
  testthat::expect_true(nrow(cs_pts) >= nrow(transects) * MIN_PTS_PER_CS)
  
})



