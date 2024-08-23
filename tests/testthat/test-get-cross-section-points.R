library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::cut_cross_sections() ----
# -------------------------------------------------------------------

check_cs_pts_has_exact_cols <- function(cs_pts, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
                     )
  
  return(
    all(expected_cols %in% names(cs_pts)) && length(expected_cols) == length(names(cs_pts))
  )
}

check_cs_pts_and_transect_cols <- function(cs_pts, transects, id = "hydrofabric_id") {
  # id = "hy_id"
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
                     )
  
  return(
    all(unique(c(expected_cols, names(transects))) %in% names(cs_pts))
  )
}

check_cs_pts_has_required_cols <- function(transects, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
  )
  
  return(
    all(expected_cols %in% names(transects))
  )
}

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
    id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <- dplyr::select(transects,
                dplyr::any_of(ID_COL),
                cs_id,
                cs_lengthm
  )
  
  cs_pts = hydrofabric3D::cross_section_pts(
    cs              = transects,
    crosswalk_id    = ID_COL,
    points_per_cs   = POINTS_PER_CS,
    min_pts_per_cs  = MIN_PTS_PER_CS,
    dem             = DEM_PATH
  )
 
  # cross section points should have the minimum number of columns, 
  # should have transect columns in addition to the default cs pts cols 
  # Which in this case, the transect cols + cs_pt cols ALL overlap
  testthat::expect_true(
    check_cs_pts_has_exact_cols(cs_pts, id = ID_COL)
  ) 
  
  # make sure that the cs_pts have the default cs_pts columns AND the columns from the input transects 
  testthat::expect_true(
    check_cs_pts_and_transect_cols(cs_pts, transects, id = ID_COL)
  )
  
  # check has minimum required output columns
  testthat::expect_true(
    check_cs_pts_has_required_cols(cs_pts, id = ID_COL)
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
    id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  )
  
   testthat::expect_error(
    hydrofabric3D::cross_section_pts(
      cs             = dplyr::select(transects,
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
      cs             = dplyr::select(transects,
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
      cs             = dplyr::select(transects,
                                     dplyr::any_of(ID_COL),
                                     cs_id
      ),
      crosswalk_id             = ID_COL,
      points_per_cs  = POINTS_PER_CS,
      min_pts_per_cs = MIN_PTS_PER_CS,
      dem            = DEM_PATH
    )
  )
    
   # incorrecct 'id' value
   testthat::expect_error(
    hydrofabric3D::cross_section_pts(
      cs             = dplyr::select(transects,
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
    cs             = dplyr::select(transects,
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
    id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  )
  
  cs_pts = hydrofabric3D::cross_section_pts(
    cs             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  )
  
  # the cross section points should NOT have the minimum cs_pts columns, they should have columns from the input transects in addition to the columns added by cross_section_pts() 
  testthat::expect_false(
   check_cs_pts_has_exact_cols(cs_pts, id = ID_COL)
   ) 
  
  testthat::expect_true(
    check_cs_pts_and_transect_cols(cs_pts, transects, ID_COL)
  )
 
  # check has minimum required output columns
  testthat::expect_true(
    check_cs_pts_has_required_cols(cs_pts, id = ID_COL)
  ) 
  
  # atleast the minimum number of points were extracted 
  testthat::expect_true(nrow(cs_pts) >= nrow(transects) * MIN_PTS_PER_CS)
  
})



