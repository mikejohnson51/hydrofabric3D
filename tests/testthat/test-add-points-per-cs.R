

library(testthat)
library(dplyr)
library(sf)
# library(hydrofabric3D)
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::add_points_per_cs() ----
# -------------------------------------------------------------------
testthat::test_that("correct points per cross section on 3 transects from single flowline w/ minimum req columns, POINTS_PER_CS = NULL", {
  
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
  
  # minimum required column
  transects <- dplyr::select(transects, cs_lengthm)
  
  cs_ppcs <- hydrofabric3D:::add_points_per_cs(
    cs               = transects,
    points_per_cs    = POINTS_PER_CS,
    min_pts_per_cs   = MIN_PTS_PER_CS, 
    dem              = DEM_PATH
  )
  
  # hydrofabric3D:::add_points_per_cs(
    # cs               = transects,
    # points_per_cs    = POINTS_PER_CS,
    # min_pts_per_cs   = MIN_PTS_PER_CS, 
    # dem              = DEM_PATH
  # ) 
  
  # has the correct added column
  has_ppcs_col <- "points_per_cs" %in% names(cs_ppcs)
  testthat::expect_true(has_ppcs_col)
  
  # all points per cross section are greater than or equal to the specified MINIMUM
  all_ppcs_ge_min_ppcs <- all(cs_ppcs$points_per_cs >= MIN_PTS_PER_CS)
  testthat::expect_true(all_ppcs_ge_min_ppcs)
  
})

testthat::test_that("correct points per cross section on 3 transects from
                    single flowline w/ minimum req columns, POINTS_PER_CS = 20, MIN PTS = 10", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- 20
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
  
  # minimum required column
  transects <- dplyr::select(transects, cs_lengthm)
  
  cs_ppcs <- hydrofabric3D:::add_points_per_cs(
    cs               = transects,
    points_per_cs    = POINTS_PER_CS,
    min_pts_per_cs   = MIN_PTS_PER_CS, 
    dem              = DEM_PATH
  )
  
  # has the correct added column
  has_ppcs_col <- "points_per_cs" %in% names(cs_ppcs)
  testthat::expect_true(has_ppcs_col)
  
  # all points per cross section are greater than or equal to the specified MINIMUM
  all_ppcs_ge_min_ppcs <- all(cs_ppcs$points_per_cs >= MIN_PTS_PER_CS)
  testthat::expect_true(all_ppcs_ge_min_ppcs)
  
})

testthat::test_that("correct points per cross section on 3 transects from 
                    single flowline w/ minimum req columns, POINTS_PER_CS = NULL, MIN PTS PER CS less than equal to 2", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 0
  
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
  
  # minimum required column
  transects <- dplyr::select(transects, cs_lengthm)
  
  # cs_ppcs <- 
  
  testthat::expect_true(
    all(
      hydrofabric3D:::add_points_per_cs(
          cs               = transects,
          points_per_cs    = NULL,
          min_pts_per_cs   = 0, 
          dem              = DEM_PATH
        ) %>% 
        .$points_per_cs == 2
      )    
    )
  
  testthat::expect_true(
      all( 
        hydrofabric3D:::add_points_per_cs(
          cs               = transects,
          points_per_cs    = NULL,
          min_pts_per_cs   = 1, 
          dem              = DEM_PATH
          ) %>% 
          .$points_per_cs == 2
        )
      )
  
  testthat::expect_true(
     all( 
      hydrofabric3D:::add_points_per_cs(
        cs               = transects,
        points_per_cs    = NULL,
        min_pts_per_cs   = 2, 
        dem              = DEM_PATH
      ) %>% 
      .$points_per_cs == 2
     )
    )
})

testthat::test_that("dem argument is ignored if a 'points_per_cs' is given", {
  
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- 20
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
  
 testthat::expect_true( 
  all(
    hydrofabric3D:::add_points_per_cs(
        cs               = transects, 
        points_per_cs    = POINTS_PER_CS,
        min_pts_per_cs   = MIN_PTS_PER_CS, 
        dem              = testthat::test_path("testdata", "dem_flowlines.tif")
      )  %>% 
      .$points_per_cs == POINTS_PER_CS
  )
  )
 testthat::expect_true( 
  all(
      hydrofabric3D:::add_points_per_cs(
      cs               = transects, 
      points_per_cs    = POINTS_PER_CS,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = TRUE
      ) %>% 
      .$points_per_cs == POINTS_PER_CS
    )
 )
  testthat::expect_true(
      all(
        hydrofabric3D:::add_points_per_cs(
          cs               = transects, 
          points_per_cs    = POINTS_PER_CS,
          min_pts_per_cs   = MIN_PTS_PER_CS, 
          dem              = FALSE
        ) %>% 
          .$points_per_cs == POINTS_PER_CS
    )
  )
  testthat::expect_true(
    all(
      hydrofabric3D:::add_points_per_cs(
        cs               = transects, 
        points_per_cs    = POINTS_PER_CS,
        min_pts_per_cs   = MIN_PTS_PER_CS, 
        dem              = c(213324)
      ) %>% 
        .$points_per_cs == POINTS_PER_CS
    )
  )
  
  testthat::expect_true(
    all(
      hydrofabric3D:::add_points_per_cs(
        cs               = transects, 
        points_per_cs    = POINTS_PER_CS,
        min_pts_per_cs   = MIN_PTS_PER_CS, 
        dem              = c("AAA")
      ) %>% 
        .$points_per_cs == POINTS_PER_CS
    )
  )
  
})

testthat::test_that("error giving bad dem arguments when POINTS_PER_CS is NULL", {
  
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- FALSE
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
  
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = transects, 
      points_per_cs    = NULL,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = FALSE
    )  
  )
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = transects, 
      points_per_cs    = NULL,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = TRUE
    )  
  )
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = transects, 
      points_per_cs    = NULL,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = c(213324)
    )  
  )
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = transects, 
      points_per_cs    = NULL,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = c("AAA")
    )  
  )
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = transects, 
      points_per_cs    = NULL,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = 100
    )  
  )
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = transects, 
      points_per_cs    = NULL,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = "AAAAAAA" 
    )  
  )
  
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = transects, 
      points_per_cs    = NULL,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = NULL 
    )  
  )
  
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = transects, 
      points_per_cs    = NULL,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = NA
    )  
  )
  
})

testthat::test_that("error flowline WITHOUT minimum required columns", {
  
  
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
  
  # Select everything EXCEPT the minimum required column (cs_lengthm)
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = dplyr::select(transects, -cs_lengthm),
      points_per_cs    = POINTS_PER_CS,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = DEM_PATH
    )
  )
  
  # misspelled the minimum required column (cs_lengthm)
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = dplyr::select(transects, lengthm = cs_lengthm),
      points_per_cs    = POINTS_PER_CS,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = DEM_PATH
    )
  )
  
  # no columns except geometry (LINESTRING)
  testthat::expect_error(
    hydrofabric3D:::add_points_per_cs(
      cs               = dplyr::select(transects),
      points_per_cs    = POINTS_PER_CS,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = DEM_PATH
    )
  )
  
})

  