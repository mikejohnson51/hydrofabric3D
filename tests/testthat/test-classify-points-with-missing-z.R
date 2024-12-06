
library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")

# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::classify_points() when cs_pts have missing Z values ----
# -------------------------------------------------------------------

testthat::test_that("classify_points() - throws an error when empty cross section points dataframe is given and when na.rm = TRUE", {
                      
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_IDS_OF_INTEREST    <- c(65, 66, 67)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- 
    TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH %>% 
    sf::read_sf() %>% 
    dplyr::filter(cs_id %in% CS_IDS_OF_INTEREST)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = DEM_PATH
  ) %>% 
    dplyr::slice(0)
  
  
  testthat::expect_error(
    hydrofabric3D::classify_points(cs_pts, crosswalk_id = ID_COL, na.rm = TRUE)
  )
  
})

testthat::test_that("classify_points() - throws an error when empty cross section points dataframe is given and when na.rm = FALSE", {
  
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_IDS_OF_INTEREST    <- c(65, 66, 67)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- 
    TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH %>% 
    sf::read_sf() %>% 
    dplyr::filter(cs_id %in% CS_IDS_OF_INTEREST)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = DEM_PATH
  ) %>% 
    dplyr::slice(0)
  
  testthat::expect_error(
    hydrofabric3D::classify_points(cs_pts, crosswalk_id = ID_COL, na.rm = FALSE)
  )
  
})

testthat::test_that("classify_points() - throws an error when given a not present crosswalk_id and na.rm = TRUE", {
  
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  CS_IDS_OF_INTEREST    <- c(65, 66, 67)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  
  transects    <- 
    TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH %>% 
    sf::read_sf() %>% 
    dplyr::filter(cs_id %in% CS_IDS_OF_INTEREST)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = "hy_id",
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = DEM_PATH
  ) 
  
  testthat::expect_error(
    hydrofabric3D::classify_points(cs_pts, crosswalk_id = "not_a_column", na.rm = TRUE)
  )
  
})

testthat::test_that("classify_points() - throws an error when given a not present crosswalk_id and na.rm = FALSE", {
  
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  CS_IDS_OF_INTEREST    <- c(65, 66, 67)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  
  transects    <- 
    TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH %>% 
    sf::read_sf() %>% 
    dplyr::filter(cs_id %in% CS_IDS_OF_INTEREST)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = "hy_id",
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = DEM_PATH
  ) 
  
  testthat::expect_error(
    hydrofabric3D::classify_points(cs_pts, crosswalk_id = "not_a_column", na.rm = FALSE)
  )
  
})


testthat::test_that("classify_points() - 1 transects with 1+ NA values 
                    gives a warning when na.rm = TRUE and there are no output classified points because of the NA removal", {
  
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_IDS_MISSING_Z    <- c(66)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
                    dplyr::filter(cs_id %in% CS_IDS_MISSING_Z)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  ) 
  
  testthat::expect_warning(
    hydrofabric3D::classify_points(cs_pts, crosswalk_id = ID_COL, na.rm = TRUE)
  )
})

testthat::test_that("classify_points() - 1 transects with 1+ NA values 
                    does NOT give a warning when na.rm = FALSE because the NA cs pts are kept", {
  
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_IDS_MISSING_Z    <- c(66)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
    dplyr::filter(cs_id %in% CS_IDS_MISSING_Z)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  ) 
  
  testthat::expect_no_warning(
    hydrofabric3D::classify_points(cs_pts, crosswalk_id = ID_COL, na.rm = FALSE)
  )
})

testthat::test_that("classify_points() - (1 transects) - cs_pts that have 1+ NA values gives a warning when na.rm = TRUE and its the only cross section in the set", {
  
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_IDS_MISSING_Z    <- c(66)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
    dplyr::filter(cs_id %in% CS_IDS_MISSING_Z)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  ) 
  testthat::expect_warning(
    classified_pts <- hydrofabric3D::classify_points(cs_pts, 
                                     crosswalk_id = ID_COL, 
                                     na.rm = TRUE
                                     )
  )
  
  
  # has_bottom_point_type <- c("bottom") %in% classified_pts$point_type
  # testthat::expect_true(has_bottom_point_type)
  
})

testthat::test_that("classify_points() - cs_pts for 1 transect that have 1+ NA values returns an 
                    empty classified pts dataframe when na.rm = TRUE and 
                    the cs_pts being classified have an NA and its the only cross section in the set", {
  
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_IDS_MISSING_Z    <- c(66)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
    dplyr::filter(cs_id %in% CS_IDS_MISSING_Z)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  ) 
    classified_pts <- hydrofabric3D::classify_points(cs_pts, 
                                                     crosswalk_id = ID_COL, 
                                                     na.rm = TRUE
    )
  
    testthat::expect_true(
      nrow(classified_pts) == 0
    )
})

testthat::test_that("classify_points() - (1 transects) - cs_pts that have 1+ NA values returns a classified pts dataframe 
                    with all NA cs pts attributes values when na.rm = FALSE its the only cross section in the set", {
  
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_IDS_MISSING_Z    <- c(66)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
    dplyr::filter(cs_id %in% CS_IDS_MISSING_Z)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  ) 
  
  classified_pts <- hydrofabric3D::classify_points(cs_pts, 
                                                   crosswalk_id = ID_COL, 
                                                   na.rm = FALSE
  )
  classified_pts
  
  testthat::expect_true(
    nrow(classified_pts) == nrow(cs_pts)
  )
  
  testthat::expect_true(
    classified_pts$Z %>% is.na() %>% any()
  )
  
  testthat::expect_true(
    all(c("class", "point_type", "bottom", "left_bank", "right_bank", "valid_banks", "has_relief") %in% names(classified_pts))
  )
  
})

testthat::test_that("classify_points() - cs_pts for 2 transects where one of the transects has 1+ NA values gets 
                  removed when na.rm = TRUE and there are more than just that NA filled set of cs pts 
                  (i.e. theres another cross section that does NOT contain any NA values)
                    ", {
  
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_ID_WITH_MISSING_Z  <- 66
  CS_ID_WITH_COMPLETE_Z <- 67
  
  CS_IDS_OF_INTEREST <- c(CS_ID_WITH_MISSING_Z, CS_ID_WITH_COMPLETE_Z)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
    dplyr::filter(cs_id %in% CS_IDS_OF_INTEREST)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    transects             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  ) 
  
  classified_pts <- hydrofabric3D::classify_points(cs_pts, 
                                   crosswalk_id = ID_COL, 
                                   na.rm = TRUE
  )
  
  # the cs_id with known missing Z values is NOT in output set
  testthat::expect_true(
    !CS_ID_WITH_MISSING_Z %in% unique(classified_pts$cs_id)
  )
  
  # cs_ids with all good Z values are all in the output set
  testthat::expect_true(
    all(CS_ID_WITH_COMPLETE_Z %in% unique(classified_pts$cs_id))
  )
  
})


testthat::test_that("classify_points() - (1 transects) - cs_pts that have 1+ NA values is kept 
                  when na.rm = FALSE and there are more than just that NA filled set of cs pts 
                  (i.e. theres another cross section that does NOT contain any NA values)", {
                      
      TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
      ID_COL              <- "hy_id"
      CS_ID_WITH_MISSING_Z <- 66
      CS_ID_WITH_COMPLETE_Z <- 67
      
      CS_IDS_OF_INTEREST <- c(CS_ID_WITH_MISSING_Z, CS_ID_WITH_COMPLETE_Z)
      
      # Cross section point inputs
      DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
      POINTS_PER_CS       <- NULL
      MIN_PTS_PER_CS      <- 10
      
      transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
        dplyr::filter(cs_id %in% CS_IDS_OF_INTEREST)
      
      # mapview::mapview(raster::raster(DEM_PATH)) + transects
      
      cs_pts <- hydrofabric3D::cross_section_pts(
        transects             = transects,
        crosswalk_id   = ID_COL,
        points_per_cs  = POINTS_PER_CS,
        min_pts_per_cs = MIN_PTS_PER_CS,
        dem            = DEM_PATH
      ) 
      
      classified_pts <- hydrofabric3D::classify_points(cs_pts, 
                                                         crosswalk_id = ID_COL, 
                                                         na.rm = FALSE
      )
      
      # all of the cs_ids are all in the output set
      testthat::expect_true(
        all(CS_IDS_OF_INTEREST %in% unique(classified_pts$cs_id))
      )
      
    })

testthat::test_that("classify_points() - (1 transects) - cs_pts that have 1+ NA values have all NA values for classification attributes
          when na.rm = FALSE", {
                    
    TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
    ID_COL              <- "hy_id"
    CS_ID_WITH_MISSING_Z <- 66
    CS_ID_WITH_COMPLETE_Z <- 67
    
    CS_IDS_OF_INTEREST <- c(CS_ID_WITH_MISSING_Z, CS_ID_WITH_COMPLETE_Z)
    
    # Cross section point inputs
    DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
    POINTS_PER_CS       <- NULL
    MIN_PTS_PER_CS      <- 10
    
    transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
      dplyr::filter(cs_id %in% CS_IDS_OF_INTEREST)
    
    # mapview::mapview(raster::raster(DEM_PATH)) + transects
    
    cs_pts <- hydrofabric3D::cross_section_pts(
      transects             = transects,
      crosswalk_id   = ID_COL,
      points_per_cs  = POINTS_PER_CS,
      min_pts_per_cs = MIN_PTS_PER_CS,
      dem            = DEM_PATH
    ) 
    
    classified_pts <- hydrofabric3D::classify_points(cs_pts, 
                                                       crosswalk_id = ID_COL, 
                                                       na.rm = FALSE
    )
    
    # check all of the values of the CS_ID with missing Z that they are all NA values
    testthat::expect_true(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
        dplyr::pull(class) %>% 
        is.na() %>% 
        all()
    )
    
    testthat::expect_true(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
        dplyr::pull(point_type) %>% 
        is.na() %>% 
        all()
    )
    
    testthat::expect_true(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
        dplyr::pull(bottom) %>% 
        is.na() %>% 
        all()
    )
    
    testthat::expect_true(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
        dplyr::pull(left_bank) %>% 
        is.na() %>% 
        all()
    )
    
    testthat::expect_true(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
        dplyr::pull(right_bank) %>% 
        is.na() %>% 
        all()
    )
    
    testthat::expect_true(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
        dplyr::pull(valid_banks) %>% 
        is.na() %>% 
        all()
    )
    
    testthat::expect_true(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
        dplyr::pull(has_relief) %>% 
        is.na() %>% 
        all()
    )
    
    # Check NON MISSING Z cs_ids for NO NA values in attributes columns 
    testthat::expect_false(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
        dplyr::pull(class) %>% 
        is.na() %>% 
        any()
    )
    
    testthat::expect_false(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
        dplyr::pull(point_type) %>% 
        is.na() %>% 
        any()
    )
    
    testthat::expect_false(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
        dplyr::pull(bottom) %>% 
        is.na() %>% 
        any()
    )
    
    testthat::expect_false(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
        dplyr::pull(left_bank) %>% 
        is.na() %>% 
        any()
    )
    
    testthat::expect_false(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
        dplyr::pull(right_bank) %>% 
        is.na() %>% 
        any()
    )
    
    testthat::expect_false(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
        dplyr::pull(valid_banks) %>% 
        is.na() %>% 
        any()
    )
    
    testthat::expect_false(
      classified_pts %>% 
        dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
        dplyr::pull(has_relief) %>% 
        is.na() %>% 
        any()
    )
    
  })


testthat::test_that("classify_points() - (1 transects) - cs_pts that have 1+ NA values have all NA values for classification attributes
          when na.rm = FALSE", {
            
            TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
            ID_COL              <- "hy_id"
            CS_ID_WITH_MISSING_Z <- 66
            CS_ID_WITH_COMPLETE_Z <- 67
            
            CS_IDS_OF_INTEREST <- c(CS_ID_WITH_MISSING_Z, CS_ID_WITH_COMPLETE_Z)
            
            # Cross section point inputs
            DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
            POINTS_PER_CS       <- NULL
            MIN_PTS_PER_CS      <- 10
            
            transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
              dplyr::filter(cs_id %in% CS_IDS_OF_INTEREST)
            
            # mapview::mapview(raster::raster(DEM_PATH)) + transects
            
            cs_pts <- hydrofabric3D::cross_section_pts(
              transects             = transects,
              crosswalk_id   = ID_COL,
              points_per_cs  = POINTS_PER_CS,
              min_pts_per_cs = MIN_PTS_PER_CS,
              dem            = DEM_PATH
            ) 
            
            classified_pts <- hydrofabric3D::classify_points(cs_pts, 
                                                               crosswalk_id = ID_COL, 
                                                               na.rm = FALSE
            )
            
            # check all of the values of the CS_ID with missing Z that they are all NA values
            testthat::expect_true(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
                dplyr::pull(class) %>% 
                is.na() %>% 
                all()
            )
            
            testthat::expect_true(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
                dplyr::pull(point_type) %>% 
                is.na() %>% 
                all()
            )
            
            testthat::expect_true(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
                dplyr::pull(bottom) %>% 
                is.na() %>% 
                all()
            )
            
            testthat::expect_true(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
                dplyr::pull(left_bank) %>% 
                is.na() %>% 
                all()
            )
            
            testthat::expect_true(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
                dplyr::pull(right_bank) %>% 
                is.na() %>% 
                all()
            )
            
            testthat::expect_true(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
                dplyr::pull(valid_banks) %>% 
                is.na() %>% 
                all()
            )
            
            testthat::expect_true(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_MISSING_Z) %>% 
                dplyr::pull(has_relief) %>% 
                is.na() %>% 
                all()
            )
            
            # Check NON MISSING Z cs_ids for NO NA values in attributes columns 
            testthat::expect_false(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
                dplyr::pull(class) %>% 
                is.na() %>% 
                any()
            )
            
            testthat::expect_false(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
                dplyr::pull(point_type) %>% 
                is.na() %>% 
                any()
            )
            
            testthat::expect_false(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
                dplyr::pull(bottom) %>% 
                is.na() %>% 
                any()
            )
            
            testthat::expect_false(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
                dplyr::pull(left_bank) %>% 
                is.na() %>% 
                any()
            )
            
            testthat::expect_false(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
                dplyr::pull(right_bank) %>% 
                is.na() %>% 
                any()
            )
            
            testthat::expect_false(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
                dplyr::pull(valid_banks) %>% 
                is.na() %>% 
                any()
            )
            
            testthat::expect_false(
              classified_pts %>% 
                dplyr::filter(cs_id %in% CS_ID_WITH_COMPLETE_Z) %>% 
                dplyr::pull(has_relief) %>% 
                is.na() %>% 
                any()
            )
            
          })

# testthat::test_that("check that missing NA value is identified and added as a NA Z value", {
  
  # flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines_missing_depth.gpkg")) 
  # # flowlines    <- dplyr::slice(flowlines, 1)
  # 
  # dem <- raster::raster(DEM_PATH)
  # 
  # MIN_BF_WIDTH       <- 25
  # ID_COL             <- "hy_id"
  # NUM_OF_TRANSECTS   <- 150
  # 
  # # Cross section point inputs
  # DEM_PATH          <- testthat::test_path("testdata", "dem_missing_depth.tif")
  # POINTS_PER_CS     <- NULL
  # MIN_PTS_PER_CS    <- 10
  # 
  # flowlines <- 
  #   flowlines %>% 
  #   prep_flowlines_for_transect_cuts(ID_COL, MIN_BF_WIDTH)
  # transects <- 
  #   flowlines %>% 
  #   hydrofabric3D::cut_cross_sections(
  #     crosswalk_id = ID_COL,
  #     num = NUM_OF_TRANSECTS,
  #     cs_widths  = flowlines$bf_width
  #   ) 
  # transects_missing_depth <- 
  #   transects %>% 
  #   dplyr::filter(cs_id %in% c(64, 65, 66, 67, 68))
  # sf::write_sf(transects_missing_depth)
    # dplyr::select(dplyr::any_of(ID_COL),
                  # cs_id,
                  # cs_lengthm
                  # )
  # mapview::mapview(dem) + transects  
  # })

# library(sf)
# library(dplyr)
# library(geos)
# library(terra)

# cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
# cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_11.gpkg")
# cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_classified_11.gpkg")
  # dplyr::rename(hy_id = id)
# flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
# transects <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_transects_11.gpkg")

# library(sf)
# library(dplyr)
# library(geos)
# library(terra)
# 
# # NOTE: Need nextgen flowlines for VPU 13
# net <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_11.gpkg")
# 
# # flowlines with known issues (missing Z data in the DEM)
# missing_Z_ids <- c("wb-2146599")
# # missing_Z_ids <- c("wb-2131572", "wb-2146599")
# 
# neighbor_ids <- dplyr::pull(
#                 tidyr::pivot_longer(
#                   dplyr::select(
#                     sf::st_drop_geometry(
#                     dplyr::mutate(  
#                       dplyr::filter(net, id %in% missing_Z_ids), 
#                       to_id = gsub("nex-", "wb-", foi$toid)
#                       )
#                     ), 
#                     id, to_id
#                   ),
#                     cols      = c(id, to_id), 
#                     names_to  = "name", 
#                     values_to = "id"
#                     ), 
#                 id
#               )
#     
# foi <- 
#   net %>% 
#   dplyr::filter(id %in% neighbor_ids) %>% 
#   dplyr::select(id, 
#                 lengthkm, mainstem, tot_drainage_areasqkm, 
#                 geometry = geom
#                 )
# 
# sf::write_sf(foi, "tests/testthat/testdata/invalid_flowlines.gpkg")

# TODO: problematic hy_ids in VPU 13 (have NA valid_banks / has_relief)
# which(transects_to_check$hy_id %in% c("wb-2131572", "wb-2146599"))
