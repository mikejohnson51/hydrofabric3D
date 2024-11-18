
library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")

# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::get_relief() ----
# -------------------------------------------------------------------

testthat::test_that("'has_relief' - (10 flowlines, 3 transects per flowline, default transect columns) - check minimum output columns and retain unique IDs (DETAILED = FALSE)", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 10
  
  PCT_OF_LENGTH_FOR_RELIEF <- 0.01
  DETAILED                 <- FALSE
  
  flowlines <-
    flowlines %>%
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
  ) %>%
    dplyr::select(
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
  
  classified <- classify_points(
    cs_pts = cs_pts,
    crosswalk_id = ID_COL,
    pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
  ) %>%
    dplyr::select(-valid_banks, -has_relief, -left_bank, -right_bank, -bottom)
  
  # hydrofabric3D::plot_cs_pts(classified, color = "point_type")
  
  relief <- get_relief(
    classified_pts           = classified,
    crosswalk_id             = ID_COL,
    pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF,
    detailed                 = DETAILED
  )
  
  has_min_output_cols <- relief_has_min_output_cols(relief, crosswalk_id = ID_COL) 
  testthat::expect_true(has_min_output_cols)
  
  same_unique_tmp_ids <- has_same_unique_tmp_ids(classified, relief, crosswalk_id = ID_COL)
  testthat::expect_true(same_unique_tmp_ids)
  
})

testthat::test_that("'has_relief' - (10 flowlines, 3 transects per flowline, default transect columns) - check minimum output columns and retain unique IDs (DETAILED = TRUE)", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 10
  
  PCT_OF_LENGTH_FOR_RELIEF <- 0.01
  DETAILED                 <- TRUE
  
  flowlines <-
    flowlines %>%
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
  ) %>%
    dplyr::select(
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
  
  classified <- classify_points(
    cs_pts = cs_pts,
    crosswalk_id = ID_COL,
    pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
  ) %>%
    dplyr::select(-valid_banks, -has_relief, -left_bank, -right_bank, -bottom)
  
  # hydrofabric3D::plot_cs_pts(classified, color = "point_type")
  
  relief <- get_relief(
    classified_pts           = classified,
    crosswalk_id             = ID_COL,
    pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF,
    detailed                 = DETAILED
  )
  
  has_min_output_cols <- relief_detailed_has_min_output_cols(relief, crosswalk_id = ID_COL) 
  testthat::expect_true(has_min_output_cols)
  
  same_unique_tmp_ids <- has_same_unique_tmp_ids(classified, relief, crosswalk_id = ID_COL)
  testthat::expect_true(same_unique_tmp_ids)
  
})

testthat::test_that("'has_relief' - (1 cross section, missing 1 of each required col) - checks an error is thrown if any of the required columns are missing in input CS", {
  
  ID_COL    <- "hy_id"
  REQ_COLS  <- c(ID_COL, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
  
  PCT_OF_LENGTH_FOR_RELIEF <- 0.01
  DETAILED                 <- TRUE
  
  all_req_cols <-
    data.frame(
      hy_id = c("A", "A", "A", "A", "A"),
      cs_id = c(1, 1, 1, 1, 1),
      pt_id = c(1, 2, 3, 4, 5),
      cs_lengthm = c(10, 10, 10, 10, 10),
      point_type = c('left_bank', 'left_bank', 'bottom', 'right_bank', 'right_bank'),
      Z = c(10, 8, 2, 8, 10)
    )
  
  # First make sure that this test data works as expected before removing columns to cause errors to be thrown
  testthat::expect_no_error(
    good_output <- get_relief(
                 classified_pts           = all_req_cols, 
                 crosswalk_id             = ID_COL, 
                 pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
                 )
    )
  
  # has valid relief 
  testthat::expect_true(
    all(good_output$has_relief)
    )
  
  # NOTE: loop over each column and remove one at a time --> expecting an error each time a required column is missing
  for (col in names(all_req_cols)) {
    # message("Removing ", col, " from cs pts test df")
    
    missing_one_col <- all_req_cols[REQ_COLS[REQ_COLS != col]]
    testthat::expect_error(
      get_relief(classified_pts           = missing_one_col, 
                 crosswalk_id             = ID_COL, 
                 pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF)
      )
  }
  
})

testthat::test_that("'has_relief' - (1 cross section, 50% length to relief, 3 cs_pts) - has_relief flag ", {
  
  ID_COL    <- "hy_id"
  REQ_COLS  <- c(ID_COL, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
  
  PCT_OF_LENGTH_FOR_RELIEF <- 0.5
  CS_LENGTHM               <- 100
  MIN_REQ_RELIEF           <- CS_LENGTHM * PCT_OF_LENGTH_FOR_RELIEF
  
  DETAILED                 <- TRUE
  
  does_have_relief <-
    data.frame(
      hy_id = c("A", "A",  "A"),
      cs_id = c(1, 1, 1),
      pt_id = c(1, 2, 3),
      cs_lengthm = c(CS_LENGTHM),
      point_type = c('left_bank', 'bottom', 'right_bank'),
      Z = c(100, MIN_REQ_RELIEF - 1, 100)
    )
  
  does_have_relief_result <- 
    get_relief(
      classified_pts           = does_have_relief, 
      crosswalk_id             = ID_COL, 
      pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
    )
  
  testthat::expect_true(
    all(does_have_relief_result$has_relief)
    )
  
  does_not_have_relief <-
    data.frame(
      hy_id = c("A", "A",  "A"),
      cs_id = c(1, 1, 1),
      pt_id = c(1, 2, 3),
      cs_lengthm = c(CS_LENGTHM),
      point_type = c('left_bank', 'bottom', 'right_bank'),
      Z = c(100, MIN_REQ_RELIEF + 1, 100)
    )
  
  does_not_have_relief_result <- 
    get_relief(
      classified_pts           = does_not_have_relief, 
      crosswalk_id             = ID_COL, 
      pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
    )
  
  testthat::expect_false(
    all(does_not_have_relief_result$has_relief)
  )
  
})

testthat::test_that("'has_relief' - (1 cross section, 0% length to relief, 3 cs_pts) - has_relief flag ", {
  
  ID_COL    <- "hy_id"
  REQ_COLS  <- c(ID_COL, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
  
  PCT_OF_LENGTH_FOR_RELIEF <- 0
  CS_LENGTHM               <- 100
  MIN_REQ_RELIEF           <- CS_LENGTHM * PCT_OF_LENGTH_FOR_RELIEF
  
  DETAILED                 <- TRUE
  
  does_have_relief <-
    data.frame(
      hy_id = c("A", "A",  "A"),
      cs_id = c(1, 1, 1),
      pt_id = c(1, 2, 3),
      cs_lengthm = c(CS_LENGTHM),
      point_type = c('left_bank', 'bottom', 'right_bank'),
      Z = c(100, MIN_REQ_RELIEF-1, 100)
    )
  
  does_have_relief_result <- 
    get_relief(
      classified_pts           = does_have_relief, 
      crosswalk_id             = ID_COL, 
      pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
    )
  
  testthat::expect_true(
    all(does_have_relief_result$has_relief)
  )
  
  does_have_relief <-
    data.frame(
      hy_id = c("A", "A",  "A"),
      cs_id = c(1, 1, 1),
      pt_id = c(1, 2, 3),
      cs_lengthm = c(CS_LENGTHM),
      point_type = c('left_bank', 'bottom', 'right_bank'),
      Z = c(100, MIN_REQ_RELIEF + 1, 100)
    )
  
  does_have_relief_result <- 
    get_relief(
      classified_pts           = does_have_relief, 
      crosswalk_id             = ID_COL, 
      pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
    )
  
  testthat::expect_true(
    all(does_have_relief_result$has_relief)
  )
  
})


testthat::test_that("'has_relief' - (1 cross section, 1% length to relief, 5 cs_pts, NO RELIEF) - flat CS, 1 left_bank, 3 bottom, 1 right bank", {
  
  ID_COL    <- "hy_id"
  REQ_COLS  <- c(ID_COL, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
  
  PCT_OF_LENGTH_FOR_RELIEF <- 0.01
  CS_LENGTHM               <- 100
  MIN_REQ_RELIEF           <- CS_LENGTHM * PCT_OF_LENGTH_FOR_RELIEF
  
  DETAILED                 <- TRUE
  
  flat_cs <-
    data.frame(
      hy_id = c("A", "A",  "A", "A", "A"),
      cs_id = c(1, 1, 1, 1, 1),
      pt_id = c(1, 2, 3, 4, 5),
      cs_lengthm = c(CS_LENGTHM),
      point_type = c('left_bank', 'bottom', 'bottom', 'bottom', 'right_bank'),
      Z = c(100, 100, 100, 100, 100)
    )
  
  flat_cs_result <- 
    get_relief(
      classified_pts           = flat_cs, 
      crosswalk_id             = ID_COL, 
      pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
    )
  
  testthat::expect_false(
    all(flat_cs_result$has_relief)
  )
  
})

# ---- GOOD TESTING DATA TO USE for get_relief()

# crosswalk_id    <- "hy_id"
# REQ_COLS  <- c(crosswalk_id, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
# 
# pct_of_length_for_relief <- 0.01
# CS_LENGTHM               <- 100
# MIN_REQ_RELIEF           <- CS_LENGTHM * pct_of_length_for_relief
# detailed                 <- FALSE
# 
# classified_pts <-
#   data.frame(
#     hy_id = c("A", "A",  "A", "A", "A"),
#     cs_id = c(1, 1, 1, 1, 1),
#     pt_id = c(1, 2, 3, 4, 5),
#     cs_lengthm = c(CS_LENGTHM),
#     point_type = c('left_bank', 'bottom', 'bottom', 'bottom', 'right_bank'),
#     Z = c(100, 10, 10, 10, 100)
#   )
# 
# classified_pts <-
#   data.frame(
#     hy_id = c("A", "A",  "A", "A", "A"),
#     cs_id = c(1, 1, 1, 1, 1),
#     pt_id = c(1, 2, 3, 4, 5),
#     cs_lengthm = c(CS_LENGTHM),
#     point_type = c('channel', 'bottom', 'bottom', 'bottom', 'right_bank'),
#     Z = c(100, 10, 10, 10, 100)
#   )
# 
# classified_pts <-
#   data.frame(
#     hy_id = c("A", "A",  "A", "A", "A",
#               "B", "B", "B", "B", "B"
#     ),
#     cs_id = c(1, 1, 1, 1, 1,
#               1, 1, 1, 1, 1
#     ),
#     pt_id = c(1, 2, 3, 4, 5,
#               1, 2, 3, 4, 5
#     ),
#     cs_lengthm = c(CS_LENGTHM),
#     point_type = c(
#       'channel', 'bottom', 'bottom', 'bottom', 'right_bank',
#       'left_bank', 'bottom', 'bottom', 'bottom', 'right_bank'
#     ),
#     Z = c(100, 10, 10, 10, 100,
#           100, 10, 10, 10, 100
#     )
#   )

testthat::test_that("'has_relief' - (1 cross section, 1% length to relief, 1 cs_pts, NO RELIEF) - 1 bottom with only a left / right bank and then channel pt on other side", {
  
  ID_COL    <- "hy_id"
  REQ_COLS  <- c(ID_COL, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
  
  PCT_OF_LENGTH_FOR_RELIEF <- 0.01
  CS_LENGTHM               <- 100
  MIN_REQ_RELIEF           <- CS_LENGTHM * PCT_OF_LENGTH_FOR_RELIEF
  
  DETAILED                 <- TRUE
  
  no_left <-
    data.frame(
      hy_id = c("A", "A", "A"),
      cs_id = c(1, 1, 1),
      pt_id = c(1, 2, 3),
      cs_lengthm = c(CS_LENGTHM),
      point_type = c('channel', 'bottom', 'right_bank'),
      Z = c(100, 100, 100)
    )
  
    no_left_result <- get_relief(
                        classified_pts           = no_left, 
                        crosswalk_id             = ID_COL, 
                        pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
                      )
    
    testthat::expect_false(
      all(no_left_result$has_relief)
    )
  
  no_right <-
    data.frame(
      hy_id = c("A", "A", "A"),
      cs_id = c(1, 1, 1),
      pt_id = c(1, 2, 3),
      cs_lengthm = c(CS_LENGTHM),
      point_type = c('left_bank', 'bottom', 'channel'),
      Z = c(100, 100, 100)
    )
  
  no_right_result <-  get_relief(
      classified_pts           = no_right, 
      crosswalk_id             = ID_COL, 
      pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
    )
  
  testthat::expect_false(
    all(no_right_result$has_relief)
  )
  
})

testthat::test_that("'has_relief' - (1 cross section, 1% length to relief, 5 cs_pts, NO RELIEF) - flat CS, ALL bottom points / no banks", {

  
  ID_COL    <- "hy_id"
  REQ_COLS  <- c(ID_COL, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
  
  PCT_OF_LENGTH_FOR_RELIEF <- 0.01
  CS_LENGTHM               <- 100
  MIN_REQ_RELIEF           <- CS_LENGTHM * PCT_OF_LENGTH_FOR_RELIEF
  
  DETAILED                 <- TRUE
  
  all_flat_bottom <-
    data.frame(
      hy_id = c("A", "A",  "A", "A", "A"),
      cs_id = c(1, 1, 1, 1, 1),
      pt_id = c(1, 2, 3, 4, 5),
      cs_lengthm = c(CS_LENGTHM),
      point_type = c('bottom', 'bottom', 'bottom', 'bottom', 'bottom'),
      Z = c(100, 100, 100, 100, 100)
    )
  
  # testthat::expect_error(
  all_flat_bottom_result <- get_relief(
                                classified_pts           = all_flat_bottom, 
                                crosswalk_id             = ID_COL, 
                                pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
                              )
  # )  
  testthat::expect_false(
    all(all_flat_bottom_result$has_relief)
  )
  
})


