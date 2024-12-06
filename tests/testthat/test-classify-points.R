library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)
# 
source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::classify_points() ----
# -------------------------------------------------------------------

testthat::test_that("check 'valid_banks' attribute of CLASSIFIED CS points from DEFAULT transects output", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 10
  
  PCT_OF_LENGTH_FOR_RELIEF <- 0.01
  
  flowlines <-
    flowlines %>% 
    # dplyr::slice(1) %>%
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
  
  # cs_pts <- 
  # cs_pts %>% 
  # dplyr::filter(hy_id == "wb-1003258", cs_id == 1)
  
  classified <- classify_points(
    cs_pts = cs_pts,
    crosswalk_id = ID_COL,
    pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
  ) 
  # classify_points(
  #   cs_pts = cs_pts,
  #   crosswalk_id = ID_COL,
  #   pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
  # ) %>% 
  #      plot_cs_pts(color = 
  #                 "point_type", size = 4)
  # 
  # classified %>% 
  #   plot_cs_pts(color = 
  #                 "point_type", size = 4)
  
  true_valid_banks <- 
    classified %>% 
    dplyr::filter(valid_banks) %>% 
    # dplyr::slice(1:10) %>% 
    dplyr::group_by(hy_id, cs_id)  %>% 
    dplyr::mutate(
      double_check_valid_banks = dplyr::case_when(
        ((left_bank > bottom) & (!is.na(left_bank))) & ((right_bank > bottom) & (!is.na(right_bank))) ~ TRUE,
        # left_bank > bottom & right_bank > bottom ~ TRUE,
        TRUE                                     ~ FALSE
      ),
      correct_valid_banks = valid_banks == double_check_valid_banks
    ) %>% 
    dplyr::relocate(correct_valid_banks, double_check_valid_banks, valid_banks) 
  
  all_valid_banks_are_correct <- all(true_valid_banks$correct_valid_banks)
  testthat::expect_true(all_valid_banks_are_correct)
  
})


# TODO: Left off here on 2024-08-27
# TODO: IDEAL SHAPE ("left_bank", "right_bank", "bottom", "channel" point types) 
#  \              /
#   \            /
#    __       __
#      \     /
#       \   /
#        __
testthat::test_that("'classify_points' - (1 cross section, VALID (relief=T, banks=T)) - left_bank above bottm, flat left channel, flat bottom, flat right channel, and right_bank above bottom", {
  ID_COL <- "hy_id"
  
  cs <-
    data.frame(
      hy_id      = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
      cs_id      = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      pt_id             = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      cs_lengthm        = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
      relative_distance = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      # point_type = c('left_bank', 'left_bank', 'channel', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'right_bank', 'right_bank'),
      Z          = c(9, 7, 5, 5, 2, 2, 5, 5, 7, 9)
    )
  
  # plot(cs$Z~cs$pt_id)
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # classify_points(ideal_pts, crosswalk_id = ID_COL) %>% 
  #   plot_cs_pts(color = "point_type", size = 4)
  
  expected_point_types <- c('left_bank', 'channel', 'channel', 'channel', 
                            'bottom', 'bottom', 
                            'channel', 'channel', 'channel', 'right_bank')
  
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
})

# TODO: Left off here on 2024-08-27
# TODO: IDEAL SHAPE ("left_bank", "right_bank", "bottom", "channel" point types) 
#   \     /
#    \   /
#      _
testthat::test_that("'classify_points' - (1 cross section, 3 pts, VALID (relief=T, banks=T)) - 3 point 'V' shaped cross section", {
  
  ID_COL            <- "hy_id"
  
  # cs <- generate_cs(
  #   num_unique_ids = 1,
  #   num_pts_per_id = 3,
  #   cs_id          = 1,
  #   cs_lengthm     = 100,
  #   num_peaks      = 1,
  #   amplitude      = 2
  # )
  # 
  # plot(cs$Z)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts %>% hydrofabric3D::plot_cs_pts(color = "point_type")
  cs <- data.frame(
    hy_id      = c("A", "A",  "A"),
    cs_id      = c(1, 1, 1),
    pt_id             = c(1, 2, 3),
    cs_lengthm        = c(100, 100, 100),
    relative_distance = c(0.333333, 0.666667, 1.0000000),
    Z = c(4, 1, 4)
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  expected_point_types <- c("left_bank", "bottom", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})

# TODO: Left off here on 2024-08-27
# TODO: IDEAL SHAPE ("left_bank", "right_bank", "bottom", "channel" point types) 
#   \     /
#    \   /
#      _
testthat::test_that("'classify_points' - (1 cross section, 5 pts, 'V' shape, VALID (relief=T, banks=T)) - 1 LB -> 1 CHAN -> 1 BOTTOM -> 1 CHAN -> 1 RB", {
  
  ID_COL            <- "hy_id"
  
  cs <- data.frame(
    hy_id      = c("A", "A",  "A", "A", "A"),
    cs_id      = c(1, 1, 1, 1, 1),
    pt_id             = c(1, 2, 3, 4, 5),
    cs_lengthm        = c(100, 100, 100, 100, 100),
    relative_distance = c(0.2, 0.4, 0.6, 0.8, 1.0),
    # relative_distance = c(0.333333, 0.666667, 1.0000000),
    Z = c(4, 3, 1, 3, 4)
  )
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "bottom", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 4)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 4)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})

# TODO: IDEAL SHAPE ("left_bank", "right_bank", "bottom", "channel" point types) 
#   \      /
#    \    /
#      __
testthat::test_that("'classify_points' - (1 cross section, 6 pts, 'V' shape, VALID (relief=T, banks=T)) - 1 LB -> 1 CHAN -> 2 BOTTOM -> 1 CHAN -> 1 RB", {
  
  ID_COL            <- "hy_id"
  NUM_CS_PTS   <- 6
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(4, 3, 1, 1, 3, 4) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # cpts$point_type
  # cpts$Z %>% plot()
  # Z_VALS %>% plot()
  
  expected_point_types <- c("left_bank", "channel", "bottom", "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 4)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 4)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})


# TODO: IDEAL SHAPE ("left_bank", "right_bank", "bottom", "channel" point types) 
#   \       /
#    \     /
#      ___
testthat::test_that("'classify_points' - (1 cross section, 7 pts, 'V' shape, VALID (relief=T, banks=T)) - 1 LB -> 1 CHAN -> 3 BOTTOM -> 1 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 7
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(4, 3, 1, 1, 1, 3, 4) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # cpts$point_type
  # cpts$Z %>% plot()
  # Z_VALS %>% plot()
  
  expected_point_types <- c("left_bank", "channel", "bottom", "bottom", "bottom", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 4)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 4)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})

# TODO: IDEAL SHAPE ("left_bank", "right_bank", "bottom", "channel" point types) 
#   \       /
#    \     /
#      ___
testthat::test_that("'classify_points' - (1 cross section, 8 pts, 'V' shape, VALID (relief=T, banks=T)) - 1 LB -> 1 CHAN -> 4 BOTTOM -> 1 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 8
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(4, 3, 1, 1, 1, 1, 3, 4) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)

  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "bottom", "bottom", "bottom", "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 4)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 4)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})


# generate_cs <- function(num_unique_ids = 1, 
#                                num_pts_per_id = 3, 
#                                cs_id = 1, 
#                                cs_lengthm = 100,
#                                num_peaks = 1, 
#                                amplitude = 1
#                                ) {
#   unique_ids <- LETTERS[1:num_unique_ids]
#   rel_dist_interval <- (cs_lengthm / num_pts_per_id) / 100
#   rel_dist <- cumsum(rep(rel_dist_interval, num_pts_per_id))
#   
#   # Generate Z values based on sine wave to simulate peaks and troughs, shifted upwards to be > 0
#   x <- seq(0, 2 * pi * num_peaks, length.out = num_pts_per_id)
#   z_values <- amplitude * sin(x) + amplitude
#   
#   cs <- data.frame(
#     hy_id = rep(unique_ids, each = num_pts_per_id),
#     cs_id = rep(cs_id, num_unique_ids * num_pts_per_id),
#     pt_id = rep(1:num_pts_per_id, times = num_unique_ids),
#     cs_lengthm = rep(cs_lengthm, num_unique_ids * num_pts_per_id),
#     relative_distance = rel_dist,
#     Z = rep(z_values, times = num_unique_ids)
#   )
#   return(cs)
# }
# 
# classify_z_pts <- function(Z) {
#   slope          <- diff(c(0, Z))  
#   bottomZ        <- min(Z)      
#   points_per_id  <- length(Z)
#   
#   classification <- sapply(1:points_per_id, function(i) {
#     message(i)
#     if (i == 1) {
#       message("LB\n")
#       return("left_bank")   # handle first point
#     } else if (i == points_per_id) {
#       message("RB\n")
#       return("right_bank")  # and the last point
#     } else if (Z[i] == bottomZ) {
#       message("BOTTOM\n")
#       return("bottom")      # identify the bottom point(s) (min Z value)
#     } else if (slope[i] > 0) {
#       if (slope[i + 1] <= 0) {
#         message("CHAN\n")
#         return("channel")     #  slope changing from rising to falling (approaching bottom)
#       } else {
#         message("RB\n")
#         return("right_bank")  # rising slope but not near the bottom
#       }
#     } else if (slope[i] < 0) {
#       if (slope[i - 1] >= 0 || slope[i + 1] == 0) {
#         message("CHAN\n")
#         return("channel")   # slope changing from falling to rising (leaving bottom)
#       } else {
#       message("LB\n")
#       return("left_bank")  #  slope  falling but not near the bottom
#       }
#     } else if(slope[i] == 0) {
#       message("SLOPE is 0 --> CHAN\n")
#       return("channel")
#     }
#     
#     message("NO MATCH\n")
#     
#   })
#   
#   return(classification)
# }



# TODO: IDEAL SHAPE ("left_bank", "right_bank", "bottom", "channel" point types) 
#   \       /
#    \     /
#      ___
testthat::test_that("'classify_points' - (1 cross section, 9 pts, 'V' shape, VALID (relief=T, banks=T)) - 1 LB -> 1 CHAN -> 5 BOTTOM -> 1 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 9
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(4, 3, 1, 1, 1, 1, 1, 3, 4) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "bottom", "bottom", "bottom", "channel", "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 4)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 4)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})


# TODO: IDEAL SHAPE ("left_bank", "right_bank", "bottom", "channel" point types) 
# NOTE: Cross section shape:
#   \        /
#    \      /
#      ____
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'V' shape, VALID (relief=T, banks=T)) - 1 LB -> 1 CHAN -> 6 BOTTOM -> 1 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(4, 3, 1, 1, 1, 1, 1, 1, 3, 4) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", "bottom", "bottom", "bottom", "bottom", "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 4)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 4)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})

# TODO: Really wide IDEAL / VALID (relief=T, banks=T) shape with single bank points
# NOTE: Cross section shape:
#   \_______________/
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'WIDE U' shape, VALID (relief=T, banks=T)) - 1 LB -> 2 CHAN -> 4 BOTTOM -> 2 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(3, 1, 1, 1, 1, 1, 1, 1, 1, 3) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", 
                            "bottom", "bottom", "bottom", "bottom", 
                            "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 3)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 3)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})

# TODO: Really wide IDEAL / VALID (relief=T, banks=T) shape with single LEFT BANK point and higher RIGHT BANK
# NOTE: Cross section shape:
#                       /
#                     /
#   \_______________/
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'WIDE U, HIGH RB' shape, VALID (relief=T, banks=T)) - 1 LB -> 2 CHAN -> 4 BOTTOM -> 2 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(3, 1, 1, 1, 1, 1, 1, 2, 3, 5) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", 
                            "bottom", "bottom", "bottom", "bottom", 
                            "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 3)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 5)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})

# TODO: Really wide IDEAL / VALID (relief=T, banks=T) shape with single RIGHT BANK points and higher LEFT BANK
# NOTE: Cross section shape:
#  \                     
#   \                  
#    \_______________/
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'WIDE U, HIGH LB' shape, VALID (relief=T, banks=T)) - 1 LB -> 2 CHAN -> 4 BOTTOM -> 2 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(5, 3, 2, 1, 1, 1, 1, 1, 1, 3) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", 
                            "bottom", "bottom", "bottom", "bottom", 
                            "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 5)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 3)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})

# TODO: IDEAL SHAPE with LIP ("left_bank", "right_bank", "bottom", "channel" point types) 
# NOTE: Cross section shape:
# __           __
#   \        /
#    \      /
#      ____
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'V + lips' shape, VALID (relief=T, banks=T)) - 2 LB -> 2 CHAN -> 2 BOTTOM -> 3 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  
  # cs <- make_cs(
  #   left_bank = c(4, 4, 2),
  #   left_channel = c(4, 1, 3),
  #   bottom = c(1, 1, 4),
  #   right_bank = c(1, 4, 3)
  # ) 
  # 
  # cs[[ID_COL]]   <- "A"
  # cs$cs_id       <- 1
  # cs$cs_lengthm  <- 100
  # cs$Z %>% plot()
  # make_cs_curve(left_y_range = c()
  
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(4, 4, 4, 3, 1, 1, 3, 4, 4, 4)
  Z_VALS %>% plot()
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "left_bank", "channel", "channel", 
                            "bottom", "bottom", "channel", 
                            "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 4)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 4)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})

# TODO: IDEAL SHAPE with a break in BOTH channels, where it flattens out
# NOTE: Cross section shape:
# \                   /
#   --             --
#      \         /
#       \      /
#         ____
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'V + breaks in both channel' shape, VALID (relief=T, banks=T)) - 1 LB -> 3 CHAN -> 2 BOTTOM -> 3 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(6, 4, 4, 3, 1, 1, 3, 4, 4, 6)
  # Z_VALS %>% plot()
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", "channel", 
                            "bottom", "bottom", "channel", 
                            "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 6)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 6)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})

# TODO: IDEAL SHAPE with a break in only LEFT side of channel, where it flattens out for a bit
# NOTE: Cross section shape:
# \                  /
#   --             /
#      \         /
#       \      /
#         ____
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'V + breaks in LEFT side of channel' shape, VALID (relief=T, banks=T)) - 1 LB -> 1 CHAN -> 6 BOTTOM -> 1 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(6, 4, 4, 3, 1, 1, 3, 4, 5, 6)
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", "channel", 
                            "bottom", "bottom", "channel", 
                            "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 6)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 6)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})
# TODO: IDEAL SHAPE with a break in only RIGHT side of channel, where it flattens out for a bit
# NOTE: Cross section shape:
#    \                  /
#     \             --
#      \         /
#       \      /
#         ____
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'V + breaks in RIGHT side of channel' shape, VALID (relief=T, banks=T)) - 1 LB -> 1 CHAN -> 6 BOTTOM -> 1 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(6, 5, 4, 3, 1, 1, 3, 4, 4, 6)
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", "channel", 
                            "bottom", "bottom", "channel", 
                            "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 6)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 6)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
})

# TODO: There is a valid bottom here but algorithm struggles to find it because 
# TODO: the minimum point that is the actual bottom is NOT in the middle third of the cross section
# NOTE: Cross section shape:
#                           -----
#                        /
#                      /
#                  ---
#                / 
#              /
#   \        /
#    \     /
#      ---
testthat::test_that("'classify_points' - (1 cross section, 16 pts, 'V on left side' shape, VALID (relief=T, banks=T)) - 1 LB -> 1 CHAN -> 6 BOTTOM -> 1 CHAN -> 1 RB", {
  
  ID_COL       <- "hy_id"
  
  # cs <- make_cs(
  #   left_bank = c(4, 4, 2),
  #   left_channel = c(4, 1, 3),
  #   bottom = c(1, 1, 4),
  #   right_bank = c(1, 4, 3)
  # ) 
  # 
  # cs[[ID_COL]]   <- "A"
  # cs$cs_id       <- 1
  # cs$cs_lengthm  <- 100
  # cs$Z %>% plot()
  # make_cs_curve(left_y_range = c()
  
  NUM_CS_PTS   <- 16
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(6, 5, 3, 2, 3, 4, 5, 6, 7, 8, 8, 8, 9, 9, 9, 9)
  
  # Z_VALS %>% plot()
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts_rev <- classify_points(cs_rev, crosswalk_id = ID_COL)
  
  # cpts$Z %>% plot()
  # cpts$point_type
  
  # Z_VALS %>% plot()
  # cpts_rev$Z %>% plot()
  # cpts_rev$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", "channel", "channel",
                            "bottom", 
                            "channel", "channel",  "channel", "channel",  
                            "channel", "channel", "channel", "channel", "right_bank", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 6)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 9)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 4)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts$valid_banks)
  )
  
  # ---- Reverse the cross section shape ----
  cs_rev <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = rev(Z_VALS) 
  )
  cpts_rev <- classify_points(cs_rev, crosswalk_id = ID_COL)
  expected_point_types_rev <- c("left_bank", "left_bank", "left_bank", "channel", "channel",
                                "channel", "channel",  "channel", "channel",  
                                "channel", "bottom", "channel", "channel", "channel", "channel", "right_bank")
  # correct point types
  testthat::expect_true(
    all(cpts_rev$point_type == expected_point_types_rev)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts_rev$left_bank == 9)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts_rev$right_bank == 6)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts_rev$bottom == 4)
  )
  
  # correct has relief
  testthat::expect_true(
    all(cpts_rev$has_relief)
  )
  
  # correct valid_banks
  testthat::expect_true(
    all(cpts_rev$valid_banks)
  )
  
})

# NOTE: Cross section shape:
#                   /
#                 /
#               /
#         _____
#       /
#     /
#   /
testthat::test_that("'classify_points' - (1 cross section, 10 pts, INVALID (relief=T, banks=F)) - left_bank below bottom, bottom is flat, and right bank is above bottom", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(1, 2, 3, 4, 4, 4, 4, 5, 6, 7) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "left_bank", "channel", 
                            "bottom", "bottom", "bottom", "bottom", 
                            "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 2)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 7)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 4)
  )
  
  # correct has relief (TRUE)
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks (FALSE)
  testthat::expect_false(
    all(cpts$valid_banks)
  )
  
})
# NOTE: Cross section shape:
#     \        
#      \       
#       \     
#        \_____
#              \
#               \
#                \
testthat::test_that("'classify_points' - (1 cross section, 10 pts, INVALID (relief=T, banks=F)) - left bank ABOVE bottom, bottom is FLAT, and right bank is BELOW bottom", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(7, 6, 5, 4, 3, 3, 3, 3, 2, 1) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", "channel",
                            "bottom", "bottom", "bottom", "channel", 
                            "right_bank", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 7)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 2)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 3)
  )
  
  # correct has relief (TRUE)
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks (FALSE)
  testthat::expect_false(
    all(cpts$valid_banks)
  )
  
})

# NOTE: Cross section shape:
#              /
#            /
#          /
#        /
#      /
#    /
#  /
testthat::test_that("'classify_points' - (1 cross section, 10 pts, INVALID (relief=T, banks=F)) - positive slope diagonal", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) 
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", 
                            "bottom", 
                            "channel", "channel", "channel", 
                            "channel", "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 1)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 10)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 4)
  )
  
  # correct has relief (TRUE)
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks (FALSE)
  testthat::expect_false(
    all(cpts$valid_banks)
  )
  
})
# NOTE: Cross section shape:
# \
#  \
#   \
#    \
#     \
#      \
testthat::test_that("'classify_points' - (1 cross section, 10 pts, INVALID (relief=T, banks=F)) - negative slope diagonal", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- NUM_CS_PTS:1
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", "channel", "channel", "channel",
                            "bottom", 
                            "channel",  "right_bank", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 10)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 2)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 4)
  )
  
  # correct has relief (TRUE)
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks (FALSE)
  testthat::expect_false(
    all(cpts$valid_banks)
  )
  
})

# TODO: FLAT cross section
# NOTE: Cross section shape:
# --------------------
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'flat line', INVALID (relief=F, banks=F)) - flat line", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- rep(1, NUM_CS_PTS)
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "left_bank", "channel", "bottom", "bottom", "bottom",
                            "bottom", 
                            "channel",  "right_bank", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 1)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 1)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief (FALSE)
  testthat::expect_false(
    all(cpts$has_relief)
  )
  
  # correct valid_banks (FALSE)
  testthat::expect_false(
    all(cpts$valid_banks)
  )
  
})

# TODO: FLAT cross section w/ single RIGHT BANK
# NOTE: Cross section shape:
#                      /
# --------------------
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'flat line w/ RIGHT BANK', INVALID (relief=T, banks=F)) - flat line", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(rep(1, NUM_CS_PTS - 1), 2)
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "left_bank", "channel", "bottom", "bottom", "bottom",
                            "bottom", 
                            "channel",  "channel", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 1)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 2)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief (FALSE)
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks (FALSE)
  testthat::expect_false(
    all(cpts$valid_banks)
  )
  
})

# TODO: FLAT cross section w/ single LEFT BANK
# NOTE: Cross section shape:
# \                    
#  --------------------
testthat::test_that("'classify_points' - (1 cross section, 10 pts, 'flat line w/ LEFT BANK', INVALID (relief=T, banks=F)) - flat line", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(2, rep(1, NUM_CS_PTS - 1))
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  expected_point_types <- c("left_bank", "channel", "channel", "bottom", "bottom", "bottom",
                            "bottom", 
                            "channel",  "right_bank", "right_bank")
  
  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )
  
  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 2)
  )
  
  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 1)
  )
  
  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 1)
  )
  
  # correct has relief (FALSE)
  testthat::expect_true(
    all(cpts$has_relief)
  )
  
  # correct valid_banks (FALSE)
  testthat::expect_false(
    all(cpts$valid_banks)
  )
  
})

# NOTE: Cross section shape:
# NOTE: this is from a real cross section (hy_id = "wb-1002006", cs_id = 3, 3DEP DEM)
#
#
#.       / \
#      /    \                 .---__---
#    /       \               |         \
#  /          \             /
#              \          /
#               \       /
#                ------
testthat::test_that("'classify_points' - (1 cross section with 29 points that goes from a low point, increases to the highest point on the left bank, drops down to a flat bottom at and the rises to a right bank lower than the left bank, make sure set_channel_surrounded_by_bottom function is correctly fixing erroneous channel point at the flat bottom", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 29
  CS_LENGTH    <- 865.4798
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(250.928558349609, 251.2162588614, 251.577647456416, 252.052094071, 252.643624199761, 253.430354365596, 
                    254.417578238028, 255.110582704897, 254.47501401548, 251.575227525499, 246.858016967773, 
                    246.858016967773, 246.858017532914, 246.858016967773, 246.858016967773, 246.858016967773, 
                    247.850519816081, 249.110019259983, 250.502717194734, 251.697868064598, 252.414806789822, 
                    252.720396253798, 252.74156358507, 252.66369233308, 252.597536440249, 252.62567364728, 
                    252.686952944155, 252.686824657299, 252.593170166016)
  
  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )
  # plot(cs$Z)
  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # Z_VALS %>% plot()
  # cpts$Z %>% plot()
  # cpts$point_type
  
  # hydrofabric3D::plot_cs_pts(cpts, crosswalk_id = ID_COL, size = 4, color = "point_type")
  # paste0(paste0("'", cpts$point_type, "'"), collapse = ", ")

  expected_point_types <- c(
                    'left_bank', 'left_bank', 'left_bank', 'left_bank', 'left_bank', 
                    'left_bank', 'left_bank', 'left_bank', 
                    'channel', 'channel', 
                    'bottom', 'bottom', 'bottom', 'bottom', 'bottom', 'bottom', 
                    'channel', 'channel', 'channel', 'channel', 'channel', 'channel', 
                    'right_bank', 'right_bank', 'right_bank', 'right_bank', 
                    'right_bank', 'right_bank', 'right_bank'
                    )
  

  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )

  # test that left bank is greater than bottom
  testthat::expect_true(
    all(cpts$left_bank >= cpts$bottom)
  )

  # test that right bank is greater than bottom
  testthat::expect_true(
    all(cpts$right_bank >= cpts$bottom)
  )

  # test that left bank is greater than right bank (in this specific test this is true)
  testthat::expect_true(
    all(cpts$left_bank >= cpts$right_bank)
  )

  # correct has relief (FALSE)
  testthat::expect_false(
    all(cpts$has_relief)
  )

  # correct valid_banks (FALSE)
  testthat::expect_true(
    all(cpts$valid_banks)
  )

})

# NOTE: Cross section shape:
# 
#  \                  /
#   \             --
#    \         /
#     \      /
#       ____
testthat::test_that("'classify_points' - decreasing left bank to a flat bottom then increasing right channel flat area then increasing right bank again", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(9, 8, 7, 5, 2, 2, 5, 5, 7, 9)

  cs <- data.frame(
    hy_id             = rep("A", NUM_CS_PTS),
    cs_id             = rep(1, NUM_CS_PTS),
    pt_id             = 1:NUM_CS_PTS,
    cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
    relative_distance = REL_DIST,
    Z                 = Z_VALS
  )

  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # cpts$Z %>% plot()
  # Z_VALS %>% plot()

  expected_point_types <- c('left_bank', 'channel', 'channel', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'channel', 'right_bank')

  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )

  # left bank greater than bottom
  testthat::expect_true(
    all(cpts$left_bank >= cpts$bottom)
  )

  # right bank greater than bottom
  testthat::expect_true(
    all(cpts$right_bank >= cpts$bottom)
  )

  # correct left bank value
  testthat::expect_true(
    all(cpts$left_bank == 9)
  )

  # correct right bank value
  testthat::expect_true(
    all(cpts$right_bank == 9)
  )

  # correct bottom value
  testthat::expect_true(
    all(cpts$bottom == 2)
  )

  # correct has relief (TRUE)
  testthat::expect_true(
    all(cpts$has_relief)
  )

  # correct valid_banks (TRUE)
  testthat::expect_true(
    all(cpts$valid_banks)
  )
   
})

# NOTE: Cross section shape:
# NOTE: this test will test a U-shaped cross section with different pct_of_length_for_relief values to check "has_relief" column
#
#  \                  /
#   \               /
#    \            /
#     \         /
#       _______
testthat::test_that("'classify_points' - U-shaped cross section with varying lengths will have a TRUE 'has_relief' value while the maximum amount of relief is greater than or equal to (cs length * pct_of_length_for_relief (given variable))", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(9, 8, 7, 5, 2, 2, 5, 5, 7, 9)
  AMOUNT_OF_RELIEF <- max(Z_VALS) - min(Z_VALS)

  CS_LENGTHS   <- 698:702

  # try a new CS_LENGTH and relative distance for all lengths in CS_LENGTHS, if amount of relief is greater than 0.01 then has relief is TRUE
  for (i in 1:length(CS_LENGTHS)) {
    message(paste0("Testing CS_LENGTH: ", CS_LENGTHS[i]))
    # i = 4
      # CS_LENGTH    <- 701
      REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)] 

      cs <- data.frame(
        hy_id             = rep("A", NUM_CS_PTS),
        cs_id             = rep(1, NUM_CS_PTS),
        pt_id             = 1:NUM_CS_PTS,
        cs_lengthm        = rep(CS_LENGTHS[i], NUM_CS_PTS),
        relative_distance = REL_DIST,
        Z                 = Z_VALS
      )

    cpts <- classify_points(cs, crosswalk_id = ID_COL)

    # AMOUNT_OF_RELIEF >= CS_LENGTHS[i] * 0.01 
    # 1000*0.01
    
    # correct has relief (TRUE)
    if (AMOUNT_OF_RELIEF >= CS_LENGTHS[i] * 0.01)  {
      testthat::expect_true(
        all(cpts$has_relief)
      )
    } else {
      testthat::expect_false(
        all(cpts$has_relief)
      )
    }

    # correct valid_banks (TRUE)
    testthat::expect_true(
      all(cpts$valid_banks)
    )
  }
})

testthat::test_that("'classify_points' - L-shaped cross section with varying lengths will have a TRUE 'has_relief' value while the maximum amount of relief is greater than or equal to (cs length * pct_of_length_for_relief (given variable))", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(9, 8, 7, 5, 3, 2, 1, 1, 1, 1)
  AMOUNT_OF_RELIEF <- max(Z_VALS) - min(Z_VALS)

  CS_LENGTHS   <- 798:802

  # try a new CS_LENGTH and relative distance for all lengths in CS_LENGTHS, if amount of relief is greater than 0.01 then has relief is TRUE
  for (i in 1:length(CS_LENGTHS)) {
    message(paste0("Testing CS_LENGTH: ", CS_LENGTHS[i]))
      # CS_LENGTH    <- 701
      REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)] 

      cs <- data.frame(
        hy_id             = rep("A", NUM_CS_PTS),
        cs_id             = rep(1, NUM_CS_PTS),
        pt_id             = 1:NUM_CS_PTS,
        cs_lengthm        = rep(CS_LENGTHS[i], NUM_CS_PTS),
        relative_distance = REL_DIST,
        Z                 = Z_VALS
      )

    cpts <- classify_points(cs, crosswalk_id = ID_COL)
    
    # cpts$Z %>% plot()
    
    # AMOUNT_OF_RELIEF >= CS_LENGTHS[i] * 0.01 
    
    # correct has relief (TRUE)
    if (AMOUNT_OF_RELIEF >= CS_LENGTHS[i] * 0.01)  {
      testthat::expect_true(
        all(cpts$has_relief)
      )
    } else {
      testthat::expect_false(
        all(cpts$has_relief)
      )
    }

    # correct valid_banks (TRUE)
    testthat::expect_false(
      all(cpts$valid_banks)
    )
  }


})

# Test an mountain-shaped cross section
# NOTE: Cross section shape:
#       ---         
#      |   |       
#     /     \     
#    |       |__
testthat::test_that("'classify_points' - check point classifications and validity for mountain-shaped cross section", {
  ID_COL       <- "hy_id"
  NUM_CS_PTS   <- 10
  CS_LENGTH    <- 100
  REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  Z_VALS       <- c(1, 2, 3, 4, 5, 5, 5, 4, 3, 1)

    cs <- data.frame(
      hy_id             = rep("A", NUM_CS_PTS),
      cs_id             = rep(1, NUM_CS_PTS),
      pt_id             = 1:NUM_CS_PTS,
      cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
      relative_distance = REL_DIST,
      Z                 = Z_VALS
    )

  cpts <- classify_points(cs, crosswalk_id = ID_COL)
  
  # cpts$Z %>% plot()
  # hydrofabric3D::plot_cs_pts(cpts, crosswalk_id = ID_COL, size = 4, color = "point_type")
  # paste0(paste0("'", cpts$point_type, "'"), collapse = ", ")

  expected_point_types <- c('left_bank', 'channel', 'channel', 'bottom', 'channel', 'channel', 'channel', 'channel', 'right_bank', 'right_bank')

  # correct point types
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
  )

  # does NOT have relief
  testthat::expect_false(
    all(cpts$has_relief)
  )

  # does NOT have valid banks
  testthat::expect_false(
    all(cpts$valid_banks)
  )

})

# --------------------------------------------------------------------------------
# ---- old-ish tests below ----
# probably been deprecated by above set of tests
# --------------------------------------------------------------------------------
testthat::test_that("'classify_points' - make sure 2 valid cross sections in one dataframe are correctly classified", {
  
  # testthat::test_that("'classify_points' - make sure 2 valid cross sections in one dataframe are correctly classified", {
  ID_COL <- "hy_id"
  
  ideal_pts1 <-
    data.frame(
      hy_id      = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
      cs_id      = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      pt_id             = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      cs_lengthm        = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
      relative_distance = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      # point_type = c('left_bank', 'left_bank', 'channel', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'right_bank', 'right_bank'),
      Z          = c(9, 7, 5, 5, 2, 2, 5, 5, 7, 9)
    )
  
  # classify_z_pts(ideal_pts1$Z, 10)
  # hydrofabric3D::classify_points(ideal_pts1, crosswalk_id = "hy_id")$point_type
  
  # Identical cross section as above with different "hy_id"
  ideal_pts2 <-
    data.frame(
      hy_id      = c("B", "B", "B", "B", "B", "B", "B", "B", "B", "B"),
      cs_id      = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      pt_id             = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      cs_lengthm        = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
      relative_distance = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      # point_type = c('left_bank', 'left_bank', 'channel', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'right_bank', 'right_bank'),
      Z          = c(9, 7, 5, 5, 2, 2, 5, 5, 7, 9)
    )
  
  two_cs <- 
    dplyr::bind_rows(
      ideal_pts1,
      ideal_pts2
    )
  
  # plot(two_cs$Z~two_cs$pt_id)
  
  cpts <- classify_points(two_cs, crosswalk_id = ID_COL)
  
  expected_point_types <- c('left_bank', 'channel', 'channel', 'channel', 
                            'bottom', 'bottom', 
                            'channel', 'channel', 'channel', 'right_bank')
  
  # make sure both hy_ids got correct classification
  testthat::expect_true(
    all(dplyr::filter(cpts, hy_id == "A")$point_type == expected_point_types)
  )
  
  testthat::expect_true(
    all(dplyr::filter(cpts, hy_id == "B")$point_type == expected_point_types)
  )
  
  # make sure both HY IDs have relief
  testthat::expect_true(
    all(dplyr::filter(cpts, hy_id == "A")$has_relief)
  )
  
  testthat::expect_true(
    all(dplyr::filter(cpts, hy_id == "B")$has_relief)
  )
  
  # make sure both HY IDs have valid banks
  testthat::expect_true(
    all(dplyr::filter(cpts, hy_id == "A")$valid_banks)
  )
  
  testthat::expect_true(
    all(dplyr::filter(cpts, hy_id == "B")$valid_banks)
  )
  
  # make sure all unique tmp_ids were preserved 
  testthat::expect_true(
    all(
      hydrofabric3D::get_unique_tmp_ids(cpts) %in% hydrofabric3D::get_unique_tmp_ids(two_cs) 
    )
  )
  
  
  
})


# TODO:
testthat::test_that("'classify_points()' has correct output columns, minimum required inputs", {
  
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
  # cs_pts  %>% 
  # # dplyr::select(hy_id, cs_id, pt_id, Z, relative_distance)  %>% 
  # sf::st_drop_geometry() %>%
  #  head(20)
  
  classified <- classify_points(
  # classified <- classify_points(
    cs_pts = cs_pts,
    crosswalk_id = ID_COL,
    pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
  ) 
  
  # check if minimum required output columns 
  testthat::expect_true(
    classified_cs_pts_has_min_output_cols(classified_pts = classified, crosswalk_id = ID_COL)
    )
  
  # check that if NO 'id' is specified, then the default output columns will NOT match the classified points that DID have a specified 'id' 
  testthat::expect_false(
    classified_cs_pts_has_min_output_cols(classified_pts = classified )
    )
  
  # test same number of input points are in classified output points 
  same_num_pts_after_classifying <- nrow(cs_pts)  == nrow(classified)
  testthat::expect_true(same_num_pts_after_classifying)
 
  # make sure all the unique tmp_ids (id + cs_id) are the same in the input AND output 
  same_unique_tmp_ids <- has_same_unique_tmp_ids(x = cs_pts, y = classified, crosswalk_id = ID_COL)
  testthat::expect_true(same_unique_tmp_ids)
  
})
  
  
# TODO:
testthat::test_that("check 'valid_banks' attribute of CLASSIFIED CS points from DEFAULT transects output", {
  
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
  
  flowlines <-
    flowlines %>% 
    # dplyr::slice(1) %>%
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
  
  classified <- classify_points(
  # classified <- classify_points(
    cs_pts = cs_pts,
    crosswalk_id = ID_COL,
    pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
  ) 
  
  true_valid_banks <- 
    classified %>% 
    dplyr::filter(valid_banks) %>% 
    # dplyr::slice(1:10) %>% 
    dplyr::group_by(hy_id, cs_id)  %>% 
    dplyr::mutate(
      double_check_valid_banks = dplyr::case_when(
        ((left_bank > bottom) & (!is.na(left_bank))) & ((right_bank > bottom) & (!is.na(right_bank))) ~ TRUE,
        # left_bank > bottom & right_bank > bottom ~ TRUE,
        TRUE                                     ~ FALSE
      ),
      correct_valid_banks = valid_banks == double_check_valid_banks
    ) %>% 
    dplyr::relocate(correct_valid_banks, double_check_valid_banks, valid_banks) 
  
  all_valid_banks_are_correct <- all(true_valid_banks$correct_valid_banks)
  testthat::expect_true(all_valid_banks_are_correct)
  
})


# TODO: Left off here on 2024-08-27
# TODO: IDEAL SHAPE ("left_bank", "right_bank", "bottom", "channel" point types) 
#  \              /
#   \            /
#    __       __
#      \     /
#       \   /
#        __
testthat::test_that("'classify_points' - (1 cross section, VALID) - left_bank above bottm, flat left channel, flat bottom, flat right channel, and right_bank above bottom", {
  ID_COL <- "hy_id"
  
  
  ideal_pts <-
    data.frame(
      hy_id      = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
      cs_id      = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      pt_id             = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      cs_lengthm        = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
      relative_distance = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      # point_type = c('left_bank', 'left_bank', 'channel', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'right_bank', 'right_bank'),
      Z          = c(9, 7, 5, 5, 2, 2, 5, 5, 7, 9)
    )
  
  # plot(ideal_pts$Z~ideal_pts$pt_id)

  cpts <- classify_points(ideal_pts, crosswalk_id = ID_COL)
  
  expected_point_types <- c('left_bank', 'channel', 'channel', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'channel', 'right_bank')
  
  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
    )
})

  
# TODO:
testthat::test_that("old test for testing out new bottom_finder.R methods", {
  
  # flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  #   # dplyr::slice(10)
  # # flowlines    <- dplyr::slice(flowlines, 1)
  # 
  # MIN_BF_WIDTH       <- 50
  # ID_COL             <- "hy_id"
  # NUM_OF_TRANSECTS   <- 3
  # 
  # # Cross section point inputs
  # DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  # POINTS_PER_CS     <- NULL
  # MIN_PTS_PER_CS    <- 10
  # 
  # PCT_OF_LENGTH_FOR_RELIEF <- 0.01
  # 
  # cs_pts <-
  #   flowlines %>%
  #   dplyr::rename(!!sym(ID_COL) := id) %>%
  #   dplyr::select(
  #     dplyr::any_of(ID_COL)
  #   ) %>%
  #   hydrofabric3D::cut_cross_sections(
  #     net = .,
  #     crosswalk_id  = ID_COL,
  #     num = 3
  #   ) %>%
  #   dplyr::select(
  #     dplyr::any_of(ID_COL),
  #     cs_lengthm,
  #     cs_id
  #   ) %>%
  #   hydrofabric3D::cross_section_pts(
  #     transects              = .,
  #     crosswalk_id    = ID_COL,
  #     points_per_cs   = 30,
  #     min_pts_per_cs  = 10,
  #     dem             = DEM_PATH
  #   )
  # 
  # cs <-
  #   data.frame(
  #     hy_id      = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
  #     cs_id      = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  #     pt_id             = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  #     cs_lengthm        = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
  #     relative_distance = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  #     # point_type = c('left_bank', 'left_bank', 'channel', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'right_bank', 'right_bank'),
  #     Z          = c(5, 8, 10, 5, 2, 2, 4, 6, 7, 8,  9, 9, 10, 11, 10, 12, 14,13, 12, 8)
  #   )

  #   length( 
  #     c(5, 8, 10, 5, 2, 2, 4, 6, 7, 8, 8,  9, 9, 10, 11, 10, 12, 13, 14,13, 12, 11, 10, 9)
  #           )
  #   
  #   plot(      c(5, 8, 10, 5, 2, 2, 4, 6, 7, 8, 8,  9, 9, 10, 11, 10, 12, 13, 14,13, 12, 11, 10, 9))
  #   plot(cs$Z)
  #   
  #   # ID_COL       <- "hy_id"
  #   # NUM_CS_PTS   <- 26
  #   # CS_LENGTH    <- 100
  #   # REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  #   # Z_VALS       <- c(5, 8, 10, 5, 2, 2, 4, 6, 7, 8, 8,  9, 9, 10, 11, 10, 12, 12, 13, 14,13, 12, 11, 10, 11, 9)
  #   
  #   ID_COL       <- "hy_id"
  #   # NUM_CS_PTS   <- 35
  #   # CS_LENGTH    <- 100
  #   # REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  #   Z_VALS       <- c(5, 8, 10, 5, 3, 2, 1, 4, 6, 8, 9, 10, 12, 11, 12, 13, 13,  13, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
  #                    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 12, 12, 12, 11, 11, 11, 10, 10, 10, 10, 6, 10, 10, 15, 16, 17, 17, 17)
  #   
  #   Z_VALS       <- c(5, 8, 10, 5, 3, 2, 1, 4, 6, 8, 9, 10, 12, 11, 12, 13, 13,  13, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
  #                     15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 12, 12, 12, 11, 11, 11, 10, 10, 10, 10, 6, 6, 6, 6, 6, 6, 10, 10, 15, 16, 17, 17, 17)
  #   
  #   NUM_CS_PTS   <- length(Z_VALS)
  #   CS_LENGTH    <- 100
  #   REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  #   # length(Z_VALS)
  #   
  #   cs_pts <- data.frame(
  #     hy_id             = rep("A", NUM_CS_PTS),
  #     cs_id             = rep(1, NUM_CS_PTS),
  #     pt_id             = 1:NUM_CS_PTS,
  #     cs_lengthm        = rep(CS_LENGTH, NUM_CS_PTS),
  #     relative_distance = REL_DIST,
  #     Z                 = Z_VALS
  #   )
  #   plot(cs_pts$Z)
  #   
  #   
  #   
  #   classified_pts <-     
  #     cs_pts %>% 
  #     hydrofabric3D::classify_points(crosswalk_id = ID_COL)
  #   
  #   classified_pts %>% 
  #     hydrofabric3D::plot_cs_pts(
  #       crosswalk_id = "hy_id",
  #       color = "point_type",
  #       size = 4
  #     )
  #   
  #   # cs_pts %>% 
  #   # hydrofabric3D::classify_points(crosswalk_id = ID_COL) %>% 
  #   # hydrofabric3D::plot_cs_pts(
  #   #   crosswalk_id = "hy_id",
  #   #   color = "point_type",
  #   #   size = 4
  #   # )
  #   crosswalk_id = "hy_id"
  #   
  #   # # remove any columns that already exist
  #   cs_pts <- dplyr::select(cs_pts, 
  #                           !dplyr::any_of(c("class", "point_type", "bottom", "left_bank", 
  #                                            "right_bank", "valid_banks", "has_relief"))
  #   )
  #   
  #   # required cols that will be selected from the classified_pts object and in this order
  #   output_cols       <- c(crosswalk_id, "cs_id", "pt_id", "Z", "relative_distance", 
  #                          "cs_lengthm", "class", "point_type")
  #   
  #   # any starting columns in the original data 
  #   starting_cols  <- names(cs_pts)
  #   
  #   # name and order of columns to select with
  #   cols_to_select <- c(output_cols, starting_cols[!starting_cols %in% output_cols])
  #   
  #   # check if we're missing the required points_per_cs column, if so, 
  #   # we generate one based on the number of points in each cross section
  #   is_missing_points_per_cs <- !"point_per_cs" %in% names(cs_pts)
  #   
  #   if (is_missing_points_per_cs) {
  #     cs_pts <- 
  #       cs_pts %>% 
  #       dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
  #       dplyr::mutate(points_per_cs = dplyr::n()) %>% 
  #       dplyr::ungroup()
  #   } 
  #   
  #   classify_banks_and_bottoms2 <- function(
  #     num_of_pts, 
  #     pt_ids, 
  #     depths
  #     ) {
  #     
  #     num_of_pts <- cs_pts$points_per_cs[1]
  #     pt_ids <- cs_pts$pt_id
  #     depths <- cs_pts$Z
  #     depths
  #     num_of_pts
  #     
  #     library(glue)
  #     
  #     local_mins <- list()
  #     local_maxs <- list()
  #     
  #     for (i in 2:(length(depths)-1)) {
  #       # i = 2
  #       depths[14]
  #      
  #       depth      <- depths[i]
  #       prev_depth <- depths[i-1]
  #       next_depth <- depths[i + 1]
  #       
  #       
  #       message( glue::glue("({i}):\n {prev_depth} -> {depth} -> {next_depth}"))
  #       
  #       is_local_min <- depth < prev_depth && depth < next_depth
  #       is_local_max <- depth > prev_depth && depth > next_depth
  #       
  #       message( glue::glue(" > Local min? {is_local_min}"))
  #       message( glue::glue(" > Local max? {is_local_max}"))
  #       # local_mins
  #       if(is_local_min) {
  #         local_mins[[i]] <- i
  #         
  #       }
  #       
  #     }
  #     
  #     # calc the 
  #     # - number of points in a third of the cross section (third)
  #     quarter              <- ceiling(num_of_pts / 4)
  #     
  #     # num_of_pts
  #     
  #     # 7 * 4
  #     
  #     first_quarter_left_idx    <- quarter
  #     second_quarter_right_idx  <- ((3 * quarter) - 1)
  #     
  #     mid_quarters_idxs    <- first_quarter_left_idx:second_quarter_right_idx
  #     mid_quarters_low_pt  <- min(depths[mid_quarters_idxs])
  #     
  #     # logic for determining if its a bottom point (at lowest depth AND in middle third)
  #     is_at_bottom_Z         <- depths <= mid_quarters_low_pt
  #     is_in_middle_quarters  <- dplyr::between(pt_ids, 
  #                                              first_quarter_left_idx, 
  #                                              second_quarter_right_idx
  #     )
  #     
  #     point_type <- ifelse(is_at_bottom_Z & is_in_middle_quarters, "bottom", "bank")
  #     
  #     return(point_type)
  #   }
  #   find_anchor_pts2 <- function(depths, 
  #                               num_of_pts,
  #                               cs_length, 
  #                               relative_distance, 
  #                               point_types
  #   ) {
  #     
  #     
  #     # ------------------------------
  #     # depths            = depths
  #     # num_of_pts        = num_of_pts
  #     # relative_distance = relative_distances
  #     # cs_length         = cs_length
  #     # point_types       = point_types
  #     
  #     # num_of_pts <- tmp$points_per_cs[1]
  #     # pt_ids <- tmp$pt_id
  #     # depths <- tmp$Z
  #     # point_types = tmp$class
  #     # cs_length <- tmp$cs_lengthm[1]
  #     # relative_distance <- tmp$relative_distance
  #     
  #     # ------------------------------
  #     third          = ceiling(num_of_pts / 3)
  #     # quarter          = ceiling(num_of_pts / 4) 
  #     
  #     dist_between_pts   <- mean(diff(relative_distance))
  #     in_channel_pts     <- ceiling(cs_length / dist_between_pts)
  #     
  #     b1  <- ceiling(in_channel_pts / 2) # b1
  #     b2  <- in_channel_pts - b1 # b2
  #     
  #     # bank_idxs      <- find_in_channel_pts(relative_distances, cs_length)
  #     # 
  #     # left_bank      <- bank_idxs[1] 
  #     # right_bank     <- bank_idxs[2]
  #     
  #     bottom_idxs <- which(point_types == "bottom")
  #     # point_type_is_bottom = which(point_types == "bottom")
  #     
  #     min_bottom  <- bottom_idxs[1]
  #     mid_bottom  <- bottom_idxs[ceiling(length(bottom_idxs) / 2)]
  #     max_bottom  <- bottom_idxs[length(bottom_idxs)]
  #     # min_bottom     = which(point_types == "bottom")[1]
  #     # mid_bottom     = which(point_types == "bottom")[ceiling(length(which(point_types == "bottom"))/2)]
  #     # max_bottom     = which(point_types == "bottom")[length(which(point_types == "bottom"))]
  #     
  #     L1             = pmax(1, mid_bottom - b1)
  #     L2             = pmax(1, mid_bottom - b2)
  #     
  #     R1             = pmin(mid_bottom + b2, num_of_pts)
  #     R2             = pmin(mid_bottom + b1, num_of_pts)
  #     
  #     anchor         = ifelse(depths[R2] < depths[L1], 2, 1)
  #     
  #     LEFT           = pmax(third, ifelse(anchor == 1, L1, L2))
  #     RIGHT          = pmin(2 * third, ifelse(anchor == 1, R1, R2))
  #     
  #     # LEFT           = pmax(quarter, ifelse(anchor == 1, L1, L2))
  #     # RIGHT          = pmin(3 * quarter, ifelse(anchor == 1, R1, R2))
  #     
  #     count_left     = min_bottom - LEFT
  #     count_right    = RIGHT - max_bottom
  #     
  #     LEFT           = ifelse(count_left == 0, LEFT - count_right, LEFT)
  #     RIGHT          = ifelse(count_right == 0, RIGHT + count_left, RIGHT)
  #     
  #     return(
  #       c(LEFT, mid_bottom, RIGHT)
  #       )
  #     
  #   }
  #   # cs_pts2 <- cs_pts
  #   tmp <- 
  #     dplyr::filter(cs_pts) %>% 
  #     dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
  #     # dplyr::group_by(hy_id, cs_id) %>%
  #     dplyr::mutate(
  #       class       = classify_banks_and_bottoms2(
  #         num_of_pts = points_per_cs[1],
  #         pt_ids     = pt_id,
  #         depths     = Z
  #       ),
  #         Z           = use_smoothed_depths(
  #           start_depths    = Z, 
  #           smoothed_depths = smooth_depths(Z, window = 3), 
  #           point_types     = class
  #         ),
  #         anchors     = list(
  #           find_anchor_pts(
  #             depths             = Z,
  #             num_of_pts         = points_per_cs[1],
  #             cs_length          = cs_lengthm[1],
  #             relative_distance  = relative_distance,
  #             point_types        = class
  #           )
  #         ),
  #         L              = anchors[[1]][1],
  #         R              = anchors[[1]][3],
  #         class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
  #         class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
  #         class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class),
  #         
  #         # get classification of concavity based on 1st and 2nd derivatives of depth points
  #         deriv_type   = classify_derivatives(Z)
  #       
  #     )
  #   
  # # depths <- c(1, 1, 1, 2, 1)
  # Z          = c(5, 8, 10, 5, 2, 2, 4, 6, 7, 8,  9, 9, 10, 11, 10, 12, 14,13, 12, 8)
  # # Z          = c(1, 1, 1, 2, 1)
  # cs_pts <-
  #   data.frame(
  #     # Z          = c(5, 8, 10, 5, 2, 2, 4, 6, 7, 8,  9, 9, 10, 11, 10, 12, 14,13, 12, 8)
  #     Z          = Z
  #   ) %>% 
  #   dplyr::mutate(
  #     hy_id          = "A",
  #     cs_id          = 1,
  #     cs_lengthm     = 100,
  #     points_per_cs  = dplyr::n(),
  #     pt_id          = 1:dplyr::n()
  #   )
  # 
  # NUM_CS_PTS   <- nrow(cs_pts)
  # REL_DIST     <- seq(0, 1, length.out=NUM_CS_PTS+1)[2:(NUM_CS_PTS+1)]
  # cs_pts$relative_distance <- REL_DIST
  # 
  # cs_pts <- 
  #   cs_pts %>% 
  #   dplyr::relocate(hy_id, cs_id, pt_id, cs_lengthm, points_per_cs, relative_distance, Z)
  # 
  # cs_pts
  # 
  # # find_bottom_candidates()
  # plot(cs_pts$Z)
  # 
  # crosswalk_id = "hy_id"
  # cs_pts
  # #   # create classifications for points
  #   # classified_pts <-
  #     # dplyr::filter(cs_pts) %>%
  # 
  # classify_banks_and_bottoms2 <- function(
  #   pt_ids,
  #   depths
  # ) {
  #   
  #   # pt_ids <- cs_pts$pt_id
  #   # depths <- cs_pts$Z
  #   
  #   # find_bottom_candidates(depths)
  #   bottom_candidates <- 
  #     depths %>% 
  #     find_bottom_candidates() 
  #     # rm_edge_buckets() 
  #     # anchor_picker()
  #   
  #   # bottom_candidates <-
  #     depths %>% 
  #     find_bottom_candidates() %>% 
  #     rm_edge_buckets() %>% 
  #     anchor_picker()
  #   
  #     # if(length(bucket_indexes) > 0) {
  #       # sort_order <- order( 
  #       #   -sapply(bottom_candidates, `[[`, "depth"),
  #       #   sapply(bottom_candidates, `[[`, "distance_to_center"),
  #       #   -sapply(bottom_candidates, `[[`, "width")
  #       # )
  #       # anchor <- bucket_indexes[sort_order[1]]
  #       # return(anchor)
  #     # } 
  #   
  #   classify_banks_and_bottoms(num_of_pts = length(pt_ids), 
  #                              pt_ids = pt_ids,
  #                              depths = depths
  #                              )
  #   
  #   # bottom_candidates
  #   # bottom_candidates[[1]]$left_maximum
  #   
  #   # point_type_list <- vector(mode = "character", length = length(pt_ids))
  #   point_types <- rep("bank", length(pt_ids))
  #   
  #   for (i in seq_along(bottom_candidates)) {
  #     
  #     candidate <- bottom_candidates[[i]]
  #     
  #     # L  <- ifelse(is.null(candidate$left_max), 1, candidate$left_max)
  #     # R  <- ifelse(is.null(candidate$right_max), length(depths), candidate$right_max)
  #     
  #     L   <- candidate$left_max
  #     R   <- candidate$right_max
  #     
  #     L   <- ifelse(is.null(L), 1, L)
  #     R   <- ifelse(is.null(R), length(depths), R)
  #     
  #     bottom_range     <- L:R
  #     
  #     bottom_min       <- min(depths[bottom_range])
  #     is_at_bottom_Z   <- depths <= bottom_min
  #     # is_at_bottom_Z   <- depths[bottom_range] <= bottom_min
  #     
  #     is_in_bucket     <- dplyr::between(pt_ids, 
  #                                        L, 
  #                                        R 
  #                                        )
  #     
  #     point_types[which(is_at_bottom_Z & is_in_bucket)] <- "bottom"
  #     
  #   }
  #   
  #   # bottom_ranges   <- lapply(bottom_candidates, function(i) { i$left_max:i$right_max  }) 
  #   # point_type_list <- lapply(bottom_candidates, function(i) { 
  #   #   bottom_range     <- i$left_max:i$right_max 
  #   #   bottom_min       <- min(depths[bottom_range])
  #   #   is_at_bottom_Z   <- depths[bottom_range] <= bottom_min
  #   #   is_in_bucket     <- dplyr::between(pt_ids[bottom_range], 
  #   #                                      i$left_max, 
  #   #                                      i$right_max 
  #   #                                      )
  #   #   ifelse(is_at_bottom_Z & is_in_bucket, "bottom", "bank") 
  #   #   })
  # 
  #   return(point_types)
  # }
  # 
  # 
  # find_anchor_pts2 <- function(
  #   depths,
  #   num_of_pts,
  #   cs_length, 
  #   relative_distance, 
  #   point_types
  # ) {
  #   
  #   # pt_ids        <- cs_pts2$pt_id
  #   # depths        <- cs_pts2$Z
  #   # num_of_pts         = cs_pts2$points_per_cs[1]
  #   # cs_length          = cs_pts2$cs_lengthm[1]
  #   # relative_distance  = cs_pts2$relative_distance
  #   # point_types        <- cs_pts2$class
  #   
  #   # backup_anchor <- find_anchor_pts(
  #   #   depths             = depths,
  #   #   num_of_pts         = num_of_pts,
  #   #   cs_length          = cs_length,
  #   #   relative_distance  = relative_distance,
  #   #   point_types        = point_types
  #   # ) 
  #   # 
  #   # backup_anchor <- list(
  #   #                     L = backup_anchor[1],
  #   #                     M = backup_anchor[2],
  #   #                     R = backup_anchor[3]
  #   #                   )
  #   # depths <- c(1, 1, 1, 2, 1)
  #   
  #   # find_bottom_candidates(depths)
  #   anchor_pts <- 
  #     depths %>% 
  #     find_bottom_candidates() %>%
  #     rm_edge_buckets() %>%
  #     anchor_picker() 
  #   
  #   if (length(anchor_pts) > 0) {
  #     
  #     L <- ifelse(is.null(anchor_pts[[1]]$left_max), 1, anchor_pts[[1]]$left_max)
  #     M <- anchor_pts[[1]]$minimum
  #     R <- ifelse(is.null(anchor_pts[[1]]$right_max), length(depths), anchor_pts[[1]]$right_max)
  #     
  #     return (
  #       c(L, M, R)
  #         # list(
  #         #   L = L,
  #         #   M = M,
  #         #   R = R
  #         #   # L = anchor_pts[[1]]$left_max,
  #         #   # M = anchor_pts[[1]]$minimum,
  #         #   # R = anchor_pts[[1]]$right_max
  #         # )
  #       )
  #   }
  #   
  #   # if other anchor picker returns no results, then fall back on using the middle third based anchor pts
  #   backup_anchor <- find_anchor_pts(
  #     depths             = depths,
  #     num_of_pts         = num_of_pts,
  #     cs_length          = cs_length,
  #     relative_distance  = relative_distance,
  #     point_types        = point_types
  #   ) 
  #   
  #   L = backup_anchor[1]
  #   M = backup_anchor[2]
  #   R = backup_anchor[3]
  #   
  #   return(
  #     c(L, M, R)
  #     # list(
  #     #   L = backup_anchor[1],
  #     #   M = backup_anchor[2],
  #     #   R = backup_anchor[3]
  #     # )
  #   )
  #     
  # }
  # 
  # cs_pts2 <-
  #   cs_pts %>% 
  #   dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%
  #   # dplyr::group_by(hy_id, cs_id) %>%
  #   dplyr::mutate(
  #     # class       = classify_banks_and_bottoms(
  #     #   num_of_pts = points_per_cs[1],
  #     #   pt_ids     = pt_id,
  #     #   depths     = Z
  #     # ),
  #     class       = classify_banks_and_bottoms2(
  #       pt_ids     = pt_id,
  #       depths     = Z
  #     ),
  #     Z           = use_smoothed_depths(
  #       start_depths    = Z,
  #       smoothed_depths = smooth_depths(Z, window = 3),
  #       point_types     = class
  #     ),
  #     anchors     = list(
  #       find_anchor_pts2(
  #         depths             = Z,
  #         num_of_pts         = points_per_cs[1],
  #         cs_length          = cs_lengthm[1],
  #         relative_distance  = relative_distance,
  #         point_types        = class
  #       )
  #     ),
  #     # anchors     = list(
  #     #   find_anchor_pts(
  #     #     depths             = Z,
  #     #     num_of_pts         = points_per_cs[1],
  #     #     cs_length          = cs_lengthm[1],
  #     #     relative_distance  = relative_distance,
  #     #     point_types        = class
  #     #   )
  #     # ),
  #     L = anchors[[1]][1],
  #     R = anchors[[1]][3],
  #     # L = anchors[[1]]$L,
  #     # R = anchors[[1]]$R
  #     class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
  #     class          = ifelse(class %in% c('bank', 'bottom') & pt_id <= L[1], "left_bank", class),
  #     class          = ifelse(class %in% c('bank', 'bottom') & pt_id >= R[1], "right_bank", class),
  #     # class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
  #     # class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class),
  #     deriv_type   = classify_derivatives(Z),
  #     deriv_type   = dplyr::case_when(
  #       (grepl("concave", deriv_type) | deriv_type == "linear") & class != "bottom" ~ "channel",
  #       TRUE                         ~ class
  #     ),
  #     deriv_type   = clean_point_types(deriv_type),
  #     # # deriv_type   = set_missing_bottom(
  #     # #                                depths      = Z,
  #     # #                               point_types = deriv_type
  #     # #                                ),
  #     # deriv_type   = set_channel_anchors(deriv_type),
  #     # deriv_type   = set_bank_anchors_with_channel(
  #     #   depths = Z,
  #     #   point_types = deriv_type,
  #     #   L = L[1],
  #     #   R = R[1]
  #     # ),
  #     # deriv_type   = set_missing_bottom(
  #     #   depths      = Z,
  #     #   point_types = deriv_type
  #     # ),
  #     # deriv_type   = set_left_bank(
  #     #   point_types = deriv_type
  #     # ),
  #     # deriv_type   = set_right_bank(
  #     #   point_types = deriv_type
  #     # ),
  #     # deriv_type   = set_channel_surrounded_by_bottom(
  #     #   depths      = Z,
  #     #   point_types = deriv_type
  #     # ),
  #     # class        = deriv_type,
  #     # point_type   = deriv_type
  #   )
  # 
  #   # .$Z %>%
  #   # plot()
  # cs_pts2
  # # cs_pts2$Z %>% plot()
  # 
  # cs_pts2  %>% 
  #   hydrofabric3D::plot_cs_pts("hy_id", color = "point_type", size = 3)
  # 
  # cs_pts %>% 
  #     dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%
  #     # dplyr::group_by(hy_id, cs_id) %>%
  #     dplyr::mutate(
  #       class       = classify_banks_and_bottoms2(
  #         num_of_pts = points_per_cs[1],
  #         pt_ids     = pt_id,
  #         depths     = Z
  #       ),
  #       Z           = use_smoothed_depths(
  #         start_depths    = Z,
  #         smoothed_depths = smooth_depths(Z, window = 3),
  #         point_types     = class
  #       ),
  #       anchors     = list(
  #         find_anchor_pts(
  #           depths             = Z,
  #           num_of_pts         = points_per_cs[1],
  #           cs_length          = cs_lengthm[1],
  #           relative_distance  = relative_distance,
  #           point_types        = class
  #         )
  #       ),
  #       L              = anchors[[1]][1],
  #       R              = anchors[[1]][3],
  #       class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
  #       class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
  #       class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class),
  # 
  #       # get classification of concavity based on 1st and 2nd derivatives of depth points
  #       deriv_type   = classify_derivatives(Z),
  #       deriv_type   = dplyr::case_when(
  #         (grepl("concave", deriv_type) | deriv_type == "linear") & class != "bottom" ~ "channel",
  #         TRUE                         ~ class
  #       ),
  #       # side = dplyr::case_when(
  #       #   pt_id >= R[1] ~ "right_side",
  #       #   pt_id <= L[1] ~ "left_side",
  #       #   TRUE          ~ "middle"
  #       # ),
  #       # # max_left      = which.max(Z[1:L[1]]),
  #       # # max_right     = which.max(Z[R[1]:length(Z)]) + R[1],
  #       # max_left      = pmax(which.max(Z[1:L[1]]), 1),
  #       # max_right     = pmin(which.max(Z[R[1]:length(Z)]) + R[1], points_per_cs[1]),
  # 
  #       deriv_type   = clean_point_types(deriv_type),
  #       # deriv_type   = set_missing_bottom(
  #       #                                 depths      = Z,
  #       #                                 point_types = deriv_type
  #       #                                 ),
  #       deriv_type   = set_channel_anchors(deriv_type),
  #       deriv_type   = set_bank_anchors_with_channel(
  #         depths = Z,
  #         point_types = deriv_type,
  #         L = L[1],
  #         R = R[1]
  #       ),
  #       deriv_type   = set_missing_bottom(
  #         depths      = Z,
  #         point_types = deriv_type
  #       ),
  #       deriv_type   = set_left_bank(
  #         point_types = deriv_type
  #       ),
  #       deriv_type   = set_right_bank(
  #         point_types = deriv_type
  #       ),
  #       deriv_type   = set_channel_surrounded_by_bottom(
  #         depths      = Z,
  #         point_types = deriv_type
  #       ),
  #       class        = deriv_type,
  #       point_type   = deriv_type
  #       # class      = clean_point_types(class),
  #       # point_type = class
  #     ) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::select(dplyr::any_of(cols_to_select))  
  #       hydrofabric3D::plot_cs_pts("hy_id", size = 4, color = "point_type")
  #   # hydrofabric3D::classify_points(crosswalk_id = ID_COL) %>%
  #   # dplyr::mutate(
  #   #   pt_bin = dplyr::case_when(
  #   #     has_relief & valid_banks    ~ "valid banks + relief",
  #   #     !has_relief & valid_banks   ~ "valid banks + NO relief",
  #   #     has_relief & !valid_banks   ~ "NO valid banks + relief",
  #   #     !has_relief & !valid_banks  ~ "NO valid banks + NO relief",
  #   #     TRUE                        ~ NA
  #   #   )
  #   # ) %>%
  #   # hydrofabric3D::add_tmp_id(x = ID_COL)
  # 
  # # # cs_pts %>%
  # # #   ggplot2::ggplot() +
  # # #   ggplot2::geom_point(
  # # #     ggplot2::aes(x = pt_id,
  # # #                  y = Z,
  # # #                  color = pt_bin
  # # #                  ),
  # # #     size = 4
  # # #     ) +
  # # #   ggplot2::facet_wrap(hy_id~cs_id, scales = "free")
  # # 
  # # 
  # # # cs_pts %>%
  # # #   hydrofabric3D::plot_cs_pts(color = "point_type", size =4)
  # # 
  # # cs_pts %>%
  # #   dplyr::filter(has_relief, valid_banks) %>%
  # #   hydrofabric3D::plot_cs_pts(color = "point_type", size =4) +
  # #   ggplot2::labs(title = "valid banks + relief")
  # # 
  # # cs_pts %>%
  # #   dplyr::filter(!has_relief, valid_banks) %>%
  # #   hydrofabric3D::plot_cs_pts(color = "point_type", size =4) +
  # #   ggplot2::labs(title = "valid banks + NO relief")
  # # 
  # # cs_pts %>%
  # #   dplyr::filter(has_relief, !valid_banks) %>%
  # #   hydrofabric3D::plot_cs_pts(color = "point_type", size =4) +
  # #   ggplot2::labs(title = "NO valid banks + relief")
  # # 
  # # cs_pts %>%
  # #   dplyr::filter(!has_relief, !valid_banks) %>%
  # #   hydrofabric3D::plot_cs_pts(color = "point_type", size =4) +
  # #   ggplot2::labs(title = "NO valid banks + NO relief")
  # # 
  # # cs_pts
  # # 
  # # adjust <- function(v){
  # #   if(length(v) == 1){
  # #     return(v)
  # #   }
  # #   for(i in 2:length(v)){
  # #     v[i] = ifelse(v[i] > v[i-1], v[i-1], v[i])
  # #   }
  # #   v
  # # }
  # # 
  # # slope <-
  # #   cs_pts %>%
  # #   sf::st_drop_geometry() %>%
  # #   dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%
  # #   # dplyr::group_by(hy_id, cs_id) %>%
  # #   dplyr::summarise(min_ch = min(Z[point_type == "channel"])) %>%
  # #   dplyr::mutate(
  # #     a = adjust(min_ch),
  # #     adjust = adjust(min_ch) - min_ch
  # #     ) %>%
  # #   dplyr::ungroup() %>%
  # #   dplyr::select(
  # #     dplyr::any_of(crosswalk_id),
  # #     cs_id,
  # #     adjust
  # #   )
  # # # dplyr::select(hy_id, cs_id, adjust)
  # # 
  # # cs_pts <-
  # #   dplyr::left_join(
  # #     cs_pts,
  # #     slope,
  # #     by = c(crosswalk_id, "cs_id")
  # #     # by = c("hy_id", "cs_id")
  # #   ) %>%
  # #   dplyr::mutate(
  # #     Z = dplyr::case_when(
  # #       point_type  == "channel" ~ Z + adjust,
  # #       TRUE ~ Z
  # #     )
  # #   ) %>%
  # #   dplyr::select(-adjust)
  # # 
})
  
  
  
  