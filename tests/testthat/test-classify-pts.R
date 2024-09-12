library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::classify_pts() ----
# -------------------------------------------------------------------

# TODO:
testthat::test_that("check CLASSIFIED CS points default output columns, from DEFAULT transects output", {
  
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
  # cs_pts  %>% 
  # # dplyr::select(hy_id, cs_id, pt_id, Z, relative_distance)  %>% 
  # sf::st_drop_geometry() %>%
  #  head(20)
  classified <- classify_points(
    cs_pts = cs_pts,
    crosswalk_id = ID_COL,
    pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
  ) 
  
  # check if minimum required output columns 
  testthat::expect_true(
    classified_cs_pts_has_min_output_cols(classified_pts = classified, id = ID_COL)
    )
  
  # check that if NO 'id' is specified, then the default output columns will NOT match the classified points that DID have a specified 'id' 
  testthat::expect_false(
    classified_cs_pts_has_min_output_cols(classified_pts = classified )
    )
  
  # test same number of input points are in classified output points 
  same_num_pts_after_classifying <- nrow(cs_pts)  == nrow(classified)
  testthat::expect_true(same_num_pts_after_classifying)
 
  # make sure all the unique tmp_ids (id + cs_id) are the same in the input AND output 
  same_unique_tmp_ids <- has_same_unique_tmp_ids(x = cs_pts, y = classified, id = ID_COL)
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
  
  classified <- classify_points(
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
  
  # is_missing_points_per_cs_col <- !"point_per_cs" %in% names(ideal_pts)
  # 
  # if (is_missing_points_per_cs_col) {
  #   ideal_pts <- 
  #     ideal_pts %>% 
  #     dplyr::group_by(dplyr::across(dplyr::any_of(c(ID_COL, "cs_id")))) %>% 
  #     dplyr::mutate(points_per_cs = dplyr::n()) %>% 
  #     dplyr::ungroup()
  # }
  # 
  # dplyr::filter(ideal_pts) %>% 
  #   dplyr::group_by(dplyr::across(dplyr::any_of(c(ID_COL, "cs_id"))))
  
  # plot(ideal_pts$Z~ideal_pts$pt_id)

  cpts <- classify_points(ideal_pts, crosswalk_id = ID_COL)
  
  # classify_points(ideal_pts, crosswalk_id = ID_COL) %>% 
  #   plot_cs_pts(color = "point_type", size = 4)
  
  expected_point_types <- c('left_bank', 'left_bank', 'left_bank', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'right_bank', 'right_bank')

  testthat::expect_true(
    all(cpts$point_type == expected_point_types)
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


# generate_cs(
#   num_unique_ids = 1,
#   num_pts_per_id = 10,
#   cs_id          = 1,
#   cs_lengthm     = 100,
#   num_peaks = 1,
#   amplitude = 1
# ) 

# TODO: WORK IN PROGRESS______
# TODO: Left off here on 2024-08-27
# TODO: IDEAL SHAPE ("left_bank", "right_bank", "bottom", "channel" point types) 
#   \     /
#    \   /
#      _
testthat::test_that("'classify_points' - (1 cross section, VALID) - 3 point 'V' shaped cross section", {

  # ID_COL            <- "hy_id"
  # NUM_UNIQUE_IDS    = 1
  # UNIQUE_IDS        = LETTERS[1:NUM_UNIQUE_IDS]
  # NUM_PTS = 4
  # NUM_PTS_PER_ID    = 4
  # CS_ID             = 1
  # CS_LENGTHM        = 100
  # REL_DIST_INTERVAL = (CS_LENGTHM / NUM_PTS) / 100
  # REL_DIST          = cumsum(rep(REL_DIST_INTERVAL, NUM_PTS))
  # 
  # # cs <- generate_cs(
  # #   num_unique_ids = 1,
  # #   num_pts_per_id = 3,
  # #   cs_id          = 1,
  # #   cs_lengthm     = 100,
  # #   num_peaks      = 1,
  # #   amplitude      = 2
  # # )
  # # 
  # # plot(cs$Z)
  # # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # # cpts %>% hydrofabric3D::plot_cs_pts(color = "point_type")
  # cs <- data.frame(
  #   hy_id      = c("A", "A",  "A"),
  #   cs_id      = c(1, 1, 1),
  #   pt_id             = c(1, 2, 3),
  #   cs_lengthm        = c(100, 100, 100),
  #   relative_distance = c(0.333333, 0.666667, 1.0000000),
  #   Z = c(4, 1, 4)
  # )
  # 
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # # cpts
  # # cpts$has_relief
  # 
  # cs <- data.frame(
  #   hy_id      = c("A", "A",  "A", "A"),
  #   cs_id      = c(1, 1, 1, 1),
  #   pt_id             = c(1, 2, 3, 4),
  #   cs_lengthm        = c(100, 100, 100, 100),
  #   relative_distance = c(0.25, 0.50, 0.75, 1.0000000),
  #   Z = c(4, 1, 1, 4)
  # )
  # cpts <- classify_points(cs, crosswalk_id = ID_COL)
  # # cpts
  # # cpts$has_relief
  # # cpts %>% hydrofabric3D::plot_cs_pts(color = "point_type")
  # # classify_z_pts(c(4, 1, 4))
  # # classify_z_pts(c(10, 8, 7,4, 0, 4, 5, 10, 11, 9))
  # 
  # ideal_pts <-
  #   data.frame(
  #     hy_id      = c("A", "A", "A"),
  #     cs_id      = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  #     pt_id             = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  #     cs_lengthm        = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
  #     relative_distance = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  #     Z          = c(9, 7, 5, 5, 2, 2, 5, 5, 7, 9)
  #   )
  # 
  # # plot(ideal_pts$Z~ideal_pts$pt_id)
  # 
  # cpts <- classify_points(ideal_pts, crosswalk_id = ID_COL)
  # 
  # expected_point_types <- c('left_bank', 'left_bank', 'left_bank', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'right_bank', 'right_bank')
  # 
  # testthat::expect_true(
  #   all(cpts$point_type == expected_point_types)
  # )
})

testthat::test_that("'classify_points' - (2 cross section, both VALID) - left_bank above bottm, flat left channel, flat bottom, flat right channel, and right_bank above bottom", {
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
  
  expected_point_types <- c('left_bank', 'left_bank', 'left_bank', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'right_bank', 'right_bank')
  
  # make sure both hy_ids got correct classification
  testthat::expect_true(
    all(dplyr::filter(cpts, hy_id == "A")$point_type == expected_point_types)
  )
 
  testthat::expect_true(
    all(dplyr::filter(cpts, hy_id == "B")$point_type == expected_point_types)
  )
  
  # make sure all unique tmp_ids were preserved 
  testthat::expect_true(
    all(
      hydrofabric3D::get_unique_tmp_ids(cpts) %in% hydrofabric3D::get_unique_tmp_ids(two_cs) 
      )
  )
  
})

# testthat::test_that("check 'get_bank_attributes' has valid output columns from CLASSIFIED CS from DEFAULT transects output", {
#   
#   flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
#   # flowlines    <- dplyr::slice(flowlines, 1)
#   
#   MIN_BF_WIDTH       <- 50
#   ID_COL             <- "hy_id"
#   NUM_OF_TRANSECTS   <- 3
#   
#   # Cross section point inputs
#   DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
#   POINTS_PER_CS     <- NULL
#   MIN_PTS_PER_CS    <- 10
#   
#   PCT_OF_LENGTH_FOR_RELIEF <- 0.01
#   
#   flowlines <-
#     flowlines %>%
#     add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%
#     dplyr::rename(!!sym(ID_COL) := id) %>%
#     dplyr::select(
#       dplyr::any_of(ID_COL),
#       tot_drainage_areasqkm,
#       bf_width,
#       geom
#     )
#   
#   transects <- cut_cross_sections(
#     net = flowlines,
#     id  = ID_COL,
#     num = NUM_OF_TRANSECTS
#   ) %>%
#     dplyr::select(
#       dplyr::any_of(ID_COL),
#       cs_id,
#       cs_lengthm
#     )
#   
#   cs_pts = hydrofabric3D::cross_section_pts(
#     cs              = transects,
#     crosswalk_id    = ID_COL,
#     points_per_cs   = POINTS_PER_CS,
#     min_pts_per_cs  = MIN_PTS_PER_CS,
#     dem             = DEM_PATH
#   )
#   
#   classified <- classify_points(
#     cs_pts = cs_pts,
#     crosswalk_id = ID_COL,
#     pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
#   ) %>%
#     dplyr::select(-valid_banks, -has_relief, -left_bank, -right_bank, -bottom)
#   
#   # hydrofabric3D::plot_cs_pts(classified, color = "point_type")
#   
#   bank_attrs <- get_bank_attributes(
#     classified_pts = classified,
#     crosswalk_id   = ID_COL
#   )
#   
#   has_all_required_output_cols <- bank_attrs_has_min_output_cols(bank_attrs = bank_attrs, id = ID_COL)
#   testthat::expect_true(has_all_required_output_cols)
#   
# })
# 
# testthat::test_that("'get_bank_attributes' - No left_bank points", {
#   ID_COL <- "hy_id"
#   
#   no_left_bank_df <-
#     data.frame(
#       hy_id = c("A", "A", "A"),
#       cs_id = c(1, 1, 1),
#       pt_id = c(1, 2, 3),
#       point_type = c('bottom', 'right_bank', 'right_bank'),
#       Z = c(1, 5, 8)
#     )
#   
#   #  <-
#   #   data.frame(
#   #     hy_id = c("A", "A", "A", "B", "B", "B"),
#   #     cs_id = c(1, 1, 1, 1, 1, 1),
#   #     pt_id = c(1, 2, 3, 1, 2, 3),
#   #     point_type = c('bottom', 'right_bank', 'right_bank', "left_bank", "bottom", "right_bank"),
#   #     Z = c(1, 5, 8, 10, 2, 12)
#   #   )
#   
#   # # Add columns with the counts of point types
#   # classified_pts <- add_point_type_counts(no_left_bank_df, ID_COL)
#   # # classified_pts <- hydrofabric3D::add_point_type_counts2(classified_pts, crosswalk_id)
#   #
#   # # TODO: Need to add code that will just set aside the geometries and add them back to the final output dataset
#   # # For now we will just drop geometries as safety precaution (as to not summarize() on a massive number of sf geometries)
#   # classified_pts <- sf::st_drop_geometry(classified_pts)
#   #
#   # classified_pts <-
#   #   classified_pts %>%
#   #   # sf::st_drop_geometry() %>%  # drop sf geometry as a safety precaution to make sure returned data is a dataframe
#   #   dplyr::mutate(
#   #     valid_count = dplyr::case_when(
#   #       (bottom_count > 0 &
#   #          left_bank_count > 0 &
#   #          right_bank_count > 0)  ~ TRUE,
#   #       TRUE                      ~ FALSE
#   #     )
#   #   )
#   #
#   # crosswalk_id = "hy_id"
#   # # Add minimum bottom Z, max left and right bank Z, and
#   # # flags noting if the left/right banks are "valid" (i.e. max left/right bank values are greater than the bottom Z)
#   # bank_validity <-
#   #   classified_pts %>%
#   #   # classified_pts2 %>%
#   #   # sf::st_drop_geometry() %>%  # drop sf geometry as a safety precaution to make sure returned data is a dataframe
#   #   dplyr::filter(point_type %in% c("bottom", "left_bank", "right_bank")) %>%
#   #   # dplyr::filter(point_type %in% c("left_bank", "right_bank")) %>%
#   #   dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, Z, point_type) %>%
#   #   dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id", "point_type")))) %>%
#   #   # dplyr::select(hy_id, cs_id, pt_id, Z, point_type) %>%
#   #   # dplyr::group_by(hy_id, cs_id, point_type) %>%
#   #   dplyr::summarise(
#   #     minZ = min(Z, na.rm = TRUE),
#   #     maxZ = max(Z, na.rm = TRUE)
#   #   ) %>%
#   #   dplyr::ungroup() %>%
#   #   tidyr::pivot_wider(
#   #     names_from  = point_type,
#   #     values_from = c(minZ, maxZ)
#   #   ) %>%
#   #   dplyr::select(
#   #     dplyr::any_of(
#   #       c(
#   #       crosswalk_id,
#   #       "cs_id",
#   #       "minZ_bottom",
#   #       "maxZ_left_bank",
#   #       "maxZ_right_bank"
#   #       ))
#   #     # cs_id,
#   #     # bottom     = minZ_bottom,
#   #     # left_bank  = maxZ_left_bank,
#   #     # right_bank = maxZ_right_bank
#   #   ) %>%
#   #   dplyr::rename(
#   #     dplyr::any_of(c(
#   #       bottom     = "minZ_bottom",
#   #       left_bank  = "maxZ_left_bank",
#   #       right_bank = "maxZ_right_bank"
#   #       ))
#   #     )
#   #
#   # required_pt_cols <- c("bottom", "left_bank", "right_bank")
#   #
#   # for (col in required_pt_cols) {
#   #   # message(col)
#   #   if (!col %in% names(bank_validity)) {
#   #     # message("---> missing column ", col)
#   #     bank_validity[[col]] <- NA
#   #   }
#   # }
#   
#   bank_attributes <- get_bank_attributes(no_left_bank_df, crosswalk_id = ID_COL)
#   
#   testthat::expect_true(
#     is.na(bank_attributes$left_bank)
#   )
#   
#   testthat::expect_false(
#     bank_attributes$valid_banks
#   )
# })
# 
# # hy_id cs_id pt_id point_type  Z
# # 1     A     1     1  left_bank 10
# # 2     A     1     2     bottom  1
# # 3     A     1     3    channel  3
# # 4     A     1     4 right_bank  5
# # 5     A     1     5 right_bank  8
# 
# testthat::test_that(" 'get_bank_attributes' - 2 right_bank values w/ furthest right pt being the greatest value", {
#   ID_COL <- "hy_id"
#   
#   right_bank_df <-
#     data.frame(
#       hy_id = c("A", "A", "A", "A", "A"),
#       cs_id = c(1, 1, 1, 1, 1),
#       pt_id = c(1, 2, 3, 4, 5),
#       point_type = c('left_bank', 'bottom', 'channel', 'right_bank', 'right_bank'),
#       Z = c(10, 1, 3, 5, 8)
#     )
#   
#   max_right_Z <-
#     right_bank_df %>%
#     dplyr::filter(point_type == "right_bank") %>%
#     dplyr::pull(Z) %>%
#     max()
#   
#   right_bank_val <- get_bank_attributes(right_bank_df, crosswalk_id = ID_COL)$right_bank
#   
#   
#   
#   testthat::expect_true(
#     max_right_Z == right_bank_val
#   )
# })
# 
# testthat::test_that("'get_bank_attributes' - 2 left_bank values w/ furthest left pt being the greatest value", {
#   ID_COL <- "hy_id"
#   
#   left_bank_df <-
#     data.frame(
#       hy_id = c("A", "A", "A", "A", "A"),
#       cs_id = c(1, 1, 1, 1, 1),
#       pt_id = c(1, 2, 3, 4, 5),
#       point_type = c('left_bank', 'left_bank', 'bottom', 'right_bank', 'right_bank'),
#       Z = c(10, 8, 1, 5, 8)
#     )
#   
#   max_left_Z <-
#     left_bank_df %>%
#     dplyr::filter(point_type == "left_bank") %>%
#     dplyr::pull(Z) %>%
#     max()
#   
#   left_bank_val <- get_bank_attributes(left_bank_df, crosswalk_id = ID_COL)$left_bank
#   
#   testthat::expect_true(
#     max_left_Z == left_bank_val
#   )
#   
# })
# 
# testthat::test_that("error 'get_bank_attributes' when given dataframe with wrong/missing required columns", {
#   ID_COL <- "hy_id"
#   
#   wrong_cols_df <-
#     data.frame(
#       hy_id = c("A", "A", "B", "B"),
#       cs_identifier = c(1, 1, 1, 1), # NOTE: this is the wrong column name, needs to be "cs_id"
#       pt_id = c(1, 2, 1, 2),
#       point_type = c('left_bank', 'right_bank', 'left_bank', 'bottom'),
#       Z = c(1, 3, 4, 2)
#     )
#   
#   testthat::expect_error(
#     get_bank_attributes(wrong_cols_df, crosswalk_id = ID_COL)
#   )
#   
#   # missing "cs_id" column
#   missing_cols_df <-
#     data.frame(
#       hy_id = c("A", "A", "B", "B"),
#       pt_id = c(1, 2, 1, 2),
#       point_type = c('left_bank', 'right_bank', 'left_bank', 'bottom'),
#       Z = c(1, 3, 4, 2)
#     )
#   
#   testthat::expect_error(
#     get_bank_attributes(missing_cols_df, crosswalk_id = ID_COL)
#   )
#   
#   # wrong 'crosswalk_id' column specified
#   wrong_crosswalk_id_df <-
#     data.frame(
#       hy_id = c("A", "A", "B", "B"),
#       cs_id = c(1, 1, 1, 1),
#       pt_id = c(1, 2, 1, 2),
#       point_type = c('left_bank', 'right_bank', 'left_bank', 'bottom'),
#       Z = c(1, 3, 4, 2)
#     )
#   
#   testthat::expect_error(
#     get_bank_attributes(wrong_crosswalk_id_df, crosswalk_id = "other_id")
#   )
# })


















  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  