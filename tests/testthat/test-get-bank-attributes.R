library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::get_bank_attributes() ----
# -------------------------------------------------------------------

testthat::test_that("check 'get_bank_attributes' has valid output columns from CLASSIFIED CS from DEFAULT transects output", {

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

  bank_attrs <- get_bank_attributes(
    classified_pts = classified,
    crosswalk_id   = ID_COL
  )

  has_all_required_output_cols <- bank_attrs_has_min_output_cols(bank_attrs = bank_attrs, id = ID_COL)
  testthat::expect_true(has_all_required_output_cols)

})

testthat::test_that("'get_bank_attributes' - No left_bank points", {
  ID_COL <- "hy_id"

  no_left_bank_df <-
    data.frame(
      hy_id = c("A", "A", "A"),
      cs_id = c(1, 1, 1),
      pt_id = c(1, 2, 3),
      point_type = c('bottom', 'right_bank', 'right_bank'),
      Z = c(1, 5, 8)
    )

  #  <-
  #   data.frame(
  #     hy_id = c("A", "A", "A", "B", "B", "B"),
  #     cs_id = c(1, 1, 1, 1, 1, 1),
  #     pt_id = c(1, 2, 3, 1, 2, 3),
  #     point_type = c('bottom', 'right_bank', 'right_bank', "left_bank", "bottom", "right_bank"),
  #     Z = c(1, 5, 8, 10, 2, 12)
  #   )

  bank_attrs <- get_bank_attributes(no_left_bank_df, crosswalk_id = ID_COL)

  testthat::expect_true(
    is.na(bank_attrs$left_bank)
  )

  testthat::expect_false(
    bank_attrs$valid_banks
  )
})

testthat::test_that("'get_bank_attributes' - No left_bank points on 1 cross section but not the other", {
  ID_COL <- "hy_id"
  
  some_missing_left_banks <- 
    data.frame(
      hy_id = c("A", "A", "A", "B", "B", "B"),
      cs_id = c(1, 1, 1, 1, 1, 1),
      pt_id = c(1, 2, 3, 1, 2, 3),
      point_type = c('bottom', 'right_bank', 'right_bank', "left_bank", "bottom", "right_bank"),
      Z = c(1, 5, 8, 10, 2, 12)
    )
  
  bank_attrs <- get_bank_attributes(some_missing_left_banks, crosswalk_id = ID_COL)
  
  cs_A_missing_left_bank <- is.na(
                            bank_attrs %>% 
                              dplyr::filter(
                                hy_id == "A"
                              ) %>% 
                              dplyr::pull(left_bank)
                          )
  
  testthat::expect_true(cs_A_missing_left_bank)
  cs_B_has_left_bank <- !is.na(
                            bank_attrs %>% 
                              dplyr::filter(
                                hy_id == "B"
                              ) %>% 
                              dplyr::pull(left_bank)
                          )
  testthat::expect_true(cs_B_has_left_bank)
  
})

testthat::test_that("'get_bank_attributes' - No right_bank points", {
  ID_COL <- "hy_id"  
  
  no_right_bank_df <- 
    data.frame(
      hy_id = c("A", "A", "A"),
      cs_id = c(1, 1, 1), 
      pt_id = c(1, 2, 3),
      point_type = c('left_bank', 'left_bank', 'bottom'),
      Z = c(10, 8, 1)
    )
  
  bank_attributes <- get_bank_attributes(no_right_bank_df, crosswalk_id = ID_COL)
  
  testthat::expect_true(
    is.na(bank_attributes$right_bank)
  )
  
  testthat::expect_false(
    bank_attributes$valid_banks
  )
})

testthat::test_that("'get_bank_attributes' - Multiple bottom points with different Z values", {
  ID_COL <- "hy_id"  
  
  multiple_bottom_df <- 
    data.frame(
      hy_id = c("A", "A", "A", "A"),
      cs_id = c(1, 1, 1, 1), 
      pt_id = c(1, 2, 3, 4),
      point_type = c('bottom', 'bottom', 'left_bank', 'right_bank'),
      Z = c(2, 1, 10, 5)
    )
  plot(multiple_bottom_df$Z)
  bank_attributes <- get_bank_attributes(multiple_bottom_df, crosswalk_id = ID_COL)
  
  testthat::expect_equal(
    bank_attributes$bottom, 1
  )
  
  testthat::expect_true(
    bank_attributes$valid_banks
  )
})

testthat::test_that("'get_bank_attributes' - Invalid configuration (banks below bottom)", {
  ID_COL <- "hy_id"  
 
  invalid_banks_df <- 
    data.frame(
      hy_id = c("A", "A", "A"),
      cs_id = c(1, 1, 1), 
      pt_id = c(1, 2, 3),
      point_type = c('bottom', 'left_bank', 'right_bank'),
      Z = c(5, 3, 4)
    )
  
  bank_attributes <- get_bank_attributes(invalid_banks_df, crosswalk_id = ID_COL)
  
  testthat::expect_equal(
    bank_attributes$left_bank, 3
  )
  
  testthat::expect_equal(
    bank_attributes$right_bank, 4
  )
  
  testthat::expect_false(
    bank_attributes$valid_banks
  )
})

testthat::test_that("'get_bank_attributes' - All point_types missing", {
  ID_COL <- "hy_id"  
  #  some_invalid <- 
  #   data.frame(
  #     hy_id = c("A", "A", "A", "B", "B", "B"),
  #     cs_id = c(1, 1, 1, 1, 1, 1),
  #     pt_id = c(1, 2, 3, 1, 2, 3),
  #     point_type = c('channel', 'channel', 'channel', "left_bank", "bottom", "right_bank"),
  #     Z = c(1, 5, 8, 10, 2, 12)
  #   )
  # 
  # no_valid_points_df <- 
  #   data.frame(
  #     hy_id = c("A", "A", "A"),
  #     cs_id = c(1, 1, 1), 
  #     pt_id = c(1, 2, 3),
  #     point_type = c('channel', 'channel', 'channel'),
  #     Z = c(3, 5, 7)
  #   )
  # 
  # bank_attributes <- get_bank_attributes(some_invalid, crosswalk_id = ID_COL)
  # 
  # 
  # 
  # testthat::expect_true(
  #   is.na(bank_attributes$bottom)
  # )
  # 
  # testthat::expect_true(
  #   is.na(bank_attributes$left_bank)
  # )
  # 
  # testthat::expect_true(
  #   is.na(bank_attributes$right_bank)
  # )
  # 
  # testthat::expect_false(
  #   bank_attributes$valid_banks
  # )
})
# hy_id cs_id pt_id point_type  Z
# 1     A     1     1  left_bank 10
# 2     A     1     2     bottom  1
# 3     A     1     3    channel  3
# 4     A     1     4 right_bank  5
# 5     A     1     5 right_bank  8

testthat::test_that(" 'get_bank_attributes' - 2 right_bank values w/ furthest right pt being the greatest value", {
  ID_COL <- "hy_id"

  right_bank_df <-
    data.frame(
      hy_id = c("A", "A", "A", "A", "A"),
      cs_id = c(1, 1, 1, 1, 1),
      pt_id = c(1, 2, 3, 4, 5),
      point_type = c('left_bank', 'bottom', 'channel', 'right_bank', 'right_bank'),
      Z = c(10, 1, 3, 5, 8)
    )

  max_right_Z <-
    right_bank_df %>%
    dplyr::filter(point_type == "right_bank") %>%
    dplyr::pull(Z) %>%
    max()

  right_bank_val <- get_bank_attributes(right_bank_df, crosswalk_id = ID_COL)$right_bank



  testthat::expect_true(
    max_right_Z == right_bank_val
    )
})

testthat::test_that("'get_bank_attributes' - 2 left_bank values w/ furthest left pt being the greatest value", {
  ID_COL <- "hy_id"

  left_bank_df <-
    data.frame(
      hy_id = c("A", "A", "A", "A", "A"),
      cs_id = c(1, 1, 1, 1, 1),
      pt_id = c(1, 2, 3, 4, 5),
      point_type = c('left_bank', 'left_bank', 'bottom', 'right_bank', 'right_bank'),
      Z = c(10, 8, 1, 5, 8)
    )

  max_left_Z <-
    left_bank_df %>%
    dplyr::filter(point_type == "left_bank") %>%
    dplyr::pull(Z) %>%
    max()

  left_bank_val <- get_bank_attributes(left_bank_df, crosswalk_id = ID_COL)$left_bank

  testthat::expect_true(
    max_left_Z == left_bank_val
  )

})

testthat::test_that("error 'get_bank_attributes' when given dataframe with wrong/missing required columns", {
  ID_COL <- "hy_id"

  wrong_cols_df <-
    data.frame(
      hy_id = c("A", "A", "B", "B"),
      cs_identifier = c(1, 1, 1, 1), # NOTE: this is the wrong column name, needs to be "cs_id"
      pt_id = c(1, 2, 1, 2),
      point_type = c('left_bank', 'right_bank', 'left_bank', 'bottom'),
      Z = c(1, 3, 4, 2)
    )

  testthat::expect_error(
    get_bank_attributes(wrong_cols_df, crosswalk_id = ID_COL)
    )

  # missing "cs_id" column
  missing_cols_df <-
    data.frame(
      hy_id = c("A", "A", "B", "B"),
      pt_id = c(1, 2, 1, 2),
      point_type = c('left_bank', 'right_bank', 'left_bank', 'bottom'),
      Z = c(1, 3, 4, 2)
    )

  testthat::expect_error(
    get_bank_attributes(missing_cols_df, crosswalk_id = ID_COL)
  )

  # wrong 'crosswalk_id' column specified
  wrong_crosswalk_id_df <-
    data.frame(
      hy_id = c("A", "A", "B", "B"),
      cs_id = c(1, 1, 1, 1),
      pt_id = c(1, 2, 1, 2),
      point_type = c('left_bank', 'right_bank', 'left_bank', 'bottom'),
      Z = c(1, 3, 4, 2)
    )

  testthat::expect_error(
    get_bank_attributes(wrong_crosswalk_id_df, crosswalk_id = "other_id")
  )
})


