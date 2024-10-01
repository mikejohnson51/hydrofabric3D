library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::find_braids() ----
# -------------------------------------------------------------------
testthat::test_that("extend_by_percent() correct output columns - default inputs w/ length_col (50% increase)", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "id"
  LENGTH_COL_NAME    <- "cs_lengthm"
  PCT <- 0.5 
  NUM_OF_TRANSECTS = 3
  
  flowlines <-
    flowlines %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
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
  
  # transects 
  transects <- dplyr::select(transects,
                             dplyr::any_of(ID_COL),
                             cs_id,
                             cs_lengthm
  )
  
  new_trans <- hydrofabric3D:::extend_by_percent(x = transects, 
                                                 crosswalk_id = ID_COL, 
                                                 pct = PCT, 
                                                 length_col = LENGTH_COL_NAME
  )
  
  # make sure the output has the min required columns for a transect 
  has_min_output_cols <- check_min_transect_output_cols(transects, id = ID_COL)
  testthat::expect_true(has_min_output_cols)
  
})

# TODO:
testthat::test_that("extend_by_percent() correct change in geometry length, default inputs w/ length_col (50% increase)", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "id"
  LENGTH_COL_NAME    <- "cs_lengthm"
  PCT <- 0.5 
  NUM_OF_TRANSECTS = 3
  
  flowlines <-
    flowlines %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
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
  
  # transects 
  transects <- dplyr::select(transects,
                             dplyr::any_of(ID_COL),
                             cs_id,
                             cs_lengthm
  )
  
  START_TRANSECT_LENGTHS  <- rep(100, nrow(transects))
  END_TRANSECT_LENGTHS    <- rep(150, nrow(transects))
  
  new_trans <- hydrofabric3D:::extend_by_percent(x = transects, 
                                                 crosswalk_id = ID_COL, 
                                                 pct = PCT, 
                                                 length_col = LENGTH_COL_NAME
  )
  
  new_lengths   <- round(new_trans[[LENGTH_COL_NAME]], 0) 
  
  has_correctly_changed_lengths_col <- all(new_lengths == END_TRANSECT_LENGTHS)
  testthat::expect_true(has_correctly_changed_lengths_col)
  
  # check that the new geometries and the length values line up
  geo_length <- as.numeric(sf::st_length(new_trans))
  
  lines_length_matches_new_length_col <- all(dplyr::between(geo_length, new_lengths - 2, new_lengths + 2))
  lines_length_matches_old_length_col <- all(dplyr::between(geo_length, END_TRANSECT_LENGTHS - 2, END_TRANSECT_LENGTHS + 2))
  
  testthat::expect_true(lines_length_matches_new_length_col)
  testthat::expect_true(lines_length_matches_old_length_col)
  
})


testthat::test_that("default inputs w/ length_col (0% increase - returns input data w/ added length column)", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "id"
  LENGTH_COL_NAME    <- "cs_lengthm"
  PCT <- 0
  NUM_OF_TRANSECTS = 3
  
  flowlines <-
    flowlines %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
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
  
  # transects 
  transects <- dplyr::select(transects,
                             dplyr::any_of(ID_COL),
                             cs_id,
                             cs_lengthm
  )
  
  START_TRANSECT_LENGTHS  <- rep(100, nrow(transects))
  END_TRANSECT_LENGTHS    <- rep(100, nrow(transects))
  
  new_trans <- hydrofabric3D:::extend_by_percent(x = transects, 
                                                 crosswalk_id = ID_COL, 
                                                 pct = PCT, 
                                                 length_col = LENGTH_COL_NAME
                                                 )
  
  new_lengths   <- round(new_trans[[LENGTH_COL_NAME]], 0) 
  
  # make sure the output has the min required columns for a transect 
  has_min_output_cols <- check_min_transect_output_cols(transects, id = ID_COL)
  testthat::expect_true(has_min_output_cols)
  
  has_correctly_changed_lengths_col <- all(new_lengths == END_TRANSECT_LENGTHS)
  testthat::expect_true(has_correctly_changed_lengths_col)
  
  has_correctly_changed_lengths_col <- all(dplyr::between(new_lengths, END_TRANSECT_LENGTHS - 2, END_TRANSECT_LENGTHS + 2))
  testthat::expect_true(has_correctly_changed_lengths_col)
  
  # check that the new geometries and the length values line up
  geo_length <- as.numeric(sf::st_length(new_trans))
  
  lines_length_matches_new_length_col <- all(dplyr::between(geo_length, new_lengths - 2, new_lengths + 2))
  lines_length_matches_old_length_col <- all(dplyr::between(geo_length, END_TRANSECT_LENGTHS - 2, END_TRANSECT_LENGTHS + 2))
  
  testthat::expect_true(lines_length_matches_new_length_col)
  testthat::expect_true(lines_length_matches_old_length_col)
  
})

testthat::test_that("default inputs w/o length_col (50% increase - length column gets added)", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  
  ID_COL             <- "id"
  LENGTH_COL_NAME    <- "cs_lengthm"
  PCT <- 0.5
  NUM_OF_TRANSECTS = 3
  
  flowlines <-
    flowlines %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
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
  
  # transects 
  transects <- dplyr::select(transects,
                             dplyr::any_of(ID_COL),
                             cs_id
                             # cs_lengthm
  )
  
  START_TRANSECT_LENGTHS <- rep(100, nrow(transects))
  END_TRANSECT_LENGTHS    <- rep(150, nrow(transects))
  
  new_trans <- hydrofabric3D:::extend_by_percent(x = transects, 
                                                 crosswalk_id = ID_COL, 
                                                 pct = PCT, 
                                                 length_col = NULL
                                                 )
  
  # make sure the output has the min required columns for a transect 
  has_min_output_cols <- check_min_transect_output_cols(transects, id = ID_COL)
  testthat::expect_true(has_min_output_cols)
  
  # TODO: need to recalculate length column value because no length_col is specified so 
  new_lengths <- round(add_length_col(new_trans, LENGTH_COL_NAME)[[LENGTH_COL_NAME]], 0)
  
  # new_lengths   <- ceiling(new_trans$cs_lengthm)
  # calc_new_length  <- ceiling(old_lengths + (old_lengths * PCT)) 
  # has_correctly_changed_lengths_col <- all(dplyr::between(new_lengths, calc_new_length - 2, calc_new_length + 2))
  
  # new_lengths   <- round(new_trans[[LENGTH_COL_NAME]], 0) 
  
  has_correctly_changed_lengths_col <- all(new_lengths == END_TRANSECT_LENGTHS)
  testthat::expect_true(has_correctly_changed_lengths_col)
  
  # check that the new geometries and the length values line up
  geo_length <- as.numeric(sf::st_length(new_trans))
  
  lines_length_matches_new_length_col <- all(dplyr::between(geo_length, new_lengths - 2, new_lengths + 2))
  lines_length_matches_old_length_col <- all(dplyr::between(geo_length, END_TRANSECT_LENGTHS - 2, END_TRANSECT_LENGTHS + 2))
  
  testthat::expect_true(lines_length_matches_new_length_col)
  testthat::expect_true(lines_length_matches_old_length_col)
  
})
