library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::extend_transects_any_side() ----
# -------------------------------------------------------------------

# ---------------------------------------------------------------------------
# ---- check that output data / columns are correct ----
# ---------------------------------------------------------------------------

testthat::test_that("extend_transects_any_side() correct output columns", {

  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  GROUPING_ID        <- "id"
  # GROUPING_ID        <- "mainstem"
  NUM_OF_TRANSECTS   <- 3
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  flowlines <-
    flowlines %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID, GROUPING_ID)), 
      geom
    ) 
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <-
    transects %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = 25
    )

  
  extensions <- extend_transects_any_side(transects = transects, 
                            flowlines = flowlines, 
                            crosswalk_id = CROSSWALK_ID, 
                            cs_id = "cs_id",
                            grouping_id = CROSSWALK_ID
                            )
  
  correct_output_cols <- check_min_extended_transects_output_cols(extensions, 
                                                                  id = CROSSWALK_ID, 
                                                                  cs_id = CS_ID)
  testthat::expect_true(correct_output_cols)
  
})

# ---------------------------------------------------------------------------
# ---- test flags indicating changes in lengths ----
# ---- test length changes vs the starting lengths ----
# ---------------------------------------------------------------------------

testthat::test_that("extend_transects_any_side() test that any transects labeled as 'left/right_is_extended' is longer than the corresponding starting transect", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  GROUPING_ID        <- "id"
  # GROUPING_ID        <- "mainstem"
  NUM_OF_TRANSECTS   <- 3
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  flowlines <-
    flowlines %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID, GROUPING_ID)), 
      geom
    ) 
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <-
    transects %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = 25
    )
  
  
  extensions <- extend_transects_any_side(transects    = transects, 
                                          flowlines    = flowlines, 
                                          crosswalk_id = CROSSWALK_ID, 
                                          cs_id        = CS_ID,
                                          grouping_id  = CROSSWALK_ID
                                          )
  
  correct_output_cols <- check_min_extended_transects_output_cols(extensions, 
                                                                  id = CROSSWALK_ID, 
                                                                  cs_id = CS_ID)
  testthat::expect_true(correct_output_cols)
  
  # is_extended <- 
  #   extensions %>% 
  #   dplyr::filter(left_is_extended | right_is_extended) 
  
  # get the starting vs ending lengths of each transect and compare to make sure ending is longer than starting
  lengths_df <-
    dplyr::left_join(
      # get the starting lengths
      transects %>% 
        dplyr::mutate(
          start_length = as.numeric(sf::st_length(sf::st_geometry(.)))
        ) %>% 
        sf::st_drop_geometry() %>% 
        dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, start_length),
      
      # get the extended lengths
      extensions %>% 
          dplyr::filter(
            left_is_extended | right_is_extended
          ) %>% 
          dplyr::mutate(
            end_length = as.numeric(sf::st_length(sf::st_geometry(.)))
          ) %>% 
          sf::st_drop_geometry() %>% 
          dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, end_length),
      by = c(CROSSWALK_ID, "cs_id")
    ) %>% 
    dplyr::filter(!is.na(start_length) & !is.na(end_length)) %>% 
    dplyr::mutate(
      end_is_longer_than_start = end_length >= start_length
    )
  
  all_extensions_longer_than_starting_transects <- all(lengths_df$end_is_longer_than_start)
  testthat::expect_true(all_extensions_longer_than_starting_transects)
    
})

testthat::test_that("extend_transects_any_side() test that no transect is extended if extension_distance is 0", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  GROUPING_ID        <- "id"
  # GROUPING_ID        <- "mainstem"
  NUM_OF_TRANSECTS   <- 3
  EXTENSION_DIST     <- 0
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  flowlines <-
    flowlines %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID, GROUPING_ID)), 
      geom
    ) 
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <-
    transects %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = EXTENSION_DIST
    )
  
  
  extensions <- extend_transects_any_side(transects    = transects, 
                                          flowlines    = flowlines, 
                                          crosswalk_id = CROSSWALK_ID, 
                                          cs_id        = CS_ID,
                                          grouping_id  = CROSSWALK_ID
                                          )
  
  # make sure minimum required output columns
  correct_output_cols <- check_min_extended_transects_output_cols(extensions, 
                                                                  id = CROSSWALK_ID, 
                                                                  cs_id = CS_ID)
  testthat::expect_true(correct_output_cols)
  
  # make sure than no transect was marked as being extended left OR right
  no_transect_is_extended_to_right <- all(!extensions$right_is_extended)
  no_transect_is_extended_to_left  <- all(!extensions$left_is_extended)
  
  testthat::expect_true(no_transect_is_extended_to_right)
  testthat::expect_true(no_transect_is_extended_to_left)
  
  
  # get the starting vs ending lengths of each transect and compare to make sure ending transect is less than or equal to the starting transects length
  lengths_df <-
    dplyr::left_join(
      # get the starting lengths
      transects %>% 
        dplyr::mutate(
          start_length = as.numeric(sf::st_length(sf::st_geometry(.)))
        ) %>% 
        sf::st_drop_geometry() %>% 
        dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, start_length),
      
      # get the extended lengths
      extensions %>% 
        dplyr::mutate(
          end_length = as.numeric(sf::st_length(sf::st_geometry(.)))
        ) %>% 
        sf::st_drop_geometry() %>% 
        dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, end_length),
      by = c(CROSSWALK_ID, "cs_id")
    ) %>% 
    dplyr::filter(!is.na(start_length) & !is.na(end_length)) %>% 
    dplyr::mutate(
      end_is_le_start = end_length <= start_length
    )
  
  # test to make sure the lengths of the ending transects are LESS THAN OR EQUAL to the starting transects
  no_transect_is_longer_than_it_started <- all(lengths_df$end_is_le_start)
  testthat::expect_true(no_transect_is_longer_than_it_started)
  
})

# ---------------------------------------------------------
# ---- Error and invalid input parameter tests ----
# ---------------------------------------------------------
testthat::test_that("extend_transects_any_side() incorrect input columns in FLOWLINES throws an error", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  GROUPING_ID        <- "id"
  # GROUPING_ID        <- "mainstem"
  NUM_OF_TRANSECTS   <- 3
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  flowlines <-
    flowlines %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID, GROUPING_ID)), 
      geom
    ) %>% 
    dplyr::mutate(
      extra_col = "test_col"
    ) %>% 
    hydroloom::rename_geometry("geometry")
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <-
    transects %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = 25
    )
  
  
  req_cols            <- unique(c(CROSSWALK_ID, GROUPING_ID))
  df_cols      <- names(flowlines)
  
  for (i in seq_along(req_cols)) {
    # message(i)
    col_to_ignore       <- req_cols[i]
    select_col_indexes  <- which(df_cols != col_to_ignore)
    
    # message(i, " - Ignoring column > '", col_to_ignore, "'") 
    
    # missing cols = ERROR
    testthat::expect_error(
      extend_transects_any_side(
        transects = transects, 
        flowlines = flowlines[ , select_col_indexes], # select all columns except the current REQUIRED column (force throws an error)
        crosswalk_id = CROSSWALK_ID, 
        cs_id        = CS_ID,
        grouping_id  = GROUPING_ID
      )
    )
  }
  
})

testthat::test_that("extend_transects_any_side() incorrect input columns in TRANSECTS throws an error", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  GROUPING_ID        <- "id"
  # GROUPING_ID        <- "mainstem"
  NUM_OF_TRANSECTS   <- 3
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  flowlines <-
    flowlines %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID, GROUPING_ID)), 
      geom
    ) %>% 
    dplyr::mutate(
      extra_col = "test_col"
    ) %>% 
    hydroloom::rename_geometry("geometry")
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <-
    transects %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = 25
    )
  
  req_cols            <- unique(c(CROSSWALK_ID, CS_ID, GROUPING_ID, 
                                  "extension_distance"
                                  # "geometry"
                                  ))
  
  # req_cols            <- unique(c(CROSSWALK_ID, GROUPING_ID))
  df_cols      <- names(transects)
  
  for (i in seq_along(req_cols)) {
    # i = 2
    col_to_ignore       <- req_cols[i]
    select_col_indexes  <- which(df_cols != col_to_ignore)
    message(i, " - Ignoring column > '", col_to_ignore, "'") 
   
    # missing cols = ERROR
    testthat::expect_error(
      extend_transects_any_side(
        transects     = transects[, select_col_indexes],  # select all columns except the current REQUIRED column (force throws an error)
        flowlines     = flowlines,
        crosswalk_id  = CROSSWALK_ID, 
        cs_id         = CS_ID,
        grouping_id   = GROUPING_ID
      )
    )
  }
  
})


testthat::test_that("extend_transects_any_side() mismatched CROSSWALK_ID column names in flowlines vs. transects throws an error (Flowlines has incorrect CROSSWALK_ID column)", {
  
  CROSSWALK_ID            <- "id"
  MISMATCH_CROSSWALK_ID   <- "fake_id"
  CS_ID                   <- "cs_id"
  GROUPING_ID             <- "id"
  # GROUPING_ID        <- "mainstem"
  NUM_OF_TRANSECTS   <- 3
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  flowlines <-
    flowlines %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID, GROUPING_ID)), 
      geom
    ) %>% 
    hydroloom::rename_geometry("geometry") 
    # dplyr::rename(!!sym(id_col) := id) 
    # dplyr::rename(!!sym(BAD_CROSSWALK_ID) := !!sym(CROSSWALK_ID))
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <-
    transects %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = 25
    ) 
  
  flowlines <- 
    flowlines %>% 
    dplyr::rename(!!sym(MISMATCH_CROSSWALK_ID) := !!sym(CROSSWALK_ID))
  
  testthat::expect_error(
    extend_transects_any_side(
      transects     = transects,  # select all columns except the current REQUIRED column (force throws an error)
      flowlines     = flowlines,
      crosswalk_id  = CROSSWALK_ID, 
      cs_id         = CS_ID,
      grouping_id   = CROSSWALK_ID
    )
  )
  
})


testthat::test_that("extend_transects_any_side() mismatched CROSSWALK_ID column names in flowlines vs. transects throws an error (TRANSECTS has incorrect CROSSWALK_ID column)", {
  
  CROSSWALK_ID            <- "id"
  MISMATCH_CROSSWALK_ID   <- "fake_id"
  CS_ID                   <- "cs_id"
  GROUPING_ID             <- "id"
  # GROUPING_ID        <- "mainstem"
  NUM_OF_TRANSECTS   <- 3
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  flowlines <-
    flowlines %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID, GROUPING_ID)), 
      geom
    ) %>% 
    hydroloom::rename_geometry("geometry") 
  # dplyr::rename(!!sym(id_col) := id) 
  # dplyr::rename(!!sym(BAD_CROSSWALK_ID) := !!sym(CROSSWALK_ID))
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <-
    transects %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = 25
    ) 
  
  transects <- 
    transects %>% 
    dplyr::rename(!!sym(MISMATCH_CROSSWALK_ID) := !!sym(CROSSWALK_ID))
  
  testthat::expect_error(
    extend_transects_any_side(
      transects     = transects,  # select all columns except the current REQUIRED column (force throws an error)
      flowlines     = flowlines,
      crosswalk_id  = CROSSWALK_ID, 
      cs_id         = CS_ID,
      grouping_id   = CROSSWALK_ID
    )
  )
  
})


# ---------------------------------------------------------------------------
# ---- test that left OR right can be extended (doesn't need to be both) ----
# ---------------------------------------------------------------------------

testthat::test_that("extend_transects_any_side() test that a transect can be extended from 1 side and not the other side", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
                  ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  # create_v_line(lat = LAT, 
  #               lon = LON, 
  #               crs = 4326
  # ) %>% 
  #   dplyr::mutate(
  #     id = 1:dplyr::n()
  #   ) %>% 
  #   sf::st_transform(5070) %>%
  #   sf::st_crs() %>% 
  #   names()
  
  transects <- cut_cross_sections(
      net = flowlines,
      id  = CROSSWALK_ID,  
      num = 10
    ) %>% 
    dplyr::filter(cs_id == 4) %>% 
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = 100
    )
  
  extensions <- extend_transects_any_side(transects    = transects, 
                                          flowlines    = flowlines, 
                                          crosswalk_id = CROSSWALK_ID, 
                                          cs_id        = CS_ID,
                                          grouping_id  = CROSSWALK_ID
  )
  # extensions_any <- extend_transects_any_side(transects    = transects, 
  #                                         flowlines    = flowlines, 
  #                                         crosswalk_id = CROSSWALK_ID, 
  #                                         cs_id        = CS_ID,
  #                                         grouping_id  = CROSSWALK_ID
  # )
  # extensions_both <- extend_transects_both_sides(transects    = transects, 
  #                                         flowlines    = flowlines, 
  #                                         crosswalk_id = CROSSWALK_ID, 
  #                                         cs_id        = CS_ID,
  #                                         grouping_id  = CROSSWALK_ID
  # )
  # plot(flowlines$geometry)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  # # 
  # mapview::mapview(flowlines) +
  #   # mapview::mapview(extensions, color = "red") +
  #   mapview::mapview(extensions_any, color = "red") +
  #   mapview::mapview(extensions_both, color = "red") +
  #   mapview::mapview(transects, color = "green")
  
  left_is_extended  <- extensions$left_is_extended
  right_is_extended <- extensions$right_is_extended
  
  # left gets extended but right does NOT
  testthat::expect_true(left_is_extended)
  testthat::expect_false(right_is_extended)
  
})

testthat::test_that("extend_transects_any_side() test that both sides of a transect will be extended if they're both valid extensions", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  # create_v_line(lat = LAT, 
  #               lon = LON, 
  #               crs = 4326
  # ) %>% 
  #   dplyr::mutate(
  #     id = 1:dplyr::n()
  #   ) %>% 
  #   sf::st_transform(5070) %>%
  #   sf::st_crs() %>% 
  #   names()
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(cs_id == 4) %>% 
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = 25
    )
  
  extensions <- extend_transects_any_side(transects    = transects, 
                                          flowlines    = flowlines, 
                                          crosswalk_id = CROSSWALK_ID, 
                                          cs_id        = CS_ID,
                                          grouping_id  = CROSSWALK_ID
  )
  # extensions_any <- extend_transects_any_side(transects    = transects, 
  #                                         flowlines    = flowlines, 
  #                                         crosswalk_id = CROSSWALK_ID, 
  #                                         cs_id        = CS_ID,
  #                                         grouping_id  = CROSSWALK_ID
  # )
  # extensions_both <- extend_transects_both_sides(transects    = transects, 
  #                                         flowlines    = flowlines, 
  #                                         crosswalk_id = CROSSWALK_ID, 
  #                                         cs_id        = CS_ID,
  #                                         grouping_id  = CROSSWALK_ID
  # )
  # plot(flowlines$geometry)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)

  
  # mapview::mapview(flowlines) +
  #   mapview::mapview(extensions, color = "red") +
  # #   mapview::mapview(extensions_any, color = "red") +
  # #   mapview::mapview(extensions_both, color = "red") +
  #   mapview::mapview(transects, color = "green")
  
  left_is_extended  <- extensions$left_is_extended
  right_is_extended <- extensions$right_is_extended
  
  # left gets extended but right does NOT
  testthat::expect_true(left_is_extended)
  testthat::expect_true(right_is_extended)
  
})

testthat::test_that("extend_transects_any_side() test that both sides can be extended even if one of the sides has to be cut in half in order to not intersect flowline more than once", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 50
  HALF_EXT_DIST <- EXT_DIST / 2
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  # create_v_line(lat = LAT, 
  #               lon = LON, 
  #               crs = 4326
  # ) %>% 
  #   dplyr::mutate(
  #     id = 1:dplyr::n()
  #   ) %>% 
  #   sf::st_transform(5070) %>%
  #   sf::st_crs() %>% 
  #   names()
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(cs_id == 4) %>% 
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
    )
  
  extensions <- extend_transects_any_side(transects    = transects, 
                                          flowlines    = flowlines, 
                                          crosswalk_id = CROSSWALK_ID, 
                                          cs_id        = CS_ID,
                                          grouping_id  = CROSSWALK_ID
  )
  # extensions_any <- extend_transects_any_side(transects    = transects, 
  #                                         flowlines    = flowlines, 
  #                                         crosswalk_id = CROSSWALK_ID, 
  #                                         cs_id        = CS_ID,
  #                                         grouping_id  = CROSSWALK_ID
  # )
  # extensions_both <- extend_transects_both_sides(transects    = transects, 
  #                                         flowlines    = flowlines, 
  #                                         crosswalk_id = CROSSWALK_ID, 
  #                                         cs_id        = CS_ID,
  #                                         grouping_id  = CROSSWALK_ID
  # )
  # plot(flowlines$geometry)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  #
  
  # mapview::mapview(flowlines) +
  #   mapview::mapview(extensions, color = "red") +
  # #   mapview::mapview(extensions_any, color = "red") +
  # #   mapview::mapview(extensions_both, color = "red") +
  #   mapview::mapview(transects, color = "green")
  
  left_is_extended  <- extensions$left_is_extended
  right_is_extended <- extensions$right_is_extended
  
  # both sides get extended
  testthat::expect_true(left_is_extended)
  testthat::expect_true(right_is_extended)
  
  full_left_extension  <- extensions$left_distance == EXT_DIST
  half_right_extension <- extensions$right_distance == HALF_EXT_DIST
  
  # both sides get extended but right side is only extended halfway
  testthat::expect_true(full_left_extension)
  testthat::expect_true(half_right_extension)
  
})


testthat::test_that("extend_transects_any_side() test that only 1 side will be extended when the otherside will hit a flowline twice at both the full extension distance AND at half of the full extension distance", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 100
  HALF_EXT_DIST <- EXT_DIST / 2
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  # create_v_line(lat = LAT, 
  #               lon = LON, 
  #               crs = 4326
  # ) %>% 
  #   dplyr::mutate(
  #     id = 1:dplyr::n()
  #   ) %>% 
  #   sf::st_transform(5070) %>%
  #   sf::st_crs() %>% 
  #   names()
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(cs_id == 4) %>% 
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
    )
  
  extensions <- extend_transects_any_side(transects    = transects, 
                                          flowlines    = flowlines, 
                                          crosswalk_id = CROSSWALK_ID, 
                                          cs_id        = CS_ID,
                                          grouping_id  = CROSSWALK_ID
  )
  
  # # plot(flowlines$geometry)
  # # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # # plot(transects$geometry, col = "green", lwd = 2, add = T)
  # # 
  
  # mapview::mapview(flowlines) +
  #   mapview::mapview(extensions, color = "red") +
  # #   mapview::mapview(extensions_any, color = "red") +
  # #   mapview::mapview(extensions_both, color = "red") +
  #   mapview::mapview(transects, color = "green")
  
  # extensions$left_distance
  # extensions$right_distance
  
  left_is_extended  <- extensions$left_is_extended
  right_is_extended <- extensions$right_is_extended
  
  # both sides get extended
  testthat::expect_true(left_is_extended)
  testthat::expect_false(right_is_extended)
  
  full_left_extension     <- extensions$left_distance == EXT_DIST
  right_extension_is_zero <- extensions$right_distance == 0
  
  # both sides get extended but right side is only extended halfway
  testthat::expect_true(full_left_extension)
  testthat::expect_true(right_extension_is_zero)
  
})

