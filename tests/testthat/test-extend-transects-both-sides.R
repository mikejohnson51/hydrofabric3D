library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::extend_transects_both_sides() ----
# -------------------------------------------------------------------
testthat::test_that("trans -> cs_pts -> extend trans -> new cs_pts -> compare diff -> updated trans + cs_pts", {
  
  # flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # 
  # # Transects inputs
  # CS_SOURCE            <- 'hydrofabric3D_test'
  # NUM_OF_TRANSECTS     <- 3
  # RM_SELF_INTERSECTS   <- TRUE
  # ID_COL               <- "hy_id"
  # 
  # # Cross section point inputs
  # DEM_PATH       <- testthat::test_path("testdata", "dem_flowlines.tif")
  # POINTS_PER_CS     <- NULL
  # MIN_PTS_PER_CS    <- 10
  # 
  # # NOTE: 0.01 (1%) of cros sections length is the default
  # # percent of cross section length (cs_lengthm) to use as the
  # # threshold depth for classifying whether a cross section has "relief"
  # PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF <- 0.01
  # 
  # EXTENSION_PCT = 0.5
  # 
  # flowlines <-
  #   flowlines %>%
  #   dplyr::mutate(
  #     bf_width        = hydrofabric3D::calc_powerlaw_bankful_width(tot_drainage_areasqkm),
  #     # bf_width        = hydrofabric3D::calc_powerlaw_bankful_width(tot_drainage_areasqkm),
  #     # bf_width        = pmax(50, bf_width * 2)
  #     bf_width        = pmax(50, bf_width * 11)
  #   ) %>%
  #   dplyr::select(
  #     hy_id = id,
  #     # tot_drainage_areasqkm,
  #     bf_width,
  #     # input_bf_width,
  #     geometry = geom
  #   )
  # # dplyr::slice(1)
  # 
  # # plot(flowlines$geometry)
  # 
  # transects <- hydrofabric3D::cut_cross_sections(
  #   net               = flowlines,
  #   crosswalk_id                = ID_COL,
  #   cs_widths         = flowlines$bf_width,
  #   num               = NUM_OF_TRANSECTS,
  #   rm_self_intersect = RM_SELF_INTERSECTS
  # )
  # 
  # mapview::mapview(transects, color = "green") + 
  #   mapview::mapview(flowlines, color = "red")
  # 
  # # select columns
  # transects <-
  #   transects %>%
  #   # dplyr::mutate(
  #   #   cs_source = CS_SOURCE
  #   # ) %>%
  #   dplyr::select(
  #     hy_id,
  #     cs_id,
  #     cs_lengthm,
  #     # cs_source,
  #     cs_measure,
  #     geometry
  #   )
  # 
  # # ----------------------------------------------------------------------------------------------------------------
  # # ---- Cross section points ----
  # # ----------------------------------------------------------------------------------------------------------------
  # 
  # # ---- STEP 1: Extract cs points from DEM ----
  # 
  # # get cross section point elevations
  # cs_pts <- hydrofabric3D::cross_section_pts(
  #   cs             = transects,
  #   crosswalk_id   = ID_COL,
  #   points_per_cs  = POINTS_PER_CS,
  #   min_pts_per_cs = MIN_PTS_PER_CS,
  #   dem            = DEM_PATH
  # )
  # 
  # # ----------------------------------------------------------------------------------------------------------------
  # # ---- STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points ----
  # # ----------------------------------------------------------------------------------------------------------------
  # 
  # # system.time({
  # NA_Z_COUNT <- sum(is.na(cs_pts$Z))
  # # cs_pts[!is.na(cs_pts$Z), ][[ID_COL]]
  # 
  # # STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points
  # cs_pts <-
  #   cs_pts %>%
  #   hydrofabric3D::drop_incomplete_cs_pts("hy_id")
  # 
  # classified_pts <- 
  #   cs_pts %>% 
  #   hydrofabric3D:::classify_points(
  #     crosswalk_id             = ID_COL, 
  #     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #   )
  # 
  # classified_pts %>% 
  #   sf::st_drop_geometry() %>% 
  #   dplyr::group_by(hy_id, cs_id) %>% 
  #   dplyr::slice(1) %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::select(hy_id, cs_id, valid_banks, has_relief)
  # 
  # transects <- hydrofabric3D:::add_cs_attributes_to_transects(
  #                       transects = transects,
  #                       cs_pts = classified_pts,
  #                       crosswalk_id = "hy_id"
  #                     )
  # 
  # extended <- hydrofabric3D:::extend_transects_by_cs_attributes(flowlines = flowlines,
  #                                                   transects = transects, 
  #                                                   crosswalk_id = "hy_id", 
  #                                                   scale = 0.5,
  #                                                   keep_lengths = TRUE
  #                                                   )
  # # get cross section point elevations
  # new_cs_pts <- hydrofabric3D::cross_section_pts(
  #   cs             = extended,
  #   crosswalk_id   = ID_COL,
  #   points_per_cs  = POINTS_PER_CS,
  #   min_pts_per_cs = MIN_PTS_PER_CS,
  #   dem            = DEM_PATH
  # ) %>%
  #   hydrofabric3D::drop_incomplete_cs_pts("hy_id") %>% 
  #   hydrofabric3D:::classify_points(
  #     crosswalk_id             = "hy_id", 
  #     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #   )
  # 
  # 
  # get_validity_tally(classified_pts, "hy_id")
  # get_validity_tally(new_cs_pts, "hy_id")
  # 
  # old_validity_scores <- calc_validity_scores(classified_pts, "hy_id")
  # new_validity_scores <- calc_validity_scores(new_cs_pts, "hy_id")
  # 
  # # mark as "improved" for any hy_id/cs_ids that increased "validity score" after extending
  # check_for_improvement <- dplyr::left_join(
  #   dplyr::select(
  #     dplyr::filter(old_validity_scores, tmp_id %in% unique(new_validity_scores$tmp_id)),  
  #     dplyr::any_of(crosswalk_id), cs_id, old_validity_score
  #   ), 
  #   dplyr::select(
  #     new_validity_scores, 
  #     dplyr::any_of(crosswalk_id), cs_id, new_validity_score
  #   ),
  #   by = c(crosswalk_id, "cs_id")
  #   # by = c("hy_id", "cs_id")
  # ) %>% 
  #   dplyr::mutate(
  #     improved = dplyr::case_when(
  #       new_validity_score > old_validity_score ~ TRUE,
  #       TRUE                                    ~ FALSE
  #     )
  #   ) %>% 
  #   dplyr::select(dplyr::any_of(crosswalk_id), cs_id, improved)
  # 
  # get_validity_tally <- function(x, crosswalk_id = NULL) {
  #     # x <- classified_pts
  #     # crosswalk_id = "hy_id"
  # 
  #     validity_tally <- 
  #       x %>% 
  #       sf::st_drop_geometry() %>% 
  #       dplyr::select(dplyr::any_of(crosswalk_id), cs_id, valid_banks, has_relief) %>% 
  #       dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
  #       dplyr::slice(1) %>% 
  #       dplyr::ungroup() %>% 
  #       dplyr::count(valid_banks, has_relief)
  #     
  #     return(validity_tally)
  #   
  # }
  # cs_pts
  # new_cs_pts
  # 
  
  })


testthat::test_that("extend_transects_both_sides() test that both sides of a transect will be extended if they're both valid extensions", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST   <- 25
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(cs_id == 4) %>% 
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
    )
 
  # transects$geometry %>% plot()
  # shorter <- hydrofabric3D::geos_extend_line(
  #   line = geos::as_geos_geometry(transects),
  #   distance = -110,
  #   dir = "both"
  #   ) %>% 
  #   sf::st_as_sf()
  # 
  # 
  # length(transects)
  # transects$geometry %>% sf::st_length()
  # sf::st_length(shorter)
  # plot(transects$geometry)
  # plot(shorter, add = T, col = "red")
  
  extensions <- extend_transects_both_sides(transects    = transects, 
                                          flowlines    = flowlines, 
                                          crosswalk_id = CROSSWALK_ID, 
                                          cs_id        = CS_ID,
                                          grouping_id  = CROSSWALK_ID
  )
  
  # plot(flowlines$geometry)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  # # 
  
  # mapview::mapview(flowlines) +
  #   mapview::mapview(extensions, color = "red") +
  # #   mapview::mapview(extensions_any, color = "red") +
  # #   mapview::mapview(extensions_both, color = "red") +
  #   mapview::mapview(transects, color = "green")
  
  left_is_extended  <- extensions$left_is_extended
  right_is_extended <- extensions$right_is_extended
  
  # both sides get extended as both are valid
  testthat::expect_true(left_is_extended)
  testthat::expect_true(right_is_extended)
  
})

testthat::test_that("extend_transects_both_sides() test that both neither side of a transect will be extended if they're both INVALID extensions", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 500
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(cs_id == 4) %>% 
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
    )
  
  
  extensions <- extend_transects_both_sides(transects    = transects, 
                                          flowlines    = flowlines, 
                                          crosswalk_id = CROSSWALK_ID, 
                                          cs_id        = CS_ID,
                                          grouping_id  = CROSSWALK_ID
  )
  
  
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
  
  # Neither side gets extended because the right side hits a flowline twice
  testthat::expect_false(left_is_extended)
  testthat::expect_false(right_is_extended)
  
})

testthat::test_that("extend_transects_both_sides() test extensions will only go out to the maximum specified distance and no further", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 500
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(cs_id == 1) %>%
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
    )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)

  
  extensions <- extend_transects_both_sides(transects    = transects, 
                                            flowlines    = flowlines, 
                                            crosswalk_id = CROSSWALK_ID, 
                                            cs_id        = CS_ID,
                                            grouping_id  = CROSSWALK_ID
  )
  
  # plot(flowlines$geometry)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  # 
  
  # mapview::mapview(flowlines) +
  #   mapview::mapview(extensions, color = "red") +
  # #   mapview::mapview(extensions_any, color = "red") +
  # #   mapview::mapview(extensions_both, color = "red") +
  #   mapview::mapview(transects, color = "green")
  
  # has_correct_left_extension_dist_value   <- extensions$left_distance == EXT_DIST
  # has_correct_right_extension_dist_value  <- extensions$right_distance == EXT_DIST
  
  # both the left and right side have the correct extension distance 
  testthat::expect_equal(extensions$left_distance, EXT_DIST)
  testthat::expect_equal( extensions$right_distance, EXT_DIST)
  
  starting_length   <- as.numeric(sf::st_length(transects))
  new_total_length  <- as.numeric(sf::st_length(extensions))
  testthat::expect_equal(starting_length + (EXT_DIST * 2), new_total_length)
  
  # (starting_length + (EXT_DIST * 2)) == new_total_length
  
})

testthat::test_that("extend_transects_both_sides() test extension will not happen for a transect either side, if one of the extensions intersects with another transect (before the other transect was extended, i.e. an original width transect)", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 500
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(
      cs_id %in% c(2, 6)
      # cs_id %in% c(5, 6)
      # cs_id %in% c(5, 6, 7, 8)
      ) %>%
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
    )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  
  
  extensions <- extend_transects_both_sides(transects    = transects, 
                                            flowlines    = flowlines, 
                                            crosswalk_id = CROSSWALK_ID, 
                                            cs_id        = CS_ID,
                                            grouping_id  = CROSSWALK_ID
  )
  
  # plot(flowlines$geometry)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  
  # the first transect gets to be extended
  testthat::expect_true(
    extensions %>% 
      dplyr::filter(cs_id == 1) %>% 
      dplyr::pull(left_is_extended, right_is_extended) %>% 
      all()
  )
  
  # the second transect does NOT get extended because it would interesect the other transect (even before it was extended)
  testthat::expect_false(
    extensions %>% 
      dplyr::filter(cs_id == 2) %>% 
      dplyr::pull(left_is_extended, right_is_extended) %>% 
      all()
  )
  
})

testthat::test_that("extend_transects_both_sides() test extension will happen for an upstream transect, 
                    which then forces downstream flowline to NOT be extended because it would hit the newly extended upstream transect", {
  
  CROSSWALK_ID       <- "id"
  CS_ID              <- "cs_id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  # EXT_DIST <- 1
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 10
  ) %>% 
    dplyr::filter(
      # cs_id %in% c(1,2, 3, 6)
      cs_id %in% c(3, 6)
      # cs_id %in% c(5, 6)
      # cs_id %in% c(5, 6, 7, 8)
    ) %>%
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
    dplyr::mutate(
      extension_distance = c(10, 25)
    )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  
  extensions <- extend_transects_both_sides(transects    = transects, 
                                            flowlines    = flowlines, 
                                            crosswalk_id = CROSSWALK_ID, 
                                            cs_id        = CS_ID,
                                            grouping_id  = CROSSWALK_ID
  )
  
  
  # plot(flowlines$geometry, add = F)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  
  # plot(extensions$geometry, col = "black", lwd = 4, add = F)
  # plot(flowlines$geometry, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  
  # the first transect gets to be extended
  testthat::expect_true(
    extensions %>% 
      dplyr::filter(cs_id == 1) %>% 
      dplyr::pull(left_is_extended, right_is_extended) %>% 
      all()
  )
  
  # the second transect does NOT get extended because it would interesect the other transect (even before it was extended)
  testthat::expect_false(
    extensions %>% 
      dplyr::filter(cs_id == 2) %>% 
      dplyr::pull(left_is_extended, right_is_extended) %>% 
      all()
  )
  
})

testthat::test_that("extend_transects_both_sides() test extension will happen for a downstream transect, 
                    when it does not intersect an upstream transect even if the upstream transect was extended but not far enough to hit downstream transect", {
                      
                      CROSSWALK_ID       <- "id"
                      CS_ID              <- "cs_id"
                      LAT        <- 34.41249
                      LON        <- -119.74095
                      CRS_OF_PT  <- 4326
                      # EXT_DIST <- 1
                      
                      flowlines <- 
                        create_v_line(lat = LAT, 
                                      lon = LON, 
                                      crs = CRS_OF_PT
                        ) %>% 
                        dplyr::mutate(
                          id = 1:dplyr::n()
                        ) %>% 
                        sf::st_transform(5070)
                      
                      transects <- cut_cross_sections(
                        net = flowlines,
                        crosswalk_id  = CROSSWALK_ID,  
                        num = 10
                      ) %>% 
                        dplyr::filter(
                          # cs_id %in% c(1,2, 3, 6)
                          cs_id %in% c(3, 6)
                          # cs_id %in% c(5, 6)
                          # cs_id %in% c(5, 6, 7, 8)
                        ) %>%
                        hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
                        dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
                        dplyr::mutate(
                          extension_distance = c(1, 25)
                        )
                      
                      # plot(flowlines$geometry)
                      # plot(transects$geometry, col = "green", lwd = 2, add = T)
                      
                      extensions <- extend_transects_both_sides(transects    = transects, 
                                                                flowlines    = flowlines, 
                                                                crosswalk_id = CROSSWALK_ID, 
                                                                cs_id        = CS_ID,
                                                                grouping_id  = CROSSWALK_ID
                      )
                      
                      
                      # plot(flowlines$geometry, add = F)
                      # plot(extensions$geometry, col = "black", lwd = 4, add = T)
                      # plot(transects$geometry, col = "green", lwd = 2, add = T)
                      
                      # plot(extensions$geometry, col = "black", lwd = 4, add = F)
                      # plot(flowlines$geometry, add = T)
                      # plot(transects$geometry, col = "green", lwd = 2, add = T)
                      
                      testthat::expect_true(
                        extensions %>% 
                          dplyr::filter(cs_id == 1) %>% 
                          dplyr::pull(left_is_extended, right_is_extended) %>% 
                          all()
                      )
                      
                      testthat::expect_true(
                        extensions %>% 
                          dplyr::filter(cs_id == 2) %>% 
                          dplyr::pull(left_is_extended, right_is_extended) %>% 
                          all()
                      )
                      
                    })

testthat::test_that("extend_transects_both_sides() test extension wont happen for either transect because the 
                    extended version of both transects will result in an intersection of the transects", {
                      
                      CROSSWALK_ID       <- "id"
                      CS_ID              <- "cs_id"
                      LAT        <- 34.41249
                      LON        <- -119.74095
                      CRS_OF_PT  <- 4326
                      EXT_DIST <- 150
                      
                      flowlines <- 
                        create_v_line(lat = LAT, 
                                      lon = LON, 
                                      crs = CRS_OF_PT
                        ) %>% 
                        dplyr::mutate(
                          id = 1:dplyr::n()
                        ) %>% 
                        sf::st_transform(5070)
                      
                      transects <- cut_cross_sections(
                        net = flowlines,
                        crosswalk_id  = CROSSWALK_ID,  
                        num = 10
                      ) %>% 
                        dplyr::filter(
                          cs_id %in% c(2, 5, 6)
                          # cs_id %in% c(3, 6)
                          # cs_id %in% c(5, 6)
                          # cs_id %in% c(5, 6, 7, 8)
                        ) %>%
                        hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
                        dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
                        dplyr::mutate(
                          extension_distance = EXT_DIST
                        )
                      # plot(flowlines$geometry)
                      # plot(transects$geometry, col = "green", lwd = 2, add = T)
                      
                      extensions <- extend_transects_both_sides(transects    = transects, 
                                                                flowlines    = flowlines, 
                                                                crosswalk_id = CROSSWALK_ID, 
                                                                cs_id        = CS_ID,
                                                                grouping_id  = CROSSWALK_ID
                      )
                      
                      
                      # plot(flowlines$geometry, add = F)
                      # plot(extensions$geometry, col = "black", lwd = 4, add = T)
                      # plot(transects$geometry, col = "green", lwd = 2, add = T)
                      # 
                      # plot(extensions$geometry, col = "black", lwd = 4, add = F)
                      # plot(flowlines$geometry, add = T)
                      # plot(transects$geometry, col = "green", lwd = 2, add = T)
                      
                      testthat::expect_false(
                        extensions %>% 
                          dplyr::filter(cs_id == 2) %>% 
                          dplyr::pull(left_is_extended, right_is_extended) %>% 
                          all()
                      )
                      
                      testthat::expect_false(
                        extensions %>% 
                          dplyr::filter(cs_id == 3) %>% 
                          dplyr::pull(left_is_extended, right_is_extended) %>% 
                          all()
                      )
                      
                    })

testthat::test_that("extend_transects_both_sides() test extension of downstream transect that would intersect multiple upstream transects", {
                      
                      CROSSWALK_ID       <- "id"
                      CS_ID              <- "cs_id"
                      LAT        <- 34.41249
                      LON        <- -119.74095
                      CRS_OF_PT  <- 4326
                      EXT_DIST <- 150
                      
                      flowlines <- 
                        create_v_line(lat = LAT, 
                                      lon = LON, 
                                      crs = CRS_OF_PT
                        ) %>% 
                        dplyr::mutate(
                          id = 1:dplyr::n()
                        ) %>% 
                        sf::st_transform(5070)
                      
                      transects <- cut_cross_sections(
                        net = flowlines,
                        crosswalk_id  = CROSSWALK_ID,  
                        num = 10
                      ) %>% 
                        dplyr::filter(
                          # cs_id %in% c(2, 5, 6)
                          # cs_id %in% c(3, 6)
                          # cs_id %in% c(5, 6)
                          cs_id %in% c(5, 6, 7, 8)
                        ) %>%
                        hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
                        dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID))) %>% 
                        dplyr::mutate(
                          extension_distance = EXT_DIST
                          # cs_id = dplyr::n():1
                        ) 
                        # dplyr::arrange(cs_id)
                      
                      # plot(flowlines$geometry)
                      # plot(transects$geometry, col = "green", lwd = 2, add = T)
                      # plot(transects$geometry[4], col = "orange", lwd = 2, add = T)
                      
                      extensions <- extend_transects_both_sides(transects    = transects, 
                                                                flowlines    = flowlines, 
                                                                crosswalk_id = CROSSWALK_ID, 
                                                                cs_id        = CS_ID,
                                                                grouping_id  = CROSSWALK_ID
                      )
                      
                      
                      # plot(flowlines$geometry, add = F)
                      # plot(extensions$geometry, col = "black", lwd = 4, add = T)
                      # plot(transects$geometry, col = "green", lwd = 2, add = T)
                      # 
                      # plot(extensions$geometry, col = "black", lwd = 4, add = F)
                      # plot(flowlines$geometry, add = T)
                      # plot(transects$geometry, col = "green", lwd = 2, add = T)
                      
                      testthat::expect_false(
                        extensions %>% 
                          dplyr::filter(cs_id == 1) %>% 
                          dplyr::pull(left_is_extended, right_is_extended) %>% 
                          all()
                      )
                      
                      testthat::expect_true(
                        extensions %>% 
                          dplyr::filter(cs_id == 2) %>% 
                          dplyr::pull(left_is_extended, right_is_extended) %>% 
                          all()
                      )
                      
                      testthat::expect_true(
                        extensions %>% 
                          dplyr::filter(cs_id == 3) %>% 
                          dplyr::pull(left_is_extended, right_is_extended) %>% 
                          all()
                      )
                      
                      testthat::expect_false(
                        extensions %>% 
                          dplyr::filter(cs_id == 4) %>% 
                          dplyr::pull(left_is_extended, right_is_extended) %>% 
                          all()
                      )
                      
                    })

