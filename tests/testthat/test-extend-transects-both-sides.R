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

testthat::test_that("extend_transects_both_sides() test no extensions on upstream transects that would intersect with the original
                    unexteneded transect lines, but the last transect is able to be extended because it doesn't interest any transects after extensions", {
  
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
    num = 20
  ) %>% 
    dplyr::filter(
      # cs_id %in% c(2, 5, 6)
      # cs_id %in% c(3, 6)
      # cs_id %in% c(5, 6)
      cs_id %in% c(4, 8, 9, 10, 14)
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
  # # 
  # mapview::mapview(transects, color = "red") +
  #   mapview::mapview(extensions, color = "green")
  # plot(extensions$geometry, col = "black", lwd = 4, add = F)
  # plot(flowlines$geometry, add = T)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  
  testthat::expect_false(
    extensions %>% 
      dplyr::filter(cs_id == 1) %>% 
      dplyr::pull(left_is_extended, right_is_extended) %>% 
      all()
  )
  
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
  
  testthat::expect_false(
    extensions %>% 
      dplyr::filter(cs_id == 4) %>% 
      dplyr::pull(left_is_extended, right_is_extended) %>% 
      all()
  )
  
  testthat::expect_true(
    extensions %>% 
      dplyr::filter(cs_id == 5) %>% 
      dplyr::pull(left_is_extended, right_is_extended) %>% 
      all()
  )
  
})

testthat::test_that("extend_transects_both_sides() when given transects that already contains intersecting transect lines, 
                    those already intersecting transects do NOT get extended because they are violating the interesection rules from the beginning", {
  
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
  
  transects1 <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 5
  ) %>% 
    dplyr::filter(
      cs_id %in% c(2, 3)
    ) %>%
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID, "cs_measure"))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
      # cs_id = dplyr::n():1
    ) 
  
  transects2 <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 30
  ) %>% 
    dplyr::filter(
      cs_id %in% c(6, 8, 9)
    ) %>%
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID, "cs_measure"))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
      # cs_id = dplyr::n():1
    ) 
  
  # plot(flowlines$geometry)
  # plot(transects1$geometry, col = "green", lwd = 5, add = T)
  # plot(transects2$geometry, col = "red", lwd = 2, add = T)
  
  merged_transects <- dplyr::bind_rows(
                          transects1,
                          transects2
                        ) %>% 
                          dplyr::arrange(cs_measure) %>% 
                          dplyr::mutate(
                            cs_id = 1:dplyr::n()
                          )
  
  # plot(flowlines$geometry)
  # plot(merged_transects$geometry, col = "green", lwd = 5, add = T)
  
  extensions <- extend_transects_both_sides(transects    = merged_transects, 
                                            flowlines    = flowlines, 
                                            crosswalk_id = CROSSWALK_ID, 
                                            cs_id        = CS_ID,
                                            grouping_id  = CROSSWALK_ID
  )
  
  # plot(flowlines$geometry, add = F)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(merged_transects$geometry, col = "green", lwd = 2, add = T)
  
  # none of the LEFT sides get extended 
  testthat::expect_false(
    extensions %>% 
      dplyr::pull(left_is_extended) %>% 
      all()
  )
  
  # none of the RIGHT sides get extended 
  testthat::expect_false(
    extensions %>% 
      dplyr::pull(right_is_extended) %>% 
      all()
  )
  
  
})

testthat::test_that("extend_transects_both_sides() when given transects that already contains intersecting transect lines, 
                    those already intersecting transects do NOT get extended because they are violating the interesection rules from the beginning
                    BUT any transects that can be extended are still extended as needed", {
                      
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
  
  transects1 <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 5
  ) %>% 
    dplyr::filter(
      cs_id %in% c(3)
    ) %>%
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID, "cs_measure"))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
      # cs_id = dplyr::n():1
    ) 
  
  transects2 <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 30
  ) %>% 
    dplyr::filter(
      cs_id %in% c(6, 8, 9)
    ) %>%
    hydrofabric3D:::renumber_cs_ids(crosswalk_id = CROSSWALK_ID) %>% 
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, CS_ID, "cs_measure"))) %>% 
    dplyr::mutate(
      extension_distance = EXT_DIST
      # cs_id = dplyr::n():1
    ) 
  
  # plot(flowlines$geometry)
  # plot(transects1$geometry, col = "green", lwd = 5, add = T)
  # plot(transects2$geometry, col = "red", lwd = 2, add = T)

  merged_transects <- dplyr::bind_rows(
    transects1,
    transects2
  ) %>% 
    dplyr::arrange(cs_measure) %>% 
    dplyr::mutate(
      cs_id = 1:dplyr::n()
    )
  
  # plot(flowlines$geometry)
  # plot(merged_transects$geometry, col = "green", lwd = 5, add = T)
  
  extensions <- extend_transects_both_sides(transects    = merged_transects, 
                                            flowlines    = flowlines, 
                                            crosswalk_id = CROSSWALK_ID, 
                                            cs_id        = CS_ID,
                                            grouping_id  = CROSSWALK_ID
  )
  
  # plot(flowlines$geometry, add = F)
  # plot(extensions$geometry, col = "black", lwd = 4, add = T)
  # plot(merged_transects$geometry, col = "green", lwd = 2, add = T)
  
  testthat::expect_true(
    extensions %>% 
      dplyr::filter(cs_id %in% c(1)) %>% 
      dplyr::pull(left_is_extended, right_is_extended) %>% 
      all()
  )
  
  # none of the LEFT sides get extended that violate the intersection rules
  testthat::expect_false(
    extensions %>% 
      dplyr::filter(!cs_id %in% c(1)) %>% 
      dplyr::pull(left_is_extended) %>% 
      all()
  )
  
  # none of the RIGHT sides get extended that violate the intersection rules
  testthat::expect_false(
    extensions %>% 
      dplyr::filter(!cs_id %in% c(1)) %>% 
      dplyr::pull(right_is_extended) %>% 
      all()
  )
  
  
  })
