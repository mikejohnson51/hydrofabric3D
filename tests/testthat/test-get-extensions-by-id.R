
library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::get_extensions_by_id() ----
# -------------------------------------------------------------------
testthat::test_that("get_extensions_by_id() correct output columns - default inputs", {
  # transect_lines, 
  # polygons, 
  # flowlines, 
  # crosswalk_id = 'hy_id',  
  # grouping_id = 'mainstem',
  # max_extension_distance = 3000
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  

  
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  # buff <- 
  #   flowlines %>% 
  #   sf::st_buffer(200) 
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  # LENGTH_COL_NAME    <- "cs_lengthm"
  # PCT <- 0.5 
  NUM_OF_TRANSECTS  <- 3
  MAX_EXT_DIST      <- 3000
  BUFF_DIST         <- 200 
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      # tot_drainage_areasqkm,
      # bf_width,
      geom
    ) 
  
  # create testiung polygons by buffering the flowlines
  polygons <- 
      flowlines %>%
      sf::st_buffer(BUFF_DIST)
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  # transects 
  transects <- 
    transects %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
      )
  
  
  ext_dists <- hydrofabric3D::get_extensions_by_id(
                  transects = transects,
                  polygons = polygons, 
                  crosswalk_id = CROSSWALK_ID, 
                  max_extension_distance = MAX_EXT_DIST
                )
  
  # minimum expected cols
  expected_cols <- c(CROSSWALK_ID, 
                     "cs_id", 
                     "left_distance", 
                     "right_distance")
  
  has_required_output_cols <- check_required_cols(ext_dists, expected_cols = expected_cols)
  testthat::expect_true(has_required_output_cols)
  
})

testthat::test_that("get_extensions_by_id() retains all unique transects (unique crosswalk_id / cs_id) - default inputs", {
  # transect_lines, 
  # polygons, 
  # flowlines, 
  # crosswalk_id = 'hy_id',  
  # grouping_id = 'mainstem',
  # max_extension_distance = 3000
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  
  
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  # buff <- 
  #   flowlines %>% 
  #   sf::st_buffer(200) 
  
  CROSSWALK_ID       <- "id"
  NUM_OF_TRANSECTS  <- 3
  MAX_EXT_DIST      <- 3000
  BUFF_DIST         <- 200 
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID )), 
      # tot_drainage_areasqkm,
      # bf_width,
      geom
    ) 
  
  # create testiung polygons by buffering the flowlines
  polygons <- 
    flowlines %>%
    sf::st_buffer(BUFF_DIST)
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  # transects 
  transects <- 
    transects %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )
  
  
  ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects = transects,
    polygons = polygons, 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  # hydrofabric3D::
  all_unique_ids_kept <- has_same_unique_tmp_ids(transects, ext_dists, id = CROSSWALK_ID)
  testthat::expect_true(all_unique_ids_kept)
  
})

testthat::test_that("get_extensions_by_id() retains all unique transects (unique crosswalk_id / cs_id) - not all transects have a polygon to extend out to", {

  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  CROSSWALK_ID       <- "id"
  NUM_OF_TRANSECTS   <- 3
  MAX_EXT_DIST       <- 3000
  BUFF_DIST          <- 200 
  
  flowlines <-
    flowlines %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID )), 
      geom
    ) 
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  # transects 
  transects <- 
    transects %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )
  
  # mapview::mapview(transects) + flowlines + polygons
  # # get only the relevent polygons/transects
  # transect_subset   <- hydrofabric3D:::subset_transects_in_polygons(transects, polygons)
  # polygons_subset   <- hydrofabric3D:::subset_polygons_in_transects(transects, polygons)
  # 
  # hydrofabric3D:::subset_transects_in_polygons()
  
  # get a dataframe that tells you how far to extend each line in either direction
  # extensions_by_id  <- get_extensions_by_id(transect_subset, polygons_subset, crosswalk_id, max_extension_distance)
  
  
  # transect_subset
  # polygons_subset 
  # crosswalk_id
  # max_extension_distance
  # 
  # # transects = transect_subset
  # # polygons = polygons_subset 
  # crosswalk_id = CROSSWALK_ID
  # max_extension_distance = MAX_EXT_DIST
  # 
  # left_partition <- partition_transects_for_extension(
  #   transects, 
  #   polygons, 
  #   dir = "left"
  # ) %>% 
  #   wrangle_paritioned_transects(
  #     dir          = "left", 
  #     crosswalk_id = crosswalk_id
  #   )
  # 
  # right_partition <- partition_transects_for_extension(
  #   transects, 
  #   polygons, 
  #   dir = "right"
  # ) %>% 
  #   wrangle_paritioned_transects(
  #     dir          = "right", 
  #     crosswalk_id = crosswalk_id
  #   )
  # 
  # # Convert the polygon to a MULTILINESTRING geometry for checking extension distances
  # mls <- sf_polygons_to_geos_multilinestrings(polygons, 200)
  # 
  # message("Generating left side distances....") 
  # left_distances <- calc_extension_distances(
  #   geos_geoms             = geos::as_geos_geometry(left_partition),
  #   ids                    = left_partition$tmp_id,
  #   lines_to_cut           = mls,
  #   lines_to_cut_indices   = left_partition$polygon_index,
  #   direction              = "head",
  #   max_extension_distance = max_extension_distance
  # )
  # 
  # message("Generating right side distances...")
  # right_distances <- calc_extension_distances(
  #   geos_geoms             = geos::as_geos_geometry(right_partition),
  #   ids                    = right_partition$tmp_id,
  #   lines_to_cut           = mls,
  #   lines_to_cut_indices   = right_partition$polygon_index,
  #   direction              = "tail",
  #   max_extension_distance = max_extension_distance
  # )
  # 
  # left_partition$left_distance   <- left_distances
  # right_partition$right_distance <- right_distances
  # 
  # # Distance to extend LEFT and/or RIGHT for each hy_id/cs_id
  # extensions_by_id <- dplyr::left_join(
  #   sf::st_drop_geometry(
  #     dplyr::select(left_partition, 
  #                   dplyr::any_of(crosswalk_id),
  #                   cs_id,
  #                   left_distance
  #     )
  #   ),
  #   sf::st_drop_geometry(
  #     dplyr::select(right_partition, 
  #                   dplyr::any_of(crosswalk_id),
  #                   cs_id, 
  #                   right_distance
  #     )
  #   ),
  #   by = c(crosswalk_id, "cs_id")
  # )
  # 
  # # add any missing crosswalk_id/cs_id that didnt have any extension distance w/ values of 0
  # extensions_by_id <- dplyr::bind_rows(
  #                       extensions_by_id, 
  #                       transects %>% 
  #                         sf::st_drop_geometry() %>% 
  #                         hydrofabric3D::add_tmp_id(x = crosswalk_id) %>% 
  #                         dplyr::filter(!tmp_id %in% hydrofabric3D::add_tmp_id(extensions_by_id, x = crosswalk_id)$tmp_id) %>% 
  #                         dplyr::select(-tmp_id) %>% 
  #                         dplyr::mutate(
  #                           left_distance  = 0,
  #                           right_distance = 0
  #                         )
  #                     ) 
  # 
  
  # # create testiung polygons by buffering the flowlines
  # polygons <- 
  #   flowlines %>%
  #   dplyr::slice(flowlines, 1) %>%
  #   sf::st_buffer( 
  #     dplyr::slice(flowlines, 1), 
  #     BUFF_DIST
  #     )
  
  # only 1 polygon, most transects are NOT extendable to the polygon, results in all unique crosswalk_id/cs_id IDs being kept
  ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects = transects,
    polygons  =  sf::st_buffer( 
                    dplyr::slice(flowlines, 1), 
                    BUFF_DIST
                  ), 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  # hydrofabric3D::
  all_unique_ids_kept <- has_same_unique_tmp_ids(transects, ext_dists, id = CROSSWALK_ID)
  testthat::expect_true(all_unique_ids_kept)
  
  # ALL transects have a corresponding polygon,  still results in all unique crosswalk_id/cs_id IDs being kept
  ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects = transects,
    polygons  =  sf::st_buffer(flowlines, BUFF_DIST), 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  # hydrofabric3D::
  all_unique_ids_kept <- has_same_unique_tmp_ids(transects, ext_dists, id = CROSSWALK_ID)
  testthat::expect_true(all_unique_ids_kept)
  
})


# buff <- 
#   flowlines %>% 
#   sf::st_buffer(200) 