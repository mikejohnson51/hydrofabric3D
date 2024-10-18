
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
    crosswalk_id  = CROSSWALK_ID,  
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
    crosswalk_id  = CROSSWALK_ID,  
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
  all_unique_ids_kept <- has_same_unique_tmp_ids(transects, ext_dists, crosswalk_id = CROSSWALK_ID)
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
    crosswalk_id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  )
  
  # transects 
  transects <- 
    transects %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )
  
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
  all_unique_ids_kept <- has_same_unique_tmp_ids(transects, ext_dists, crosswalk_id = CROSSWALK_ID)
  testthat::expect_true(all_unique_ids_kept)
  
  # ALL transects have a corresponding polygon,  still results in all unique crosswalk_id/cs_id IDs being kept
  ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects = transects,
    polygons  =  sf::st_buffer(flowlines, BUFF_DIST), 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  # hydrofabric3D::
  all_unique_ids_kept <- has_same_unique_tmp_ids(transects, ext_dists, crosswalk_id = CROSSWALK_ID)
  testthat::expect_true(all_unique_ids_kept)
  
})


testthat::test_that("get_extensions_by_id() polygon is farther away than max extension distance so transect only goes to max_extension_distance", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  CROSSWALK_ID       <- "id"
  NUM_OF_TRANSECTS   <- 3
  MAX_EXT_DIST       <- 10
  BUFF_DIST          <- 1000 
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID )), 
      geom
    ) 
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )
  
  # buffer flowline out by BUFF DIST
  buffed_flowlines <- sf::st_buffer(flowlines, BUFF_DIST)
  # mapview::mapview(buffed_flowlines) + transects + flowlines
  
  # only 1 polygon, most transects are NOT extendable to the polygon, results in all unique crosswalk_id/cs_id IDs being kept
  ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects    = transects,
    polygons     =  buffed_flowlines, 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  # make sure that because the polygon for the transects is WAY bigger than the max extension dist + starting transect length, 
  # then the transects are just extended to the max possible length because they never actually intersect the bounds of the
  left_side_of_transects_is_max_extended   <- all(ext_dists$left_distance == MAX_EXT_DIST)
  right_side_of_transects_is_max_extended  <- all(ext_dists$right_distance == MAX_EXT_DIST)
  
  testthat::expect_true(left_side_of_transects_is_max_extended)
  testthat::expect_true(right_side_of_transects_is_max_extended)
  
})

testthat::test_that("get_extensions_by_id() max extension distance is long enough that transects reach edge of polygons ", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  CROSSWALK_ID       <- "id"
  NUM_OF_TRANSECTS   <- 3
  CS_WIDTH           <- 100
  MAX_EXT_DIST       <- 1200
  BUFF_DIST          <- 1000 
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID )), 
      geom
    ) 
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS,
    cs_widths = CS_WIDTH
  ) %>% 
  dplyr::select(
    dplyr::any_of(c(CROSSWALK_ID)), cs_id
  )
  
  # buffer flowline out by BUFF DIST
  buffed_flowlines <- sf::st_buffer(flowlines, BUFF_DIST)
  # mapview::mapview(buffed_flowlines) + transects + flowlines
  
  # only 1 polygon, most transects are NOT extendable to the polygon, results in all unique crosswalk_id/cs_id IDs being kept
  ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects    = transects,
    polygons     =  buffed_flowlines, 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  # make sure that the transects get an extension distance less than the max because the max starting transect length + the max extension distance is much bigger 
  # than the actual distance that the polygon is from either end of the transects
  # (i.e. the left/right side of the transects do NOT need to go the full MAX EXTENSION DISTANCE to hit the edge of the polygon)
  left_side_of_transects_is_less_than_max_extended  <- all(ext_dists$left_distance < MAX_EXT_DIST)
  right_side_of_transects_is_less_than_max_extended <- all(ext_dists$right_distance < MAX_EXT_DIST)
  
  testthat::expect_true(left_side_of_transects_is_less_than_max_extended)
  testthat::expect_true(right_side_of_transects_is_less_than_max_extended)
  
})

testthat::test_that("get_extensions_by_id() transects that are already longer than and reach outside of the edge of a polygon get extension distance of 0", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  CROSSWALK_ID       <- "id"
  NUM_OF_TRANSECTS   <- 3
  CS_WIDTH           <- 100
  MAX_EXT_DIST       <- 200
  BUFF_DIST          <- 10
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID )), 
      geom
    ) 
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS,
    cs_widths = CS_WIDTH
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )
  
  # buffer flowline out by BUFF DIST
  buffed_flowlines <- sf::st_buffer(flowlines, BUFF_DIST)
  # mapview::mapview(buffed_flowlines) + transects + flowlines
  
  # only 1 polygon, most transects are NOT extendable to the polygon, results in all unique crosswalk_id/cs_id IDs being kept
  ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects    = transects,
    polygons     =  buffed_flowlines, 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  # make sure that transects that are already longer than a polygon they intersect with, do NOT get extended
  # TODO: maybe an ideal world would give a negative distance (i.e. shorten the transect lines)
  no_left_side_extension   <- all(ext_dists$left_distance == 0)
  no_right_side_extension  <- all(ext_dists$right_distance == 0)
  
  testthat::expect_true(no_left_side_extension)
  testthat::expect_true(no_right_side_extension)
  
})

testthat::test_that("get_extensions_by_id() transects intersect with 2 overlapping polygons (i.e. 2 layered polygons, 1 entirely encompassing the other, both fully encompass the transect lines)", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  CROSSWALK_ID       <- "id"
  NUM_OF_TRANSECTS   <- 3
  CS_WIDTH           <- 100
  MAX_EXT_DIST       <- 1200
  BUFF_DIST_1        <- 1000
  BUFF_DIST_2        <- BUFF_DIST_1 / 2
  
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID )), 
      geom
    ) 
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS,
    cs_widths = CS_WIDTH
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )
  
  # buffer flowline out by 2 different distances and merge them back together
  big_polygons   <- sf::st_buffer(flowlines, BUFF_DIST_1)
  small_polygons <- sf::st_buffer(flowlines, BUFF_DIST_2)
  
  buffed_flowlines <- dplyr::bind_rows(
    big_polygons,
    small_polygons
  )
  
  # mapview::mapview(buffered_flowlines) + transects + flowlines
  small_ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects    = transects,
    polygons     =  small_polygons, 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  all_ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects    = transects,
    polygons     =  buffed_flowlines, 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  # hardcoded correct extension distances to the smaller polygon
  expected_left_distance  <- c(475, 585, 328)
  expected_right_distance <- c(420, 441, 383)
  
  # make sure the algo just extends out to the smaller of the overlayed polygons
  left_side_transects_extend_only_to_closer_of_overlayed_polygons  <- all(expected_left_distance == all_ext_dists$left_distance)
  right_side_transects_extend_only_to_closer_of_overlayed_polygons <- all(expected_right_distance == all_ext_dists$right_distance)
   
  testthat::expect_true(left_side_transects_extend_only_to_closer_of_overlayed_polygons)
  testthat::expect_true(right_side_transects_extend_only_to_closer_of_overlayed_polygons)
  
  # TODO: Not sure if this is bad practice for tests to use the function they are testing to check for correctness....
  # TODO: these tests get the extension distances to JUST the smaller polygons and compare those distances to the extension distances calculated for the OVERLAYED polygons
  # TODO: They should be the same extension distances because the extenssion algo will use the first set of polygon edges it reaches
  # left_side_extensions_match_extensions_for_only_smaller_polygons   <- all(small_ext_dists$left_distance == all_ext_dists$left_distance)
  # right_side_extensions_match_extensions_for_only_smaller_polygons  <- all(small_ext_dists$right_distance == all_ext_dists$right_distance)
  # 
  # testthat::expect_true(left_side_extensions_match_extensions_for_only_smaller_polygons)
  # testthat::expect_true(right_side_extensions_match_extensions_for_only_smaller_polygons)
  
})

testthat::test_that("get_extensions_by_id() transects intersect with 2 overlapping polygons (i.e. 2 layered polygons, 1 entirely encompassing the other, neither encompass the transect lines) should result in 0 extension distance", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  CROSSWALK_ID       <- "id"
  NUM_OF_TRANSECTS   <- 3
  CS_WIDTH           <- 100
  MAX_EXT_DIST       <- 1200
  
  BUFF_DIST_1        <- 10
  BUFF_DIST_2        <- BUFF_DIST_1 / 2
  
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID )), 
      geom
    ) 
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS,
    cs_widths = CS_WIDTH
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )
  
  # buffer flowline out by 2 different distances and merge them back together
  big_polygons   <- sf::st_buffer(flowlines, BUFF_DIST_1)
  small_polygons <- sf::st_buffer(flowlines, BUFF_DIST_2)
  
  buffed_flowlines <- dplyr::bind_rows(
    big_polygons,
    small_polygons
  )
  
  # mapview::mapview(buffed_flowlines) + transects + flowlines
  
  small_ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects    = transects,
    polygons     =  small_polygons, 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  all_ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects    = transects,
    polygons     =  buffed_flowlines, 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  # hardcoded correct extension distances to the smaller polygon
  expected_left_distance  <- c(0, 0, 0)
  expected_right_distance <- c(0, 0, 0)
  
  # make sure the algo just extends out to the smaller of the overlayed polygons
  left_side_transects_extend_only_to_closer_of_overlayed_polygons  <- all(expected_left_distance == all_ext_dists$left_distance)
  right_side_transects_extend_only_to_closer_of_overlayed_polygons <- all(expected_right_distance == all_ext_dists$right_distance)
  
  testthat::expect_true(left_side_transects_extend_only_to_closer_of_overlayed_polygons)
  testthat::expect_true(right_side_transects_extend_only_to_closer_of_overlayed_polygons)
  
  # TODO: Not sure if this is bad practice for tests to use the function they are testing to check for correctness....
  # TODO: these tests get the extension distances to JUST the smaller polygons and compare those distances to the extension distances calculated for the OVERLAYED polygons
  # TODO: They should be the same extension distances because the extenssion algo will use the first set of polygon edges it reaches
  # left_side_extensions_match_extensions_for_only_smaller_polygons   <- all(small_ext_dists$left_distance == all_ext_dists$left_distance)
  # right_side_extensions_match_extensions_for_only_smaller_polygons  <- all(small_ext_dists$right_distance == all_ext_dists$right_distance)
  # 
  # testthat::expect_true(left_side_extensions_match_extensions_for_only_smaller_polygons)
  # testthat::expect_true(right_side_extensions_match_extensions_for_only_smaller_polygons)
  
})

testthat::test_that("get_extensions_by_id() transects intersect with 2 overlapping polygons (i.e. 2 layered polygons, 1 entirely encompassing the other, 1 polygon fully encompasses the transect lines, the other polygon does NOT, and is thus shorter than the transect lines) should result in transect lines getting extended to the bigger of the overlayed polygons", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  CROSSWALK_ID       <- "id"
  NUM_OF_TRANSECTS   <- 3
  CS_WIDTH           <- 100
  MAX_EXT_DIST       <- 1200
  
  BUFF_DIST_1        <- 1000
  BUFF_DIST_2        <- 10
  
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID )), 
      geom
    ) 
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = NUM_OF_TRANSECTS,
    cs_widths = CS_WIDTH
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )
  
  # buffer flowline out by 2 different distances and merge them back together
  big_polygons   <- sf::st_buffer(flowlines, BUFF_DIST_1)
  small_polygons <- sf::st_buffer(flowlines, BUFF_DIST_2)
  
  buffed_flowlines <- dplyr::bind_rows(
    big_polygons,
    small_polygons
  )
  
  # mapview::mapview(buffed_flowlines) + transects + flowlines
  
  small_ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects    = transects,
    polygons     =  small_polygons, 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  all_ext_dists <- hydrofabric3D::get_extensions_by_id(
    transects    = transects,
    polygons     =  buffed_flowlines, 
    crosswalk_id = CROSSWALK_ID, 
    max_extension_distance = MAX_EXT_DIST
  )
  
  # hardcoded correct extension distances to the smaller polygon
  expected_left_distance  <- c(955, 952, 882)
  expected_right_distance <- c(937, 939, 952)
  
  # make sure the algo extends out to the LARGER of the overlayed polygons
  left_side_transects_extend_only_to_closer_of_overlayed_polygons  <- all(expected_left_distance == all_ext_dists$left_distance)
  right_side_transects_extend_only_to_closer_of_overlayed_polygons <- all(expected_right_distance == all_ext_dists$right_distance)
  
  testthat::expect_true(left_side_transects_extend_only_to_closer_of_overlayed_polygons)
  testthat::expect_true(right_side_transects_extend_only_to_closer_of_overlayed_polygons)
  
  # TODO: Not sure if this is bad practice for tests to use the function they are testing to check for correctness....
  # TODO: these tests get the extension distances to JUST the smaller polygons and compare those distances to the extension distances calculated for the OVERLAYED polygons
  # TODO: They should be the same extension distances because the extenssion algo will use the first set of polygon edges it reaches
  # left_side_extensions_match_extensions_for_only_smaller_polygons   <- all(small_ext_dists$left_distance == all_ext_dists$left_distance)
  # right_side_extensions_match_extensions_for_only_smaller_polygons  <- all(small_ext_dists$right_distance == all_ext_dists$right_distance)
  # 
  # testthat::expect_true(left_side_extensions_match_extensions_for_only_smaller_polygons)
  # testthat::expect_true(right_side_extensions_match_extensions_for_only_smaller_polygons)
  
})

# buff <- 
#   flowlines %>% 
#   sf::st_buffer(200) 