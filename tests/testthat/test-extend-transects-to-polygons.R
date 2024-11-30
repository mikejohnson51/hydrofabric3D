
library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::extend_transects_to_polygons() ----
# -------------------------------------------------------------------
testthat::test_that("trim_transects_to_polygons() correct output columns - default inputs", {
  # transect_lines, 
  # polygons, 
  # flowlines, 
  # crosswalk_id = 'hy_id',  
  # grouping_id = 'mainstem',
  # max_extension_distance = 3000
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    dplyr::slice(10)
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      geom
    ) 
  
  # create testiung polygons by buffering the flowlines
  polygons <- 
    flowlines %>%
    sf::st_buffer(500)
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    cs_widths = 200,
    num = 10
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )
  
  # plot(polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(transects$geometry, add = T)
  # plot(flowlines$geom, col = "blue", add = T)
  
  ext_trans <- hydrofabric3D::extend_transects_to_polygons(
    transect_lines = transects, 
    polygons = polygons, 
    flowlines = flowlines, 
    crosswalk_id = CROSSWALK_ID,  
    grouping_id = CROSSWALK_ID,
    max_extension_distance = 3000,
    tolerance = NULL,
    keep_lengths = FALSE,
    reindex_cs_ids = FALSE,
    verbose = TRUE
  )
  
  
  # minimum expected cols
  expected_cols <- c(CROSSWALK_ID, 
                     "cs_id", 
                     "cs_lengthm", 
                     "left_distance",
                     "right_distance")
  
  has_required_output_cols <- check_required_cols(ext_trans, expected_cols = expected_cols)
  testthat::expect_true(has_required_output_cols)
  
})

testthat::test_that("extend_transects_to_polygons() 2 transect lines get extended out to polygon boundaries but stop at specified max distance of 500m", {
  # transect_lines, 
  # polygons, 
  # flowlines, 
  # crosswalk_id = 'hy_id',  
  # grouping_id = 'mainstem',
  # max_extension_distance = 3000
  
  flowlines <-
    sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) %>% 
       hydrofabric3D::add_powerlaw_bankful_width(
         total_drainage_area_sqkm_col = "tot_drainage_areasqkm",
         min_bf_width = 50
       ) %>%
       dplyr::select(
         id,
         lengthkm,
         mainstem,
         tot_drainage_areasqkm,
         geometry = geom
       )  %>% 
    dplyr::filter(
      id %in% c("wb-1010908","wb-1002024" )
    )
    # dplyr::slice(3)

  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  # LENGTH_COL_NAME    <- "cs_lengthm"
  # PCT <- 0.5 
  # NUM_OF_TRANSECTS  <- 3
  # MAX_EXT_DIST      <- 5000
  # BUFF_DIST         <- 1000
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      # tot_drainage_areasqkm,
      # bf_width,
      geometry
    ) 
  
  # create testiung polygons by buffering the flowlines
  polygons <- 
    flowlines %>%
    sf::st_buffer(1000) %>% 
    sf::st_union() %>% 
    sf::st_as_sf() %>% 
    dplyr::rename(geometry = x)
  
  # plot(polygons$geometry) 
  # plot(flowlines$geometry, col = "blue", add = T) 
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 50,
    densify = 5,
    cs_widths = 750,
    smooth = TRUE
  )
  
  # transects 
  transects <- 
    transects %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id, cs_measure
    ) %>% 
    dplyr::group_by(id) %>% 
    # dplyr::slice(c(1, dplyr::n())) %>% 
    dplyr::slice(c(dplyr::n())) %>% 
    dplyr::ungroup() %>% 
    hydrofabric3D:::renumber_cs_ids("id")
  
  # plot(polygons$geometry) 
  # plot(flowlines$geometry, col = "blue", add = T) 
  # plot(transects$geometry, col = "red", add = T) 
  
  # transects %>% 
  #   dplyr::group_by(id) %>% 
  #   dplyr::slice(c(dplyr::n())) %>% 
  #   dplyr::ungroup() %>% 
  #   hydrofabric3D:::renumber_cs_ids("id")
  
  extended <- hydrofabric3D:::extend_transects_to_polygons(
    transect_lines = transects,
    polygons = polygons,
    flowlines = flowlines,
    crosswalk_id = CROSSWALK_ID,
    grouping_id = CROSSWALK_ID,
    max_extension_distance = 500
    # reindex_cs_ids = TRUE
  )
  
  # extended2 <- hydrofabric3D:::extend_transects_to_polygons2(
  #   transect_lines = transects,
  #   polygons = polygons,
  #   flowlines = flowlines,
  #   crosswalk_id = CROSSWALK_ID,
  #   grouping_id = CROSSWALK_ID,
  #   max_extension_distance = 5000
  # )
  # 
  
  testthat::expect_equal(extended$left_distance, rep(500, nrow(extended)))
  testthat::expect_equal(extended$right_distance, rep(500, nrow(extended)))
  
  # mls <- sf_polygons_to_geos_multilinestrings(polygons, 200)
  # mapview::mapview(flowlines, color = "cyan") +
  #   mapview::mapview(transects, color = "red")   +
  #    mapview::mapview(extended, color = "green") + polygons + mapview::mapview(sf::st_as_sf(mls))
  #    # mapview::mapview(extended2, color = "green")
  # # Convert the polygon to a MULTILINESTRING geometry for checking extension distances
  #   
  # plot(polygons$geometry)
  # plot(flowlines$geometry, col = "blue", add = T)
  # # plot(extended2$geometry, col = "pink", lwd= 7, add = T)
  # plot(extended$geometry, col = "green", lwd=3, add = T)
  # plot(transects$geometry, col = "red", lwd= 3, add = T)
})
