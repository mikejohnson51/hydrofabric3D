# library(testthat)
# library(dplyr)
# library(sf)
# # library(hydrofabric3D)
# devtools::load_all()
# 
# # --------------------------------------------------------------------------------------------------------------------
# # ---- Testing a full workflow ---- 
# # generating transects/cross section points from hydrological network (flowlines)
# # --------------------------------------------------------------------------------------------------------------------
# 
# # TODO: with NO SELF INTERSECTIONS
# testthat::test_that("Full workflow - 1 (Flowlines -> transects -> CS points)", {
#   flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
# 
#   # Transects inputs
#   CS_SOURCE            <- 'hydrofabric3D_test'
#   NUM_OF_TRANSECTS     <- 3
#   RM_SELF_INTERSECTS   <- TRUE
#   ID_COL               <- "hy_id"
#   
#   # Cross section point inputs
#   DEM_PATH       <- testthat::test_path("testdata", "dem_flowlines.tif")
#   POINTS_PER_CS     <- NULL
#   MIN_PTS_PER_CS    <- 10
#   
#   # NOTE: 0.01 (1%) of cros sections length is the default
#     # percent of cross section length (cs_lengthm) to use as the 
#     # threshold depth for classifying whether a cross section has "relief"
#   PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF <- 0.01
#   
#   flowlines <- 
#     flowlines %>% 
#     dplyr::mutate(
#       bf_width        = hydrofabric3D::calc_powerlaw_bankful_width(tot_drainage_areasqkm),
#       # bf_width        = hydrofabric3D::calc_powerlaw_bankful_width(tot_drainage_areasqkm),
#       # bf_width        = pmax(50, bf_width * 2)
#       bf_width        = pmax(50, bf_width * 11)
#     ) %>% 
#      dplyr::select(
#       hy_id = id, 
#       # tot_drainage_areasqkm, 
#       bf_width,
#       # input_bf_width,
#       geometry = geom
#     ) 
#     # dplyr::slice(1)
#   
#   # plot(flowlines$geometry)
#   
#   transects <- hydrofabric3D::cut_cross_sections(
#     net               = flowlines,
#     id                = ID_COL,
#     cs_widths         = flowlines$bf_width,
#     num               = NUM_OF_TRANSECTS,
#     rm_self_intersect = RM_SELF_INTERSECTS
#   )
#   
#   # mapview::mapview(transects, color = "green") + mapview::mapview(flowlines, color = "red")
#   
#   TOTAL_TRANSECTS_COUNT <- nrow(transects)
#   
#   max_transect_count <- 
#     transects %>% 
#     sf::st_drop_geometry() %>% 
#     dplyr::group_by(hy_id) %>% 
#     dplyr::count() %>% 
#     dplyr::ungroup() %>% 
#     dplyr::pull(n) %>% 
#     max()
#   
#   # Test that NO flowline has MORE than the prescribed number of transects (NUM_OF_TRANSECTS)
#   testthat::expect_true(max_transect_count <= NUM_OF_TRANSECTS)
#   
#   # select columns
#   transects <-
#     transects %>%  
#     # dplyr::mutate(
#     #   cs_source = CS_SOURCE
#     # ) %>% 
#     dplyr::select(
#       hy_id, 
#       cs_id, 
#       cs_lengthm,
#       # cs_source,
#       cs_measure,
#       geometry
#     )
#   
#   # ----------------------------------------------------------------------------------------------------------------
#   # ---- Cross section points ----
#   # ----------------------------------------------------------------------------------------------------------------
#   
#   # ---- STEP 1: Extract cs points from DEM ----
#   
#   # get cross section point elevations
#   cs_pts <- hydrofabric3D::cross_section_pts(
#     cs             = transects,
#     points_per_cs  = POINTS_PER_CS,
#     min_pts_per_cs = MIN_PTS_PER_CS,
#     dem            = DEM_PATH
#   )
#   
#   TOTAL_CS_PTS_COUNT <- nrow(cs_pts)
#   
#   # test that the minimum number of cross section points was generated (minimum of N points per transect, i.e. MIN_PTS_PER_CS * TOTAL_TRANSECTS_COUNT) 
#   testthat::expect_true(TOTAL_CS_PTS_COUNT >= MIN_PTS_PER_CS * TOTAL_TRANSECTS_COUNT)
#   
#   # ----------------------------------------------------------------------------------------------------------------
#   # ---- STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points ----
#   # ----------------------------------------------------------------------------------------------------------------
#   
#   # system.time({
#   NA_Z_COUNT <- sum(is.na(cs_pts$Z))
#   # cs_pts[!is.na(cs_pts$Z), ][[ID_COL]]
#   
#   # STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points 
#   cs_pts <-
#     cs_pts %>% 
#     dplyr::group_by(dplyr::across(dplyr::any_of(c(ID_COL, "cs_id")))) %>% 
#     dplyr::filter(!any(is.na(Z))) %>% 
#     dplyr::ungroup()
#   
#   cs_pts_classed <-
#     cs_pts %>% 
#     hydrofabric3D::classify_points(pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF)  
#   
# 
#   # })
#   
#   ids_original_cs_pts <- hydrofabric3D::add_tmp_id(cs_pts)$tmp_id
#   
#   # sf::write_sf(cs_pts, "/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
#   # sf::write_sf(flines, "/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
#   # sf::write_sf(transects, "/Users/anguswatters/Desktop/test_improve_transects_06.gpkg")
#   # # 
#   
#   
#   # ----------------------------------------------------------------------------------------------------------------
#   # ---- STEP 3: Try to rectify any no relief and invalid banks cross sections ----
#   # ----------------------------------------------------------------------------------------------------------------
#   # dplyr::rename(flines, hy_id = id)
#   # profvis::profvis({
#   # system.time({
#   
#   # NOTE: new inplace method for improving (rectifying) any invalid cross sections where we dont have banks and relief
#   fixed_pts <- hydrofabric3D::improve_invalid_cs2(
#     cs_pts         = cs_pts,    # cross section points generated from hydrofabric3D::cross_section_pts()
#     net            = dplyr::rename(flines, hy_id = id),    # original flowline network
#     # net            = flines,    # original flowline network
#     transects      = transects, # original transect lines
#     points_per_cs  = NULL, 
#     min_pts_per_cs = 10, # number of points per cross sections
#     dem            = DEM_URL, # DEM to extract points from
#     scale          = EXTENSION_PCT, # How far to extend transects if the points need to be rechecked
#     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF, # percent of cross sections length to be needed in relief calculation to consider cross section to "have relief"
#     fix_ids = FALSE,
#     verbose = TRUE
#   )
#   
#   
# })
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
