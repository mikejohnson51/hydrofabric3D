library(testthat)
library(dplyr)
library(sf)
# library(hydrofabric3D)

# source("tests/testthat/testing_utils.R")
# devtools::load_all()
# devtools::load_all()

# 
# --------------------------------------------------------------------------------------------------------------------
# ---- Testing a full workflow ----
# generating transects/cross section points from hydrological network (flowlines)
# --------------------------------------------------------------------------------------------------------------------
# TODO: with NO SELF INTERSECTIONS
testthat::test_that("Full workflow - 1 (Flowlines -> transects -> CS points)3", {
  
  # path <- "/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg/nextgen_06.gpkg"
  # sf::st_layers(path)
  # 
  # flowlines <- sf::read_sf(path, layer = "flowpaths") %>% 
  #   dplyr::slice(1:1000)
  # 
  # # flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # DEM_PATH            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
  # # Transects inputs
  # CS_SOURCE            <- 'hydrofabric3D_test'
  # NUM_OF_TRANSECTS     <- 3
  # RM_SELF_INTERSECTS   <- TRUE
  # ID_COL               <- "hy_id"
  # 
  # # Cross section point inputs
  # # DEM_PATH       <- testthat::test_path("testdata", "dem_flowlines.tif")
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
  # system.time({
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
  # })
  # 
  # cpts <- 
  #   cs_pts %>% 
  #     hydrofabric3D:::drop_incomplete_cs_pts(crosswalk_id = ID_COL) %>% 
  #     hydrofabric3D:::classify_points(
  #       crosswalk_id             = ID_COL, 
  #       pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #     )
  # 
  # cpts2 <- 
  #   cs_pts %>% 
  #   hydrofabric3D:::drop_incomplete_cs_pts(crosswalk_id = ID_COL) %>% 
  #   hydrofabric3D:::classify_points(
  #     crosswalk_id             = ID_COL, 
  #     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #   )
  # 
  # cpts3 <- 
  #   cs_pts %>% 
  #   hydrofabric3D:::drop_incomplete_cs_pts(crosswalk_id = ID_COL) %>% 
  #   classify_points3(
  #     crosswalk_id             = ID_COL, 
  #     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #   )
  # 
  # u_ids <- unique(cpts$hy_id)
  # 
  # nids <- 10
  # 
  # sub_ids <- u_ids[20:30]
  # 
  # comb <- dplyr::bind_rows(
  #           cpts %>% 
  #             dplyr::filter(hy_id %in% sub_ids) %>% 
  #             dplyr::mutate(
  #               new_version = FALSE
  #             ), 
  #           cpts2 %>% 
  #             dplyr::filter(hy_id %in% sub_ids) %>% 
  #             dplyr::mutate(
  #               new_version = TRUE
  #             )
  #         )
  # 
  # # comb %>% 
  # #   ggplot2::ggplot() + 
  # #   ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = point_type), size = 3) + 
  # #   ggplot2::facet_wrap(hy_id~cs_id)
  # # 
  # cpts %>% 
  #   dplyr::filter(hy_id %in% sub_ids) %>% 
  #   ggplot2::ggplot() + 
  #   ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = point_type), size = 3) + 
  #   # ggplot2::facet_grid(hy_id~cs_id, scales = "free_y")
  #   ggplot2::facet_wrap(hy_id~cs_id,  scales = "free_y") + 
  #   ggplot2::labs(title = "OLD")
  # 
  # cpts2 %>% 
  #   dplyr::filter(hy_id %in% sub_ids) %>% 
  #   ggplot2::ggplot() + 
  #   ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = point_type), size = 3) + 
  #   # ggplot2::facet_grid(hy_id~cs_id, scales = "free_y")
  #   ggplot2::facet_wrap(hy_id~cs_id,  scales = "free_y") + 
  #   ggplot2::labs(title = "NEW")
  # 
  # cpts3 %>% 
  #   dplyr::filter(hy_id %in% sub_ids) %>% 
  #   ggplot2::ggplot() + 
  #   ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = point_type), size = 3) + 
  #   # ggplot2::facet_grid(hy_id~cs_id, scales = "free_y")
  #   ggplot2::facet_wrap(hy_id~cs_id,  scales = "free_y") + 
  #   ggplot2::labs(title = "NEW (2)")
  # 
  # 
  # 
  # cpts2 %>% 
  #   dplyr::slice(1:10) %>% 
  #   hydrofabric3D::plot_cs_pts(color = "point_type")
  # 
  # # test that the minimum number of cross section points was generated (minimum of N points per transect, i.e. MIN_PTS_PER_CS * TOTAL_TRANSECTS_COUNT)
  # testthat::expect_true(TOTAL_CS_PTS_COUNT >= MIN_PTS_PER_CS * TOTAL_TRANSECTS_COUNT)
  # 
  # cs_pts
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
  #   dplyr::group_by(dplyr::across(dplyr::any_of(c(ID_COL, "cs_id")))) %>%
  #   dplyr::filter(!any(is.na(Z))) %>%
  #   dplyr::ungroup()
  # 
  # classified_pts <- 
  #   cs_pts %>% 
  #   hydrofabric3D:::classify_points(
  #     crosswalk_id             = ID_COL, 
  #     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #   )
  # 
  # # mapview::mapview(transects, color = "green") + mapview::mapview(flowlines, color = "red")
  # 
  # TOTAL_TRANSECTS_COUNT <- nrow(transects)
  # 
  # max_transect_count <-
  #   transects %>%
  #   sf::st_drop_geometry() %>%
  #   dplyr::group_by(hy_id) %>%
  #   dplyr::count() %>%
  #   dplyr::ungroup() %>%
  #   dplyr::pull(n) %>%
  #   max()
  # 
  # # Test that NO flowline has MORE than the prescribed number of transects (NUM_OF_TRANSECTS)
  # testthat::expect_true(max_transect_count <= NUM_OF_TRANSECTS)
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
  # TOTAL_CS_PTS_COUNT <- nrow(cs_pts)
  # 
  # # test that the minimum number of cross section points was generated (minimum of N points per transect, i.e. MIN_PTS_PER_CS * TOTAL_TRANSECTS_COUNT)
  # testthat::expect_true(TOTAL_CS_PTS_COUNT >= MIN_PTS_PER_CS * TOTAL_TRANSECTS_COUNT)
  # 
  # cs_pts
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
  #   dplyr::group_by(dplyr::across(dplyr::any_of(c(ID_COL, "cs_id")))) %>%
  #   dplyr::filter(!any(is.na(Z))) %>%
  #   dplyr::ungroup()
  # 
  # classified_pts <- 
  #   cs_pts %>% 
  #   hydrofabric3D:::classify_points(
  #     crosswalk_id             = ID_COL, 
  #     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #   )
  # # })
  # 
  # ids_original_cs_pts <- hydrofabric3D::add_tmp_id(cs_pts)$tmp_id
  # 
  # # sf::write_sf(cs_pts, "/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # # sf::write_sf(flines, "/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
  # # sf::write_sf(transects, "/Users/anguswatters/Desktop/test_improve_transects_06.gpkg")
  # 
  # # ----------------------------------------------------------------------------------------------------------------
  # # ---- STEP 3: Try to rectify any no relief and invalid banks cross sections ----
  # # ----------------------------------------------------------------------------------------------------------------
  # 
  # # system.time({
  # # NOTE: new inplace method for improving (rectifying) any invalid cross sections where we dont have banks and relief
  # fixed_pts <- hydrofabric3D::get_improved_cs_pts(
  #   # cs_pts         = cs_pts,    # cross section points generated from hydrofabric3D::cross_section_pts()
  #   cs_pts         = classified_pts,
  #   net            = flowlines,    # original flowline network
  #   # net            = flines,     # original flowline network
  #   transects      = transects,    # original transect lines
  #   crosswalk_id   = ID_COL,
  #   points_per_cs  = POINTS_PER_CS,
  #   min_pts_per_cs = MIN_PTS_PER_CS, # number of points per cross sections
  #   dem            = DEM_PATH, # DEM to extract points from
  #   scale          = EXTENSION_PCT, # How far to extend transects if the points need to be rechecked
  #   pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF, # percent of cross sections length to be needed in relief calculation to consider cross section to "have relief"
  #   fix_ids = FALSE,
  #   verbose = FALSE
  # )
  # # })
  # 
  # # check "is_extended" column was added
  # testthat::expect_true("is_extended" %in% names(fixed_pts))
  # 
  # fixed_pts_cols      <- names(fixed_pts)
  # classified_pts_cols <- names(classified_pts)
  # has_expected_cols   <- all(sort(fixed_pts_cols) == sort(c(classified_pts_cols, "is_extended")))
  # 
  # # check that all expected columns are present 
  # testthat::expect_true(has_expected_cols)
  # 
  # start_valid_banks <- sum(classified_pts$valid_banks)
  # end_valid_banks   <- sum(fixed_pts$valid_banks)
  # 
  # testthat::expect_true(end_valid_banks >= start_valid_banks)
  # 
  # start_has_relief  <- sum(classified_pts$has_relief)
  # end_has_relief    <- sum(fixed_pts$has_relief)
  # 
  # testthat::expect_true(end_has_relief >= start_has_relief)
  # 
  # extended_pts <- 
  #   fixed_pts %>% 
  #   dplyr::filter(is_extended) %>% 
  #   hydrofabric3D::add_tmp_id(x = ID_COL)
  # 
  # pts_before_extension <- 
  #   classified_pts %>% 
  #   hydrofabric3D::add_tmp_id(x = ID_COL) %>% 
  #   dplyr::filter(tmp_id %in% extended_pts$tmp_id)
  # 
  # cs_length_ge_pre_extension_cs_length <- all(extended_pts$cs_lengthm >= pts_before_extension$cs_lengthm)
  # 
  # testthat::expect_true(cs_length_ge_pre_extension_cs_length)
  # 
  # start_uids <- hydrofabric3D::get_unique_tmp_ids(classified_pts, x = ID_COL)
  # end_uids   <- hydrofabric3D::get_unique_tmp_ids(fixed_pts, x = ID_COL) 
  # 
  # 
  # same_uids_after_fixing <- all(start_uids %in% end_uids) && all(end_uids %in% start_uids)
  # 
  # testthat::expect_true(same_uids_after_fixing)
  # 
  # # Test to make sure all valid points (valid_banks AND has_relief) have a bottom that is lower than the rest of cross section
  # valid_pts <-
  #   fixed_pts %>% 
  #   sf::st_drop_geometry() %>% 
  #   dplyr::select(hy_id, cs_id, pt_id, Z, point_type, valid_banks, has_relief) %>%
  #   dplyr::ungroup() %>% 
  #   dplyr::filter(valid_banks, has_relief)
  # 
  # bottoms <-
  #   valid_pts %>% 
  #   dplyr::filter(point_type == "bottom") %>% 
  #   dplyr::group_by(hy_id, cs_id)  %>% 
  #   dplyr::summarise(bottomZ = min(Z, na.rm =TRUE)) %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::select(hy_id, cs_id, bottomZ)
  # 
  # all_valid_pts_above_bottom <- 
  #   valid_pts %>% 
  #   dplyr::left_join(
  #     bottoms,
  #     by = c("hy_id", "cs_id")
  #   ) %>% 
  #   dplyr::filter(point_type != "bottom") %>% 
  #   dplyr::group_by(hy_id, cs_id) %>% 
  #   dplyr::mutate(
  #     Z_ge_bottom = Z >= bottomZ
  #   ) %>% 
  #   dplyr::ungroup() %>% 
  #   .$Z_ge_bottom %>% 
  #   all()
  # 
  # testthat::expect_true(all_valid_pts_above_bottom)
  
})


# TODO: with NO SELF INTERSECTIONS
testthat::test_that("Full workflow - 1 (Flowlines -> transects -> CS points)", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))

  # Transects inputs
  CS_SOURCE            <- 'hydrofabric3D_test'
  NUM_OF_TRANSECTS     <- 3
  RM_SELF_INTERSECTS   <- TRUE
  ID_COL               <- "hy_id"

  # Cross section point inputs
  DEM_PATH       <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 10

  # NOTE: 0.01 (1%) of cros sections length is the default
    # percent of cross section length (cs_lengthm) to use as the
    # threshold depth for classifying whether a cross section has "relief"
  PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF <- 0.01

  EXTENSION_PCT = 0.5
  
  flowlines <-
    flowlines %>%
    hydrofabric3D::add_powerlaw_bankful_width("tot_drainage_areasqkm", 50) %>%
    # dplyr::mutate(
    #   bf_width        = hydrofabric3D:::calc_powerlaw_bankful_width(tot_drainage_areasqkm),
    #   bf_width        = pmax(50, bf_width * 11)
    # ) %>%
     dplyr::select(
      hy_id = id,
      # tot_drainage_areasqkm,
      bf_width,
      # input_bf_width,
      geometry = geom
    )
    # dplyr::slice(1)

  # plot(flowlines$geometry)

  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowlines,
    crosswalk_id                = ID_COL,
    cs_widths         = flowlines$bf_width,
    num               = NUM_OF_TRANSECTS,
    rm_self_intersect = RM_SELF_INTERSECTS
  )

  # mapview::mapview(transects, color = "green") + mapview::mapview(flowlines, color = "red")

  TOTAL_TRANSECTS_COUNT <- nrow(transects)

  max_transect_count <-
    transects %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(hy_id) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::pull(n) %>%
    max()

  # Test that NO flowline has MORE than the prescribed number of transects (NUM_OF_TRANSECTS)
  testthat::expect_true(max_transect_count <= NUM_OF_TRANSECTS)

  # select columns
  transects <-
    transects %>%
    # dplyr::mutate(
    #   cs_source = CS_SOURCE
    # ) %>%
    dplyr::select(
      hy_id,
      cs_id,
      cs_lengthm,
      # cs_source,
      cs_measure,
      geometry
    )

  # ----------------------------------------------------------------------------------------------------------------
  # ---- Cross section points ----
  # ----------------------------------------------------------------------------------------------------------------

  # ---- STEP 1: Extract cs points from DEM ----

  # get cross section point elevations
  cs_pts <- hydrofabric3D::cross_section_pts(
    cs             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  )

  TOTAL_CS_PTS_COUNT <- nrow(cs_pts)

  # test that the minimum number of cross section points was generated (minimum of N points per transect, i.e. MIN_PTS_PER_CS * TOTAL_TRANSECTS_COUNT)
  testthat::expect_true(TOTAL_CS_PTS_COUNT >= MIN_PTS_PER_CS * TOTAL_TRANSECTS_COUNT)
  
  cs_pts
  
  # ----------------------------------------------------------------------------------------------------------------
  # ---- STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points ----
  # ----------------------------------------------------------------------------------------------------------------

  # system.time({
  NA_Z_COUNT <- sum(is.na(cs_pts$Z))
  # cs_pts[!is.na(cs_pts$Z), ][[ID_COL]]

  # STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points
  cs_pts <-
    cs_pts %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(ID_COL, "cs_id")))) %>%
    dplyr::filter(!any(is.na(Z))) %>%
    dplyr::ungroup()
  
  classified_pts <- 
    cs_pts %>% 
    hydrofabric3D:::classify_points(
      crosswalk_id             = ID_COL, 
      pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
      )
  # })
  
  ids_original_cs_pts <- hydrofabric3D::add_tmp_id(cs_pts)$tmp_id

  # sf::write_sf(cs_pts, "/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # sf::write_sf(flines, "/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
  # sf::write_sf(transects, "/Users/anguswatters/Desktop/test_improve_transects_06.gpkg")

  # ----------------------------------------------------------------------------------------------------------------
  # ---- STEP 3: Try to rectify any no relief and invalid banks cross sections ----
  # ----------------------------------------------------------------------------------------------------------------
   
  # system.time({
    # NOTE: new inplace method for improving (rectifying) any invalid cross sections where we dont have banks and relief
    fixed_pts <- hydrofabric3D::get_improved_cs_pts(
      # cs_pts         = cs_pts,    # cross section points generated from hydrofabric3D::cross_section_pts()
      cs_pts         = classified_pts,
      net            = flowlines,    # original flowline network
      # net            = flines,     # original flowline network
      transects      = transects,    # original transect lines
      crosswalk_id   = ID_COL,
      points_per_cs  = POINTS_PER_CS,
      min_pts_per_cs = MIN_PTS_PER_CS, # number of points per cross sections
      dem            = DEM_PATH, # DEM to extract points from
      scale          = EXTENSION_PCT, # How far to extend transects if the points need to be rechecked
      pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF, # percent of cross sections length to be needed in relief calculation to consider cross section to "have relief"
      fix_ids = FALSE,
      verbose = FALSE
    )
  # })
  
  # check "is_extended" column was added
  testthat::expect_true("is_extended" %in% names(fixed_pts))
  
  fixed_pts_cols      <- names(fixed_pts)
  classified_pts_cols <- names(classified_pts)
  has_expected_cols   <- all(sort(fixed_pts_cols) == sort(c(classified_pts_cols, "is_extended")))
  
  # check that all expected columns are present 
  testthat::expect_true(has_expected_cols)
  
  start_valid_banks <- sum(classified_pts$valid_banks)
  end_valid_banks   <- sum(fixed_pts$valid_banks)
  
  testthat::expect_true(end_valid_banks >= start_valid_banks)
  
  start_has_relief  <- sum(classified_pts$has_relief)
  end_has_relief    <- sum(fixed_pts$has_relief)
 
  testthat::expect_true(end_has_relief >= start_has_relief)
  
  extended_pts <- 
    fixed_pts %>% 
    dplyr::filter(is_extended) %>% 
    hydrofabric3D::add_tmp_id(x = ID_COL)
  
  pts_before_extension <- 
    classified_pts %>% 
    hydrofabric3D::add_tmp_id(x = ID_COL) %>% 
    dplyr::filter(tmp_id %in% extended_pts$tmp_id)
  
  cs_length_ge_pre_extension_cs_length <- all(extended_pts$cs_lengthm >= pts_before_extension$cs_lengthm)
  
  testthat::expect_true(cs_length_ge_pre_extension_cs_length)
 
  start_uids <- hydrofabric3D::get_unique_tmp_ids(classified_pts, x = ID_COL)
  end_uids   <- hydrofabric3D::get_unique_tmp_ids(fixed_pts, x = ID_COL) 
  
  
  same_uids_after_fixing <- all(start_uids %in% end_uids) && all(end_uids %in% start_uids)
  
  testthat::expect_true(same_uids_after_fixing)
  
  # Test to make sure all valid points (valid_banks AND has_relief) have a bottom that is lower than the rest of cross section
  valid_pts <-
    fixed_pts %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(hy_id, cs_id, pt_id, Z, point_type, valid_banks, has_relief) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(valid_banks, has_relief)
  
  bottoms <-
    valid_pts %>% 
    dplyr::filter(point_type == "bottom") %>% 
    dplyr::group_by(hy_id, cs_id)  %>% 
    dplyr::summarise(bottomZ = min(Z, na.rm =TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(hy_id, cs_id, bottomZ)
  
  all_valid_pts_above_bottom <- 
    valid_pts %>% 
    dplyr::left_join(
      bottoms,
      by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::filter(point_type != "bottom") %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      Z_ge_bottom = Z >= bottomZ
    ) %>% 
    dplyr::ungroup() %>% 
    .$Z_ge_bottom %>% 
    all()
  
  testthat::expect_true(all_valid_pts_above_bottom)
  
})




















