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
testthat::test_that("Full workflow - 1 (Flowlines -> transects -> CS points)", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    hydroloom::rename_geometry("geometry") %>% 
    dplyr::select(id, geometry)

  # Transects inputs
  CS_SOURCE            <- 'hydrofabric3D_test'
  CS_WIDTHS            <- 500
  NUM_OF_TRANSECTS     <- 10
  RM_SELF_INTERSECTS   <- TRUE
  ID_COL               <- "id"

  # Cross section point inputs
  DEM_PATH       <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 10

  # NOTE: 0.01 (1%) of cros sections length is the default
    # percent of cross section length (cs_lengthm) to use as the
    # threshold depth for classifying whether a cross section has "relief"
  PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF <- 0.01

  EXTENSION_PCT = 0.5
  
  # plot(flowlines$geometry)

  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowlines,
    crosswalk_id      = ID_COL,
    cs_widths         = CS_WIDTHS,
    num               = NUM_OF_TRANSECTS,
    rm_self_intersect = TRUE
  )

  # mapview::mapview(transects, color = "green") + mapview::mapview(flowlines, color = "red")

  TOTAL_TRANSECTS_COUNT <- nrow(transects)

  max_transect_count <-
    transects %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(id) %>%
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::pull(n) %>%
    max()

  # Test that NO flowline has MORE than the prescribed number of transects (NUM_OF_TRANSECTS)
  testthat::expect_true(max_transect_count <= NUM_OF_TRANSECTS)
  
  transects <- 
    transects %>% 
    hydrofabric3D::select_transects(ID_COL)
  
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
  
  # there should NOT be any NA Z values in this area
  testthat::expect_true(  !any(is.na(cs_pts$Z)))
  
  
  # should validate 
  testthat::expect_no_error(
    validate_cs_pts(cs_pts, ID_COL)
  ) 
  
  testthat::expect_no_error(
    validate_cs_pts_against_transects(cs_pts, transects, ID_COL)
  ) 
  
  #expect an error to be thrown bc of missing classification columns
  testthat::expect_error(
    validate_classified_cs_pts(cs_pts, ID_COL)
  ) 
  
  # ----------------------------------------------------------------------------------------------------------------
  # ---- STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points ----
  # ----------------------------------------------------------------------------------------------------------------

  
  classified_pts <- 
    cs_pts %>% 
    hydrofabric3D:::classify_points(
      crosswalk_id             = ID_COL, 
      pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF,
      na.rm          = TRUE
      ) 
  
  # passes both cs_pts validator functions 
  testthat::expect_no_error(
    validate_cs_pts(classified_pts, ID_COL)
  ) 
  testthat::expect_no_error(
    validate_classified_cs_pts(classified_pts, ID_COL)
  ) 
  
  testthat::expect_no_error(
    validate_cs_pts_against_transects(classified_pts, transects, ID_COL)
  ) 
  
  testthat::expect_no_error(
    validate_classified_cs_pts_against_transects(classified_pts, transects, ID_COL)
  )  
  
  # sf::write_sf(cs_pts, "/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # sf::write_sf(flines, "/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
  # sf::write_sf(transects, "/Users/anguswatters/Desktop/test_improve_transects_06.gpkg")
  
  # --------------------------------------------------------------------------
  # ---- Add CS attributes to transects data ----
  # --------------------------------------------------------------------------
  # message("Adding CS attributes to transects data (", Sys.time(), ")")  
  
  transects <-
    transects %>% 
    hydrofabric3D:::add_cs_attributes_to_transects(classified_pts, ID_COL)  %>% 
    # fill the NA valid_banks / has_relief columns 
    # with all TRUEs (ignores them for extension step, i.e. all TRUEs means they'll be disregarded in CS attribute extension step)
    hydrofabric3D:::fill_missing_cs_attributes() 
  
  testthat::expect_true(
    all(c("valid_banks", "has_relief") %in% names(transects))
  )
  
  testthat::expect_true(
    all(transects$valid_banks %>% is.logical())
  )
  
  testthat::expect_true(
    all(transects$has_relief %>% is.logical())
  )
  
  # --------------------------------------------------------------------------
  # ---- Extend transects based on valid_banks / has_relief attributes ----
  # --------------------------------------------------------------------------
  num_transects_to_extend <- 
    transects %>% 
    get_validity_tally(ID_COL) %>% 
    dplyr::filter(!(valid_banks & has_relief)) %>% 
    dplyr::pull(n) %>% 
    sum()
  
  extended_transects <- hydrofabric3D::extend_transects_by_cs_attributes(
    transects = transects, 
    flowlines = flowlines,
    crosswalk_id = ID_COL,
    scale = EXTENSION_PCT,
    keep_lengths = TRUE,
    keep_extension_distances = TRUE,
    reindex_cs_ids = FALSE 
  )  %>% 
    dplyr::mutate(
      changed = dplyr::case_when(
        left_distance > 0 | right_distance > 0 ~ TRUE,
        TRUE                                   ~ FALSE
      )
    )
  
  testthat::expect_true(
    hydrofabric3D:::has_same_uids(extended_transects, transects, ID_COL)
  )
  
  testthat::expect_message(
    hydrofabric3D::extend_transects_by_cs_attributes(
      transects = transects, 
      flowlines = flowlines,
      crosswalk_id = ID_COL,
      scale = EXTENSION_PCT,
      keep_lengths = TRUE,
      keep_extension_distances = TRUE,
      reindex_cs_ids = FALSE 
    ),
    paste0("Extending ", num_transects_to_extend, " transects without valid banks or relief by 50%...")
  )
  
  # mapview::mapview(extended_transects, color = "green") + 
  #   mapview::mapview(transects, color = "red") + flowlines
  
  # check theyre both valid 
  testthat::expect_no_error(
    is_extended_transects_valid <- hydrofabric3D::validate_transects(extended_transects, ID_COL)
  )
  
  testthat::expect_no_error(
    is_extended_transects_against_flowlines_valid <- hydrofabric3D::validate_transects_against_flowlines(extended_transects, flowlines, ID_COL)
  )
  
  # the number of ACTUALLY extended transect lines cant be more than the number of transects eligible for extension (num_transects_to_extend)
  num_actually_extended <- 
    extended_transects %>% 
    dplyr::pull(changed) %>% 
    sum()
  
  testthat::expect_true(
    num_transects_to_extend >= num_actually_extended
  ) 
  
  # the lengths of extended transects is ACTUALLY different than the input length
  all_extended_transects_are_diff_lengths <- 
    extended_transects %>% 
    dplyr::select(dplyr::any_of(ID_COL), cs_id, changed, geometry) %>% 
    dplyr::mutate(
      new_length = as.numeric(sf::st_length(.))
    ) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::left_join(
      transects %>% 
        dplyr::select(dplyr::any_of(ID_COL), cs_id, geometry) %>% 
        dplyr::mutate(
          old_length = as.numeric(sf::st_length(.))
        ) %>% 
        sf::st_drop_geometry(),
      by = c(ID_COL, "cs_id")
    ) %>% 
    dplyr::filter(changed) %>% 
    dplyr::mutate(
      is_diff_length  = new_length != old_length
    ) %>% 
    dplyr::pull(is_diff_length) %>% 
    all()
  
  testthat::expect_true(
    all_extended_transects_are_diff_lengths
  )
  
  # --------------------------------------------------------------------------
  # ---- Extend transects based on valid_banks / has_relief attributes ----
  # --------------------------------------------------------------------------
  # message("Flagging new extended transects based on improvements in CS attributes (", Sys.time(), ")")
  
  flagged_transects <-
    extended_transects %>%
    hydrofabric3D::flag_transects_for_change(
      crosswalk_id   = ID_COL,
      points_per_cs  = POINTS_PER_CS,
      min_pts_per_cs = MIN_PTS_PER_CS,
      dem            = DEM_PATH,
      pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF,
      na.rm          = FALSE
    ) 
  
  testthat::expect_true(
   all(c("flagged", "is_improved", "extension_distance") %in% names(flagged_transects))
  )
  
  message("Adjusting transect lengths based on CS attribute comparisons flag (", Sys.time(), ")")
  
  final_transects <- hydrofabric3D::adjust_flagged_transects2(
    # final_transects <- hydrofabric3D::adjust_flagged_transects(
    x = flagged_transects, 
    crosswalk_id = ID_COL, 
    reindex_cs_ids = FALSE
  )
  
  testthat::expect_warning(
    hydrofabric3D::adjust_flagged_transects2(
      # final_transects <- hydrofabric3D::adjust_flagged_transects(
      x = flagged_transects, 
      crosswalk_id = ID_COL, 
      reindex_cs_ids = TRUE
    ),
    "Re-indexing cs_ids may result in a mismatch between unique crosswalk_id/cs_ids in input 'transects' and the output unique crosswalk_id/cs_ids"
  )
  
  testthat::expect_no_warning(
    hydrofabric3D::adjust_flagged_transects2(
      # final_transects <- hydrofabric3D::adjust_flagged_transects(
      x = flagged_transects, 
      crosswalk_id = ID_COL, 
      reindex_cs_ids = FALSE
    )
  )
  
  testthat::expect_true(
    all(c(ID_COL, "cs_id", "cs_lengthm", "cs_measure", "sinuosity", "geometry") %in% names(final_transects))
  )
  
  testthat::expect_no_error(
    is_final_transects_valid <- hydrofabric3D::validate_transects(final_transects, ID_COL)
  )
  
  testthat::expect_no_error(
    is_final_transects_against_flowlines_valid <- hydrofabric3D::validate_transects_against_flowlines(final_transects, flowlines, ID_COL)
  )
  
  all_flagged_transects_changed_lengths <- 
    final_transects %>% 
    dplyr::left_join(
      flagged_transects %>% 
        sf::st_drop_geometry() %>% 
        dplyr::select(dplyr::any_of(ID_COL), cs_id, flagged),
      by = c(ID_COL, "cs_id")
    ) %>% 
    dplyr::filter(flagged) %>% 
    dplyr::select(dplyr::any_of(ID_COL), cs_id) %>% 
    dplyr::mutate(
      new_length = as.numeric(sf::st_length(.))
    ) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::left_join(
        extended_transects %>% 
          dplyr::left_join(
              flagged_transects %>% 
                sf::st_drop_geometry() %>% 
                dplyr::select(dplyr::any_of(ID_COL), cs_id, flagged),
              by = c(ID_COL, "cs_id")
          ) %>% 
          dplyr::filter(flagged) %>% 
          dplyr::select(dplyr::any_of(ID_COL), cs_id) %>% 
          dplyr::mutate(
            old_length = as.numeric(sf::st_length(.))
          ) %>% 
          sf::st_drop_geometry(), 
      by = c(ID_COL, "cs_id")
    ) %>% 
    dplyr::mutate(
      is_diff_length  = new_length != old_length
    ) %>% 
    dplyr::pull(is_diff_length) %>% 
    all()
  
  testthat::expect_true(all_flagged_transects_changed_lengths)
  
  testthat::expect_true(
    has_same_uids(
      extended_transects,
      final_transects,
      crosswalk_id = ID_COL
      )
  )
  
  # all(hydrofabric3D::get_unique_tmp_ids(extended_transects, x = ID_COL) %in% hydrofabric3D::get_unique_tmp_ids(final_transects, x = ID_COL))
  # all(hydrofabric3D::get_unique_tmp_ids(final_transects, x = ID_COL) %in% hydrofabric3D::get_unique_tmp_ids(extended_transects, x = ID_COL))
  
  
  # mapview::mapview(transects, color = "green") +
  #   mapview::mapview(extended, color = "green") +
  #   # mapview::mapview(extended, color = "gold") +
  #   mapview::mapview(flag_adjusts, color = "red") +
  #   mapview::mapview(flowlines, color = "dodgerblue")
  
  
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
      transects      = transects %>% dplyr::select(!dplyr::any_of(c("valid_banks", "has_relief"))),    # original transect lines
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
  
  testthat::expect_no_error(
    hydrofabric3D::validate_cs_pts(fixed_pts, ID_COL)
  )
  testthat::expect_no_error(
    hydrofabric3D:::validate_classified_cs_pts(fixed_pts, ID_COL)
  )
  
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
    dplyr::select(dplyr::any_of(ID_COL), cs_id, pt_id, Z, point_type, valid_banks, has_relief) %>%
    dplyr::ungroup() %>% 
    dplyr::filter(valid_banks, has_relief)
  
  bottoms <-
    valid_pts %>% 
    dplyr::filter(point_type == "bottom") %>% 
    dplyr::group_by(id, cs_id)  %>% 
    dplyr::summarise(bottomZ = min(Z, na.rm =TRUE)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(dplyr::any_of(ID_COL), cs_id, bottomZ)
  
  all_valid_pts_above_bottom <- 
    valid_pts %>% 
    dplyr::left_join(
      bottoms,
      by = c(ID_COL, "cs_id")
    ) %>% 
    dplyr::filter(point_type != "bottom") %>% 
    dplyr::group_by(id, cs_id) %>% 
    dplyr::mutate(
      Z_ge_bottom = ((Z - bottomZ) + 1 ) > 0 ,
      # Z_ge_bottom = Z >= bottomZ 
    ) %>% 
    dplyr::ungroup() %>% 
    .$Z_ge_bottom %>% 
    all()
  
  testthat::expect_true(all_valid_pts_above_bottom)
  testthat::expect_no_error(
    hydrofabric3D::validate_cs_pts(fixed_pts, ID_COL)
  )
  
  
})


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

















