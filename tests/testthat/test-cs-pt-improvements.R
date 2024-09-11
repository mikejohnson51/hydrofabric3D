# library(testthat)
# library(dplyr)
# library(sf)
# # # library(hydrofabric3D)
# 
# # source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()
# 
# # -------------------------------------------------------------------
# # ---- hydrofabric3D::classify_pts() ----
# # -------------------------------------------------------------------
# 
# # TODO:
# testthat::test_that("check 'valid_banks' attribute of CLASSIFIED CS points from DEFAULT transects output", {
# 
#   flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
#   # flowlines    <- dplyr::slice(flowlines, 1)
# 
#   MIN_BF_WIDTH       <- 50
#   ID_COL             <- "hy_id"
#   NUM_OF_TRANSECTS   <- 3
# 
#   # Cross section point inputs
#   DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
#   POINTS_PER_CS     <- NULL
#   MIN_PTS_PER_CS    <- 20
# 
#   PCT_OF_LENGTH_FOR_RELIEF <- 0.01
# 
#   flowlines <-
#     flowlines %>%
#     # dplyr::slice(1) %>%
#     # dplyr::slice(1:3) %>%
#     add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%
#     dplyr::rename(!!sym(ID_COL) := id) %>%
#     dplyr::select(
#       dplyr::any_of(ID_COL),
#       tot_drainage_areasqkm,
#       bf_width,
#       geom
#     )
# 
#   transects <- cut_cross_sections(
#     net = flowlines,
#     id  = ID_COL,
#     num = NUM_OF_TRANSECTS
#   ) %>%
#   dplyr::select(
#               dplyr::any_of(ID_COL),
#               cs_id,
#               cs_lengthm
#               )
#     # dplyr::filter(hy_id == "wb-1003259", cs_id == 1)
#   # dplyr::filter(hy_id == "wb-1003266", cs_id %in% c(1, 2))
#   # dplyr::filter(hy_id == "wb-1003263", cs_id %in% c(1, 2))
#   # dplyr::filter(hy_id == "wb-1003266", cs_id == 1)
#     # dplyr::filter(hy_id == "wb-1003267", cs_id == 3)
#     # dplyr::filter(hy_id == "wb-1003266", cs_id == 2)
#   # dplyr::filter(hy_id == "wb-1003261", cs_id == 2)
#     # dplyr::filter(hy_id == "wb-1003260", cs_id == 2)
#     # dplyr::filter(hy_id == "wb-1003258", cs_id == 1)
# 
#   cs_pts = hydrofabric3D::cross_section_pts(
#     cs              = transects,
#     crosswalk_id    = ID_COL,
#     points_per_cs   = POINTS_PER_CS,
#     min_pts_per_cs  = MIN_PTS_PER_CS,
#     dem             = DEM_PATH
#   )
# 
#   # -------------------------------------------------------------------
#   # ----- Inputs -----
#   # -------------------------------------------------------------------
#   plots1 <-
#     cs_pts %>%
#     hydrofabric3D::classify_points(crosswalk_id="hy_id") %>%
#     hydrofabric3D::plot_cs_pts(color = "point_type", size = 6)
#   plots1 <-
#     plots1 +
#     ggplot2::labs(title = "Method #1") +
#     ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold"))
#   plots1
#   # num_of_pts          <- cs_pts$points_per_cs[1]
#   # pt_ids              <- cs_pts$pt_id
#   # relative_distances  <- cs_pts$relative_distance
#   # depths              <- cs_pts$Z
#   # cs_length           <- cs_pts$cs_lengthm[1]
# 
#   # -------------------------------------------------------------------
#   # -------------------------------------------------------------------
#   # cs_pts %>%
#     # classify_points(crosswalk_id = "hy_id") %>%
#     # dplyr::select(pt_id, Z, point_type)
# 
#   classified2 <-
#    # plots2 <-
#     cs_pts %>%
#     dplyr::group_by(hy_id, cs_id) %>%
#     dplyr::mutate(
#       point_type = classify_banks_and_bottoms(
#         num_of_pts = points_per_cs[1],
#         pt_ids     = pt_id,
#         depths     = Z
#       ),
#       # smooth_Z = smooth_depths(Z, window = 3),
#       # Z2      = use_smoothed_depths(Z, smooth_Z, point_type)
#       # Z_start = Z,
#       Z           = use_smoothed_depths(start_depths = Z,
#                                         smoothed_depths = smooth_depths(Z, window = 3),
#                                         point_types = point_type
#                                         ),
# 
#       point_type2 = classify_z_pts(
#         depths = Z,
#         num_of_pts        = points_per_cs[1],
#         cs_length         = cs_lengthm[1],
#         relative_distance = relative_distance,
#         pt_ids = pt_id,
#         point_types = point_type
#       ),
#       point_type3 = classify_pts_from_slope(depths = Z)
#       # point_type3 = get_cs_point_types(
#       #                   depths            = Z,
#       #                   num_of_pts        = points_per_cs[1],
#       #                   cs_length         = cs_lengthm[1],
#       #                   relative_distance = relative_distance,
#       #                   point_types       = point_type
#       #                 )
#     )
#   classified2$point_type3
#   plots2 <-
#     classified2 %>%
#     hydrofabric3D::plot_cs_pts(color = "point_type3", size = 6)
#     # dplyr::relocate(Z, point_type, point_type2)
#     # dplyr::select(hy_id, cs_id, pt_id, Z, point_type)
#     # ggplot2::ggplot() +
#     # ggplot2::geom_point(
#     #   ggplot2::aes(
#     #     x = pt_id,
#     #     y = Z,
#     #     color = point_type
#     #   ), size = 5
#     # )
# 
#     plots2 <-
#       plots2 +
#       ggplot2::labs(title = "Method #2") +
#       ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold"))
# 
#     plots2
# 
#   library(patchwork)
#   plots1 / plots2
#   plots1 + plots2
# 
#   cs_pts %>%
#     classify_points(crosswalk_id = "hy_id") %>%
#     dplyr::select(hy_id, cs_id, pt_id, Z, point_type) %>%
#     ggplot2::ggplot() +
#     ggplot2::geom_point(
#       ggplot2::aes(
#         x = pt_id,
#         y = Z,
#         color = point_type
#       ), size = 5
#     )
# 
#   cs_pts %>%
#     dplyr::group_by(hy_id, cs_id) %>%
#     dplyr::mutate(
#       third          = ceiling(dplyr::n() / 3),
#       mean_dist      = mean(diff(relative_distance)),
#       in_channel_pts = ceiling(cs_lengthm[1] / mean_dist),
#       b1             = ceiling(in_channel_pts / 2),
#       b2             = in_channel_pts - b1,
#       low_pt         = min(Z[third[1]:(2*third[1] - 1)]),
#       class          = ifelse(Z <= low_pt & dplyr::between(pt_id, third[1], (2*third[1] - 1)),
#                               "bottom",
#                               "bank"
#       ),
#       Z2             = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()]),
#       Z              = ifelse(class == "bottom", Z, Z2),
#       min_bottom     = which(class == "bottom")[1],
#       mid_bottom     = which(class == "bottom")[ceiling(length(which(class == "bottom"))/2)],
#       max_bottom     = which(class == "bottom")[length(which(class == "bottom"))]
#       # L1             = pmax(1, mid_bottom - b1),
#       # L2             = pmax(1, mid_bottom - b2),
#       # R1             = pmin(mid_bottom + b2, n()),
#       # R2             = pmin(mid_bottom + b1, n()),
#       # anchor         = ifelse(Z[R2] < Z[L1], 2, 1),
#       # L              = pmax(third, ifelse(anchor == 1, L1, L2)),
#       # R              = pmin(2*third[1], ifelse(anchor == 1, R1, R2)),
#       # count_left     = min_bottom - L,
#       # count_right    = R - max_bottom,
#       # L              = ifelse(count_left == 0, L - count_right, L),
#       # R              = ifelse(count_right == 0, R + count_left, R),
#       # class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
#       # class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
#       # class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)
#     ) %>%
#     dplyr::relocate(third, mean_dist, in_channel_pts, b1, b2, low_pt, class, Z, Z2, min_bottom, relative_distance, pt_id)
# 
#   classified <- classify_points(
#     cs_pts = cs_pts,
#     crosswalk_id = ID_COL,
#     pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
#   )
# 
#   classified %>%
#     hydrofabric3D::plot_cs_pts(color = "point_type")
# 
#   true_valid_banks <-
#     classified %>%
#     dplyr::filter(valid_banks) %>%
#     # dplyr::slice(1:10) %>%
#     dplyr::group_by(hy_id, cs_id)  %>%
#     dplyr::mutate(
#       double_check_valid_banks = dplyr::case_when(
#         ((left_bank > bottom) & (!is.na(left_bank))) & ((right_bank > bottom) & (!is.na(right_bank))) ~ TRUE,
#         # left_bank > bottom & right_bank > bottom ~ TRUE,
#         TRUE                                     ~ FALSE
#       ),
#       correct_valid_banks = valid_banks == double_check_valid_banks
#     ) %>%
#     dplyr::relocate(correct_valid_banks, double_check_valid_banks, valid_banks)
# 
#   all_valid_banks_are_correct <- all(true_valid_banks$correct_valid_banks)
#   testthat::expect_true(all_valid_banks_are_correct)
# 
# })
