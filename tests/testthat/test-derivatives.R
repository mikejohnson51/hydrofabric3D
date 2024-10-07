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
#     dplyr::select( 
#       dplyr::any_of(ID_COL),
#       cs_id,
#       cs_lengthm
#     ) 
#   # dplyr::filter(hy_id == "wb-1003267", cs_id == 3)
#   # dplyr::filter(hy_id == "wb-1003267", cs_id == 2)
#   # dplyr::filter(hy_id == "wb-1003259", cs_id == 1)
#   # dplyr::filter(hy_id == "wb-1003266", cs_id %in% c(1, 2))
#   # dplyr::filter(hy_id == "wb-1003263", cs_id %in% c(1, 2))
#   # dplyr::filter(hy_id == "wb-1003266", cs_id == 1)
#   # dplyr::filter(hy_id == "wb-1003267", cs_id == 3)
#   # dplyr::filter(hy_id == "wb-1003266", cs_id == 2)
#   # dplyr::filter(hy_id == "wb-1003261", cs_id == 2)
#   # dplyr::filter(hy_id == "wb-1003260", cs_id == 2)
#   # dplyr::filter(hy_id == "wb-1003258", cs_id == 1)
#   
#   cs_pts = hydrofabric3D::cross_section_pts(
#     cs              = transects,
#     crosswalk_id    = ID_COL,
#     points_per_cs   = POINTS_PER_CS,
#     min_pts_per_cs  = MIN_PTS_PER_CS,
#     dem             = DEM_PATH
#   )
#  
#   cs_pts %>% 
#     hydrofabric3D::classify_points(crosswalk_id = "hy_id") %>% 
#     hydrofabric3D::plot_cs_pts(color = "point_type", size = 5) + 
#     ggplot2::labs(title = "STANDARD")
#    
#   
#   cs_pts %>% 
#     hydrofabric3D::classify_points(crosswalk_id = "hy_id") %>% 
#     hydrofabric3D::plot_cs_pts(color = "point_type", size = 5) + 
#     ggplot2::labs(title = "DERIVATIVE")
#   
#   smoothed <- 
#     cs_pts %>% 
#     dplyr::group_by(hy_id, cs_id) %>% 
#     dplyr::mutate(
#       class = classify_banks_and_bottoms(
#         num_of_pts = points_per_cs[1],
#         pt_ids     = pt_id,
#         depths     = Z
#       ),
#       Z       = use_smoothed_depths(start_depths = Z, 
#                                         smoothed_depths = smooth_depths(Z, window = 3), 
#                                         point_types = class
#       ),
#       anchors = list(
#                   find_anchor_pts(
#                     depths             = Z,
#                     num_of_pts         = points_per_cs[1],
#                     cs_length          = cs_lengthm[1],
#                     relative_distance  = relative_distance,
#                     point_types        = class
#                   )
#                 ),
#       L  = anchors[[1]][1],
#       R  = anchors[[1]][2],
#       class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
#       class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
#       class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class),
#       deriv_type     = classify_derivatives(Z),
#       deriv_type     = dplyr::case_when(
#         (grepl("concave", deriv_type) | deriv_type == "linear") & class != "bottom" ~ "channel",
#         TRUE                         ~ class
#       ),
#       deriv_type   = clean_point_types(deriv_type),
#       deriv_type   = set_bank_anchors(
#                                     depths      = Z,
#                                     point_types = deriv_type,
#                                     L = L[1],
#                                     R = R[1]
#                                     ),
#       point_type   = class
#       
#       # side = dplyr::case_when(
#       #   pt_id >= R[1] ~ "right_side",
#       #   pt_id <= L[1] ~ "left_side",
#       #   TRUE          ~ "middle"
#       # ),
#       # # max_left      = which.max(Z[1:L[1]]),
#       # # max_right     = which.max(Z[R[1]:length(Z)]) + R[1],
#       # max_left      = pmax(which.max(Z[1:L[1]]), 1),
#       # max_right     = pmin(which.max(Z[R[1]:length(Z)]) + R[1], points_per_cs[1]),
#       # deriv_type    = dplyr::case_when(
#       #   # pt_id <= max_left ~ "left_bank",
#       #   # pt_id >= max_right ~ "right_bank",
#       #   pt_id == max_left ~ "left_bank",
#       #   pt_id == max_right ~ "right_bank",
#       #   TRUE ~ deriv_type
#       #   ),
#       # point_type = class,
#       # clean_pt   = clean_point_types(point_type),
#       # clean_pt   = set_bank_anchors(depths = Z, 
#       #                               point_types = clean_pt,
#       #                               L = L[1],
#       #                               R = R[1]
#       #                               ),
#       # clean_der   = clean_point_types(deriv_type),
#       # clean_der2  = set_bank_anchors(depths = Z,
#       #                                point_types = clean_der,
#       #                                L = L[1],
#       #                                R = R[1]
#       # )
#     )
#   
#   # smoothed %>%
#   #   # dplyr::filter(hy_id == "wb-1003265", cs_id == 2) %>%
#   #   hydrofabric3D::plot_cs_pts(color = "point_type", size = 4) +
#   #   ggplot2::labs(title = "point type")
#   
#   # smoothed %>% 
#   #   # dplyr::filter(hy_id == "wb-1003265", cs_id == 2) %>% 
#   #   hydrofabric3D::plot_cs_pts(color = "clean_pt", size = 4) + 
#   #   ggplot2::labs(title = "clean point type")
#   
#   smoothed %>% 
#     # dplyr::filter(hy_id == "wb-1003265", cs_id == 2) %>% 
#     hydrofabric3D::plot_cs_pts(color = "deriv_type", size = 4) + 
#     ggplot2::labs(title = "deriv")
#   
#   smoothed %>% 
#     # dplyr::filter(hy_id == "wb-1003265", cs_id == 2) %>% 
#     hydrofabric3D::plot_cs_pts(color = "clean_der", size = 4) + 
#     ggplot2::labs(title = "clean deriv")
#   
#   smoothed %>% 
#     # dplyr::filter(hy_id == "wb-1003265", cs_id == 2) %>% 
#     hydrofabric3D::plot_cs_pts(color = "clean_der2", size = 4) + 
#     ggplot2::labs(title = "clean deriv 2")
# })
# })