# library(testthat)
# library(dplyr)
# library(sf)
# # # library(hydrofabric3D)

# source("testing_utils.R")
# # source("tests/testthat/testing_utils.R")
# # devtools::load_all()

# # -----------------------------------------------------------------------------------------

# classify_pts_tmp = function(
#     cs_pts, 
#     crosswalk_id = NULL,
#     pct_of_length_for_relief = 0.01
#     ){
  
#   . <-  L <-  L1 <-  L2  <-  R  <-  R1 <-  R2  <- Z  <-  Z2 <-  anchor <-  b1  <- b2  <- cs_lengthm  <- count_left <- 
#     count_right  <-  cs_id <-  hy_id <-  in_channel_pts  <- lengthm <-  low_pt  <- max_bottom  <- mean_dist <-  mid_bottom  <- min_bottom  <- pt_id <- relative_distance <-  third <- NULL
#     cs_pts

#     cs_pts %>%
#         dplyr::slice(1:10)  %>% 
#         sf::st_drop_geometry() %>%
#         dplyr::select(-points_per_cs) %>%
#         dplyr::group_by(hy_id, cs_id) %>%
#         dplyr::mutate(
#             third          = calculate_third(dplyr::n()),
#             mean_dist      = calculate_mean_dist(relative_distance),
#             in_channel_pts = calculate_in_channel_pts(cs_lengthm[1], mean_dist),
#             b1             = calculate_b1(in_channel_pts),
#             b2             = calculate_b2(in_channel_pts, b1),
#             low_pt         = find_low_pt(Z, third[1]),
#             class          = classify_initial(Z, low_pt, pt_id, third[1]),
#             # Z2             = smooth_depth(Z, class)
#             Z2             = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()]),
#             Z              = ifelse(class == "bottom", Z, Z2),
#             min_bottom     = find_min_bottom(class),
#             mid_bottom     = find_mid_bottom(class),
#             max_bottom     = find_max_bottom(class),
#             L1             = calculate_L1(mid_bottom, b1),
#             L2             = calculate_L2(mid_bottom, b2),
#             R1             = calculate_R1(mid_bottom, b2, dplyr::n()),
#             R2             = calculate_R2(mid_bottom, b1, dplyr::n()),
#             anchor         = determine_anchor(Z, L1, R2),
#             L              = adjust_L(third, anchor, L1, L2),
#             R              = adjust_R(third, anchor, R1, R2),
#             count_left     = calculate_count_left(min_bottom, L),
#             count_right    = calculate_count_right(R, max_bottom),
#             # boundaries     = adjust_boundaries(L, count_left, count_right, R)
#             L              = ifelse(count_left == 0, L - count_right, L),
#             R              = ifelse(count_right == 0, R + count_left, R),
#             class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
#             class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
#             class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)
#             # L              = boundaries$L,
#             # R              = boundaries$R,
#             # class          = final_classify(pt_id, L[1], R[1], class)
#         )  %>% 
#         dplyr::relocate(Z2, Z, class, third, mean_dist, in_channel_pts, b1, b2, low_pt) 

# # Adjust the boundaries if there are no points to the left or right
# adjust_boundaries <- function(L, count_left, count_right, R) {
#   L <- ifelse(count_left == 0, L - count_right, L)
#   R <- ifelse(count_right == 0, R + count_left, R)
#   return(list(L = L, R = R))
# }
#     cpts <-
#         cs_pts %>%
#         dplyr::group_by(hy_id, cs_id) %>%
#         dplyr::mutate(
#             third          = calculate_third(dplyr::n()),
#             mean_dist      = calculate_mean_dist(relative_distance),
#             in_channel_pts = calculate_in_channel_pts(cs_lengthm[1], mean_dist),
#             b1             = calculate_b1(in_channel_pts),
#             b2             = calculate_b2(in_channel_pts, b1),
#             low_pt         = find_low_pt(Z, third[1]),
#             class          = classify_initial(Z, low_pt, pt_id, third[1]),
#             Z2             = smooth_depth(Z, class),
#             Z              = ifelse(class == "bottom", Z, Z2),
#             min_bottom     = find_min_bottom(class),
#             mid_bottom     = find_mid_bottom(class),
#             max_bottom     = find_max_bottom(class),
#             L1             = calculate_L1(mid_bottom, b1),
#             L2             = calculate_L2(mid_bottom, b2),
#             R1             = calculate_R1(mid_bottom, b2, dplyr::n()),
#             R2             = calculate_R2(mid_bottom, b1, dplyr::n()),
#             anchor         = determine_anchor(Z, L1, R2),
#             L              = adjust_L(third, anchor, L1, L2),
#             R              = adjust_R(third, anchor, R1, R2),
#             count_left     = calculate_count_left(min_bottom, L),
#             count_right    = calculate_count_right(R, max_bottom),
            
#             # boundaries     = adjust_boundaries(L, count_left, count_right, R),
#             # L              = boundaries$L,
#             # R              = boundaries$R,
#             # class          = final_classify(pt_id, L[1], R[1], class)
#         ) %>%
#         dplyr::ungroup()

#     cpts2 <-
#         dplyr::filter(cs_pts) %>% 
#         # dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
#         dplyr::group_by(hy_id, cs_id) %>%
#         dplyr::mutate(
#         third          = ceiling(dplyr::n() / 3),
#         mean_dist      = mean(diff(relative_distance)),
#         in_channel_pts = ceiling(cs_lengthm[1] / mean_dist),
#         b1             = ceiling(in_channel_pts / 2),
#         b2             = in_channel_pts - b1,
#         low_pt         = min(Z[third[1]:(2*third[1] - 1)]),
#         class          = ifelse(Z <= low_pt & dplyr::between(pt_id, third[1], (2*third[1] - 1)), 
#                                 "bottom", 
#                                 "bank"
#         ),
#         Z2             = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()]),
#         Z              = ifelse(class == "bottom", Z, Z2),
#         min_bottom     = which(class == "bottom")[1],
#         mid_bottom     = which(class == "bottom")[ceiling(length(which(class == "bottom"))/2)],
#         max_bottom     = which(class == "bottom")[length(which(class == "bottom"))],
#         L1             = pmax(1, mid_bottom - b1),
#         L2             = pmax(1, mid_bottom - b2),
#         R1             = pmin(mid_bottom + b2, n()),
#         R2             = pmin(mid_bottom + b1, n()),
#         anchor         = ifelse(Z[R2] < Z[L1], 2, 1),
#         L              = pmax(third, ifelse(anchor == 1, L1, L2)),
#         R              = pmin(2*third[1], ifelse(anchor == 1, R1, R2)),
#         count_left     = min_bottom - L,
#         count_right    = R - max_bottom,
#         L              = ifelse(count_left == 0, L - count_right, L),
#         R              = ifelse(count_right == 0, R + count_left, R),
#         class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
#         class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
#         class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)
#         ) %>%
#         dplyr::ungroup() 
# }


# calculate_third <- function(n) {
#   ceiling(n / 3)
# }

# calculate_mean_dist <- function(relative_distance) {
#   mean(diff(relative_distance))
# }

# calculate_in_channel_pts <- function(cs_lengthm, mean_dist) {
#   ceiling(cs_lengthm / mean_dist)
# }

# calculate_b1 <- function(in_channel_pts) {
#   ceiling(in_channel_pts / 2)
# }

# calculate_b2 <- function(in_channel_pts, b1) {
#   in_channel_pts - b1
# }

# find_low_pt <- function(Z, third) {
#   min(Z[third:(2*third - 1)])
# }

# classify_initial <- function(Z, low_pt, pt_id, third) {
#   ifelse(Z <= low_pt & dplyr::between(pt_id, third, (2*third - 1)), 
#          "bottom", 
#          "bank")
# }

# cs_pts

# smooth_depth <- function(Z, class) {
#     message("Z: ", Z)
#     message("class(Z): ", class(Z))

#     message("class: ", class)
#     message("class(class): ", class(class))

#   if (class == "bottom") {
#     Z
#   } else {
#     c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()])
#   }
# }

# find_min_bottom <- function(class) {
#   which(class == "bottom")[1]
# }

# find_mid_bottom <- function(class) {
#   which(class == "bottom")[ceiling(length(which(class == "bottom"))/2)]
# }

# find_max_bottom <- function(class) {
#   which(class == "bottom")[length(which(class == "bottom"))]
# }

# calculate_L1 <- function(mid_bottom, b1) {
#   pmax(1, mid_bottom - b1)
# }

# calculate_L2 <- function(mid_bottom, b2) {
#   pmax(1, mid_bottom - b2)
# }

# calculate_R1 <- function(mid_bottom, b2, n) {
#   pmin(mid_bottom + b2, n)
# }

# calculate_R2 <- function(mid_bottom, b1, n) {
#   pmin(mid_bottom + b1, n)
# }

# determine_anchor <- function(Z, L1, R2) {
#   ifelse(Z[R2] < Z[L1], 2, 1)
# }

# adjust_L <- function(third, anchor, L1, L2) {
#   pmax(third, ifelse(anchor == 1, L1, L2))
# }

# adjust_R <- function(third, anchor, R1, R2) {
#   pmin(2*third, ifelse(anchor == 1, R1, R2))
# }

# calculate_count_left <- function(min_bottom, L) {
#   min_bottom - L
# }

# calculate_count_right <- function(R, max_bottom) {
#   R - max_bottom
# }

# adjust_boundaries <- function(L, count_left, count_right, R) {
#   L <- ifelse(count_left == 0, L - count_right, L)
#   R <- ifelse(count_right == 0, R + count_left, R)
#   return(list(L = L, R = R))
# }

# final_classify <- function(pt_id, L, R, class) {
#   class <- ifelse(dplyr::between(pt_id, L, R) & class != 'bottom', "channel", class)
#   class <- ifelse(class == 'bank' & pt_id <= L, "left_bank", class)
#   class <- ifelse(class == 'bank' & pt_id >= R, "right_bank", class)
#   return(class)
# }

# # -------------------------------------------------------------------
# # ---- hydrofabric3D::classify_pts() ----
# # -------------------------------------------------------------------

# # TODO:
# testthat::test_that("check CLASSIFIED CS points default output columns, from DEFAULT transects output", {
  
#   flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
#   # flowlines    <- dplyr::slice(flowlines, 1)
  
#   MIN_BF_WIDTH       <- 50
#   ID_COL             <- "hy_id"
#   NUM_OF_TRANSECTS   <- 3
  
#   # Cross section point inputs
#   DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
#   POINTS_PER_CS     <- NULL
#   MIN_PTS_PER_CS    <- 10
  
#   PCT_OF_LENGTH_FOR_RELIEF <- 0.01
  
#   flowlines <-
#     flowlines %>% 
#     dplyr::slice(1) %>%
#     # dplyr::slice(1:3) %>% 
#     add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
#     dplyr::rename(!!sym(ID_COL) := id) %>% 
#     dplyr::select(
#       dplyr::any_of(ID_COL), 
#       tot_drainage_areasqkm,
#       bf_width,
#       geom
#     ) 
  
#   transects <- cut_cross_sections(
#     net = flowlines,
#     id  = ID_COL,  
#     num = NUM_OF_TRANSECTS
#   )
  
#   transects <- dplyr::select(transects,
#                              dplyr::any_of(ID_COL),
#                              cs_id,
#                              cs_lengthm
#   )
  
#   cs_pts = hydrofabric3D::cross_section_pts(
#     cs              = transects,
#     crosswalk_id    = ID_COL,
#     points_per_cs   = POINTS_PER_CS,
#     min_pts_per_cs  = MIN_PTS_PER_CS,
#     dem             = DEM_PATH
#   )
#   # cs_pts  %>% 
#   # # dplyr::select(hy_id, cs_id, pt_id, Z, relative_distance)  %>% 
#   # sf::st_drop_geometry() %>%
#   #  head(20)
#   classified <- classify_points(
#     cs_pts = cs_pts,
#     crosswalk_id = ID_COL,
#     pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
#   ) 

# })