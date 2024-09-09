# File containing a bunch of utility functions used for testing 
# - checking correct column outputs
# - checking SF characteristics (geometry type, CRS, etc)
#  

# library(hydrofabric3D)


prep_flowlines_for_transect_cuts <- function(flines, id_col, min_bf_width) {

  flines <- 
    flines %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", min_bf_width) %>%  
    dplyr::rename(!!sym(id_col) := id) %>% 
    nhdplusTools::rename_geometry("geometry") %>% 
    dplyr::select(
      dplyr::any_of(id_col), 
      tot_drainage_areasqkm,
      bf_width,
      geometry
    ) 
  
  return(flines)
  
}

# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
relief_detailed_has_min_output_cols <- function(relief, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id" ,
                     "cs_lengthm",
                     "has_relief",
                     "max_relief", 
                     "pct_of_length_for_relief"
                     )
  
  return(
    all(expected_cols %in% names(relief)) && length(expected_cols) == length(names(relief))
  )
}


# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
relief_has_min_output_cols <- function(relief, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id" ,
                     "has_relief"
  )
  
  return(
    all(expected_cols %in% names(relief)) && length(expected_cols) == length(names(relief))
  )
}


has_same_unique_tmp_ids <- function(x, y, id = "hydrofabric_id") {
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  start_ids <- hydrofabric3D:::get_unique_tmp_ids(df = x, x = get(id))
  end_ids   <- hydrofabric3D:::get_unique_tmp_ids(df = y, x = get(id))
  
  # all IDs are in x AND y and same number of ids
  same_unique_ids <- all(start_ids %in% end_ids) && all(end_ids %in% start_ids) && length(start_ids) == length(end_ids)
  
  return(same_unique_ids)
}

# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
bank_attrs_has_min_output_cols <- function(bank_attrs, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id" ,
                     "bottom",
                     "left_bank",
                     "right_bank",
                     "valid_banks"
  )
  
  return(
    all(expected_cols %in% names(bank_attrs)) && length(expected_cols) == length(names(bank_attrs))
  )
}

# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
classified_cs_pts_has_min_output_cols <- function(classified_pts, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "class", "point_type", 
                     "points_per_cs", 
                     "bottom",
                     "left_bank",
                     "right_bank",
                     "valid_banks",
                     "has_relief",
                     "geometry"
                     )
  
  return(
    all(expected_cols %in% names(classified_pts)) && length(expected_cols) == length(names(classified_pts))
  )
}

# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
cs_pts_has_min_output_cols <- function(cs_pts, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
  )
  
  return(
    all(expected_cols %in% names(cs_pts)) && length(expected_cols) == length(names(cs_pts))
  )
}

check_cs_pts_has_exact_cols <- function(cs_pts, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
                     )
  
  return(
    all(expected_cols %in% names(cs_pts)) && length(expected_cols) == length(names(cs_pts))
  )
}


# TODO: this is a better named version of check_cs_pts_and_transect_cols() (DUPLICATE)
cs_pts_has_correct_cols_from_transects <- function(cs_pts, transects, id = "hydrofabric_id") {
  # id = "hy_id"
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
  )
  
  return(
    all(unique(c(expected_cols, names(transects))) %in% names(cs_pts))
  )
}

check_cs_pts_and_transect_cols <- function(cs_pts, transects, id = "hydrofabric_id") {
  # id = "hy_id"
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
                     )
  
  return(
    all(unique(c(expected_cols, names(transects))) %in% names(cs_pts))
  )
}

check_cs_pts_has_required_cols <- function(transects, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
  )
  
  return(
    all(expected_cols %in% names(transects))
  )
}

has_same_crs <- function(sf1, sf2) {
  
  return (
    sf::st_crs(sf1) == sf::st_crs(sf2)
  )
  
}
get_cs_point_types2 <- function(
    depths,
    num_of_pts,
    cs_length,
    relative_distance,
    point_types
) {
  
  # ----------------------------------------------------------------------
  # num_of_pts          <- cs_pts$points_per_cs[1]
  # pt_ids              <- cs_pts$pt_id
  # relative_distance   <- cs_pts$relative_distance
  # depths              <- cs_pts$Z
  # cs_length           <- cs_pts$cs_lengthm[1]
  # 
  # point_types = classify_banks_and_bottoms(
  #   num_of_pts = num_of_pts,
  #   pt_ids     = pt_ids,
  #   depths     = depths
  # )
  # 
  # # smooth_Z = smooth_depths(Z, window = 3),
  # # Z2      = use_smoothed_depths(Z, smooth_Z, point_type)
  # # Z_start = Z,
  # 
  # depths           = use_smoothed_depths(start_depths = depths, 
  #                                   smoothed_depths = smooth_depths(depths, window = 3), 
  #                                   point_types = point_types
  # )
  # 
  # # plot(Z)
  
  # ----------------------------------------------------------------------
  # ----------------------------------------------------------------------
  
  # get the number of points in each third of the cross section  
  third          <- ceiling(num_of_pts / 3)
  
  # Find the anchor points that tell us where the left / right banks start 
  # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
  anchors <- find_anchor_pts(
    depths            = depths,
    num_of_pts        = num_of_pts,
    cs_length         = cs_length,
    relative_distance = relative_distance,
    point_types       = point_types
  )
  
  left_anchor    <- anchors[1]
  bottom_anchor  <- anchors[2]
  right_anchor   <- anchors[3]
  
  left_to_middle  <- 1:left_anchor
  middle_to_right <- right_anchor:length(depths)
  
  left_side    <- depths[left_to_middle]
  right_side   <- depths[middle_to_right]
  
  # middle_third <- (left_anchor + 1):(right_anchor - 1)
  middle_third <- (left_anchor):(right_anchor)
  
  point_types[middle_third]  <- ifelse(
    point_types[middle_third] != "bottom", 
    "channel", 
    point_types[middle_third]
  )
  
  point_types[1:(left_anchor-1)]                      <- "left_bank"
  point_types[(right_anchor+1):length(point_types)]   <- "right_bank"
  # point_types[1:left_anchor]                      <- "left_bank"
  # point_types[right_anchor:length(point_types)]   <- "right_bank"
  
  return(point_types)
  
}


get_cs_point_types <- function(
    depths,
    num_of_pts,
    cs_length,
    relative_distance,
    point_types
) {
  
  # ----------------------------------------------------------------------
  # num_of_pts          <- cs_pts$points_per_cs[1]
  # pt_ids              <- cs_pts$pt_id
  # relative_distance   <- cs_pts$relative_distance
  # depths              <- cs_pts$Z
  # cs_length           <- cs_pts$cs_lengthm[1]
  # 
  # point_types = classify_banks_and_bottoms(
  #   num_of_pts = num_of_pts,
  #   pt_ids     = pt_ids,
  #   depths     = depths
  # )
  # 
  # # smooth_Z = smooth_depths(Z, window = 3),
  # # Z2      = use_smoothed_depths(Z, smooth_Z, point_type)
  # # Z_start = Z,
  # 
  # depths           = use_smoothed_depths(start_depths = depths, 
  #                                   smoothed_depths = smooth_depths(depths, window = 3), 
  #                                   point_types = point_types
  # )
  # 
  # # plot(Z)
  
  # ----------------------------------------------------------------------
  # ----------------------------------------------------------------------
  
  # get the number of points in each third of the cross section  
  third          <- ceiling(num_of_pts / 3)
  
  # Find the anchor points that tell us where the left / right banks start 
  # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
  anchors <- find_anchor_pts(
    depths            = depths,
    num_of_pts        = num_of_pts,
    cs_length         = cs_length,
    relative_distance = relative_distance,
    point_types       = point_types
  )
  
  left_anchor    <- anchors[1]
  bottom_anchor  <- anchors[2]
  right_anchor   <- anchors[3]
  
  left_to_middle  <- 1:left_anchor
  middle_to_right <- right_anchor:length(depths)
  
  # plot(depths)
  # plot(depths[left_to_middle])
  
  # plot_df <-
  #   data.frame(Z = depths) %>%
  #   dplyr::mutate(
  #     n = 1:dplyr::n(),
  #     side = dplyr::case_when(
  #       n %in% left_to_middle ~ "left_side",
  #       n %in% middle_to_right ~ "right_side",
  #       TRUE ~ "bottom"
  #     ))
  # plot_df %>%
  #   ggplot2::ggplot() +
  #     ggplot2::geom_point(
  #       ggplot2::aes(
  #         x = n,
  #         y = Z,
  #         color = side), size = 4)
  
  left_side    <- depths[left_to_middle]
  right_side   <- depths[middle_to_right]
  
  # middle_third <- (left_anchor + 1):(right_anchor - 1)
  # 
  # point_types[middle_third]  <- ifelse(
  #                                   point_types[middle_third] != "bottom", 
  #                                   "channel", 
  #                                   point_types[middle_third]
  #                                   )
  # 
  # point_types[1:left_anchor]                      <- "left_bank"
  # point_types[right_anchor:length(point_types)]   <- "right_bank"
  
  
  # middle_to_right
  # TODO: this could throw an error
  # max(depths[0])
  
  # get the maximum depth (highest elevation) point on the LEFT side and all points equal to that max elevation
  left_max       <- max(left_side)
  left_max_pts   <- which(left_side == left_max)
  
  # rev_max_left_idx <- which.max(rev(left_side))
  # (length(left_side) - rev_max_left_idx) + 1
  # which.max(left_side)
  # which.max(rev(left_side))
  
  # get the maximum depth (highest elevation) point on the RIGHT side and all points equal to that max elevation
  right_max       <- max(right_side)
  right_max_pts   <- (which(right_side == right_max) + right_anchor) - 1
  # right_max_pts   <- which(right_side == right_max)
  
  # (right_max_pts + right_anchor) - 1
  # (num_of_pts - right_max_pts ) + 1
  
  start_of_left_bank  <- left_max_pts[length(left_max_pts)]
  start_of_right_bank <- right_max_pts[1]
  # start_of_right_bank <- right_max_pts[length(right_max_pts)]
  
  # assign points to the LEFT of the max LEFT SIDE elevation to "left_bank", then
  # set the remaining points between that high elevation and the "bottom" to "channel" points
  point_types[1:(start_of_left_bank-1)]                    <- "left_bank"
  
  # if (start_of_left_bank == left_anchor) {
  #   point_types[(start_of_left_bank + 1)]              <- "channel"
  # } else {
  #   point_types[(start_of_left_bank + 1):left_anchor]  <- "channel"
  # }
  #
  # if (start_of_left_bank != left_anchor) {
  #   point_types[(start_of_left_bank + 1):left_anchor]  <- "channel"
  # } else {
  #   point_types[(start_of_left_bank + 1)]              <- "channel"
  # }
  
  # assign points to the RIGHT of the max RIGHT SIDE elevation to "right_bank", then
  # set the remaining points between the right side of the "bottom" and the right side high elevation point to "channel" points
  point_types[(start_of_right_bank + 1):length(point_types)]   <- "right_bank"
  # point_types[right_anchor:(start_of_right_bank - 1)]  <- "channel"
  
  # left_of_right_side <- (start_of_right_bank - 1)
  #
  # if (start_of_right_bank == right_anchor) {
  #   point_types[left_of_right_side]               <- "channel"
  # } else {
  #   point_types[right_anchor:(start_of_right_bank - 1)]  <- "channel"
  # }
  
  # point_types != "bottom"
  
  # middle_section <- point_types[(left_anchor + 1):(right_anchor - 1)]
  # point_types[(left_anchor + 1):(right_anchor - 1)] != "bottom"
  
  middle_third <- (left_anchor):(right_anchor)
  # middle_third <- (left_anchor + 1):(right_anchor - 1)
  
  point_types[middle_third]  <- ifelse(
    point_types[middle_third] != "bottom",
    "channel",
    point_types[middle_third]
  )
  
  # point_types[left_anchor]
  
  # # TODO: this sets any remaining bank points to "channel" points as needed
  point_types[which(point_types == "bank")]              <- "channel"
  
  # right_anchor:(start_of_right_bank - 1)
  return(point_types)
  
}

classify_cs_pts <- function(
    depths,
    pt_ids,
    num_of_pts,
    relative_distances,
    cs_length
) {
  
  point_types <- classify_banks_and_bottoms(
    num_of_pts = num_of_pts,
    pt_ids     = pt_ids,
    depths     = depths
  )
  # point_types
  # depths %>% 
  #   smooth_depths(depths, window = 3) %>% 
  #   use_smoothed_depths(depths, ., point_types)
  # smooth_Z <- use_smoothed_depths(
  #                 depths, 
  #                 smooth_depths(depths, window = 3), 
  #                 point_types
  #                 # )
  # smoothed_depths <- smooth_depths(
  #   depths = depths, 
  #   window = 3
  #   )
  
  # TODO: Good simple plot showing smoothing changes from DEM ----> smoothed DEM pts
  # plot(depths)
  # plot(smooth_depths(depths, window = 3))
  # plot(use_smoothed_depths(
  #       depths, 
  #       smooth_depths(depths, window = 3),  # get a smoothed version of the DEM points
  #       point_types
  #     ))
  
  # # get a smoothed version of the DEM points
  # depths <- use_smoothed_depths(depths, smoothed_depths, point_types)
  
  depths <- use_smoothed_depths(
    depths, 
    smooth_depths(depths, window = 3),  # get a smoothed version of the DEM points
    point_types
  )
  
  # plot(smooth_depth)
  # banks_and_anchor <- find_banks_and_anchor_pts(depths = depths,num_of_pts  = num_of_pts,
  # cs_length = cs_length, relative_distance = relative_distances,
  # point_types = point_types)
  
  
  # Find the anchor points that tell us where the left / right banks start 
  # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
  anchors <- find_anchor_pts(
    depths            = depths,
    num_of_pts        = num_of_pts,
    cs_length         = cs_length,
    relative_distance = relative_distances,
    point_types       = point_types
  )
  
  left_anchor    <- anchors[1]
  bottom_anchor  <- anchors[2]
  right_anchor   <- anchors[3]
  
  third          <- ceiling(num_of_pts / 3)
  
  # left_bank    <- banks_and_anchor[1]
  # anchor       <- banks_and_anchor[2]
  # right_bank   <- banks_and_anchor[3]
  
  left_to_middle  <- 1:left_anchor
  middle_to_right <- right_anchor:length(depths)
  
  # plot(depths)
  # plot(depths[left_to_middle])
  
  # plot_df <- 
  #   data.frame(
  #   Z = depths
  # ) %>% 
  #   dplyr::mutate(
  #     n = 1:dplyr::n(),
  #     side = dplyr::case_when(
  #       n %in% left_to_middle ~ "left_side",
  #       n %in% middle_to_right ~ "right_side",
  #       TRUE ~ "bottom"
  #     )
  #   )
  # 
  # plot_df %>% 
  #   ggplot2::ggplot() +
  #     ggplot2::geom_point(
  #       ggplot2::aes(
  #         x = n,
  #         y = Z,
  #         color = side
  #       ),
  #       size = 4
  #     )
  # 
  
  left_side    <- depths[left_to_middle]
  right_side   <- depths[middle_to_right]
  
  # middle_to_right
  # TODO: this could throw an error 
  # max(depths[0])
  
  # get the maximum depth (highest elevation) point on the LEFT side and all points equal to that max elevation
  left_max       <- max(left_side)
  left_max_pts   <- which(left_side == left_max)
  
  # rev_max_left_idx <- which.max(rev(left_side))
  # (length(left_side) - rev_max_left_idx) + 1
  # which.max(left_side)
  # which.max(rev(left_side))
  
  # get the maximum depth (highest elevation) point on the RIGHT side and all points equal to that max elevation
  right_max       <- max(right_side)
  right_max_pts   <- (which(right_side == right_max) + right_anchor) - 1
  # right_max_pts   <- which(right_side == right_max)
  # (right_max_pts + right_anchor) - 1
  # (num_of_pts - right_max_pts ) + 1
  
  start_of_left_bank  <- left_max_pts[length(left_max_pts)]
  start_of_right_bank <- right_max_pts[1]
  # start_of_right_bank <- right_max_pts[length(right_max_pts)]
  
  # assign points to the LEFT of the max LEFT SIDE elevation to "left_bank", then
  # set the remaining points between that high elevation and the "bottom" to "channel" points
  point_types[1:start_of_left_bank]                    <- "left_bank"
  point_types[(start_of_left_bank + 1):left_anchor]    <- "channel"
  
  # assign points to the RIGHT of the max RIGHT SIDE elevation to "right_bank", then
  # set the remaining points between the right side of the "bottom" and the right side high elevation point to "channel" points
  point_types[start_of_right_bank:length(point_types)] <- "right_bank"
  point_types[right_anchor:(start_of_right_bank - 1)]  <- "channel"
  
  # right_anchor:(start_of_right_bank - 1)
  return(point_types)
  
}

# depths
# 
# left_max_Z
# 
# classed_pts <- 
#   cs_pts %>%
#     sf::st_drop_geometry() %>% 
#     hydrofabric3D::classify_points(crosswalk_id = "hy_id") %>% 
#     dplyr::mutate(
#       new_classes = FALSE
#     )
# 
# classed_pts2 <- classed_pts 
# classed_pts2$Z          <- depths
# classed_pts2$point_type <- classify_z_pts(
#                                 depths      = depths,
#                                 pt_ids      = pt_ids,
#                                 point_types = point_types,
#                                 L           = left_bank,
#                                 R           = right_bank
#                               )
# classed_pts2 <- 
#   classed_pts2 %>% 
#   dplyr::mutate(
#     new_classes = TRUE
#   )
# 
# dplyr::bind_rows(
#   classed_pts,
#   classed_pts2
# ) %>% 
# # classed_pts %>% 
#   ggplot2::ggplot() +
#   ggplot2::geom_point(
#     ggplot2::aes(
#       x = pt_id,
#       y = Z,
#       color = point_type
#     ), 
#     size = 5
#   ) +
#   ggplot2::facet_wrap(~new_classes)

# hydrofabric3D::plot_cs_pts(color = "point_type2")

# bank_idxs <- find_in_channel_pts(relative_distances, cs_length) 
# 
# LEFT = L
# RIGHT = R
# 
# class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class)
# class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class)
# class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)

# # compute the first derivative (slope)
# slope <- diff(depths) / diff(relative_distances)
# 
# # compute the second derivative (curvature)
# second_derivative <- diff(slope) / diff(relative_distances[-1])
# 
# second_derivative %>% round(5)

classify_banks_and_bottoms <- function(
    num_of_pts, 
    pt_ids, 
    depths
) {
  
  # calc the 
  # - number of points in a third of the cross section (third)
  third              <- ceiling(num_of_pts / 3)
  
  mid_third_left_idx   <- third
  mid_third_right_idx  <- ((2 * third) - 1)
  
  mid_third_idxs    <- mid_third_left_idx:mid_third_right_idx
  mid_third_low_pt  <- min(depths[mid_third_idxs])
  
  # logic for determining if its a bottom point (at lowest depth AND in middle third)
  is_at_bottom_Z      <- depths <= mid_third_low_pt
  is_at_middle_third  <- dplyr::between(pt_ids, mid_third_left_idx, mid_third_right_idx)
  
  point_type <- ifelse(is_at_bottom_Z & is_at_middle_third, "bottom", "bank")
  
  return(point_type)
}

# Use the bottom points from the original depth values otherwise use the (rolling mean) smoothed depth values
use_smoothed_depths <- function(
    start_depths, 
    smoothed_depths, 
    point_types
) {
  return(
    ifelse(point_types == "bottom", start_depths, smoothed_depths)
  )
}

find_in_channel_pts <- function(
    relative_distances, 
    cs_length
) {
  
  # calc the:
  # - mean distance between points (mean_dist)
  # - how much in channel points we have (in_channel_pts)
  
  dist_between_pts   <- mean(diff(relative_distances))
  in_channel_pts     <- ceiling(cs_length / dist_between_pts)
  
  right_anchor <- ceiling(in_channel_pts / 2)
  left_anchor  <- in_channel_pts - right_anchor
  
  return(c(left_anchor, right_anchor))
}

# given depths and relative distances and points classified into banks vs bottoms
# determine the indices for 
# - start of the left side bank/channel 
# - start of theright/side bank channel
# - the index of the point at the middle of the bottom 
# Returns a numeric vector of length 3 with the index of the left anchor, middle point, and right anchor (i.e. c(2, 5, 9) -> c(left_anchor, middle_bottom, right_anchor))
find_anchor_pts <- function(depths, 
                            num_of_pts,
                            cs_length, 
                            relative_distance, 
                            point_types
) {
  
  
  # ------------------------------
  # depths            = depths
  # num_of_pts        = num_of_pts
  # relative_distance = relative_distances
  # cs_length         = cs_length
  # point_types       = point_types
  
  # ------------------------------
  third          = ceiling(num_of_pts / 3)
  
  dist_between_pts   <- mean(diff(relative_distance))
  in_channel_pts     <- ceiling(cs_length / dist_between_pts)
  
  b1  <- ceiling(in_channel_pts / 2) # b1
  b2  <- in_channel_pts - b1 # b2
  
  # bank_idxs      <- find_in_channel_pts(relative_distances, cs_length)
  # 
  # left_bank      <- bank_idxs[1] 
  # right_bank     <- bank_idxs[2]
  
  bottom_idxs <- which(point_types == "bottom")
  # point_type_is_bottom = which(point_types == "bottom")
  
  min_bottom  <- bottom_idxs[1]
  mid_bottom  <- bottom_idxs[ceiling(length(bottom_idxs) / 2)]
  max_bottom  <- bottom_idxs[length(bottom_idxs)]
  # min_bottom     = which(point_types == "bottom")[1]
  # mid_bottom     = which(point_types == "bottom")[ceiling(length(which(point_types == "bottom"))/2)]
  # max_bottom     = which(point_types == "bottom")[length(which(point_types == "bottom"))]
  
  L1             = pmax(1, mid_bottom - b1)
  L2             = pmax(1, mid_bottom - b2)
  
  R1             = pmin(mid_bottom + b2, num_of_pts)
  R2             = pmin(mid_bottom + b1, num_of_pts)
  
  anchor         = ifelse(depths[R2] < depths[L1], 2, 1)
  
  LEFT           = pmax(third, ifelse(anchor == 1, L1, L2))
  RIGHT          = pmin(2*third, ifelse(anchor == 1, R1, R2))
  
  count_left     = min_bottom - LEFT
  count_right    = RIGHT - max_bottom
  
  LEFT           = ifelse(count_left == 0, LEFT - count_right, LEFT)
  RIGHT          = ifelse(count_right == 0, RIGHT + count_left, RIGHT)
  
  return(c(LEFT, mid_bottom, RIGHT))
  
}

smooth_depths <- function(
    depths,
    num_of_pts = NULL,
    window     = 3
) {
  
  # depths <- cs_pts$Z
  # num_of_pts          <- 10
  # window = 3
  
  if(is.null(num_of_pts)) {
    num_of_pts <- length(depths)    
  } 
  
  # calculate a rolling mean of the depths between the starting and ending depth
  smoothed_depths <- c(
    depths[1],
    zoo::rollmean(depths, window),
    depths[num_of_pts]
  )
  
  return(smoothed_depths)
}

classify_z_pts <- function(
    depths, 
    num_of_pts,
    cs_length,
    relative_distance,
    pt_ids, 
    point_types
    # L,
    # R
) {
  
  # Find the anchor points that tell us where the left / right banks start 
  # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
  anchors <- find_anchor_pts(
    depths            = depths,
    num_of_pts        = num_of_pts,
    cs_length         = cs_length,
    relative_distance = relative_distance,
    point_types       = point_types
  )
  
  # left_anchor    <- anchors[1]
  # bottom_anchor  <- anchors[2]
  # right_anchor   <- anchors[3]
  
  L    <- anchors[1]
  # bottom_anchor  <- anchors[2]
  R   <- anchors[3]
  # depths      = depths
  # pt_ids      = pt_ids
  # point_types = point_types
  # L           = left_bank
  # R           = right_bank
  
  slope          <- diff(c(0, depths))  
  bottom_depth   <- min(depths)      
  points_per_id  <- length(depths)
  
  # bank_or_bottom <- point_types
  # i = 3 
  # i =4 
  classification <- sapply(1:points_per_id, function(i) {
    message(i)
    if (i == 1) {
      
      message("LB\n")
      return("left_bank")   # handle first point
      
    } else if (i == points_per_id) {
      
      message("RB\n")
      return("right_bank")  # and the last point
      
    } else if (depths[i] == bottom_depth & point_types[i] == "bottom") {
      
      message("BOTTOM\n")
      return("bottom")      # identify the bottom point(s) (min depths value)
      
      
    }
    # RIGHT SIDE OF BOTTOM
    else if (pt_ids[i] >= R) {
      
      if (slope[i + 1] <= 0) {
        
        message("Right bank\n")
        return("right_bank")     #  slope changing from rising to falling (approaching bottom)
        
      } else {
        
        message("Right channel\n")
        return("right_channel")  # rising slope but not near the bottom
        
      }
      
    }
    # LEFT SIDE OF THE BOTTOM
    else if (pt_ids[i] <= L) {
      
      # second derivate is 
      if (slope[i - 1] <= 0) {
        
        message("Left channel\n")
        return("left_channel")   # slope changing from falling to rising (leaving bottom)
        
      } else {
        
        message("Left bank\n")
        return("left_bank")  #  slope  falling but not near the bottom
        
      }
      
    } else{
      message("NOT ON LEFT OR RIGHT SIDE (i.e. bottom...?)")
      return("bottom")
    } 
    # else if(slope[i] == 0) {
    # message("SLOPE is 0 --> CHAN\n")
    # return("channel")
    # }
    
    message("NO MATCH\n")
    
  })
  classification[classification %in% c("left_channel", "right_channel")] <- "channel"
  
  return(classification)
}