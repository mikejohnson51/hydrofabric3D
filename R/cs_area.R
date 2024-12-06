utils::globalVariables(
  c(".", "hy_id", "cs_id", "pt_id", "Z", "middle_index", "point_type", "minZ", 
    "maxZ", "minZ_bottom", "maxZ_left_bank", "maxZ_right_bank", "valid_left_bank", 
    "valid_right_bank", "bottom", "left_bank", "right_bank", "valid_banks", 
    "relative_distance", "cs_lengthm", "default_middle", "has_relief", 
    "max_relief", "braid_id", "geometry",
    
    "comid", "fromnode", "tonode", 
    "tocomid", "divergence", "cycle_id", "node", "braid_vector", "totdasqkm", 
    "changed", "relative_position", "head_distance", "tail_distance", 
    "component_id", "cs_measure", "ds_distance", "along_channel", "euclid_dist", 
    "sinuosity", "points_per_cs", "Z_at_bottom", "lower_bound", "upper_bound", 
    "ge_bottom", "is_near_bottom", "pts_near_bottom", "total_valid_pts", 
    "pct_near_bottom", 
    "member_braids",  "braid_members", "diff_pts", "is_extended", 
    "new_cs_id", "split_braid_ids",
    
    "braid_length", 
    "crosswalk_id", 
    "lengthm", 
    "check_z_values", 
    "geom", 
    "is_same_Z", 
    "is_multibraid", 
    "channel", "unique_count",
    "left_bank_count", "right_bank_count", "channel_count", "bottom_count", 
    "terminalID",
    "tmp_id",
    "make_geoms_to_cut_plot",
    "Y", "improved", "length_vector_col", "median", "min_ch", "new_validity_score",
    "old_validity_score", "transects", "validity_score", "x",
    "A", "DEPTH", "DINGMAN_R", "TW", "X", "X_end", "X_start", "Y_end", "Y_start",
    "ahg_a", "ahg_index", "ahg_x", "ahg_y", 
    "bottom_end", "bottom_length", "bottom_midpoint", 
    "bottom_start", "cs_partition", "distance_interval", "fixed_TW", 
    "has_new_DEPTH", "has_new_TW", "ind", "is_dem_point", "left_max", 
    "left_start", "max_right_position", "new_DEPTH", "new_TW", "next_X_is_missing", "next_Y_is_missing",
    "parabola", "partition", "prev_X_is_missing", 
    "prev_Y_is_missing", "right_start", "right_start_max", "start_or_end", "start_pt_id",
    "cs_source", 
    "partition_lengthm", "left_fema_index", "right_fema_index", 
    "left_is_within_fema", "right_is_within_fema", "left_distance", "right_distance",
    "new_cs_lengthm"
  )
)

#' Given a specific depth, an array of depths, and relative_distances return the cross sectional area
#'
#' @param depth numeric, depth at a specific point along CS
#' @param depth_array numeric set of all depths for CS
#' @param relative_distance numeric vector of distances from 0 to length of CS
#' @importFrom DescTools AUC
#' @return numeric cross sectional area value, 0 if no area can be calculated
#' @noRd
#' @keywords internal
find_cs_area <- function(depth, depth_array, relative_distance){
  
  # Y <- NULL
  depth_under      <-  depth_array[depth_array < depth]
  rel_dist_under   <-  relative_distance[depth_array < depth]
  
  auc1 <- DescTools::AUC(
    x = rel_dist_under, 
    y = rep(depth, length(depth_under)), 
    absolutearea = FALSE
  )
  
  auc2 <- DescTools::AUC(
    x = rel_dist_under, 
    y = depth_under, 
    absolutearea = FALSE
  )
  
  cs_area <- pmax(0, auc1 - auc2)
  
  return(
    ifelse(is.na(cs_area), 0, cs_area)
  )
  
}

#' Adds a cs_area column to a set of cross section points
#'
#' @param cs_pts dataframe or sf dataframe of CS points with crosswalk_id, cs_id, Z, and relative distance columns
#' @param crosswalk_id character, unique ID column name
#' @importFrom dplyr group_by mutate across any_of ungroup
#' @importFrom purrr map_dbl 
#' @return cs_pts dataframe with added numeric 'cs_area' column
#' @export
add_cs_area <- function(cs_pts, 
                        crosswalk_id = NULL) {
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "Z", "relative_distance")
  
  is_valid  <- validate_df(cs_pts, REQUIRED_COLS, "cs_pts")
  
  cs_pts <- 
    cs_pts %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      cs_area = purrr::map_dbl(Z, function(z) {
        find_cs_area(
          depth = z,
          depth_array = Z,
          relative_distance = relative_distance
        )
      })) %>%
    dplyr::ungroup()
  
  return(cs_pts)
  
}

# # Given a specific depth, an array of depths, and relative_distances return the cross sectional area (v2)
# #
# # @param pt_id numeric index of point within CS 
# # @param depth numeric, depth at a specific point along CS
# # @param depth_array numeric set of all depths for CS
# # @param relative_distance numeric vector of distances from 0 to length of CS
# # @importFrom DescTools AUC
# # @return numeric cross sectional area value, 0 if no area can be calculated
# # @noRd
# # @keywords internal
# find_cs_area2 <- function(pt_id,
#                           depth, 
#                           depth_array, 
#                           relative_distance
# ){
#   
#   npts     <- length(depth_array)
#   
#   midpoint <- (npts + 1) / 2
#   
#   side     <- ifelse(pt_id < midpoint, "left", "right")
#   idxs     <- switch (side,
#                       "left"  = pt_id:npts,
#                       "right" = 1:pt_id
#   )
#   
#   
#   side_depths     <- depth_array[idxs]
#   side_distances  <- relative_distance[idxs]
#   
#   depth_under     <- side_depths[side_depths < depth]
#   rel_dist_under  <- side_distances[side_depths < depth]
#   
#   auc1 <- DescTools::AUC(
#     x = rel_dist_under, 
#     y = rep(depth, length(depth_under)), 
#     absolutearea = FALSE
#   )
#   
#   auc2 <- DescTools::AUC(
#     x = rel_dist_under, 
#     y = depth_under, 
#     absolutearea = FALSE
#   )
#   
#   cs_area <- pmax(0, auc1 - auc2)
#   
#   return(
#     ifelse(is.na(cs_area), 0, cs_area)
#   )
#   
# }


# # Adds a cs_area column to a set of cross section points (v2)
# #
# # @param cs_pts dataframe or sf dataframe of CS points with crosswalk_id, cs_id, Z, and relative distance columns
# # @param crosswalk_id character, unique ID column name
# # @importFrom dplyr group_by mutate across any_of ungroup
# # @importFrom purrr map_dbl 
# # @return cs_pts dataframe with added numeric 'cs_area' column
# # @noRd
# # @keywords internal
# add_cs_area2 <- function(cs_pts, 
#                         crosswalk_id = NULL) {
#   
#   REQUIRED_COLS <- c(crosswalk_id, "cs_id", "Z", "relative_distance")
#   
#   is_valid  <- validate_df(cs_pts, REQUIRED_COLS, "cs_pts")
#   
#   cs_pts <-
#     cs_pts %>%
#     dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
#     dplyr::mutate(
#       cs_area = purrr::map_dbl(seq_along(Z), function(i) {
#         find_cs_area2(
#           pt_id = pt_id[i],
#           depth = Z[i],
#           depth_array = Z,
#           relative_distance = relative_distance
#         )
#       })) %>%
#     dplyr::ungroup()
#   
#   return(cs_pts)
#   
# }


get_channel_to_channel_cs_area <- function(cs_pts, 
                                           crosswalk_id = NULL, 
                                           min_or_max = "max") {
  
  bottom_section <- 
    cs_pts %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::filter(point_type == "bottom") %>% 
    dplyr::summarise(
      min_bottom = min(pt_id),
      max_bottom = max(pt_id)
    ) %>% 
    dplyr::ungroup()
  
  left_channel <-
    cs_pts %>%
    dplyr::left_join(
      bottom_section %>% 
        dplyr::select(dplyr::any_of(crosswalk_id), cs_id, min_bottom),
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::filter(point_type == "channel", pt_id < min_bottom) %>% 
    dplyr::slice_max(Z, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, Z, point_type, cs_area)
  
  right_channel <-
    cs_pts %>%
    dplyr::left_join(
      bottom_section %>% 
        dplyr::select(dplyr::any_of(crosswalk_id), cs_id, max_bottom),
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::filter(point_type == "channel", pt_id > max_bottom) %>% 
    dplyr::slice_max(Z, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, Z, point_type, cs_area)
  
  agg_cs_area <-
    dplyr::bind_rows(
      left_channel, 
      right_channel
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) 
  
  
  if (min_or_max == "max") {
    
    agg_cs_area <- 
      agg_cs_area %>% 
      dplyr::slice_max(Z, n = 1, with_ties = FALSE) 
    
  } else {
    
    agg_cs_area <- 
      agg_cs_area %>% 
      dplyr::slice_min(Z, n = 1, with_ties = FALSE) 
  }
  
  agg_cs_area <- 
    agg_cs_area %>%
    dplyr::ungroup() %>% 
    dplyr::select(
      dplyr::any_of(crosswalk_id),
      cs_id,
      pt_id, 
      Z, 
      point_type, 
      cs_area
    )
  
  return(agg_cs_area)
  
}
