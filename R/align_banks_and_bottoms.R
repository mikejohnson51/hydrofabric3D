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
    "id", 
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
    "prev_Y_is_missing", "right_start", "right_start_max", "start_or_end", "start_pt_id"
  )
)

#' Align banks and smooth bottoms of cross section points
#' @description
#' Ensures the bottom of each cross section is lower then or equal to that one upstream.
#' To do this, we traverse down the network making sure this condition is met, and, 
#' in cases where it isn't, we will lower the in channel portion of the cross section to make it true.
#' @param cs_pts dataframe or sf dataframe of classified cross section points (output of classify_points())
#' @importFrom dplyr group_by summarise mutate ungroup select left_join case_when
#' @importFrom sf st_drop_geometry
#' @return sf dataframe of cross section points with aligned banks and smoothed bottoms
#' @export
align_banks_and_bottoms <- function(cs_pts) {
  
  adjust <- function(v){
    if(length(v) == 1){ return(v)}
    for(i in 2:length(v)){ 
      v[i] = ifelse(v[i] > v[i-1], v[i-1], v[i]) }
    v
  }
  
  slope <- 
    cs_pts %>% 
    sf::st_drop_geometry() %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::summarise(min_ch = min(Z[point_type == "channel"])) %>% 
    dplyr::mutate(adjust = adjust(min_ch) - min_ch) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(hy_id, cs_id, adjust)
  
  cs_pts <-  
    dplyr::left_join(
      cs_pts, 
      slope, 
      by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::mutate(
      Z = dplyr::case_when(
        point_type  == "channel" ~ Z + adjust, 
        TRUE ~ Z
      )
    ) %>% 
    dplyr::select(-adjust)
  
  return(cs_pts)
  
}

# 
# #################################################################################### 
# ############################ Dev testing below ##############################
# #################################################################################### 
# align_banks_and_bottoms <- function(cs_pts) {
#   
#   
#   cs_pts <- cs
#   
#   adjust = function(v){
#     if(length(v) == 1){ return(v)}
#     for(i in 2:length(v)){ v[i] = ifelse(v[i] > v[i-1], v[i-1], v[i]) }
#     v
#   }
#   cs_pts$point_type %>% unique()
#   
#   slope <- 
#     cs_pts %>% 
#     sf::st_drop_geometry() %>% 
#     dplyr::group_by(hy_id, cs_id) %>% 
#     dplyr::summarise(min_ch = min(Z[class == "channel"])) %>% 
#     dplyr::mutate(adjust = adjust(min_ch) - min_ch) %>% 
#     dplyr::ungroup() %>% 
#     dplyr::select(hy_id, cs_id, adjust)
#   
#   cs_pts_adjusted <-  
#     dplyr::left_join(
#       cs_pts, 
#       slope, 
#       by = c("hy_id", "cs_id")
#       ) %>% 
#     dplyr::mutate(
#       Z = dplyr::case_when(
#         class  == "channel" ~ Z + adjust, 
#         TRUE ~ Z
#         ),
#       is_adjusted = dplyr::case_when(
#         adjust != 0 ~ TRUE,
#         TRUE ~ FALSE
#       )
#       ) 
#     # dplyr::select(-bottom, -left_bank, -right_bank, -has_relief,
#     #               -valid_banks, -point_type, -class) %>% 
#     # hydrofabric3D::classify_points()
#   test_id <- 
#     cs_pts_adjusted %>% 
#     dplyr::filter(is_adjusted) %>% 
#     hydrofabric3D::add_tmp_id() %>% 
#     # dplyr::slice(555) %>% 
#     dplyr::pull(tmp_id) %>%
#     unique() %>% 
#     .[3]
#   
#   # cs_pts %>% 
#   #   hydrofabric3D::add_tmp_id() %>% 
#   #   dplyr::filter(tmp_id == test_id) %>%
#   #   dplyr::mutate(
#   #     updated = "ORIGINAL"
#   #   ) 
#   # cs_pts %>% 
#   #   hydrofabric3D::add_tmp_id() %>% 
#   #   dplyr::filter(tmp_id == test_id) %>%
#   #   dplyr::mutate(
#   #     updated = "Reclassified"
#   #   ) %>% 
#   #   classify_points3() 
#   
#   compare_classed <- dplyr::bind_rows(
#     cs_pts %>% 
#       hydrofabric3D::add_tmp_id() %>% 
#       dplyr::filter(tmp_id == test_id) %>%
#       dplyr::mutate(
#         updated = "ORIGINAL"
#       ),
#     cs_pts %>% 
#       hydrofabric3D::add_tmp_id() %>% 
#       dplyr::filter(tmp_id == test_id) %>%
#       dplyr::mutate(
#         updated = "ORIGINAL - Reclassified"
#       ) %>% 
#       classify_points3(),
#     cs_pts_adjusted %>% 
#       dplyr::filter(is_adjusted) %>% 
#       hydrofabric3D::add_tmp_id() %>% 
#       dplyr::filter(tmp_id == test_id) %>%
#       dplyr::mutate(
#         updated = "UPDATED"
#       ),
#     cs_pts_adjusted %>% 
#       dplyr::filter(is_adjusted) %>% 
#       hydrofabric3D::add_tmp_id() %>% 
#       dplyr::filter(tmp_id == test_id) %>%
#       # dplyr::select(-bottom, -left_bank, -right_bank, -has_relief, -valid_banks, -point_type, -class) %>%
#       # hydrofabric3D::classify_points() %>%
#       classify_points3() %>%
#       dplyr::mutate(
#         updated = "UPDATED - Reclassified"
#       )
#   )
#   
#   compare_classed %>% 
#     ggplot2::ggplot() + 
#     ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = point_type)) +
#     ggplot2::labs(title = "Reclassified set of points") + 
#     # ggplot2::facet_wrap(~updated, nrow = 4)
#     ggplot2::facet_wrap(~updated, nrow = 1)
#   ###################################  ###################################
#   ###################################  ###################################
#   compare_cs <- dplyr::bind_rows(
#     cs_pts_adjusted %>% 
#       dplyr::filter(is_adjusted) %>% 
#       hydrofabric3D::add_tmp_id() %>% 
#       dplyr::filter(tmp_id == test_id) %>%
#       #   dplyr::select(-bottom, -left_bank, -right_bank, -has_relief, -valid_banks, -point_type, -class) %>%
#       # hydrofabric3D::classify_points() %>%
#       classify_points3() %>%
#       dplyr::mutate(
#         updated = "UPDATED"
#       ),
#     cs_pts %>% 
#       hydrofabric3D::add_tmp_id() %>% 
#       dplyr::filter(tmp_id == test_id) %>%
#       dplyr::mutate(
#         updated = "ORIGINAL"
#       )
#   )
#   
#   compare_cs %>% 
#     ggplot2::ggplot() + 
#     ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = point_type)) +
#     ggplot2::labs(title = "Bank alignment and bottom smoothing") + 
#     ggplot2::facet_wrap(~updated)
#   
# }