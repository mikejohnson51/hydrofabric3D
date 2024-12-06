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
    "new_cs_lengthm", "polygon_index",
    "crosswalk_id", "extend_invalid_transects2",
    "anchors", "deriv_type", "edge", "extension_distance", 
    "left_is_extended", "right_is_extended", "to_node", "verbose", 
    "toindid", "indid", "toid", "is", "internal_is_braided2"
  )
)

#' Get a total count of the validity attributes
#'
#' @param x dataframe or sf dataframe with crosswalk_id, has_relief, and valid_banks columns
#' @param crosswalk_id character unique ID column
#'
#' @importFrom sf st_drop_geometry 
#' @importFrom dplyr select any_of group_by across slice ungroup count 
#' @return dataframe or tibble
#' @export
get_validity_tally <- function(x, crosswalk_id = NULL) {

  validity_tally <-
    x %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, valid_banks, has_relief) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::count(valid_banks, has_relief)
  
  return(validity_tally)
  
}

#' Calculates a validity score column based on valid_banks and has_relief columns in a set of cross section points
#'
#' @param cs_to_validate dataframe
#' @param crosswalk_id character, ID column
#' @param validity_col_name name of the output validity score column
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr group_by slice ungroup mutate select any_of
#' @return dataframe with added validity_score column
calc_validity_scores <- function(cs_to_validate, 
                                 crosswalk_id = NULL, 
                                 validity_col_name = "validity_score") {
  
  scores <- 
    cs_to_validate %>% 
    sf::st_drop_geometry() %>% 
    hydrofabric3D::add_tmp_id(x = crosswalk_id) %>% 
    dplyr::group_by(tmp_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      validity_score = valid_banks + has_relief
    ) %>% 
    dplyr::select(
      # hy_id, 
      dplyr::any_of(crosswalk_id),
      cs_id, valid_banks, has_relief, validity_score)
  
  names(scores) <- c(crosswalk_id, "cs_id", "valid_banks", "has_relief", validity_col_name)
  
  return(scores)
  
}

#' Compare valid_banks and has_relief between 2 sets of cross section points 
#'
#' @param cs_pts1 dataframe or sf dataframe of CS pts
#' @param cs_pts2 dataframe or sf dataframe of CS pts
#' @param crosswalk_id character unique ID
#' @importFrom dplyr rename filter any_of mutate select left_join case_when
#' @return dataframe, tibble
#' @export
compare_cs_validity <- function(cs_pts1, 
                                cs_pts2, 
                                crosswalk_id = NULL
) {
  
  validity_scores1 <- 
    cs_pts1 %>% 
    calc_validity_scores(crosswalk_id) %>% 
    add_tmp_id(crosswalk_id) %>% 
    dplyr::rename(score1 = validity_score)
  
  validity_scores2 <-
    cs_pts2 %>% 
    calc_validity_scores(crosswalk_id) %>% 
    add_tmp_id(crosswalk_id) %>% 
    dplyr::rename(score2 = validity_score)
  
  # mark as "improved" for any hy_id/cs_ids that increased "validity score" after extending
  check_for_improvement <- dplyr::left_join(
    # OLD SCORES
    validity_scores1 %>%
      dplyr::filter(
        tmp_id %in% unique(validity_scores2$tmp_id)
      ) %>% 
      dplyr::select(dplyr::any_of(crosswalk_id), cs_id, score1),
    
    # NEW SCORES
    validity_scores2 %>% 
      dplyr::select(dplyr::any_of(crosswalk_id), cs_id, score2),
    by = c(crosswalk_id, "cs_id")
  ) %>% 
    dplyr::mutate(
      is_improved = dplyr::case_when(
        score2 > score1  ~ TRUE,
        TRUE                     ~ FALSE
      )
    ) %>%
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, 
                  score1, score2, 
                  is_improved
    )
  
  return(check_for_improvement)
  
}
