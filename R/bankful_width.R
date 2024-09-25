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
    "prev_Y_is_missing", "right_start", "right_start_max", "start_or_end", "start_pt_id",
    "cs_source", 
    "partition_lengthm", "left_fema_index", "right_fema_index", 
    "left_is_within_fema", "right_is_within_fema", "left_distance", "right_distance",
    "new_cs_lengthm", 
    "crosswalk_id", "extend_invalid_transects2",
    "anchors", "deriv_type", "edge", "extension_distance", 
    "left_is_extended", "right_is_extended", "to_node", "verbose", 
    "toindid", "indid", "toid", "is", "internal_is_braided2"
  )
)

#' Calculates the powerlaw derived Bankful width given total drainage area (square kilometers)
#' @param total_drainage_area_sqkm 
#' @return numeric, bankful width estimate
#' @export
calc_powerlaw_bankful_width <- function(total_drainage_area_sqkm) {
  
  # Check if 'total_drainage_area_sqkm' is numeric or a numeric vector
  if (!is.numeric(total_drainage_area_sqkm)) {
    stop("'total_drainage_area_sqkm' must be a numeric")
  }
  
  return(exp(0.700    + 0.365* log(total_drainage_area_sqkm)))
  
}

#' Add powerlaw_bankful_width column
#' @param total_drainage_area_sqkm_col 
#' @return character, column with the total downstrream drainage area in square kilometers (numeric column)
#' @export
add_powerlaw_bankful_width <- function(df, total_drainage_area_sqkm_col, min_bf_width) {
  
  # df <- flowlines
  # total_drainage_area_sqkm_col = "tot_drainage_areasqkm"
  # MIN_BF_WIDTH <- 50
  
  # Check if 'total_drainage_area_sqkm' is numeric or a numeric vector
  if (!is.character(total_drainage_area_sqkm_col)) {
    stop("'total_drainage_area_sqkm_col' must be a character")
  }
  
  if (!total_drainage_area_sqkm_col %in% names(df)) {
    stop("'total_drainage_area_sqkm_col' ", total_drainage_area_sqkm_col, " is not a column in input 'df'")
  }
  if(!is.numeric(df[[total_drainage_area_sqkm_col]])) {
     stop("'total_drainage_area_sqkm_col' ", total_drainage_area_sqkm_col, " must be a numeric column in input 'df'")
  }
  
  # df[1, ][[total_drainage_area_sqkm_col]] <- NA
  # df[[total_drainage_area_sqkm_col]]
  
  # fill any NA values with the given default Bankful width value
  df[is.na(df[[total_drainage_area_sqkm_col]]), ][[total_drainage_area_sqkm_col]] <- min_bf_width
  
  bf_widths <- pmax(
                min_bf_width, 
                calc_powerlaw_bankful_width(df[[total_drainage_area_sqkm_col]]) * 11
                )
  
  df['bf_width'] <- bf_widths
   
  return(df)
  
}



# flowlines %>% 
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