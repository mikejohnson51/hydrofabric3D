
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

#' @title Add relief attributes to a dataframe of cross sections points
#' Given a set of cross section points (derived from hydrofabric3D::cross_section_pts() and hydrofabric3D::classify_points()) add a "has_relief" logical
#' value to data. The "has_relief" value is indicating whether a cross section "has relief".
#' Relief is determined by checking each set of cross section points have a left OR right bank that
#' has a depth difference from the bottom that isgreater than or equal to a percentage of the cross section length (e.g. Assuming a 'pct_of_length_for_relief' of 0.01 (1%) of a 100m cross section would have a relief depth threshold of 1m)
#' @param classified_pts sf or dataframe of points with "hy_id", "cs_id", "cs_lengthm", and "point_type" columns. Output of hydrofabric3D::classify_points()
#' @param crosswalk_id character, ID column 
#' @param pct_of_length_for_relief numeric, percent of cs_lengthm to use as the threshold depth for classifying whether a cross section has "relief". Default is 0.01 (1% of the cross sections length).
#' @return sf or dataframe with added "has_relief" columns or a dataframe of dataframe of unique hy_id/cs_id and "has_relief"
#' @importFrom dplyr select group_by slice ungroup mutate filter summarise left_join case_when all_of relocate last_col
#' @importFrom tidyr pivot_wider
#' @export
add_relief <- function(
    classified_pts,
    crosswalk_id = NULL,
    pct_of_length_for_relief = 0.01
) {
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
  
  # validate input dataframe has correct columns  
  is_valid <- validate_df(classified_pts, REQUIRED_COLS, "classified_pts")  
  
  # type checking
  if (!any(class(classified_pts) %in% c("sf", "tbl_df", "tbl", "data.frame"))) {
    stop("Invalid argument type, 'classified_pts' must be of type 'sf', 'tbl_df', 'tbl' or 'data.frame', given type was '",   
         class(classified_pts), "'")
  }
  
  # type checking
  if (!is.numeric(pct_of_length_for_relief)) {
    stop("Invalid argument type, 'pct_of_length_for_relief' must be of type 'numeric', given type was '",   
         class(pct_of_length_for_relief), "'")
  }
  
  # Make sure pct_of_length_for_relief is valid percentage value (greater than 0)
  if (pct_of_length_for_relief < 0 ) {
    stop("Invalid value 'pct_of_length_for_relief' of ", pct_of_length_for_relief, ", 'pct_of_length_for_relief' must be greater than or equal to 0")
  } 
  
  # TODO: Need to add code that will just set aside the geometries and add them back to the final output dataset
  # For now we will just drop geometries as safety precaution (as to not summarize() on a massive number of sf geometries)
  classified_pts <- sf::st_drop_geometry(classified_pts)
  
  # store the cross section lengths and calculate the depth threshold as a percent of the cross sections length
  cs_lengths <- 
    classified_pts %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, cs_lengthm) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      depth_threshold = round(cs_lengthm * pct_of_length_for_relief, 3) # maybe use floor() here
    )
  
  # get the minimum bottom point and maximum left and right bank points
  relief <-
    classified_pts %>% 
    dplyr::filter(point_type %in% c("bottom", "left_bank", "right_bank")) %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, Z, point_type) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id", "point_type")))) %>% 
    dplyr::summarise(
      minZ = min(Z, na.rm = TRUE),
      maxZ = max(Z, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = point_type,
      values_from = c(minZ, maxZ)
    ) %>% 
    dplyr::select(
        dplyr::any_of(crosswalk_id), cs_id, 
        bottom     = minZ_bottom, 
        left_bank  = maxZ_left_bank, 
        right_bank = maxZ_right_bank
        ) 
  
  # join lengths and depth threshold back with relief table and
  # calculate if the max difference between left/right bank vs bottom is 
  # greater than or equal to the depth threshold
  relief <-
    relief %>% 
    dplyr::left_join(
      cs_lengths, 
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::mutate(
      depth_diff = max(c(round(right_bank - bottom, 3), 
                         round(left_bank - bottom, 3)), 
                       na.rm = TRUE)                     # TODO: removing NAs might not be the right call, 
      # removing them might set has_relief to TRUE and
      # says "there IS relief but no valid banks"
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      has_relief = dplyr::case_when(
        depth_diff >= depth_threshold ~ TRUE,
        TRUE                          ~ FALSE
      )
    )
  
  # add the new point type columns to the original dataframe
  # Join the point type counts to the original dataframe
  classified_pts <- 
    classified_pts %>% 
    dplyr::left_join(
      dplyr::select(relief, 
                    dplyr::any_of(crosswalk_id), cs_id, has_relief),
      by = c(crosswalk_id, "cs_id")
    )
  
  # check if any of the columns in 'classified_pts' are geometry types  and move them to the end column if they do exist
  classified_pts <- move_geometry_to_last(classified_pts)
  
  return(classified_pts)
  
}

#' @title Get relief attributes from a dataframe of cross sections points
#' Generate a dataframe from a set of classified cross section points indicating whether a cross section "has relief". 
#' Relief is determined by checking each set of cross section points have a left OR right bank that has a depth difference from the bottom that is
#'  greater than or equal to a percentage of the cross section length (e.g. Assuming a 'pct_of_length_for_relief' of 0.01 (1%) of a 100m cross section would have a relief depth threshold of 1m)
#' @param classified_pts sf or dataframe of points with "hy_id", "cs_id", "cs_lengthm", and "point_type" columns. Output of hydrofabric3D::classify_pts()
#' @param crosswalk_id character, ID column 
#' @param pct_of_length_for_relief numeric, percent of cs_lengthm to use as the threshold depth for classifying whether a cross section has "relief". Default is 0.01 (1% of the cross sections length).
#' @param detailed logical, whether to return only a the "has_relief" column or 
#' include all derived relief based columns such as "max_relief" and the "pct_of_length_for_relief" used. Default is FALSE and returns a dataframe with only "hy_id", "cs_id", and "has_relief".
#' @return dataframe with each row being a unique hy_id/cs_id with a "has_relief" value for each hy_id/cs_id. If detailed = TRUE, then the output dataframe will include the following additional columns: "cs_lengthm", "max_relief", "pct_of_length_for_relief".
#' @importFrom dplyr select group_by slice ungroup mutate filter summarise left_join case_when all_of relocate last_col any_of across
#' @importFrom tidyr pivot_wider
#' @export
get_relief <- function(
    classified_pts,
    crosswalk_id = NULL,
    pct_of_length_for_relief = 0.01,
    detailed = FALSE
) {
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
  
  # validate input dataframe has correct columns  
  is_valid <- validate_df(classified_pts, REQUIRED_COLS, "classified_pts")  
  
  # type checking
  if (!any(class(classified_pts) %in% c("sf", "tbl_df", "tbl", "data.frame"))) {
    stop("Invalid argument type, 'classified_pts' must be of type 'sf', 'tbl_df', 'tbl' or 'data.frame', given type was '",   class(classified_pts), "'")
  }
  
  # type checking
  if (!is.numeric(pct_of_length_for_relief)) {
    stop("Invalid argument type, 'pct_of_length_for_relief' must be of type 'numeric', given type was '",   class(pct_of_length_for_relief), "'")
  }
  
  # type checking
  if (!is.logical(detailed)) {
    stop("Invalid argument type, 'detailed' must be of type 'logical', given type was '",   class(detailed), "'")
  }
  
  # drop geometries as safety precaution
  classified_pts <- sf::st_drop_geometry(classified_pts)
  
  # store the cross section lengths and calculate the depth threshold as a percent of the cross sections length
  cs_lengths <- 
    classified_pts %>%
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, cs_lengthm) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      depth_threshold = round(cs_lengthm * pct_of_length_for_relief, 3) # maybe use floor() here
    )
  
  # get the minimum bottom point and maximum left and right bank points
  relief <-
    classified_pts %>%
    dplyr::filter(point_type %in% c("bottom", "left_bank", "right_bank")) %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, Z, point_type) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id", "point_type")))) %>% 
    dplyr::summarise(
      minZ = min(Z, na.rm = TRUE),
      maxZ = max(Z, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = point_type,
      values_from = c(minZ, maxZ)
    ) %>% 
    dplyr::select(
      dplyr::any_of(
        c(
          crosswalk_id,
          "cs_id",
          "minZ_bottom",
          "maxZ_left_bank",
          "maxZ_right_bank"
        ))
    ) %>%  
    dplyr::rename(
      dplyr::any_of(c(
        bottom     = "minZ_bottom",
        left_bank  = "maxZ_left_bank",
        right_bank = "maxZ_right_bank"
      ))
    )
  
  # make sure that all the required columns are present, if a column is missing, add that column and set the values to NA
  required_pt_cols <- c("bottom", "left_bank", "right_bank")
  
  for (col in required_pt_cols) {
    if (!col %in% names(relief)) {
      relief[[col]] <- NA
    }
  }
  
  # join lengths and depth threshold back with relief table and
  # calculate if the max difference between left/right bank vs bottom is 
  # greater than or equal to the depth threshold
  relief <-
    relief %>% 
    dplyr::left_join(
      cs_lengths, 
      by = c(crosswalk_id, "cs_id")  
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::mutate(
      max_relief = max(
        c(
          round(right_bank - bottom, 3), 
          round(left_bank - bottom, 3)
        ), 
        na.rm = TRUE
      ),  # TODO: removing NAs might not be the right call, removing them might set has_relief to TRUE and says "there IS relief but no valid banks"
      
      # TODO: if both left AND right bank are NA, then we get an -Inf which we will just set to 0 (i.e. relief of 0)
      max_relief = dplyr::case_when(
        is.infinite(max_relief) ~ 0,
        TRUE                    ~ max_relief
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # TODO: if a cross section does NOT have proper left/right banks, it by default can NOT have relief (i.e. has_relief = FALSE)
      has_missing_banks = is.na(left_bank) | is.na(right_bank),
      has_relief        = dplyr::case_when(
        (max_relief >= depth_threshold) & !has_missing_banks ~ TRUE,
        TRUE                            ~ FALSE
      ),
      pct_of_length_for_relief = pct_of_length_for_relief
    ) 
  
  # if detailed set of data is specified, return the relief dataframe with additional columns
  if(detailed) {
    relief <- 
      relief %>% 
      dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
      dplyr::mutate(
        max_relief = dplyr::case_when(
          has_missing_banks ~ 0,
          TRUE              ~ max_relief
        )
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(
        dplyr::any_of(crosswalk_id),
        cs_id, cs_lengthm, 
        has_relief, 
        max_relief, 
        pct_of_length_for_relief
      )
    
    return(relief)
    
  }
  
  # return dataframe with just hy_id/cs_id, and has_relief
  relief <-
    relief %>% 
    dplyr::select(
      dplyr::any_of(crosswalk_id), 
      cs_id, 
      has_relief
    )
  
  return(relief)
}
