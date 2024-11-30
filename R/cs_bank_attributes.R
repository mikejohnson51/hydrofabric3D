
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


#' @title Get the count of each point type in a set of cross section points
#' @description get_point_type_counts() will create a dataframe providing the counts of every point_type for each hy_id/cs_id in a set of classified cross section points (output of classify_pts())
#' @param classified_pts dataframe or sf dataframe, cross section points with a "hy_id", and "cs_id" columns as well as a 'point_type' column containing the values: "bottom", "left_bank", "right_bank", and "channel"
#' @param crosswalk_id character, ID column 
#' @return dataframe or sf dataframe with hy_id, cs_id, and <point_type>_count columns for each point_type
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr group_by count ungroup summarize filter n_distinct select slice left_join relocate all_of last_col
#' @importFrom tidyr pivot_wider pivot_longer
#' @export
get_point_type_counts <- function(classified_pts, crosswalk_id = NULL) {
  
  # classified_pts <- cs_pts %>% hydrofabric3D::classify_points()
  # add = F
  # classified_pts = classified_pts2
  # add = TRUE
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "point_type")
  
  if (!all(REQUIRED_COLS %in% names(classified_pts))) {
    missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(classified_pts))]
    stop("'classified_pts' is missing one or more of the required columns:\n > ",
         paste0(missing_cols, collapse = "\n > "))
  }
  
  # type checking
  if (!any(class(classified_pts) %in% c("sf", "tbl_df", "tbl", "data.frame"))) {
    stop("Invalid argument type, 'classified_pts' must be of type 'sf', 'tbl_df', 'tbl' or 'data.frame', given type was '",  
         class(classified_pts), "'")
  }
  
  # create a copy of the input dataset, add a tmp_id column
  stage_df <- 
    classified_pts %>% 
    sf::st_drop_geometry() %>% 
    hydrofabric3D::add_tmp_id(x = crosswalk_id) 
  
  # # create a reference dataframe with all possible combinations of tmp_id and point_type
  # reference_df <- expand.grid(
  #   tmp_id     = unique(stage_df$tmp_id),
  #   point_type = unique(stage_df$point_type)
  # )
  
  # get a count of the point_types in each hy_id/cs_id group (i.e. each cross section)
  point_type_counts <- 
    stage_df %>%
    dplyr::group_by(tmp_id, point_type) %>%
    dplyr::count() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # add levels to the point_type column so if a given point_type
      # is NOT in the cross seciton points, then it will be added with NAs in the subsequent pivot_wider
      point_type = factor(point_type, levels = c("left_bank", "bottom", "right_bank", "channel"))
    ) 
  
  # pivot data wider to get implicit missing groups with NA values
  point_type_counts <- 
    point_type_counts %>% 
    tidyr::pivot_wider(
      names_from   = point_type,
      values_from  = n,
      names_expand = TRUE
    ) 
  
  point_type_counts <- 
    point_type_counts %>% 
    tidyr::pivot_longer(
      cols      = c(bottom, channel, right_bank, left_bank),
      names_to  = "point_type",
      values_to = "n"
    ) %>% 
    dplyr::mutate(n = ifelse(is.na(n), 0, n))
  
  # # Join the count of point types in each group with the reference_df to 
  # # get rows of NA values for any group that is missing a specific point_type
  # point_type_counts <- 
  #   point_type_counts %>% 
  #   dplyr::right_join(reference_df, by = c("tmp_id", "point_type"))
  
  # # For any cross section group that does NOT contain a point type, 
  # # the point type will be NA and here we replace those NAs with 0 
  # point_type_counts$n[is.na(point_type_counts$n)] <- 0
  
  # # make sure that all tmp_id groups have all 4 point types
  check_counts <-
    point_type_counts %>%
    dplyr::group_by(tmp_id) %>%
    dplyr::summarize(unique_count = dplyr::n_distinct(point_type)) %>%
    dplyr::filter(unique_count == 4) 
  
  # if the number of distinct points types in each cross section is not 4, raise an error
  if (length(unique(stage_df$tmp_id)) != nrow(check_counts)) {
    stop("Error validating each hy_id/cs_id cross section contains exactly 4 distinct values in the 'point_type' column")  
  }
  
  # get the hy_id, cs_id for each tmp_id to cross walk back to just using hy_id/cs_id
  stage_df <- 
    stage_df %>% 
    dplyr::select(tmp_id, dplyr::any_of(crosswalk_id), cs_id) %>% 
    # dplyr::select(tmp_id, hy_id, cs_id) %>% 
    dplyr::group_by(tmp_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # convert the column of point types to be a column for each point type that 
  # has the point type count for each hy_id/cs_id (cross section)
  point_type_counts <- 
    point_type_counts %>% 
    tidyr::pivot_wider(names_from = point_type,  
                       names_glue = "{point_type}_count", 
                       values_from = n) %>% 
    dplyr::left_join(
      stage_df,
      by = "tmp_id"
    ) %>% 
    dplyr::select(
      dplyr::any_of(crosswalk_id),
      cs_id, 
      left_bank_count, right_bank_count, channel_count, bottom_count
    )
  
  # point_type_counts %>% 
  #   dplyr::arrange(-right_bank_count)
  
  return(point_type_counts)
  
}


#' @title Add the count of each point type as a column to a dataframe of section points
#' @description add_point_type_counts() will add columns to the input dataframe with the counts of every point_type for each hy_id/cs_id in the input dataframe of classified cross section points (output of classify_pts())
#' @param classified_pts dataframe or sf dataframe, cross section points with a "hy_id", and "cs_id" columns as well as a 'point_type' column containing the values: "bottom", "left_bank", "right_bank", and "channel"
#' @param crosswalk_id character, ID column 
#' @return dataframe or sf dataframe with "<point_type>_count" columns added
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr group_by count ungroup summarize filter n_distinct select slice left_join relocate all_of last_col
#' @importFrom tidyr pivot_wider pivot_longer
#' @export
add_point_type_counts <- function(classified_pts, crosswalk_id = NULL) {
  
  # classified_pts <- cs_pts %>% hydrofabric3D::classify_points()
  # add = F
  # classified_pts = classified_pts2
  # add = TRUE  
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  # type checking
  if (!any(class(classified_pts) %in% c("sf", "tbl_df", "tbl", "data.frame"))) {
    stop("Invalid argument type, 'classified_pts' must be of type 'sf', 'tbl_df', 'tbl' or 'data.frame', given type was '",  
         class(classified_pts), "'")
  }
  
  # create a copy of the input dataset, add a tmp_id column
  stage_df <- 
    classified_pts %>% 
    sf::st_drop_geometry() %>% 
    hydrofabric3D::add_tmp_id(x = crosswalk_id) 
  
  # # create a reference dataframe with all possible combinations of tmp_id and point_type
  # reference_df <- expand.grid(
  #   tmp_id     = unique(stage_df$tmp_id),
  #   point_type = unique(stage_df$point_type)
  # )
  
  # get a count of the point_types in each hy_id/cs_id group (i.e. each cross section)
  point_type_counts <- 
    stage_df %>%
    dplyr::group_by(tmp_id, point_type) %>%
    dplyr::count() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # add levels to the point_type column so if a given point_type
      # is NOT in the cross seciton points, then it will be added with NAs in the subsequent pivot_wider
      point_type = factor(point_type, levels = c("left_bank", "bottom", "right_bank", "channel"))
    ) 
  
  # pivot data wider to get implicit missing groups with NA values
  point_type_counts <- 
    point_type_counts %>% 
    tidyr::pivot_wider(
      names_from   = point_type,
      values_from  = n,
      names_expand = TRUE
    ) 
  
  point_type_counts <- 
    point_type_counts %>% 
    tidyr::pivot_longer(
      cols      = c(bottom, channel, right_bank, left_bank),
      names_to  = "point_type",
      values_to = "n"
    ) %>% 
    dplyr::mutate(n = ifelse(is.na(n), 0, n))
  
  # # Join the count of point types in each group with the reference_df to 
  # # get rows of NA values for any group that is missing a specific point_type
  # point_type_counts <- 
  #   point_type_counts %>% 
  #   dplyr::right_join(reference_df, by = c("tmp_id", "point_type"))
  
  # # For any cross section group that does NOT contain a point type, 
  # # the point type will be NA and here we replace those NAs with 0 
  # point_type_counts$n[is.na(point_type_counts$n)] <- 0
  
  # # make sure that all tmp_id groups have all 4 point types
  check_counts <-
    point_type_counts %>%
    dplyr::group_by(tmp_id) %>%
    dplyr::summarize(unique_count = dplyr::n_distinct(point_type)) %>%
    dplyr::filter(unique_count == 4) 
  
  # if the number of distinct points types in each cross section is not 4, raise an error
  if (length(unique(stage_df$tmp_id)) != nrow(check_counts)) {
    stop("Error validating each hy_id/cs_id cross section contains exactly 4 distinct values in the 'point_type' column")  
  }
  
  # get the hy_id, cs_id for each tmp_id to cross walk back to just using hy_id/cs_id
  stage_df <- 
    stage_df %>% 
    dplyr::select(tmp_id, dplyr::any_of(crosswalk_id), cs_id) %>% 
    dplyr::group_by(tmp_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # convert the column of point types to be a column for each point type that 
  # has the point type count for each hy_id/cs_id (cross section)
  point_type_counts <- 
    point_type_counts %>% 
    tidyr::pivot_wider(
      names_from  = point_type,  
      names_glue  = "{point_type}_count", 
      values_from = n
    ) %>% 
    dplyr::left_join(
      stage_df,
      by = "tmp_id"
    ) %>% 
    dplyr::select(
      dplyr::any_of(crosswalk_id),
      cs_id, 
      left_bank_count, right_bank_count, channel_count, bottom_count
    )
  
  # Join the point type counts to the original dataframe
  classified_pts <- 
    classified_pts %>% 
    dplyr::left_join(
      point_type_counts,
      by = c(crosswalk_id, "cs_id")  
      # by = c("hy_id", "cs_id")
    )
  
  # check if any of the columns in 'classified_pts' are geometry types  and move them to the end column if they do exist
  classified_pts <- move_geometry_to_last(classified_pts)
  
  return(classified_pts)
}

#' @title Adds attributes about the banks of each cross section in a dataframe of cross section points
#' Function adds "bottom", "left_bank", "right_bank" columns that are 
#' the Z values of the "lowest" bottom point, and the "highest" left and right bank Z values, respectively. If there are
#' And also a "valid_banks" column is added that is TRUE if the hy_id/cs_id set of cross section point has at least 1 bottom point with 
#' at least 1 left bank point AND 1 right bank point that are above the lowest "bottom" point.
#' @param classified_pts sf or dataframe of points with "hy_id", "cs_id", and "point_type" columns. Output of hydrofabric3D::classify_pts()
#' @return sf or dataframe with added "bottom", "left_bank", "right_bank", and "valid_banks" columns
#' @importFrom dplyr mutate case_when filter select group_by summarise ungroup left_join
#' @importFrom tidyr pivot_wider
add_bank_attributes <- function(
    classified_pts
) {
  
  # classified_pts <- output_pts
  
  # type checking, throw an error if not "sf", "tbl_df", "tbl", or "data.frame"
  if (!any(class(classified_pts) %in% c("sf", "tbl_df", "tbl", "data.frame"))) {
    stop("Invalid argument type, 'classified_pts' must be of type 'sf', 'tbl_df', 'tbl' or 'data.frame', given type was '",
         class(classified_pts), "'")
  }
  
  # Add columns with the counts of point types
  classified_pts <- hydrofabric3D::add_point_type_counts(classified_pts)
  
  # TODO: Need to add code that will just set aside the geometries and add them back to the final output dataset
  # For now we will just drop geometries as safety precaution (as to not summarize() on a massive number of sf geometries)
  classified_pts <- sf::st_drop_geometry(classified_pts)
  
  # Add a valid_count column which is TRUE 
  # if a hy_id/cs_id has a bottom point AND atleast 1 left and right bank
  classified_pts <- 
    classified_pts %>% 
    dplyr::mutate(
      valid_count = dplyr::case_when(
        (bottom_count > 0 & 
           left_bank_count > 0 & 
           right_bank_count > 0)  ~ TRUE,
        TRUE                      ~ FALSE
      )
    )
  
  # Add minimum bottom Z, max left and right bank Z, and 
  # flags noting if the left/right banks are "valid" (i.e. max left/right bank values are greater than the bottom Z)
  bank_validity <-
    classified_pts %>% 
    dplyr::filter(point_type %in% c("bottom", "left_bank", "right_bank")) %>% 
    # dplyr::filter(point_type %in% c("left_bank", "right_bank")) %>% 
    dplyr::select(hy_id, cs_id, pt_id, Z, point_type) %>% 
    dplyr::group_by(hy_id, cs_id, point_type) %>% 
    dplyr::summarise(
      minZ = min(Z, na.rm = TRUE),
      maxZ = max(Z, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from = point_type,
      values_from = c(minZ, maxZ)
    ) %>% 
    dplyr::select(hy_id, cs_id, 
                  bottom     = minZ_bottom, 
                  left_bank  = maxZ_left_bank, 
                  right_bank = maxZ_right_bank
    ) 
  
  # Get logical values of the bank validity on both sides
  bank_validity <-
    bank_validity %>% 
    dplyr::mutate(
      # bottom     = ifelse(is.na(bottom), 0, bottom),          # Old way was to set the NA left/bank/bottom Z values to 0 but i think this could lead to problems with small number of edge cases
      # right_bank = ifelse(is.na(right_bank), 0, right_bank),
      # left_bank  = ifelse(is.na(left_bank), 0, left_bank),
      valid_left_bank = dplyr::case_when(
        (left_bank > bottom) & (!is.na(left_bank))   ~ TRUE,    # Old method used: left_bank > bottom ~ TRUE,
        TRUE               ~ FALSE
      ),
      valid_right_bank = dplyr::case_when(
        (right_bank > bottom) & (!is.na(right_bank)) ~ TRUE,    # Old method used: right_bank > bottom ~ TRUE,
        TRUE                ~ FALSE
      ),
      valid_banks = valid_left_bank & valid_right_bank
    )
  # tidyr::pivot_longer(cols = c(right_bank, left_bank), 
  # names_to = "point_type", values_to = "max_Z_at_banks") %>% 
  # dplyr::mutate(max_Z_at_banks = ifelse(is.na(max_Z_at_banks), 0, max_Z_at_banks))
  
  # Add the following columns to the final output data:
  # bottom - numeric, max depth (depth of lowest "bottom" point)
  # left_bank - numeric, min depth of left bank (depth of the highest "left_bank" point). If no left_bank points exist, value is 0.
  # right_bank - numeric, min depth of right bank (depth of the highest "right_bank" point). If no right_bank points exist, value is 0.
  # valid_banks - logical, TRUE if the hy_id/cs_id has a bottom point with atleast 1 leftbank point AND 1 rightbank point that are above the lowest "bottom" point 
  classified_pts <-
    classified_pts %>% 
    dplyr::left_join(
      dplyr::select(bank_validity, 
                    hy_id, cs_id, 
                    bottom, left_bank, right_bank,
                    valid_left_bank, valid_right_bank, valid_banks
      ),
      by = c("hy_id", "cs_id")
    ) 
  # %>%
  # dplyr::mutate(valid_banks2 = valid_left_bank & valid_right_bank)
  
  # # return simple dataset if add is FALSE
  # if(!add) {
  #   # subset to just hy_id/cs_id and added bank attributes to 
  #   # return a dataframe with unique hy_id/cs_ids for each row 
  #   bank_validity %>% 
  #     sf::st_drop_geometry() %>%  # drop sf geometry as a safety precaution to make sure returned data is a dataframe
  #     dplyr::select(hy_id, cs_id, 
  #                   bottom, left_bank, right_bank, 
  #                   valid_banks)
  #   
  #   return(bank_validity)
  #   
  # }
  
  # select specific rows and returns
  classified_pts <- 
    classified_pts %>% 
    dplyr::select(hy_id, cs_id, pt_id, Z, 
                  relative_distance, cs_lengthm, 
                  class, point_type, 
                  bottom, left_bank, right_bank, valid_banks)
  
  # check if any of the columns in 'classified_pts' are geometry types  and move them to the end column if they do exist
  classified_pts <- move_geometry_to_last(classified_pts)
  
  return(classified_pts)
  
}

#' @title Get attributes about the banks of each cross section in a dataframe of cross section points 
#' Given a set of cross section points with point_type column, return a dataframe of the unique hy_id/cs_ids with the following calculated columns:
#' "bottom", "left_bank", "right_bank" columns which are the Z values of the "lowest" bottom point, and the "highest" left and right bank Z values, respectively. 
#' And a "valid_banks" column indicating whether the hy_id/cs_id set of cross section point has at least a signle bottom point with 
#' at least 1 left bank point AND 1 right bank point that are above the lowest "bottom" point.
#' @param classified_pts sf or dataframe of points with "hy_id", "cs_id", and "point_type" columns. Output of hydrofabric3D::classify_pts()
#' @param crosswalk_id character, ID column  
#' @return dataframe with each row being a unique hy_id/cs_id with "bottom", "left_bank", "right_bank", and "valid_banks" values for each hy_id/cs_id.
#' @importFrom dplyr mutate case_when filter select group_by summarise ungroup left_join rename any_of across bind_rows
#' @importFrom tidyr pivot_wider
#' @export
get_bank_attributes <- function(
    classified_pts,
    crosswalk_id = NULL
) {
  # -----------------------------------------------------
  # classified_pts <- data.frame(
  #   hy_id = c("A", "A", "A", "B", "B", "B"),
  #   cs_id = c(1, 1, 1, 1, 1, 1),
  #   pt_id = c(1, 2, 3, 1, 2, 3),
  #   point_type = c('channel', 'channel', 'channel', "left_bank", "bottom", "right_bank"),
  #   Z = c(1, 5, 8, 10, 2, 12)
  # )
  # crosswalk_id = "hy_id"
  # -----------------------------------------------------
  
  # type checking, throw an error if not "sf", "tbl_df", "tbl", or "data.frame"
  if (!any(class(classified_pts) %in% c("sf", "tbl_df", "tbl", "data.frame"))) {
    stop("Invalid argument type, 'classified_pts' must be of type 'sf', 'tbl_df', 'tbl' or 'data.frame', given type was '",
         class(classified_pts), "'")
  }
  
  # Add columns with the counts of point types
  classified_pts <- add_point_type_counts(classified_pts, crosswalk_id)
  # classified_pts <- hydrofabric3D::add_point_type_counts2(classified_pts, crosswalk_id)
  
  # TODO: Need to add code that will just set aside the geometries and add them back to the final output dataset
  # For now we will just drop geometries as safety precaution (as to not summarize() on a massive number of sf geometries)
  classified_pts <- sf::st_drop_geometry(classified_pts)
  
  # Add a valid_count column which is TRUE 
  # if a hy_id/cs_id has a bottom point AND atleast 1 left and right bank
  classified_pts <- 
    classified_pts %>% 
    # sf::st_drop_geometry() %>%  # drop sf geometry as a safety precaution to make sure returned data is a dataframe
    dplyr::mutate(
      valid_count = dplyr::case_when(
        (bottom_count > 0 & 
           left_bank_count > 0 & 
           right_bank_count > 0)  ~ TRUE,
        TRUE                      ~ FALSE
      )
    )
  
  # Add minimum bottom Z, max left and right bank Z, and 
  # flags noting if the left/right banks are "valid" (i.e. max left/right bank values are greater than the bottom Z)
  bank_validity <-
    classified_pts %>% 
    # classified_pts2 %>% 
    # sf::st_drop_geometry() %>%  # drop sf geometry as a safety precaution to make sure returned data is a dataframe
    dplyr::filter(point_type %in% c("bottom", "left_bank", "right_bank")) %>% 
    # dplyr::filter(point_type %in% c("left_bank", "right_bank")) %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, Z, point_type) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id", "point_type")))) %>% 
    # dplyr::select(hy_id, cs_id, pt_id, Z, point_type) %>% 
    # dplyr::group_by(hy_id, cs_id, point_type) %>% 
    dplyr::summarise(
      minZ = min(Z, na.rm = TRUE),
      maxZ = max(Z, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      names_from  = point_type,
      values_from = c(minZ, maxZ)
    ) %>% 
    # dplyr::select(
    #     dplyr::any_of(crosswalk_id), 
    #     cs_id, 
    #     bottom     = minZ_bottom, 
    #     left_bank  = maxZ_left_bank, 
    #     right_bank = maxZ_right_bank
    #     ) 
    dplyr::select(
      dplyr::any_of(
        c(
          crosswalk_id,
          "cs_id",
          "minZ_bottom",
          "maxZ_left_bank",
          "maxZ_right_bank"
        ))
      # cs_id,
      # bottom     = minZ_bottom, 
      # left_bank  = maxZ_left_bank, 
      # right_bank = maxZ_right_bank
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
    if (!col %in% names(bank_validity)) {
      bank_validity[[col]] <- NA
    }
  }
  
  bank_validity <-
    bank_validity %>% 
    dplyr::mutate(
      # bottom     = ifelse(is.na(bottom), 0, bottom),          # Old way was to set the NA left/bank/bottom Z values to 0 but i think this could lead to problems with small number of edge cases
      # right_bank = ifelse(is.na(right_bank), 0, right_bank),
      # left_bank  = ifelse(is.na(left_bank), 0, left_bank),
      valid_left_bank = dplyr::case_when(
        (left_bank > bottom) & (!is.na(left_bank))   ~ TRUE,    # Old method used: left_bank > bottom ~ TRUE,
        TRUE               ~ FALSE
      ),
      valid_right_bank = dplyr::case_when(
        (right_bank > bottom) & (!is.na(right_bank)) ~ TRUE,    # Old method used: right_bank > bottom ~ TRUE,
        TRUE                ~ FALSE
      ),
      valid_banks = valid_left_bank & valid_right_bank
    )
  
  # Add the following columns to the final output data:
  # bottom - numeric, max depth (depth of lowest "bottom" point)
  # left_bank - numeric, min depth of left bank (depth of the highest "left_bank" point). If no left_bank points exist, value is 0.
  # right_bank - numeric, min depth of right bank (depth of the highest "right_bank" point). If no right_bank points exist, value is 0.
  # valid_banks - logical, TRUE if the hy_id/cs_id has a bottom point with atleast 1 leftbank point AND 1 rightbank point that are above the lowest "bottom" point 
  
  # set default column values for any IDs that didnt have 'left_bank', 'right_bank', or 'bottom' point_types 
  bank_validity_tmp_ids <- add_tmp_id(bank_validity, x = crosswalk_id)$tmp_id
  
  default_bank_attrs <- 
    classified_pts %>% 
    add_tmp_id(x = crosswalk_id) %>% 
    dplyr::filter(
      !tmp_id %in% bank_validity_tmp_ids
    ) %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, tmp_id) %>% 
    dplyr::group_by(tmp_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-tmp_id) %>% 
    add_default_bank_attributes() 
  
  # subset to just hy_id/cs_id and added bank attributes to 
  # return a dataframe with unique hy_id/cs_ids for each row 
  bank_validity <- 
    bank_validity %>% 
    dplyr::select(
      dplyr::any_of(crosswalk_id), 
      cs_id,
      bottom, left_bank, right_bank, valid_banks
    ) %>% 
    dplyr::bind_rows(
      default_bank_attrs
    )
  
  return(bank_validity)
  
}

#' Add "bottom", "left_bank", "right_bank", and "valid_banks" column defaults to a dataframe
#' Internal helper function for get_bank_attributes()
#' @param df dataframe, tibble, or sf dataframe
#'
#' @return dataframe, tibble, or sf dataframe
#' @noRd
#' @keywords internal
add_default_bank_attributes <- function(df) {
  bank_attrs_cols <- c("bottom", "left_bank", "right_bank")
  
  for (col in bank_attrs_cols) {
    df[[col]] <- NA
  }
  
  df$valid_banks <- FALSE 
  
  return(df)
  
}


# TODO: DELETE add_point_type_counts2()

#' @title Add the count of each point type as a column to a dataframe of section points
#' @description add_point_type_counts() will add columns to the input dataframe with the counts of every point_type for each hy_id/cs_id in the input dataframe of classified cross section points (output of classify_pts())
#' @param classified_pts dataframe or sf dataframe, cross section points with a "hy_id", and "cs_id" columns as well as a 'point_type' column containing the values: "bottom", "left_bank", "right_bank", and "channel"
#' @return dataframe or sf dataframe with "<point_type>_count" columns added
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr group_by count ungroup summarize filter n_distinct select slice left_join relocate all_of last_col
#' @importFrom tidyr pivot_wider pivot_longer
#' @noRd
#' @keywords internal
add_point_type_counts2 <- function(classified_pts) {
  
  # classified_pts <- cs_pts %>% hydrofabric3D::classify_points()
  # add = F
  # classified_pts = classified_pts2
  # add = TRUE
  
  # type checking
  if (!any(class(classified_pts) %in% c("sf", "tbl_df", "tbl", "data.frame"))) {
    stop("Invalid argument type, 'classified_pts' must be of type 'sf', 'tbl_df', 'tbl' or 'data.frame', given type was '",  
         class(classified_pts), "'")
  }
  
  # create a copy of the input dataset, add a tmp_id column
  stage_df <- 
    classified_pts %>% 
    sf::st_drop_geometry() %>% 
    hydrofabric3D::add_tmp_id() 
  
  # # create a reference dataframe with all possible combinations of tmp_id and point_type
  # reference_df <- expand.grid(
  #   tmp_id     = unique(stage_df$tmp_id),
  #   point_type = unique(stage_df$point_type)
  # )
  
  # get a count of the point_types in each hy_id/cs_id group (i.e. each cross section)
  point_type_counts <- 
    stage_df %>%
    dplyr::group_by(tmp_id, point_type) %>%
    dplyr::count() %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # add levels to the point_type column so if a given point_type
      # is NOT in the cross seciton points, then it will be added with NAs in the subsequent pivot_wider
      point_type = factor(point_type, levels = c("left_bank", "bottom", "right_bank", "channel"))
    ) 
  
  # pivot data wider to get implicit missing groups with NA values
  point_type_counts <- 
    point_type_counts %>% 
    tidyr::pivot_wider(
      names_from   = point_type,
      values_from  = n,
      names_expand = TRUE
    ) 
  
  point_type_counts <- 
    point_type_counts %>% 
    tidyr::pivot_longer(
      cols      = c(bottom, channel, right_bank, left_bank),
      names_to  = "point_type",
      values_to = "n"
    ) %>% 
    dplyr::mutate(n = ifelse(is.na(n), 0, n))
  
  # # Join the count of point types in each group with the reference_df to 
  # # get rows of NA values for any group that is missing a specific point_type
  # point_type_counts <- 
  #   point_type_counts %>% 
  #   dplyr::right_join(reference_df, by = c("tmp_id", "point_type"))
  
  # # For any cross section group that does NOT contain a point type, 
  # # the point type will be NA and here we replace those NAs with 0 
  # point_type_counts$n[is.na(point_type_counts$n)] <- 0
  
  # # make sure that all tmp_id groups have all 4 point types
  check_counts <-
    point_type_counts %>%
    dplyr::group_by(tmp_id) %>%
    dplyr::summarize(unique_count = dplyr::n_distinct(point_type)) %>%
    dplyr::filter(unique_count == 4) 
  
  # if the number of distinct points types in each cross section is not 4, raise an error
  if (length(unique(stage_df$tmp_id)) != nrow(check_counts)) {
    stop("Error validating each hy_id/cs_id cross section contains exactly 4 distinct values in the 'point_type' column")  
  }
  
  # get the hy_id, cs_id for each tmp_id to cross walk back to just using hy_id/cs_id
  stage_df <- 
    stage_df %>% 
    dplyr::select(tmp_id, hy_id, cs_id) %>% 
    dplyr::group_by(tmp_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # convert the column of point types to be a column for each point type that 
  # has the point type count for each hy_id/cs_id (cross section)
  point_type_counts <- 
    point_type_counts %>% 
    tidyr::pivot_wider(names_from = point_type,  
                       names_glue = "{point_type}_count", 
                       values_from = n) %>% 
    dplyr::left_join(
      stage_df,
      by = "tmp_id"
    ) %>% 
    dplyr::select(hy_id, cs_id, left_bank_count, right_bank_count, channel_count, bottom_count)
  
  # Join the point type counts to the original dataframe
  classified_pts <- 
    classified_pts %>% 
    dplyr::left_join(
      point_type_counts,
      by = c("hy_id", "cs_id")
    )
  
  # check if any of the columns in 'classified_pts' are geometry types  and move them to the end column if they do exist
  classified_pts <- move_geometry_to_last(classified_pts)
  
  return(classified_pts)
}
