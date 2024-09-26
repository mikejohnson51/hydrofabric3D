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
    "new_cs_lengthm"
  )
)
#' @title Function to add a new "tmp_id" column to a dataframe from 2 other columns
#' @description
#' Internal convenience function for creating a tmp_id column from 2 other columns in a dataframe. 
#' Default is to use hy_id and cs_id columns to create a tmp_id = <hy_id>_<cs_id>.
#' @param df dataframe with x and y as columns
#' @param x The name of the column in df to make up the first part of the added tmp_id column (tmp_id = x_y). Default is hy_id.
#' @param y The name of the column in df to make up the second part of the added tmp_id column (tmp_id = x_y). Default is cs_id.
#' 
#' @return The input dataframe with the "tmp_id" column added.
#' 
#' @importFrom dplyr mutate
#' @noRd
#' @keywords internal
add_tmp_id2 <- function(df, x = hy_id, y = cs_id) {
  # # Create the "tmp_id" column by concatenating values from "x" and "y"
  # df <- dplyr::mutate(df, tmp_id = paste0({{x}}, "_", {{y}}))
  
  # first try to add the tmp_id as if 'x' and 'y' are characters
  # if that fails, then use 'x' and 'y' as tidyselectors in dplyr::mutate()
  tryCatch({
    
    tmp_ids = paste0(df[[x]], "_", df[[y]])
    df$tmp_id = tmp_ids
    
    return(df)
    
  }, error = function(e) { })
  
  # if columns are NOT characters, then try with tidyselectors...
  df <- dplyr::mutate(df, 
                      tmp_id = paste0({{x}}, "_", {{y}})) # Create the "tmp_id" column by concatenating values from "x" and "y"
  
  return(df)
}

#' @title Function to add a new "tmp_id" column to a dataframe from 2 other columns
#' @description
#' Internal convenience function for creating a tmp_id column from 2 other columns in a dataframe. 
#' Default is to use hy_id and cs_id columns to create a tmp_id = <hy_id>_<cs_id>.
#' @param df dataframe with x and y as columns
#' @param x character, column name in df to make up the first part of the added tmp_id column (tmp_id = x_y). Default is "hy_id." 
#' @param y character, column name in df to make up the second part of the added tmp_id column (tmp_id = x_y). Default is "cs_id."
#' 
#' @return The input dataframe with the "tmp_id" column added.
#' 
#' @export
add_tmp_id <- function(df, x = "hy_id", y = "cs_id") {
  # # Create the "tmp_id" column by concatenating values from "x" and "y"
  
  # first try to add the tmp_id as if 'x' and 'y' are characters
  # if that fails, then use 'x' and 'y' as tidyselectors in dplyr::mutate()
    tmp_ids = paste0(df[[x]], "_", df[[y]])
    df$tmp_id = tmp_ids

  return(df)
}

#' @title Get a list of unique tmp_ids in a dataframe 
#' @description
#' Dataframe can have "tmp_id" column already or the columns can be specified with 'x' and 'y' arguments
#' 
#' @param df dataframe with x and y as columns, with an optional "tmp_id" column, otherwise a tmp_id will be created from x_y
#' @param x The name of the column in df to make up the first part of the added tmp_id column (tmp_id = x_y). Default is hy_id.
#' @param y The name of the column in df to make up the second part of the added tmp_id column (tmp_id = x_y). Default is cs_id.
#' 
#' @return character vector of unique "tmp_id" values in the given dataframe 
#' 
#' @export
get_unique_tmp_ids <- function(df, x = "hy_id", y = "cs_id") {
  
  # if no tmp_id exists, add one
  if (!"tmp_id" %in% names(df)) {
    # message("No 'tmp_id' found, adding 'tmp_id' from 'x' and 'y' columns")
    # df <- 
    #   df %>% 
    #   hydrofabric3D::add_tmp_id(x = {{x}}, y = {{y}}) 
    # df <- 
    #   df %>% 
    #   hydrofabric3D::add_tmp_id(x = x, y = y) 
    
    tmp_ids = paste0(df[[x]], "_", df[[y]])
    df$tmp_id = tmp_ids

  }
  
  # get the unique tmp_ids
  unique_tmp_ids <- unique(df$tmp_id)
  
  return(unique_tmp_ids)
  
}

#' Add a unique 'hydrofabric_id` to each row of a dataframe
#' Internal conveniance function for when a dataframe / flowlines network does NOT have a specified ID column
#' @param df sf dataframe, tibble, or dataframe
#' @importFrom dplyr mutate n
#' @return dataframe, sf dataframe, or tibble
add_hydrofabric_id <- function(df) {
  df <- 
    df %>% 
    dplyr::mutate(
      hydrofabric_id = 1:dplyr::n()
    )
  return(df) 
}

#' @title Move Geometry Column to the last column position
#' @description 
#' Internal utility function for taking a dataframe or an sf dataframe, checks for the existence of a geometry type column, and 
#' if it exists, moves it to the last column. If no geometry column exists, it returns the input dataframe as is.
#' @param df A dataframe or an sf dataframe.
#' @return Returns the input dataframe with the geometry column moved to the last position if it exists. Otherwise, returns the input dataframe as is.
#' @importFrom dplyr relocate all_of last_col
#' @noRd
#' @keywords internal
#' @examples
#' \dontrun{
#' # Create a dataframe
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' # Add a geometry column (sf dataframe)
#' df_sf <- sf::st_sf(df, geometry = sf::st_sfc(sf::st_point(c(1, 2, 3))))
#' # move column 
#' df_sf <- dplyr::relocate(df_sf, x, geometry, y)
#' df_sf # geometry column should be move to the middle column
#' # Move geometry column to last position
#' df_sf_moved <- move_geometry_to_last(df_sf)
#' df_sf_moved # geometry column should be move the end column
#' }
move_geometry_to_last <- function(df) {
  # Check if any of the columns in the dataframe are geometry types
  check_for_geom <- sapply(df, function(col) {
    any(class(col) %in% c("sfc", "sfc_GEOMETRY", 
                         "sfc_POINT",  "sfc_MULTIPOINT", 
                          "sfc_LINESTRING", "sfc_MULTILINESTRING", 
                          "sfc_POLYGON",  "sfc_MULTIPOLYGON"
                          ))
  })
  
  # If there is a geometry type column, move it to the last position
  if (any(check_for_geom)) {
    geometry_colname <- names(df)[check_for_geom]
    
    # move geometry column to the end
    df <- 
      df %>% 
      dplyr::relocate(dplyr::all_of(geometry_colname), .after = dplyr::last_col())
  }
  
  return(df)
}

# TODO: probably delete this function given you can just use dplyr::select()....

#' Remove specified columns from a dataframe if they exist.
#'
#' @param df A dataframe.
#' @param columns_to_remove character vector specifying the names of columns to be removed.
#' @return dataframe with specified columns removed if they exist.
remove_cols_from_df <- function(df, columns_to_remove) {
  
  existing_columns <- intersect(columns_to_remove, colnames(df))
  
  # If columns exist, remove them
  if (length(existing_columns) > 0) {
    df <- df[, !colnames(df) %in% existing_columns, drop = FALSE]
  }
  
  return(df)
}

#' Reorder columns in a dataframe
#' Internal helper
#' @param df dataframe or sf dataframe
#' @param start_order character vector of columns to put first in order 
#'
#' @noRd
#' @keywords internal
#' @return dataframe with reordered columns
reorder_cols <- function(df, start_order) {
  
  col_names <- names(df)
  
  # all columns in first_cols exist in the dataframe
  missing_cols <- setdiff(start_order, col_names)
  if (length(missing_cols) > 0) {
    stop("The following columns are missing from the dataframe: ", paste(missing_cols, collapse = ", "))
  }
  
  # the new column order
  col_order <- c(start_order, col_names[!col_names %in% start_order])
  
  # # reorder cols
  # df <- df[, col_order]
  
  # reorder cols
  return(
    df[, col_order]
  )
}

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
  
  # make a unique ID if one is not given (NULL 'id')
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
  
  # make a unique ID if one is not given (NULL 'id')
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
#' @return dataframe with each row being a unique hy_id/cs_id with "bottom", "left_bank", "right_bank", and "valid_banks" values for each hy_id/cs_id.
#' @importFrom dplyr mutate case_when filter select group_by summarise ungroup left_join
#' @importFrom tidyr pivot_wider
#' @noRd
#' @keywords internal
get_bank_attributes2 <- function(
    classified_pts
) {
  
  # classified_pts <- output_pts
  # classified_pts
  # classified_pts <- classified_pts2 
  
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
  
  # subset to just hy_id/cs_id and added bank attributes to 
  # return a dataframe with unique hy_id/cs_ids for each row 
  bank_validity <- 
    bank_validity %>% 
    dplyr::select(hy_id, cs_id, 
                  bottom, left_bank, right_bank, 
                  valid_banks)
  
  return(bank_validity)
  
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


#' @title Add relief attributes to a dataframe of cross sections points
#' Given a set of cross section points (derived from hydrofabric3D::cross_section_pts() and hydrofabric3D::classify_points()) add a "has_relief" logical
#' value to data. The "has_relief" value is indicating whether a cross section "has relief".
#' Relief is determined by checking each set of cross section points have a left OR right bank that
#' has a depth difference from the bottom that isgreater than or equal to a percentage of the cross section length (e.g. Assuming a 'pct_of_length_for_relief' of 0.01 (1%) of a 100m cross section would have a relief depth threshold of 1m)
#' @param classified_pts sf or dataframe of points with "hy_id", "cs_id", "cs_lengthm", and "point_type" columns. Output of hydrofabric3D::classify_points()
#' @param pct_of_length_for_relief numeric, percent of cs_lengthm to use as the threshold depth for classifying whether a cross section has "relief". Default is 0.01 (1% of the cross sections length).
#' @return sf or dataframe with added "has_relief" columns or a dataframe of dataframe of unique hy_id/cs_id and "has_relief"
#' @importFrom dplyr select group_by slice ungroup mutate filter summarise left_join case_when all_of relocate last_col
#' @importFrom tidyr pivot_wider
#' @export
add_relief <- function(
    classified_pts,
    pct_of_length_for_relief = 0.01
) {
  
  # 34 * as.numeric("2.3")
  # classified_pts = output_pts
  # pct_of_length_for_relief = 0.01
  # classified_pts <- output_pts
  # pct_of_length_for_relief = 0.01
  
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
    dplyr::select(hy_id, cs_id, cs_lengthm) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      depth_threshold = round(cs_lengthm * pct_of_length_for_relief, 3) # maybe use floor() here
    )
  
  # get the minimum bottom point and maximum left and right bank points
  relief <-
    classified_pts %>% 
    # dplyr::filter(point_type %in% c("left_bank", "right_bank")) %>% 
    dplyr::filter(point_type %in% c("bottom", "left_bank", "right_bank")) %>% 
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
  
  # join lengths and depth threshold back with relief table and
  # calculate if the max difference between left/right bank vs bottom is 
  # greater than or equal to the depth threshold
  relief <-
    relief %>% 
    dplyr::left_join(
      cs_lengths, 
      by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
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
                    hy_id, cs_id, has_relief),
      by = c("hy_id", "cs_id")
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
#' @param pct_of_length_for_relief numeric, percent of cs_lengthm to use as the threshold depth for classifying whether a cross section has "relief". Default is 0.01 (1% of the cross sections length).
#' @param detailed logical, whether to return only a the "has_relief" column or 
#' include all derived relief based columns such as "max_relief" and the "pct_of_length_for_relief" used. Default is FALSE and returns a dataframe with only "hy_id", "cs_id", and "has_relief".
#' @return dataframe with each row being a unique hy_id/cs_id with a "has_relief" value for each hy_id/cs_id. If detailed = TRUE, then the output dataframe will include the following additional columns: "cs_lengthm", "max_relief", "pct_of_length_for_relief".
#' @importFrom dplyr select group_by slice ungroup mutate filter summarise left_join case_when all_of relocate last_col
#' @importFrom tidyr pivot_wider
#' @noRd
#' @keywords internal
get_relief2 <- function(
    classified_pts,
    pct_of_length_for_relief = 0.01,
    detailed = FALSE
) {
  
  # classified_pts
  # pct_of_length_for_relief = pct_of_length_for_relief
  # detailed                 = FALSE
  
  # classified_pts = output_pts
  # pct_of_length_for_relief = 0.01
  
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
    # classified_pts2 %>% 
    dplyr::select(hy_id, cs_id, cs_lengthm) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      depth_threshold = round(cs_lengthm * pct_of_length_for_relief, 3) # maybe use floor() here
    )
  
  # get the minimum bottom point and maximum left and right bank points
  relief <-
    classified_pts %>%
    # dplyr::filter(point_type %in% c("left_bank", "right_bank")) %>% 
    dplyr::filter(point_type %in% c("bottom", "left_bank", "right_bank")) %>% 
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
  
  # join lengths and depth threshold back with relief table and
  # calculate if the max difference between left/right bank vs bottom is 
  # greater than or equal to the depth threshold
  relief <-
    relief %>% 
    dplyr::left_join(
      cs_lengths, 
      by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      max_relief = max(c(round(right_bank - bottom, 3), 
                         round(left_bank - bottom, 3)), 
                       na.rm = TRUE)                     # TODO: removing NAs might not be the right call, removing them might set has_relief to TRUE and says "there IS relief but no valid banks"
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      has_relief = dplyr::case_when(
        max_relief >= depth_threshold ~ TRUE,
        TRUE                          ~ FALSE
      ),
      pct_of_length_for_relief = pct_of_length_for_relief
    )
  
  # if detailed set of data is specified, return the relief dataframe with additional columns
  if(detailed) {
    relief <- 
      relief %>% 
      dplyr::select(hy_id, cs_id, cs_lengthm, has_relief, max_relief, pct_of_length_for_relief)
    
    return(relief)
    
  }
  
  # return dataframe with just hy_id/cs_id, and has_relief
  relief <-
    relief %>% 
    dplyr::select(hy_id, cs_id, has_relief)
  
  return(relief)
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
  
  # ------------------------------------------------------------------------
  # ------------------------------------------------------------------------
  # crosswalk_id    <- "hy_id"
  # REQ_COLS  <- c(crosswalk_id, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
  # 
  # pct_of_length_for_relief <- 0.01
  # CS_LENGTHM               <- 100
  # MIN_REQ_RELIEF           <- CS_LENGTHM * pct_of_length_for_relief
  # detailed                 <- FALSE
  
  # classified_pts <-
  #   data.frame(
  #     hy_id = c("A", "A",  "A", "A", "A"),
  #     cs_id = c(1, 1, 1, 1, 1),
  #     pt_id = c(1, 2, 3, 4, 5),
  #     cs_lengthm = c(CS_LENGTHM),
  #     point_type = c('left_bank', 'bottom', 'bottom', 'bottom', 'right_bank'),
  #     Z = c(100, 10, 10, 10, 100)
  #   )
  # 
  # classified_pts <-
  #   data.frame(
  #     hy_id = c("A", "A",  "A", "A", "A"),
  #     cs_id = c(1, 1, 1, 1, 1),
  #     pt_id = c(1, 2, 3, 4, 5),
  #     cs_lengthm = c(CS_LENGTHM),
  #     point_type = c('channel', 'bottom', 'bottom', 'bottom', 'right_bank'),
  #     Z = c(100, 10, 10, 10, 100)
  #   )
  # 
  # classified_pts <-
  #   data.frame(
  #     hy_id = c("A", "A",  "A", "A", "A",
  #               "B", "B", "B", "B", "B"
  #               ),
  #     cs_id = c(1, 1, 1, 1, 1,
  #               1, 1, 1, 1, 1
  #               ),
  #     pt_id = c(1, 2, 3, 4, 5,
  #               1, 2, 3, 4, 5
  #               ),
  #     cs_lengthm = c(CS_LENGTHM),
  #     point_type = c(
  #       'channel', 'bottom', 'bottom', 'bottom', 'right_bank',
  #       'left_bank', 'bottom', 'bottom', 'bottom', 'right_bank'
  #       ),
  #     Z = c(100, 10, 10, 10, 100,
  #           100, 10, 10, 10, 100
  #           )
  #   )
  
  # classified_pts <-
  #   data.frame(
  #     hy_id = c("A", "A",  "A", "A", "A"),
  #     cs_id = c(1, 1, 1, 1, 1),
  #     pt_id = c(1, 2, 3, 4, 5),
  #     cs_lengthm = c(CS_LENGTHM),
  #     point_type = c('bottom', 'bottom', 'bottom', 'bottom', 'bottom'),
  #     Z = c(100, 100, 100, 100, 100)
  #   )
  
  # ------------------------------------------------------------------------
  # ------------------------------------------------------------------------
  
  # make a unique ID if one is not given (NULL 'id')
  if(is.null(crosswalk_id)) {
    # cs  <- add_hydrofabric_id(cs) 
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "cs_lengthm", "Z", "point_type")
  # REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "cs_lengthm", "relative_distance")

  if (!all(REQUIRED_COLS %in% names(classified_pts))) {
    missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(classified_pts))]
    stop("'classified_pts' is missing one or more of the required columns:\n > ",
         paste0(missing_cols, collapse = "\n > "))
  }
  
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
    # dplyr::select(hy_id, cs_id, cs_lengthm) %>% 
    # dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      depth_threshold = round(cs_lengthm * pct_of_length_for_relief, 3) # maybe use floor() here
    )
  
  # get the minimum bottom point and maximum left and right bank points
  relief <-
    classified_pts %>%
    # dplyr::filter(point_type %in% c("left_bank", "right_bank")) %>% 
    dplyr::filter(point_type %in% c("bottom", "left_bank", "right_bank")) %>% 
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
    #       dplyr::any_of(crosswalk_id),
    #       cs_id, 
    #       bottom     = minZ_bottom, 
    #       left_bank  = maxZ_left_bank, 
    #       right_bank = maxZ_right_bank
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
      # message("Missing ", col, " in relief, adding default NA")
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
      # by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::group_by(hy_id, cs_id) %>% 
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
      # has_relief = dplyr::case_when(
        # max_relief >= depth_threshold ~ TRUE,
        # TRUE                          ~ FALSE
      # ),
      pct_of_length_for_relief = pct_of_length_for_relief
    ) 
    # dplyr::select(-has_missing_banks)
  
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

#' Validate that a dataframe is valid type (inherits from a dataframe) and has the given required columns
#'
#' @param x dataframe, sf, or tibble
#' @param cols character vector of columns that are required in 'x'
#' @param obj_name character string, optional name of object to display in error messages. If not obj_name is given, obj_name defaults to 'x'
#'
#' @noRd
#' @keywords internal
#' @return logical, TRUE value if all conditions are met and valid, otherwise an error is raised
validate_df <- function(x, cols, obj_name = NULL) {
  
  if(is.null(obj_name) | !inherits(obj_name, "character")) {
    obj_name <- "x"
  }
  
  if (!inherits(x, "data.frame")) {
    stop("Invalid '", obj_name, "' type of '",  class(x), "' \n '", 
         obj_name ,"' must inherit from 'data.frame', 'tibble', or 'sf'")
  }
  
  if (!all(cols %in% names(x))) {
    
    missing_cols <- cols[which(!cols %in% names(x))]
    
    stop("'", obj_name, "' is missing one or more of the required columns:\n > ", 
         paste0(missing_cols, collapse = "\n > "))
  }
  
  return(TRUE)
  
}

#' Validate Inputs for cut_cross_sections Function
#'
#' This function validates the inputs for the cut_cross_sections function to ensure they meet the required criteria.
#'
#' @param net An sf object representing the hydrographic network.
#' @param id A unique identifier column in the network data.
#' @param cs_widths Bankfull widths (length of cross sections) for each network element.
#' @param num Number of transects per network element.
#' @param smooth Logical, whether to smooth linestring geometries or not. 
#' @param densify Numeric, the factor by which to densify the linestrings.
#' @param rm_self_intersect Logical, whether to remove self-intersecting transect linestrings.
#' @param fix_braids Logical, whether to fix braided transect lines or not.
#' @param braid_threshold Numeric, the total length of all flowlines in a braid below which fix_braid_transects should operate.
#' @param braid_method Character, the method to determine the geometries to cut. Options are "comid", "component", or "neighbor". Default is "comid".
#' @param precision Numeric, the number of meters to approximate final cross-section linestring length.
#' @param add Logical, indicating whether to add original 'net' data to the outputted transect lines.
#' @return NULL if inputs are valid; otherwise, an error is thrown.
#' @noRd
#' @keywords internal
validate_cut_cross_section_inputs <- function(net, 
                                              id,
                                              cs_widths,
                                              num, 
                                              smooth,
                                              densify, 
                                              rm_self_intersect, 
                                              fix_braids, 
                                              braid_threshold , 
                                              braid_method, 
                                              precision, 
                                              add 
) {
  
  # Check if 'net' is an sf object
  if (!inherits(net, "sf")) {
    stop("'net' must be an sf object.")
  }
  
  # # Check if 'id' is NOT a character or if its NULL 
  # if (!is.character(id) || is.null(id)) {
  #   # if (is.null(id) || !is.character(id)) {
  #   stop("'id' must be a character vector")
  # }
  # 
  
  # Check if 'id' is NOT a character AND its NOT NULL
  if (!is.character(id) && !is.null(id)) {
    # if (is.null(id) || !is.character(id)) {
    stop("'id' must be a character vector or NULL")
  }
  
  # check if NOT NULL id is a column in 'net' 
  if (!id %in% names(net) && !is.null(id)) {
    stop("'id' column ", id, " is not a valid column in 'net'. 'id' must be a character vector or NULL")
  }
  
  # Check if 'cs_widths' is numeric or a numeric vector
  if (!is.numeric(cs_widths)) {
    stop("'cs_widths' must be a numeric")
  }
  
  # # Check if 'cs_widths' is numeric or a numeric vector
  # if (!is.numeric(cs_widths) && !is.null(cs_widths)) {
  #   stop("'cs_widths' must be numeric or NULL.")
  # }
  
  # Check if 'num' is numeric or a numeric vector
  if (!is.numeric(num)) {
    stop("'num' must be numeric")
  }
  
  # # Check if 'num' is numeric or a numeric vector
  # if (!is.numeric(num) && !is.null(num)) {
  #   stop("'num' must be numeric or NULL.")
  # }
  
  # Check if 'densify' is numeric or NULL
  if (!is.numeric(densify) && !is.null(densify)) {
    stop("'densify' must be numeric or NULL.")
  }
  
  # Check if 'smooth' is a logical value
  if (!is.logical(smooth)) {
    stop("'smooth' must be a logical value.")
  }
  
  # Check if 'rm_self_intersect' is a logical value
  if (!is.logical(rm_self_intersect)) {
    stop("'rm_self_intersect' must be a logical value.")
  }
  
  # Check if 'fix_braids' is a logical value
  if (!is.logical(fix_braids)) {
    stop("'fix_braids' must be a logical value.")
  }
  
  # # Check if 'terminal_id' is NOT a character and its NOT NULL
  # if (!is.character(terminal_id) && !is.null(terminal_id)) {
  #   # if (is.null(id) || !is.character(id)) {
  #   stop("'terminal_id' must be a character vector or NULL")
  # }
  
  # Check if 'braid_threshold' is numeric or NULL
  if (!is.null(braid_threshold) && !is.numeric(braid_threshold)) {
    stop("'braid_threshold' must be numeric or NULL.")
  }
  
  # # Check if 'version' is an integer and either 1 or 2
  # if (!is.numeric(version) || !(version %in% c(1, 2))) {
  #   stop("'version' must be an integer, either 1 or 2.")
  # }
  
  # Check if 'braid_method' is one of the valid options
  valid_methods <- c("comid", "component", "neighbor")
  if (!braid_method %in% valid_methods) {
    stop("'braid_method' must be one of 'comid', 'component', or 'neighbor'.")
  }
  
  # Check if 'precision' is numeric and greater than 0
  if (!is.numeric(precision) || precision <= 0) {
    stop("'precision' must be a numeric value greater than 0.")
  }
  
  # Check if 'add' is a logical value
  if (!is.logical(add)) {
    stop("'add' must be a logical value.")
  }
  
  return(NULL)
}

#' Calculate the length between the leftmost and rightmost bottom point in each cross section 
#'
#' @param cross_section_pts dataframe, or sf dataframe of cross section points
#' @param crosswalk_id character, ID column 
#' @importFrom dplyr select mutate case_when group_by lag ungroup filter summarise left_join across any_of
#' @return summarized dataframe of input cross_section_pts dataframe with a bottom_length value for each hy_id/cs_id
#' @noRd
#' @keywords internal
get_cs_bottom_length <- function(cross_section_pts, 
                                 crosswalk_id = NULL) {
  
  # make a unique ID if one is not given (NULL 'id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "relative_distance", "point_type")
  
  # validate input graph
  is_valid <- validate_df(cross_section_pts, REQUIRED_COLS, "cross_section_pts")
  
  # get the distance between cross section pts in each cross section,
  # this will be used as a default for bottom length in case bottom length is 0
  interval_distances <- 
    cross_section_pts %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, relative_distance)
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::select(hy_id, cs_id, pt_id, relative_distance) %>% 
    # dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      distance_interval = relative_distance - dplyr::lag(relative_distance)
    ) %>% 
    dplyr::summarise(
      distance_interval = ceiling(mean(distance_interval, na.rm = TRUE)) # TODO: round up to make sure we are not underestimating 
      # the interval, we're going to use this value to 
      # derive a new Top width for each cross section if 
      # the cross section length is less than the prescribed top width
    ) %>% 
    dplyr::ungroup()
  
  # get the distance from the first and last bottom points, substittue any bottom lengths == 0 
  # with the interval between points distance
  bottom_lengths <-
    cross_section_pts %>% 
    dplyr::filter(point_type == "bottom") %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, relative_distance)
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::select(hy_id, cs_id, pt_id, relative_distance) %>% 
    # dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::summarise(
      bottom_start = min(relative_distance, na.rm = TRUE),
      bottom_end   = max(relative_distance, na.rm = TRUE)
    ) %>% 
    dplyr::left_join(
      interval_distances, 
      by = c(crosswalk_id, "cs_id")
      # by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      bottom_length = bottom_end - bottom_start
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      bottom_length = dplyr::case_when(
        floor(bottom_length) == 0 ~ distance_interval,
        TRUE                      ~ bottom_length
      )
    ) %>%
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, bottom_length)
    # dplyr::select(hy_id, cs_id, bottom_length)
  
  return(bottom_lengths)
  
}

#' Remove entire cross sections that have any NA Z (depth) values
#'
#' @param cross_section_pts cs points dataframe, tibble, or sf dataframe
#' @param id unique ID for flowline 
#' @importFrom dplyr group_by across any_of ungroup filter
#' @return cross_section_pts dataframe / tibble / sf dataframe with removed cross sections
#' @export
drop_incomplete_cs_pts <- function(cross_section_pts, id = NULL) {
  # make a unique ID if one is not given (NULL 'id')
  if(is.null(id)) {
    id  <- 'hydrofabric_id'
  }
  
  cross_section_pts <-  
    cross_section_pts %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(id, "cs_id")))) %>% 
    dplyr::filter(!any(is.na(Z))) %>% 
    dplyr::ungroup()
  
  return(cross_section_pts)
  
}

#' Calculates a validity score column based on valid_banks and has_relief columns in a set of cross section points
#'
#' @param cs_to_validate dataframe
#' @param crosswalk_id character, ID column
#' @param validity_col_name name of the output validity score column
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr group_by slice ungroup mutate select
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


#' Check if data is an SF linestring / multilinestring
#'
#' @param data dataframe, tibble, sf dataframe, geometry
#'
#' @return logical, TRUE if data is an sf linestring / multilinestring
is_sf_linestring <- function(data) {
  
  # check if the object inherits from sf class
  is_sf <- inherits(data, "sf")
  
  # Check if the object is a linestring or multilinestring
  is_linestring <- (
    inherits(data, "LINESTRING") || 
      inherits(data, "MULTILINESTRING") || 
      inherits(data, "sf_LINESTRING") || 
      inherits(data, "sf_MULTILINESTRING") || 
      inherits(data, "sfc_LINESTRING") ||
      inherits(data, "sfc_MULTILINESTRING")
  )
  
  return(is_sf && is_linestring)
}

#' Make a progress bar and return an "make_progress()" function to update the progress bar.
#' Credit to the exactextractr team: https://github.com/isciences/exactextractr/blob/5fd17dcf02717332b125345aea586304f668cf12/R/exact_extract_helpers.R#L361
#' @param progress logical, whether to make a progress bar or not (FALSE)
#' @param n numeric, total number of iterations
#' @importFrom utils txtProgressBar
#' @return make_progress function, when called will increment the progress bar text
#' @export
#'
#' @examples
#' progress=TRUE
#' x = 1:500000
#' make_progress <- make_progress_bar(progress, length(x))
#' for (i in 1:length(x)) {
#'     make_progress()
#' }
make_progress_bar <- function(progress, n) {
  if (progress && n > 1) {
    pb <- utils::txtProgressBar(min = 0, max = n, initial=0, style=3)
    make_progress <- function() {
      i <- 1 + utils::getTxtProgressBar(pb)
      utils::setTxtProgressBar(pb, i)
      if (i == n) {
        close(pb)
      }
    }
  } else {
    make_progress <- function() {}
  }
  
  return(make_progress)
}

