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
    "crosswalk_id",  
    "anchors", "deriv_type", "edge", "extension_distance", 
    "left_is_extended", "right_is_extended", "to_node", "verbose", 
    "toindid", "indid", "toid", "is", "internal_is_braided2"
  )
)


#' Given a set of transect lines, a flowline network, extend the transect lines out given distances from the left and right
#' Flowlines are required to ensure valid transect intersection relationship is maintained
#'
#' @param transects sf dataframe of linestrings, requires crosswalk_id, cs_id, grouping_id columns and numeric 'extension_distance' column indicating the distance to extend 
#' @param flowlines sf dataframe of linestrings
#' @param crosswalk_id character, column name that connects features in transects to flowlines
#' @param cs_id character, column name that uniquely identifies transects within a flowline
#' @param grouping_id character, column name in both transects and flowlines that denotes which flowlines are grouped with which transects.
#' @param direction character, whether to extend transects individually from left and right sides, or to strictly extend a transect if BOTH the left and right extension are valid. 
#' Valid inputs are either "any", "any_by_specific_distances", or "both".
#' @importFrom utils str
#' @importFrom geos as_geos_geometry
#' @importFrom wk wk_crs 
#' @importFrom sf st_geometry st_as_sf st_length
#' @importFrom nhdplusTools rename_geometry 
#' @importFrom dplyr mutate relocate any_of
#' @return transects sf dataframe with extended transect geometries, left and right distance columns, and flags indicating if the transect was extended in the left and/or right directions
#' @export
extend_transects_sides <-   function(
    transects,
    flowlines,
    crosswalk_id,
    cs_id       = "cs_id",
    grouping_id = "mainstem",
    direction   = "any"
) {
  
  valid_directions <- c("any", "both", "any_by_specific_distances")
  
  if (!direction %in% valid_directions) {
    stop("Invalid 'direction' argument '", direction, 
         "'\n'direction' must be one of ", 
         paste(valid_directions, collapse = ", "))
  }
  
  extension_function <- switch(
    direction,
    "any"  = extend_transects_any_side,
    "any_by_specific_distances"  = extend_transects_any_side_by_specific_distances,
    "both" = extend_transects_both_sides
  )
  
  extended_transects <- extension_function(
    transects     = transects,
    flowlines     = flowlines,
    crosswalk_id  = crosswalk_id,
    cs_id         = cs_id,
    grouping_id   = grouping_id
  )
  
  return(extended_transects)
  
}
#' Given a set of transect lines, a flowline network, extend the transect lines out given distances from the left and right
#' Flowlines are required to ensure valid transect intersection relationship is maintained
#'
#' @param transects sf dataframe of linestrings, requires crosswalk_id, cs_id, grouping_id columns and numeric 'extension_distance' column indicating the distance to extend 
#' @param flowlines sf dataframe of linestrings
#' @param crosswalk_id character, column name that connects features in transects to flowlines
#' @param cs_id character, column name that uniquely identifies transects within a flowline
#' @param grouping_id character, column name in both transects and flowlines that denotes which flowlines are grouped with which transects.
#' @importFrom utils str
#' @importFrom geos as_geos_geometry
#' @importFrom wk wk_crs 
#' @importFrom sf st_geometry st_as_sf st_length
#' @importFrom hydroloom rename_geometry 
#' @importFrom dplyr mutate relocate any_of
#' @return transects sf dataframe with extended transect geometries, left and right distance columns, and flags indicating if the transect was extended in the left and/or right directions
#' @noRd
#' @keywords internal
extend_transects_any_side <- function(
    transects,
    flowlines,
    crosswalk_id,
    cs_id       = "cs_id",
    grouping_id = "mainstem"
) {
  
  # ----------------------------------------------------------------------------------
  # ----------- Input checking ------
  # ----------------------------------------------------------------------------------
  
  flowlines  <- hydroloom::rename_geometry(flowlines, "geometry") 
  transects  <- hydroloom::rename_geometry(transects, "geometry") 
  
  is_flowlines_valid <- validate_df(flowlines, 
                                    c(crosswalk_id, grouping_id, "geometry"), 
                                    "flowlines")
  
  # validate input graph
  is_transects_valid <- validate_df(transects, 
                                    c(crosswalk_id, cs_id, grouping_id, "extension_distance", "geometry"), 
                                    "transects")
  
  if(!crosswalk_id %in% names(flowlines)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'flowlines' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks 'flowlines' to 'transects'")
  }
  
  if(!crosswalk_id %in% names(transects)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'transects' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks the 'transects' to 'flowlines'")
  }
  
  if(!cs_id %in% names(transects)) {
    stop("cs_id '", cs_id, "' is not a column in 'transects' input,\n", 
         "Please provide a valid 'cs_id' column name from 'transects'.\n",
         "The 'cs_id' should uniquely identify each transect lines within a flowline.\n", 
         "(ID for each transect within the crosswalk_id)")
  }
  
  if(!grouping_id %in% names(flowlines)) {
    stop("grouping_id '", grouping_id, "' is not a column in 'flowlines' input,\n", 
         "Please provide a valid 'grouping_id' that associates each transect line with 1 or more flowlines in 'flowlines'"
    )
  }
  
  if(!grouping_id %in% names(transects)) {
    stop("grouping_id '", grouping_id, "' is not a column in 'transect_lines' input,\n", 
         "Please provide a valid 'grouping_id' that associates each transect line with 1 or more flowlines in 'flowlines'"
    )
  }
  
  if(!'extension_distance' %in% names(transects)) {
    stop("transects is missing a numeric 'extension_distance' column.\n", 
         "A numeric 'extension_distance' column must be present to indicate the distance to extend each transect in the left and right directions."
    )
  }
  
  fline_id_array   <- flowlines[[crosswalk_id]]
  
  # TODO: next time, change this function to ONLY process transects that have ANY extension distance, right now we iterate through ALL transects,
  # TODO: and 'next' the ones with the no extension distance so doesn't really matter much but 
  
  # Convert the net object into a geos_geometry
  flowlines_geos       <- geos::as_geos_geometry(flowlines)
  transects_geos       <- geos::as_geos_geometry(transects)
  
  # stash the CRS of the transects to use when making the new extended transect lines
  line_crs             <- wk::wk_crs(transects)
  
  transect_crosswalk_id_array   <- transects[[crosswalk_id]]
  transect_cs_id_array          <- transects[[cs_id]]
  transect_uid_array            <- paste0(transect_crosswalk_id_array, "_", transect_cs_id_array) 
  
  # Intersect grouping IDs
  fline_group_id_array      <- flowlines[[grouping_id]]
  transect_group_id_array   <- transects[[grouping_id]]
  
  # distance vectors 
  left_distances       <- transects$extension_distance
  right_distances      <- transects$extension_distance
  
  # preallocate vectors for storing if transect was extended and from which directions
  left_extended_flag   <- rep(FALSE, length(transect_crosswalk_id_array))   
  right_extended_flag  <- rep(FALSE, length(transect_crosswalk_id_array))
  
  # number of geometries that will be iterated over, keeping this variable to reference in message block  
  total <- length(transect_crosswalk_id_array)
  
  make_progress <- make_progress_bar(TRUE, length(transect_crosswalk_id_array))
  
  for (i in seq_along(transect_crosswalk_id_array)) {
    
    make_progress()
    
    # get the current transect, hy_id, cs_id, flowline, and extension distances
    current_trans <- transects_geos[i]
    
    current_hy_id <- transect_crosswalk_id_array[i]
    current_cs_id <- transect_cs_id_array[i]
    current_uid   <- transect_uid_array[i]
    
    # distances to try extending
    left_distance_to_extend  <- left_distances[i]
    right_distance_to_extend <- right_distances[i]
    
    # skip the iteration if no extension required
    no_extension_required <- (
      (left_distance_to_extend == 0 || is.na(left_distance_to_extend) || is.null(left_distance_to_extend)) && 
        (right_distance_to_extend == 0 || is.na(right_distance_to_extend) || is.null(right_distance_to_extend)) 
    )
    # no_extension_required <- (left_distance_to_extend == 0 && right_distance_to_extend == 0)
    
    # Skip the iteration if NO extension distance in either direction
    if(no_extension_required) {
      next
    }
    
    # extend the transects by the prescribed distances
    left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            left_distance_to_extend, "head")
    right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            right_distance_to_extend, "tail")
    
    # TODO version 2:
    # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
    use_left_extension  <- is_valid_transect_line2(
      left_extended_trans,
      transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
      # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
      flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
    ) 
    
    use_right_extension <- is_valid_transect_line2(
      right_extended_trans, 
      transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
      # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
      flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
    )
    
    used_half_of_left  <- FALSE
    used_half_of_right <- FALSE
    
    # TODO: Probably should precompute this division BEFORE the loop...
    half_left_distance   <- ifelse(left_distance_to_extend > 0, left_distance_to_extend %/% 2, 0)
    half_right_distance  <- ifelse(right_distance_to_extend > 0, right_distance_to_extend %/% 2, 0)
    
    # if we CAN'T use the original LEFT extension distance, 
    # we try HALF the distance (or some distane less than we extended by in the first place)
    if (!use_left_extension) {
      
      # half_left_distance   <- ifelse(left_distance_to_extend > 0, left_distance_to_extend %/% 2, 0)
      left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
                                                              half_left_distance, "head")
      # TODO version 2:
      # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
      # if (!is.null(intersect_group_id)) {
      use_left_extension  <- is_valid_transect_line2(
        left_extended_trans,
        transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      ) 
      
      used_half_of_left <- ifelse(use_left_extension, TRUE,  FALSE)
    }
    
    # if we CAN'T use the original RIGHT extension distance, 
    # we try HALF the distance (or some distance less than we extended by in the first place)
    if (!use_right_extension) {
      
      # half_right_distance  <- ifelse(right_distance_to_extend > 0, right_distance_to_extend %/% 2, 0)
      right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
                                                              half_right_distance, "tail")
      
      use_right_extension <- is_valid_transect_line2(
        right_extended_trans, 
        transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      )
      
      used_half_of_right  <- ifelse(use_right_extension, TRUE,  FALSE)
      
    }
    
    # get the start and end point of the new line
    extension_pts <- pick_extension_pts(
      left_extended_trans,
      right_extended_trans,
      use_left_extension,
      use_right_extension
    )
    
    # single geos_geometry points
    start <- extension_pts[1]
    end   <- extension_pts[2]
    
    # create the new transect line
    updated_trans  <- make_line_from_start_and_end_pts(start, end, line_crs)
    
    left_extended_flag[i]   <- use_left_extension
    right_extended_flag[i]  <- use_right_extension
    updated_left_distance   <- ifelse(use_left_extension, 
                                      ifelse(used_half_of_left, half_left_distance, left_distance_to_extend),
                                      0
    )
    
    updated_right_distance  <- ifelse(use_right_extension, 
                                      ifelse(used_half_of_right, half_right_distance, right_distance_to_extend),
                                      0
    )
    
    left_distances[i]       <- updated_left_distance
    right_distances[i]      <- updated_right_distance
    
    # TODO: review this, didnt have this check before so theoretically, an iteration could happen where neither left or right extensions were used, but we still 
    # TODO: set the transect to the updated transect
    if (use_left_extension || use_right_extension) {
      # last step is to replace the original transect with the updated transect (extended)
      transects_geos[i] <- updated_trans
    }
    
  }      
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects)   <- sf::st_geometry(sf::st_as_sf(transects_geos))
  
  # replace the distance values so that any transects that were extended HALFWAY will be accounted for
  transects$left_distance      <- left_distances 
  transects$right_distance     <- right_distances
  
  # Flags indicating if extensions happened or not (probably can just be dropped)
  transects$left_is_extended   <- left_extended_flag
  transects$right_is_extended  <- right_extended_flag
  
  transects <- hydroloom::rename_geometry(transects, "geometry")
  
  transects <-
    transects %>% 
    dplyr::mutate(
      cs_lengthm = as.numeric(sf::st_length(.))
    ) %>% 
    dplyr::relocate(
      dplyr::any_of(c(crosswalk_id, cs_id)),
      cs_lengthm
    )
  
  transects <- move_geometry_to_last(transects)
  
  return(transects)
  
}

#' Given a set of transect lines, a flowline network, extend the transect lines out specific distances for each side from the left and/or the right
#' Flowlines are required to ensure valid transect intersection relationship is maintained.
#' A version of extend_transects_any_side() that allows for a specific 'left_distance' and 'right_distance' to specify the desired distance extension from each side.
#' 
#' @param transects sf dataframe of linestrings, requires crosswalk_id, cs_id, grouping_id columns and numeric 'left_distance' and 'right_distance' columns indicating the distance to extend each side by
#' @param flowlines sf dataframe of linestrings
#' @param crosswalk_id character, column name that connects features in transects to flowlines
#' @param cs_id character, column name that uniquely identifies transects within a flowline
#' @param grouping_id character, column name in both transects and flowlines that denotes which flowlines are grouped with which transects.
#' @importFrom utils str
#' @importFrom geos as_geos_geometry
#' @importFrom wk wk_crs 
#' @importFrom sf st_geometry st_as_sf st_length
#' @importFrom hydroloom rename_geometry 
#' @importFrom dplyr mutate relocate any_of
#' @return transects sf dataframe with extended transect geometries, left and right distance columns, and flags indicating if the transect was extended in the left and/or right directions
#' @noRd
#' @keywords internal
extend_transects_any_side_by_specific_distances <- function(
    transects,
    flowlines,
    crosswalk_id,
    cs_id       = "cs_id",
    grouping_id = "mainstem"
) {
  
  # ----------------------------------------------------------------------------------
  # ----------- Input checking ------
  # ----------------------------------------------------------------------------------
  
  flowlines  <- hydroloom::rename_geometry(flowlines, "geometry") 
  transects  <- hydroloom::rename_geometry(transects, "geometry") 
  
  is_flowlines_valid <- validate_df(flowlines, 
                                    c(crosswalk_id, grouping_id, "geometry"), 
                                    "flowlines")
  
  # validate input graph
  is_transects_valid <- validate_df(transects, 
                                    c(crosswalk_id, cs_id, grouping_id, "left_distance", "right_distance", "geometry"), 
                                    "transects")
  
  if(!'right_distance' %in% names(transects)) {
    stop("transects is missing a numeric 'extension_distance' column.\n", 
         "A numeric 'extension_distance' column must be present to indicate the distance to extend each transect in the left and right directions."
    )
  }
  
  fline_id_array   <- flowlines[[crosswalk_id]]
  
  # Convert the net object into a geos_geometry
  flowlines_geos       <- geos::as_geos_geometry(flowlines)
  transects_geos       <- geos::as_geos_geometry(transects)
  
  # stash the CRS of the transects to use when making the new extended transect lines
  line_crs             <- wk::wk_crs(transects)
  
  transect_crosswalk_id_array   <- transects[[crosswalk_id]]
  transect_cs_id_array          <- transects[[cs_id]]
  transect_uid_array            <- paste0(transect_crosswalk_id_array, "_", transect_cs_id_array) 
  
  # Intersect grouping IDs
  fline_group_id_array      <- flowlines[[grouping_id]]
  transect_group_id_array   <- transects[[grouping_id]]
  
  # distance vectors 
  left_distances       <- transects$left_distance
  right_distances      <- transects$right_distance
  
  # preallocate vectors for storing if transect was extended and from which directions
  left_extended_flag   <- rep(FALSE, length(transect_crosswalk_id_array))   
  right_extended_flag  <- rep(FALSE, length(transect_crosswalk_id_array))
  
  # number of geometries that will be iterated over, keeping this variable to reference in message block  
  total <- length(transect_crosswalk_id_array)
  
  make_progress <- make_progress_bar(TRUE, length(transect_crosswalk_id_array))
  
  for (i in seq_along(transect_crosswalk_id_array)) {
    
    make_progress()
    
    # get the current transect, hy_id, cs_id, flowline, and extension distances
    current_trans <- transects_geos[i]
    
    current_hy_id <- transect_crosswalk_id_array[i]
    current_cs_id <- transect_cs_id_array[i]
    current_uid   <- transect_uid_array[i]
    
    # distances to try extending
    left_distance_to_extend  <- left_distances[i]
    right_distance_to_extend <- right_distances[i]
    
    # skip the iteration if no extension required
    no_extension_required <- (
      (left_distance_to_extend == 0 || is.na(left_distance_to_extend) || is.null(left_distance_to_extend)) && 
        (right_distance_to_extend == 0 || is.na(right_distance_to_extend) || is.null(right_distance_to_extend)) 
    )

    # Skip the iteration if NO extension distance in either direction
    if(no_extension_required) {
      next
    }
    
    # extend the transects by the prescribed distances
    left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            left_distance_to_extend, "head")
    right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            right_distance_to_extend, "tail")
    
    # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
    use_left_extension  <- is_valid_transect_line2(
      left_extended_trans,
      transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
      flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
    ) 
    
    use_right_extension <- is_valid_transect_line2(
      right_extended_trans, 
      transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
      flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
    )
    
    used_half_of_left  <- FALSE
    used_half_of_right <- FALSE
    
    # TODO: Probably should precompute this division BEFORE the loop...
    half_left_distance   <- ifelse(left_distance_to_extend > 0, left_distance_to_extend %/% 2, 0)
    half_right_distance  <- ifelse(right_distance_to_extend > 0, right_distance_to_extend %/% 2, 0)
    
    # if we CAN'T use the original LEFT extension distance, 
    # we try HALF the distance (or some distane less than we extended by in the first place)
    if (!use_left_extension) {
      
      left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
                                                              half_left_distance, "head")

      # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
      use_left_extension  <- is_valid_transect_line2(
        left_extended_trans,
        transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      ) 
      
      used_half_of_left <- ifelse(use_left_extension, TRUE,  FALSE)
    }
    
    # if we CAN'T use the original RIGHT extension distance, 
    # we try HALF the distance (or some distance less than we extended by in the first place)
    if (!use_right_extension) {
      
      # half_right_distance  <- ifelse(right_distance_to_extend > 0, right_distance_to_extend %/% 2, 0)
      right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
                                                              half_right_distance, "tail")
      
      use_right_extension <- is_valid_transect_line2(
        right_extended_trans, 
        transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      )
      
      used_half_of_right  <- ifelse(use_right_extension, TRUE,  FALSE)
      
    }
    
    # get the start and end point of the new line
    extension_pts <- pick_extension_pts(
      left_extended_trans,
      right_extended_trans,
      use_left_extension,
      use_right_extension
    )
    
    # single geos_geometry points
    start <- extension_pts[1]
    end   <- extension_pts[2]
    
    # create the new transect line
    updated_trans  <- make_line_from_start_and_end_pts(start, end, line_crs)
    
    left_extended_flag[i]   <- use_left_extension
    right_extended_flag[i]  <- use_right_extension
    updated_left_distance   <- ifelse(use_left_extension, 
                                     ifelse(used_half_of_left, half_left_distance, left_distance_to_extend),
                                     0
                                     )
    
    updated_right_distance  <- ifelse(use_right_extension, 
                                    ifelse(used_half_of_right, half_right_distance, right_distance_to_extend),
                                    0
                                    )
    
    left_distances[i]       <- updated_left_distance
    right_distances[i]      <- updated_right_distance
    
    # TODO: review this, didnt have this check before so theoretically, an iteration could happen where neither left or right extensions were used, but we still 
    # TODO: set the transect to the updated transect
    if (use_left_extension || use_right_extension) {
      # last step is to replace the original transect with the updated transect (extended)
      transects_geos[i] <- updated_trans
    }
    
  }      
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects)   <- sf::st_geometry(sf::st_as_sf(transects_geos))
  
  # replace the distance values so that any transects that were extended HALFWAY will be accounted for
  transects$left_distance      <- left_distances 
  transects$right_distance     <- right_distances
  
  # Flags indicating if extensions happened or not (probably can just be dropped)
  transects$left_is_extended   <- left_extended_flag
  transects$right_is_extended  <- right_extended_flag
  
  transects <- hydroloom::rename_geometry(transects, "geometry")
  
  transects <-
    transects %>% 
    dplyr::mutate(
      cs_lengthm = as.numeric(sf::st_length(.))
    ) %>% 
    dplyr::relocate(
      dplyr::any_of(c(crosswalk_id, cs_id)),
      cs_lengthm
    )
  
  transects <- move_geometry_to_last(transects)
  
  return(transects)
  
}

#' Given a set of transect lines, a flowline network, extend the transect lines out given distances from the left and right
#' Flowlines are required to ensure valid transect intersection relationship is maintained
#'
#' @param transects sf dataframe of linestrings, requires crosswalk_id, cs_id, grouping_id columns and numeric 'extension_distance' column indicating the distance to extend 
#' @param flowlines sf dataframe of linestrings
#' @param crosswalk_id character, column name that connects features in transects to flowlines
#' @param cs_id character, column name that uniquely identifies transects within a flowline
#' @param grouping_id character, column name in both transects and flowlines that denotes which flowlines are grouped with which transects.
#' @importFrom utils str
#' @importFrom geos as_geos_geometry
#' @importFrom wk wk_crs 
#' @importFrom sf st_geometry st_as_sf st_length
#' @importFrom hydroloom rename_geometry 
#' @importFrom dplyr mutate relocate any_of
#' @return transects sf dataframe with extended transect geometries, left and right distance columns, and flags indicating if the transect was extended in the left and/or right directions
#' @noRd
#' @keywords internal
extend_transects_both_sides <- function(
    transects,
    flowlines,
    crosswalk_id,
    cs_id       = "cs_id",
    grouping_id = "mainstem"
) {
  
  # ----------------------------------------------------------------------------------
  # ----------- Input checking ------
  # ----------------------------------------------------------------------------------
  
  flowlines  <- hydroloom::rename_geometry(flowlines, "geometry") 
  transects  <- hydroloom::rename_geometry(transects, "geometry") 
  
  is_flowlines_valid <- validate_df(flowlines, 
                                    c(crosswalk_id, grouping_id, "geometry"), 
                                    "flowlines")
  
  # validate input graph
  is_transects_valid <- validate_df(transects, 
                                    c(crosswalk_id, cs_id, grouping_id, "extension_distance", "geometry"), 
                                    "transects")
  
  if(!crosswalk_id %in% names(flowlines)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'flowlines' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks 'flowlines' to 'transects'")
  }
  
  if(!crosswalk_id %in% names(transects)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'transects' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks the 'transects' to 'flowlines'")
  }
  
  if(!cs_id %in% names(transects)) {
    stop("cs_id '", cs_id, "' is not a column in 'transects' input,\n", 
         "Please provide a valid 'cs_id' column name from 'transects'.\n",
         "The 'cs_id' should uniquely identify each transect lines within a flowline.\n", 
         "(ID for each transect within the crosswalk_id)")
  }
  
  if(!grouping_id %in% names(flowlines)) {
    stop("grouping_id '", grouping_id, "' is not a column in 'flowlines' input,\n", 
         "Please provide a valid 'grouping_id' that associates each transect line with 1 or more flowlines in 'flowlines'"
    )
  }
  
  if(!grouping_id %in% names(transects)) {
    stop("grouping_id '", grouping_id, "' is not a column in 'transects' input,\n", 
         "Please provide a valid 'grouping_id' that associates each transect line with 1 or more flowlines in 'flowlines'"
    )
  }
  
  if(!'extension_distance' %in% names(transects)) {
    stop("transects is missing a numeric 'extension_distance' column.\n", 
         "A numeric 'extension_distance' column must be present to indicate the distance to extend each transect from both sides of the transect."
    )
  }
  
  fline_id_array   <- flowlines[[crosswalk_id]]
  
  # TODO: next time, change this function to ONLY process transects that have ANY extension distance, right now we iterate through ALL transects,
  # TODO: and 'next' the ones with the no extension distance so doesn't really matter much but 
  
  # Convert the net object into a geos_geometry
  flowlines_geos       <- geos::as_geos_geometry(flowlines)
  transects_geos       <- geos::as_geos_geometry(transects)
  
  # stash the CRS of the transects to use when making the new extended transect lines
  line_crs             <- wk::wk_crs(transects)
  
  transect_crosswalk_id_array   <- transects[[crosswalk_id]]
  transect_cs_id_array          <- transects[[cs_id]]
  transect_uid_array            <- paste0(transect_crosswalk_id_array, "_", transect_cs_id_array) 
  
  # Intersect grouping IDs
  fline_group_id_array      <- flowlines[[grouping_id]]
  transect_group_id_array   <- transects[[grouping_id]]
  
  # extension distance vector 
  extension_distances  <- transects$extension_distance
  
  # preallocate vectors for storing if transect was extended and from which directions
  is_extended_flag <- rep(FALSE, length(transect_crosswalk_id_array))
  
  # number of geometries that will be iterated over, keeping this variable to reference in message block  
  total <- length(transect_crosswalk_id_array)
  
  make_progress <- make_progress_bar(TRUE, length(transect_crosswalk_id_array))
  
  for (i in seq_along(transect_crosswalk_id_array)) {
    
    make_progress()

    # get the current transect, hy_id, cs_id, flowline, and extension distances
    current_trans <- transects_geos[i]
    
    current_hy_id  <- transect_crosswalk_id_array[i]
    current_cs_id  <- transect_cs_id_array[i]
    current_uid    <- transect_uid_array[i]
    
    # distance to try extending
    distance_to_extend <- extension_distances[i]
    
    # skip the iteration if no extension required
    no_extension_required <- distance_to_extend == 0 || is.na(distance_to_extend) || is.null(distance_to_extend)
    
    # Skip the iteration if NO extension distance is prescribed 
    if (no_extension_required) {
      next
    }
    
    # extend the transects by the prescribed distances
    extended_trans <- hydrofabric3D::geos_extend_line(
      current_trans,
      distance_to_extend,
      dir = "both"
    )
    
    # check whether the extension is valid and should be used 
    # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
    use_extension <- is_valid_transect_line2(
      extended_trans,
      transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
      flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
    )
    
    used_half_of_extension <- FALSE
    
    # TODO: Probably should precompute this division BEFORE the loop...
    half_distance   <- ifelse(distance_to_extend > 0, distance_to_extend %/% 2, 0)
    
    if (!use_extension) {
      
      extended_trans  <- hydrofabric3D::geos_extend_line(
        current_trans, 
        half_distance, 
        "both"
      )
      
      # Check that the half extension distance transect is valid
      # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
      use_extension          <- is_valid_transect_line2(
        extended_trans,
        transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      ) 
    
      # TODO: this is dumb, TRUE if TRUE ELSE FALSE 
      used_half_of_extension <- ifelse(use_extension, TRUE,  FALSE)
      
    } 
    
    # set is extended flag
    is_extended_flag[i]     <- use_extension
    
    # get the extension distance (might have been cut in half), if no extension happened, then distance is 0
    updated_distance        <- ifelse(use_extension, 
                                      ifelse(used_half_of_extension, half_distance, distance_to_extend),
                                      0
                                 )
    
    extension_distances[i]  <- updated_distance 
    
    # update the transect geos to the extended transect (if use_extension is TRUE)
    if(use_extension) {
      # TODO: Review this, i think this is right but not sure
      # last step is to replace the original transect with the updated transect (extended)
      transects_geos[i] <- extended_trans
    }
    
  }      
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects)   <- sf::st_geometry(sf::st_as_sf(transects_geos))
  
  # replace the distance values so that any transects that were extended HALFWAY will be accounted for
  transects$left_distance      <- extension_distances
  transects$right_distance     <- extension_distances
  
  # Flags indicating if extensions happened or not (probably can just be dropped)
  transects$left_is_extended   <- is_extended_flag
  transects$right_is_extended  <- is_extended_flag
  
  transects <- hydroloom::rename_geometry(transects, "geometry")
  
  transects <-
    transects %>% 
    dplyr::mutate(
      cs_lengthm = as.numeric(sf::st_length(.))
    ) %>% 
    dplyr::relocate(
      dplyr::any_of(c(crosswalk_id, cs_id)),
      cs_lengthm
    )
  
  transects <- move_geometry_to_last(transects)
  
  return(transects)
  
}

#' Given a set of transect lines, a flowline network, extend the transect lines out given distances from the left and right
#' Flowlines are required to ensure valid transect intersection relationship is maintained
#'
#' @param transects sf dataframe of linestrings, requires crosswalk_id, cs_id, grouping_id columns and numeric 'left_distance' and 'right_distance' columns
#' @param flowlines sf dataframe of linestrings
#' @param crosswalk_id character, column name that connects features in transects to flowlines
#' @param cs_id character, column name that uniquely identifies transects within a flowline
#' @param grouping_id character, column name in both transects and flowlines that denotes which flowlines are grouped with which transects.
#' @importFrom utils str
#' @importFrom geos as_geos_geometry
#' @importFrom wk wk_crs 
#' @importFrom sf st_geometry st_as_sf st_length
#' @importFrom nhdplusTools rename_geometry 
#' @importFrom dplyr mutate relocate any_of
#' @return transects sf dataframe with extended transect geometries, left and right distance columns, and flags indicating if the transect was extended in the left and/or right directions
#' @noRd
#' @keywords internal
extend_transects_by_distances <- function(
    transects,
    flowlines,
    crosswalk_id,
    cs_id       = "cs_id",
    grouping_id = "mainstem"
) {
  
  # ----------------------------------------------------------------------------------
  # ----------- Input checking ------
  # ----------------------------------------------------------------------------------
  if(!crosswalk_id %in% names(flowlines)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'flowlines' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks 'flowlines' to 'transects'")
  }
  
  if(!crosswalk_id %in% names(transects)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'transects' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks the 'transects' to 'flowlines'")
  }
  
  if(!cs_id %in% names(transects)) {
    stop("cs_id '", cs_id, "' is not a column in 'transects' input,\n", 
         "Please provide a valid 'cs_id' column name from 'transects'.\n",
         "The 'cs_id' should uniquely identify each transect lines within a flowline.\n", 
         "(ID for each transect within the crosswalk_id)")
  }
  
  if(!grouping_id %in% names(flowlines)) {
    stop("grouping_id '", grouping_id, "' is not a column in 'flowlines' input,\n", 
         "Please provide a valid 'grouping_id' that associates each transect line with 1 or more flowlines in 'flowlines'"
    )
  }
  
  if(!grouping_id %in% names(transects)) {
    stop("grouping_id '", grouping_id, "' is not a column in 'transect_lines' input,\n", 
         "Please provide a valid 'grouping_id' that associates each transect line with 1 or more flowlines in 'flowlines'"
    )
  }
  
  if(!'left_distance' %in% names(transects)) {
    stop("transect_lines is missing a numeric 'left_distance' column.\n", 
         "A numeric 'left_distance' column must be present to indicate the distance to extend each transect in the left direction."
    )
  }
  
  if(!'right_distance' %in% names(transects)) {
    stop("transect_lines is missing a numeric 'right_distance' column.\n", 
         "A numeric 'right_distance' column must be present to indicate the distance to extend each transect in the right direction."
    )
  }
  
  fline_id_array   <- flowlines[[crosswalk_id]]
  
  # TODO: next time, change this function to ONLY process transects that have ANY extension distance, right now we iterate through ALL transects,
  # TODO: and 'next' the ones with the no extension distance so doesn't really matter much but 
  
  # Convert the net object into a geos_geometry
  flowlines_geos       <- geos::as_geos_geometry(flowlines)
  transects_geos       <- geos::as_geos_geometry(transects)
  
  # stash the CRS of the transects to use when making the new extended transect lines
  line_crs             <- wk::wk_crs(transects)
  
  transect_crosswalk_id_array   <- transects[[crosswalk_id]]
  transect_cs_id_array          <- transects[[cs_id]]
  transect_uid_array            <- paste0(transect_crosswalk_id_array, "_", transect_cs_id_array) 
  
  # Intersect grouping IDs
  fline_group_id_array      <- flowlines[[grouping_id]]
  transect_group_id_array   <- transects[[grouping_id]]
  
  # distance vectors 
  left_distances       <- transects$left_distance
  right_distances      <- transects$right_distance
  
  # preallocate vectors for storing if transect was extended and from which directions
  left_extended_flag   <- rep(FALSE, length(transect_crosswalk_id_array))   
  right_extended_flag  <- rep(FALSE, length(transect_crosswalk_id_array))
  
  # number of geometries that will be iterated over, keeping this variable to reference in message block  
  total <- length(transect_crosswalk_id_array)
  
  make_progress <- make_progress_bar(TRUE, length(transect_crosswalk_id_array))
 
  for (i in seq_along(transect_crosswalk_id_array)) {

    make_progress()
    
    # get the current transect, hy_id, cs_id, flowline, and extension distances
    current_trans <- transects_geos[i]
    
    current_hy_id <- transect_crosswalk_id_array[i]
    current_cs_id <- transect_cs_id_array[i]
    current_uid   <- transect_uid_array[i]

    # TODO: set any NA, NULL, or negative distance values to 0
    # TODO: outside of loop
    # distances to try extending
    left_distance_to_extend  <- left_distances[i]
    right_distance_to_extend <- right_distances[i]
    
    # skip the iteration if no extension required (left AND right distance are both 0 or NA)
    no_extension_required <- (
                            left_distance_to_extend == 0 || is.na(left_distance_to_extend)
                            ) && (
                            right_distance_to_extend == 0 || is.na(right_distance_to_extend)
                            ) 
    
    # Skip the iteration if NO extension distance in either direction
    if(no_extension_required) {
      next
    }
    
    # extend the transects by the prescribed distances
    left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            left_distance_to_extend, "head")
    
    right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            right_distance_to_extend, "tail")
    
    # TODO version 2:
    # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
    use_left_extension  <- is_valid_transect_line2(
      left_extended_trans,
      transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
      flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
    ) 
    
    use_right_extension <- is_valid_transect_line2(
      right_extended_trans, 
      transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
      flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
    )
    
    used_half_of_left  <- FALSE
    used_half_of_right <- FALSE
    
    # TODO: Probably should precompute this division BEFORE the loop...
    half_left_distance   <- ifelse(left_distance_to_extend > 0, left_distance_to_extend %/% 2, 0)
    half_right_distance  <- ifelse(right_distance_to_extend > 0, right_distance_to_extend %/% 2, 0)
    
    # if we CAN'T use the original LEFT extension distance, 
    # we try HALF the distance (or some distane less than we extended by in the first place)
    if (!use_left_extension) {
      
      # half_left_distance   <- ifelse(left_distance_to_extend > 0, left_distance_to_extend %/% 2, 0)
      left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
                                                              half_left_distance, "head")
      # TODO version 2:
      # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
      use_left_extension  <- is_valid_transect_line2(
        left_extended_trans,
        transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      ) 
      
      # this is dumb, true if true else false
      # this flag is just helpful for reading, whether the half distance was used or not further down in the loop
      used_half_of_left <- ifelse(use_left_extension, TRUE,  FALSE)
    }
    
    # if we CAN'T use the original RIGHT extension distance, 
    # we try HALF the distance (or some distance less than we extended by in the first place)
    if (!use_right_extension) {
      
      # half_right_distance  <- ifelse(right_distance_to_extend > 0, right_distance_to_extend %/% 2, 0)
      right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
                                                              half_right_distance, "tail")
      
      use_right_extension <- is_valid_transect_line2(
        right_extended_trans, 
        transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      )
      
      # this is dumb, true if true else false
      # this flag is just helpful for reading, whether the half distance was used or not further down in the loop
      used_half_of_right  <- ifelse(use_right_extension, TRUE,  FALSE)
    }
    
    # get the start and end point of the new line
    extension_pts <- pick_extension_pts(
      left_extended_trans,
      right_extended_trans,
      use_left_extension,
      use_right_extension
    )
    
    # single geos_geometry points
    start <- extension_pts[1]
    end   <- extension_pts[2]
    
    # create the new transect line
    updated_trans  <- make_line_from_start_and_end_pts(start, end, line_crs)
    
    # flag if left extension happened AND it was greater than 0 
    left_extended_flag[i]   <-  use_left_extension && left_distance_to_extend > 0
    
    # flag if right extension happened AND it was greater than 0 
    right_extended_flag[i]  <-  use_right_extension && right_distance_to_extend > 0
    
    # get the updated extension distances (left and right, they might have been cut in half), if no extension happened, then distance is 0
    updated_left_distance   <- ifelse(use_left_extension, 
                                  ifelse(used_half_of_left, half_left_distance, left_distance_to_extend),
                                  0
                                  )
    
    updated_right_distance  <- ifelse(use_right_extension, 
                                  ifelse(used_half_of_right, half_right_distance, right_distance_to_extend),
                                  0
                                  )
    
    # update the left extension distance 
    left_distances[i]   <- updated_left_distance 
   
    # update the right extension distance 
    right_distances[i]  <- updated_right_distance
     
    # TODO: review this, didnt have this check before so theoretically, an iteration could happen where neither left or right extensions were used, but we still
    # TODO: set the transect to the updated transect
    if (use_left_extension || use_right_extension) {
      # last step is to replace the original transect with the updated transect (extended)
      transects_geos[i] <- updated_trans
    }

  }      
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects) <- sf::st_geometry(sf::st_as_sf(transects_geos))
  
  # replace the distance values so that any transects that were extended HALFWAY will be accounted for
  transects$left_distance      <- left_distances 
  transects$right_distance     <- right_distances
  
  # Flags indicating if extensions happened or not (probably can just be dropped)
  transects$left_is_extended   <- left_extended_flag
  transects$right_is_extended  <- right_extended_flag
  
  transects <- nhdplusTools::rename_geometry(transects, "geometry")
  
  transects <-
    transects %>% 
    dplyr::mutate(
      cs_lengthm = as.numeric(sf::st_length(.))
    ) %>% 
    dplyr::relocate(
      dplyr::any_of(c(crosswalk_id, cs_id)),
      cs_lengthm
    )
  
  transects <- move_geometry_to_last(transects)
  
  return(transects)
  
}

#' Extend transects for any transects with invalid cross section attributes
#'
#' @param transects sf dataframe of transect LINESTRING geometries
#' @param flowlines sf dataframe of flowline LINESTRING geometries
#' @param crosswalk_id character
#' @param scale numeric percent of original transect length to extend (in both directions). Default is 0.5 or 50% of transects length (i.e. 25% increase in length in both directions).
#' @param keep_lengths logical whether to keep a record of the original transect lengths or not, default is FALSE, original lengths are not kept 
#' @param keep_extension_distances logical whether to return the extension distance (left_distance and right_distance) columns with the output dataframe. Default is FALSE, left_distance and right_distance are NOT returned with the output.
#' @param reindex_cs_ids logical, whether to reindex the cs_ids to ensure each crosswalk_id has cs_ids of 1-number of transects. Default is FALSE, which guarantees 
#' crosswalk_id/cs_ids remain untouched as they were given in the input data. Setting this to TRUE will make sure if any cross sections were removed from a crosswalk_id, then the cs_ids are renumbered so there are no gaps between cs_ids within a crosswalk_id 
#' @param verbose logical
#'
#' @return dataframe or sf dataframe of extended transects
#' @importFrom geos as_geos_geometry geos_intersection geos_type geos_intersects
#' @importFrom sf st_geometry st_as_sf st_intersects
#' @importFrom dplyr filter bind_rows mutate case_when ungroup across any_of group_by select
#' @importFrom hydroloom rename_geometry 
#' @export
extend_transects_by_cs_attributes = function(
    transects      = NULL,
    flowlines      = NULL,
    crosswalk_id   = NULL,
    scale          = 0.5,
    keep_lengths   = FALSE,
    keep_extension_distances = FALSE,
    reindex_cs_ids = FALSE,
    verbose        = TRUE
) {
  
  # ----------------------------------------------------------------------------------
  # ----------- Input checking ------
  # ----------------------------------------------------------------------------------
  
  # stash the original starting transect line lengths
  starting_lengths <- 
    transects %>%  
    add_length_col(length_col = "initial_length") %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, initial_length)
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  # set geometry column names at beginning
  flowlines    <- hydroloom::rename_geometry(flowlines, "geometry")
  transects    <- hydroloom::rename_geometry(transects, "geometry")
  
  # validate input datas
  is_flowlines_valid        <- validate_df(flowlines, 
                                           c(crosswalk_id, "geometry"), 
                                           "flowlines")
  
  is_transects_valid  <- validate_df(transects, 
                                     c(crosswalk_id, "cs_id", "cs_lengthm", "valid_banks", "has_relief", "geometry"),
                                     "transects")
  
  # Create an "is_extended" flag to identify which transects were extended and updated
  transects$is_extended <- FALSE
  
  # keep track of any transects that having missing values in either valid_banks/has_relief columns,
  # these get added back to the updated data at the end
  missing_bank_or_relief_data <-
    transects %>%
    dplyr::filter(is.na(valid_banks) | is.na(has_relief))
  
  # TODO: Probably remove this
  count_check <- nrow(dplyr::filter(transects, valid_banks & has_relief)) +
    nrow(dplyr::filter(transects, !valid_banks | !has_relief)) ==
    nrow(transects) - nrow(missing_bank_or_relief_data)
  
  if(!count_check) {
    warning(paste0(nrow(missing_bank_or_relief_data), " transects have NA values in either 'valid_banks' or 'has_relief' columns"))
  }
  
  # add distances to extend for the left and right side of a transect
  # for any of the the already "valid transects", we just set an extension distance of 0
  # on both sides and these transects will be KEPT AS IS
  # also set any missing valid_banks or has_relief values to 0
  transects <- add_attribute_based_extension_distances(transects = transects,
                                                       scale = scale,
                                                       length_col = "cs_lengthm"
  )
  
  if(verbose) { 
    
    # count of transects to improve
    invalid_count <-
      transects %>% 
      sf::st_drop_geometry() %>% 
      dplyr::select(valid_banks, has_relief) %>% 
      dplyr::summarise(
        count = sum(!valid_banks | !has_relief, na.rm = T)
      ) %>%
      dplyr::pull(count)
    
    message(paste0("Extending ", 
                               invalid_count,
                               " transects without valid banks or relief by ",
                               scale * 100, "%...")) 
    
    }
  
  # extend the transects based on the 'extension_distance' column (meters)
  extended_transects <- extend_transects_sides(
    transects    = transects,
    flowlines    = flowlines,
    crosswalk_id = crosswalk_id,
    cs_id        = "cs_id",
    grouping_id  = crosswalk_id,
    direction    = "both"
  )

  # Set the is_extended flag based on if either the left OR the right side were extended
  extended_transects <-
    extended_transects %>%
    hydroloom::rename_geometry("geometry") %>%
    dplyr::mutate(
      is_extended = dplyr::case_when(
        left_is_extended | right_is_extended ~ TRUE,
        TRUE                                 ~ FALSE
      )
    ) %>%
    dplyr::select(
      -left_is_extended,
      -right_is_extended
    ) 
  
  # shorten any transects that intersect multiple transects back to their original lengths
  extended_transects  <- shorten_multi_transect_intersecting_extended_transects(x = extended_transects, 
                                                              crosswalk_id = crosswalk_id)
  
  # shorten any transects that intersect multiple flowlines (or a flowline more than once) back to their original lengths
  extended_transects  <- shorten_multi_flowline_intersecting_extended_transects(x = extended_transects, 
                                                                       flowlines = flowlines,
                                                                       crosswalk_id = crosswalk_id)
  
  # remove self intersections and flowline multi intersections
  extended_transects <-
    extended_transects %>% 
    rm_multi_intersects() %>% 
    rm_self_intersections() %>%
    rm_multiflowline_intersections(flowlines)
  
  # check to make sure all unique hy_id/cs_id in the INPUT are in the OUTPUT,
  # and raise an error if they're are missing hy_id/cs_ids
  input_uids    <- get_unique_tmp_ids(transects, x = crosswalk_id)
  output_uids   <- get_unique_tmp_ids(extended_transects, x = crosswalk_id)
  
  # check all of the output_uids exist in the input UIDs
  has_all_uids           <- all(output_uids %in% input_uids)
  
  # throw an error if NOT all crosswalk_id/cs_ids are the same in the input and output data
  if (!has_all_uids) {
    stop("Missing unique crosswalk_id/cs_id UIDs from input transects in the output transects")
  }
  
  if (keep_lengths) {
    extended_transects <- 
      extended_transects %>% 
      dplyr::left_join(
        starting_lengths,
        by = c(crosswalk_id, "cs_id")
      ) 
  }
  
  # re-index the cs_ids to make sure there are 1-number of transects for each crosswalk_id and that there are NO gaps between cs_ids
  if (reindex_cs_ids) {
    warning("Re-indexing cs_ids may result in a mismatch between unique crosswalk_id/cs_ids in input 'transects' and the output unique crosswalk_id/cs_ids")
    extended_transects <- renumber_cs_ids(extended_transects, crosswalk_id = crosswalk_id)
  }
  
  # ------------------------------------------------------------
  # ---- Drop specific columns based on flags ----
  # - keep_lengths (initial_length)
  # - keep_extension_distances (left_distance and right_distance)
  # ------------------------------------------------------------
  
  if (!keep_lengths) {
    # remove initial_length columns
    extended_transects <-
      extended_transects %>% 
      dplyr::select(
        !dplyr::any_of(c("initial_length"))
      )
  }
  
  if (!keep_extension_distances) {
    # remove left_distance and right_distance columns
    extended_transects <-
      extended_transects %>% 
      dplyr::select(
        !dplyr::any_of(c("left_distance", "right_distance"))
      )
  }
  
  # ----------------------------------
  # ---- Final column select ----
  # ----------------------------------
  
  # select only desired columns and remove tmp_id column
  extended_transects <-
    extended_transects %>% 
    dplyr::select(
      dplyr::any_of(
        c(crosswalk_id, 
          "cs_id", 
          "cs_lengthm",
          "cs_measure", 
          "ds_distance",
          "sinuosity", 
          
          # CS attributes
          "valid_banks", "has_relief", 
          
          "initial_length",                    # if keep_lengths = TRUE
          "left_distance", "right_distance",   # if keep_extension_distance = TRUE
          
          "cs_source",
          "geometry"
          )
        )
    )
  
  
  return(extended_transects)
  
}


# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------
# TODO: FUNCTIONS TO DELETE SOON (BELOW, after testing) 
# TODO: FUNCTIONS TO DELETE SOON (BELOW, after testing) 
# TODO: FUNCTIONS TO DELETE SOON (BELOW, after testing) 
# ----------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------

#' @title Extend a set of transects by a percentage
#'
#' @param transects_to_extend  sf linestrings, set of all transects in the network. Requires the following columns: "hy_id", "cs_id", "cs_lengthm" (length of geometry in meters),  
#' @param length_vector numeric, vector of length 'x' representing the number of meters to extend 'x' from both directions (i.e. 10 means the linestring will be extended 10m from both ends of the line)
#' @param net sf linestrings, flowline network that transects were generated from, requires "id" column (where "id" equals the "hy_id" columns in 'transects_to_extend' and 'transects' )
#' @param verbose logical, whether to print messages or not. Default is TRUE
#' @return sf linestring dataframe containing the updates transects_to_extend (with a flag denoting if the geometry was extended by "scale" % or not)
#' @importFrom geos as_geos_geometry geos_intersection geos_type geos_intersects
#' @importFrom sf st_geometry st_as_sf st_length
#' @noRd
#' @keywords internal
extend_transects_by_length <- function(
    transects_to_extend,
    length_vector,
    net, 
    verbose = TRUE
) {
  
  # Create an "is_extended" flag to identify which transects were extended and updated 
  transects_to_extend$is_extended <- FALSE
  
  if(verbose) { message(paste0("Extending ", nrow(transects_to_extend), " transects...")) }
  
  # Extend the transects by a scale % value
  extended_trans <- extend_by_length(transects_to_extend, length_vector, "cs_lengthm")
  
  # Store the identifying information to use in for loop to subset data using IDs
  fline_id_array <- net$id
  hy_id_array    <- extended_trans$hy_id
  cs_id_array    <- extended_trans$cs_id
  
  # Convert extended transects to geos
  extended_trans  <- geos::as_geos_geometry(extended_trans)
  
  # Convert the net object into a geos_geometry
  geos_net <- geos::as_geos_geometry(net)
  
  # Convert the original transect lines to geos_geometries and when 
  # a valid extension comes up in the below for loop, replace the old geometry with the newly extended one
  geos_list     <- geos::as_geos_geometry(transects_to_extend$geom)
  
  # Preallocate vectors to store the "is_extended" flag and the new lengths after extensions:
  # - if an extension is VALID (checked in the loop below), then 
  #   set the "is_extended" flag to TRUE and update the cross section length 
  #   to use the new extended length
  extended_flag <- rep(FALSE, length(extended_trans))
  
  # number of geometries that will be iterated over, keeping this variable to reference in message block  
  total <- length(extended_trans)
  
  # output a message every ~10% intervals
  message_interval <- total %/% 10
  
  # loop through geometries that might need to be extended, try to extend, and then update 
  # the 'to_extend' values IF the extended transectr does NOT violate any intersection rules
  for (i in 1:length(extended_trans)) {
    
    # Check if the iteration is a multiple of 100
    if (i %% message_interval == 0) {
      
      # get the percent complete
      percent_done <- round(i/total, 2) * 100
      
      # Print the message every "message_interval"
      if(verbose) { message(" > ", percent_done, "% ") }
    }
    
    # Get the current transect, hy_id, cs_id
    current_trans <- extended_trans[i]
    current_hy_id <- hy_id_array[i]
    current_cs_id <- cs_id_array[i]
    
    # use the hy_id from the current transect line to index the 
    # full network of flowlines to get the specific flowline for this transect (geos_geometry)
    current_fline <- geos_net[fline_id_array == current_hy_id]
    
    # # filter down to the rest of the transects on the given "hy_id", EXCLUDING SELF
    # neighbor_transects <- geos::as_geos_geometry(dplyr::filter(transects, 
    # hy_id == current_hy_id,  cs_id != current_cs_id))
    
    # Get all of the other transects on this flowline using "hy_id" and "cs_id" (EXCLUDING SELF)
    neighbor_transects <- geos::as_geos_geometry(
      transects[transects$hy_id == current_hy_id & transects$cs_id != current_cs_id, ]
    )
    
    # Make sure that newly extended transect line only intersects its origin flowline at MOST 1 time
    # AND that the newly extended transect does NOT intersect with any previously computed transect lines
    fline_intersect <- geos::geos_intersection(
      current_trans,     
      current_fline
    )
    
    # If all of these conditions are TRUE then the currently extended transect will get inserted into "to_extend"
    # - Newly extended transect intersects with its flowlines AT MOST 1 time
    # - Newly extended transect does NOT intersect with any of the other NEWLY EXTENDED transect lines
    # - Newly extended transect does NOT intersect with any of the ORIGINAL transect lines
    if (
      # Check that newly extended cross section only intersects its origin flowline at MOST 1 time 
      # (This value will be a "MULTIPOINT" if it intersects more than once and will evaluate to FALSE)
      geos::geos_type(fline_intersect) == "point" &&
      # Check that extended transect doesn't intersect with any of the NEWLY EXTENDED cross sections
      !any(geos::geos_intersects(current_trans, extended_trans[-i])) &&
      # Check that extended transect doesn't intersect with any of the original cross sections on this "hy_id"
      !any(geos::geos_intersects(current_trans, neighbor_transects))
    ) {
      
      # Update the transect geometry with the newly extended transect
      geos_list[i] <- current_trans
      
      # Set the extended flag to TRUE for this transect
      extended_flag[i] <- TRUE
    } 
  }
  
  if(verbose) { message(paste0("Complete!")) }
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects_to_extend) <- sf::st_geometry(sf::st_as_sf(geos_list))
  
  transects_to_extend$is_extended  <- extended_flag
  transects_to_extend$cs_lengthm   <- as.numeric(sf::st_length(transects_to_extend))
  
  return(transects_to_extend)
}
