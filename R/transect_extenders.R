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
    "crosswalk_id", "extend_invalid_transects2"
  )
)



#' @title Extend a set of transects by a percentage based on banks and relief
#' Given a set of transect lines with valid_banks and has_relief columns (derived from DEM extracted cross section points), extend any transects 
#' by a percentage of the transects length if the transect does NOT have valid banks (valid_banks == FALSE) OR it does NOT have relief (has_relief == FALSE).
#' @param transects_to_check sf linestrings, set of all transects in the network. Requires the following columns: "hy_id", "cs_id", "cs_lengthm" (length of geometry in meters), "valid_banks", and "has_relief"
#' @param net sf linestrings, flowline network that transects were generated from, requires "id" column (where "id" equals the "hy_id" columns in 'transects_to_check' and 'transects' )
#' @param scale numeric, percentage of current transect line length to extend transects in transects_to_extend by. Default is 0.5 (50% of the transect length)
#' @param verbose logical, whether to show a progress bar and progress messages or not. Default is TRUE.
#' @return sf linestring dataframe containing the the original transects with extensions performed on transects without valid_banks OR has_relief (a "is_extended" flag denotes if the geometry was extended by "scale" % or not)
#' @importFrom geos as_geos_geometry geos_intersection geos_type geos_intersects
#' @importFrom sf st_geometry st_as_sf
#' @importFrom dplyr filter bind_rows
#' @export
extend_invalid_transects <- function(
    transects_to_check, 
    net, 
    scale = 0.5,
    verbose = TRUE
) {
  # ----------------------------------------
  # ----------------------------------------
  
  # transects_to_check  = transects
  # net                 = net
  # scale               = scale
  # verbose             = verbose
  
  # ----------------------------------------
  # ----------------------------------------
  
  # Create an "is_extended" flag to identify which transects were extended and updated 
  transects_to_check$is_extended <- FALSE
  
  # split input transects into invalid and valid sets (valid == has valid banks AND has relief)
  invalid_transects  <- dplyr::filter(transects_to_check, !valid_banks | !has_relief)
  valid_transects    <- dplyr::filter(transects_to_check, valid_banks & has_relief)
  
  # keep track of any transects that having missing values in either valid_banks/has_relief columns, 
  # these get added back to the updated data at the end
  missing_bank_or_relief_data <- 
    transects_to_check %>% 
    dplyr::filter(is.na(valid_banks) | is.na(has_relief))
  
  # TODO: Probably remove this
  count_check <- nrow(valid_transects) + nrow(invalid_transects) == nrow(transects_to_check)
  # count_check <- nrow(valid_transects) + nrow(invalid_transects) == nrow(transects_to_check) - nrow(missing_bank_or_relief_data)
  
  if(!count_check) {
    warning(paste0(nrow(missing_bank_or_relief_data), " transects have NA values in either 'valid_banks' or 'has_relief' columns"))
    # warning(paste0("Different number of transects after splitting data by 'valid_banks' and 'has_relief' columns, ", nrow(missing_bank_or_relief_data), " transects have NA values in either 'valid_banks' or 'has_relief' columns"))
    # stop("Mismatch in number of points after splitting data by the 'valid_banks' and 'has_relief' columns, likely a missing value in either 'valid_banks' or 'has_relief' columns")
  }
  
  if(verbose) { message(paste0("Extending ", nrow(invalid_transects), " transects without valid banks or relief by ",     scale * 100, "%...")) }
  
  # Extend the transects by a scale % value
  extended_trans <- extend_by_percent(invalid_transects, scale, "cs_lengthm")
  
  # Store the identifying information to use in for loop to subset data using IDs
  fline_id_array <- net$id
  hy_id_array    <- extended_trans$hy_id
  cs_id_array    <- extended_trans$cs_id
  
  check_hy_id_array <- transects_to_check$hy_id
  check_cs_id_array <- transects_to_check$cs_id
  
  # Convert extended transects to geos
  extended_trans           <- geos::as_geos_geometry(extended_trans)
  transects_to_check_geos  <- geos::as_geos_geometry(transects_to_check)
  
  # Convert the net object into a geos_geometry
  net_geos        <- geos::as_geos_geometry(net)
  
  # if(verbose) { message(paste0("Iterating through extended geometries and checking validity...")) }
  
  # Convert the original transect lines to geos_geometries and when 
  # a valid extension comes up in the below for loop, replace the old geometry with the newly extended one
  geos_list       <- geos::as_geos_geometry(invalid_transects$geom)
  
  # Preallocate vectors to store the "is_extended" flag and the new lengths after extensions:
  # - if an extension is VALID (checked in the loop below), then 
  #   set the "is_extended" flag to TRUE and update the cross section length 
  #   to use the new extended length
  extended_flag <- rep(FALSE, length(extended_trans))
  length_list   <- invalid_transects$cs_lengthm
  
  make_progress <- make_progress_bar(verbose, length(extended_trans))
  
  # loop through geometries that might need to be extended, try to extend, and then update 
  # the 'to_extend' values IF the extended transectr does NOT violate any intersection rules
  for (i in 1:length(extended_trans)) {
    
    # Get the current transect, hy_id, cs_id
    # current_trans <- extended_trans[i]
    current_hy_id <- hy_id_array[i]
    current_cs_id <- cs_id_array[i]
    
    # # Make sure that newly extended transect line only intersects its origin flowline at MOST 1 time
    # # AND that the newly extended transect does NOT intersect with any previously computed transect lines
    # fline_intersect <- geos::geos_intersection(extended_trans[i], current_fline)
    
    # Check that the extended transect lines only intersect a single flowline in the network only ONCE
    intersects_with_flowlines <- geos::geos_intersection(
      extended_trans[i],     
      net_geos[fline_id_array == current_hy_id]
    )
    
    # Check that newly extended cross section only intersects its origin flowline at MOST 1 time 
    # (This value will be a "MULTIPOINT" if it intersects more than once and will evaluate to FALSE)
    intersects_flowline_only_once <- sum(geos::geos_type(intersects_with_flowlines) == "point") == 1 && 
      sum(geos::geos_type(intersects_with_flowlines) == "multipoint") == 0 
    
    if(!intersects_flowline_only_once) {
      # message(" -> Skipping iteration because extended transect intersects flowline more than once")
      next
    }

    # Check that extended transect doesn't intersect with any of the original cross sections on this "hy_id"
    is_intersecting_other_transects      <- any(geos::geos_intersects(
                                                extended_trans[i], 
                                                # AKA neighbor_transects
                                                transects_to_check_geos[check_hy_id_array == current_hy_id & check_cs_id_array != current_cs_id]
                                                ) 
                                               )
    
    if (is_intersecting_other_transects) {
      # message(" --> Skipping iteration because extended transect intersects another (UNEXTENDED) neighoring transect")
      next   
    }
    # Check that extended transect doesn't intersect with any of the NEWLY EXTENDED cross sections
    is_intersecting_other_extended_transects <- any(geos::geos_intersects(extended_trans[i], extended_trans[-i]))
    
    if (is_intersecting_other_extended_transects) {
      # message(" -----> Skipping iteration because extended transect intersects another (EXTENDED) neighoring transect")
      next   
    }
    
    # If all of these conditions are TRUE then the currently extended transect will get inserted into "to_extend"
    # - Newly extended transect intersects with its flowlines AT MOST 1 time
    # - Newly extended transect does NOT intersect with any of the other NEWLY EXTENDED transect lines
    # - Newly extended transect does NOT intersect with any of the ORIGINAL transect lines
    if (
      # Check that newly extended cross section only intersects its origin flowline at MOST 1 time 
      # (This value will be a "MULTIPOINT" if it intersects more than once and will evaluate to FALSE)
      intersects_flowline_only_once && 
      # geos::geos_type(fline_intersect) == "point" &&
      
      # Check that extended transect doesn't intersect with any of the NEWLY EXTENDED cross sections
      !is_intersecting_other_extended_transects && 
      # !any(geos::geos_intersects(extended_trans[i], extended_trans[-i])) &&
      
      # Check that extended transect doesn't intersect with any of the original cross sections on this "hy_id"
      !is_intersecting_other_transects
      # !any(geos::geos_intersects(extended_trans[i], neighbor_transects))
    ) {
      
      # # Calculate the updated cross section length to align with the newly extended cross section for this row
      updated_cs_length <- (length_list[i] * scale) + length_list[i]
      # updated_cs_length <- (current_length * scale) + current_length
      
      # copy the current cross section length
      length_list[i] <- updated_cs_length
      # length_list  <- vctrs::vec_c(length_list, updated_cs_length)
      
      # Update the transect geometry with the newly extended transect
      geos_list[i] <- extended_trans[i]
      # geos_list <- vctrs::vec_c(geos_list, extended_trans[i])

      # Set the extended flag to TRUE for this transect
      extended_flag[i] <- TRUE
      # extended_flag  <- vctrs::vec_c(extended_flag, TRUE)
      
    } 
    
    make_progress()
  }
  
  if(verbose) { message(paste0("Complete!")) }
  
  # Update the "invalid_transects" with new geos geometries ("geos_list")
  sf::st_geometry(invalid_transects) <- sf::st_geometry(sf::st_as_sf(geos_list))
  
  # update the "is_extended" flag and the cross section lengths to reflect any extensions
  invalid_transects$is_extended <- extended_flag
  invalid_transects$cs_lengthm  <- length_list
  
  # Combine the valid_transects with the UPDATED invalid_transects (updated by attempting extension) to get the final output dataset
  extended_transects <- dplyr::bind_rows(
    valid_transects,
    invalid_transects
  )
  
  # add back any transects that were missing banks/relief values 
  extended_transects <- dplyr::bind_rows(
    extended_transects,
    dplyr::select(missing_bank_or_relief_data, 
                  dplyr::any_of(names(extended_transects)))
  )
  
  # check to make sure all unique hy_id/cs_id in the INPUT are in the OUTPUT, 
  # and raise an error if they're are missing hy_id/cs_ids
  input_uids  <- unique(hydrofabric3D::add_tmp_id(transects_to_check)$tmp_id)
  output_uids <- unique(hydrofabric3D::add_tmp_id(extended_transects)$tmp_id)
  
  has_all_uids  <- all(output_uids %in% input_uids)
  
  # throw an error if NOT all hy_id/cs_ids are the same in the input and output data
  if(!has_all_uids) {
    stop("Missing unique hy_id/cs_id from input transects in the output transects")
  }
  
  return(extended_transects)
}

#  TODO: this IS BEST current version (extend_invalid_transects2) --> DELETE extend_invalid_transects() and replace with extend_invalid_transects2()

#' @title Extend a set of transects by a percentage based on banks and relief
#' Given a set of transect lines with valid_banks and has_relief columns (derived from DEM extracted cross section points), extend any transects 
#' by a percentage of the transects length if the transect does NOT have valid banks (valid_banks == FALSE) OR it does NOT have relief (has_relief == FALSE).
#' @param transects_to_check sf linestrings, set of all transects in the network. Requires the following columns: "hy_id", "cs_id", "cs_lengthm" (length of geometry in meters), "valid_banks", and "has_relief"
#' @param net sf linestrings, flowline network that transects were generated from, requires "id" column (where "id" equals the "hy_id" columns in 'transects_to_check' and 'transects' )
#' @param crosswalk_id character, column name that connects features in transects to net
#' @param scale numeric, percentage of current transect line length to extend transects in transects_to_extend by. Default is 0.5 (50% of the transect length)
#' @param verbose logical, whether to show a progress bar and progress messages or not. Default is TRUE.
#' @return sf linestring dataframe containing the the original transects with extensions performed on transects without valid_banks OR has_relief (a "is_extended" flag denotes if the geometry was extended by "scale" % or not)
#' @importFrom geos as_geos_geometry geos_intersection geos_type geos_intersects
#' @importFrom sf st_geometry st_as_sf
#' @importFrom dplyr filter bind_rows
#' @export
extend_invalid_transects2 <- function(
    transects_to_check, 
    net, 
    crosswalk_id,
    scale = 0.5,
    verbose = TRUE
) {
  # ----------------------------------------
  # ----------------------------------------
  
  # transects_to_check  = transects
  # net                 = net
  # crosswalk_id        = "hy_id"
  # scale               = scale
  # verbose             = verbose

  # ----------------------------------------
  # ----------------------------------------
  
  # ----------------------------------------------------------------------------------
  # ----------- Input checking ------
  # ----------------------------------------------------------------------------------
  if(!crosswalk_id %in% names(net)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'net' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks 'net' to 'transects_to_check'")
  }
  
  if(!crosswalk_id %in% names(transects_to_check)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'transects_to_check' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks the 'transects_to_check' to 'net'")
  }
  
  # set geometry coluimn name as beginning 
  transects_to_check <- nhdplusTools::rename_geometry(transects_to_check, "geometry") 
  
  # check for necessary columns
  req_cols    <- c(crosswalk_id, "cs_id", "cs_lengthm", "valid_banks", "has_relief", "geometry")
  start_cols  <- names(transects_to_check)
  
  if (!all(req_cols %in% start_cols)) {
    missing_cols <- req_cols[which(!req_cols %in% start_cols)]
    stop("'transects_to_check' is missing the following required columns: \n > ", 
         paste0(missing_cols, collapse = "\n > "))
  }
  
  # Create an "is_extended" flag to identify which transects were extended and updated 
  transects_to_check$is_extended <- FALSE
  
  # # split input transects into invalid and valid sets (valid == has valid banks AND has relief)
  # invalid_transects  <- dplyr::filter(transects_to_check, !valid_banks | !has_relief)
  # valid_transects    <- dplyr::filter(transects_to_check, valid_banks & has_relief)
  
  # keep track of any transects that having missing values in either valid_banks/has_relief columns, 
  # these get added back to the updated data at the end
  missing_bank_or_relief_data <- 
    transects_to_check %>% 
    dplyr::filter(is.na(valid_banks) | is.na(has_relief))
  
  # TODO: Probably remove this
  count_check <- nrow(dplyr::filter(transects_to_check, valid_banks & has_relief)) + 
                 nrow(dplyr::filter(transects_to_check, !valid_banks | !has_relief)) == 
                 nrow(transects_to_check) - nrow(missing_bank_or_relief_data)
  
  # count_check <- nrow(valid_transects) + nrow(invalid_transects) == nrow(transects_to_check)
  # count_check <- nrow(valid_transects) + nrow(invalid_transects) == nrow(transects_to_check) - nrow(missing_bank_or_relief_data)
  
  if(!count_check) {
    warning(paste0(nrow(missing_bank_or_relief_data), " transects have NA values in either 'valid_banks' or 'has_relief' columns"))
    # warning(paste0("Different number of transects after splitting data by 'valid_banks' and 'has_relief' columns, ", nrow(missing_bank_or_relief_data), " transects have NA values in either 'valid_banks' or 'has_relief' columns"))
    # stop("Mismatch in number of points after splitting data by the 'valid_banks' and 'has_relief' columns, likely a missing value in either 'valid_banks' or 'has_relief' columns")
  }
  
  if(verbose) { message(paste0("Extending ", nrow(transects_to_check), 
                               " transects without valid banks or relief by ",     
                               scale * 100, "%...")) }
  
  # add distances to extend for the left and right side of a transect
  # for any of the the already "valid transects", we just set an extension distance of 0 
  # on both sides and these transects will be KEPT AS IS
  transects_to_check <- 
    transects_to_check %>% 
    dplyr::mutate(
      left_distance = dplyr::case_when(
        !valid_banks | !has_relief ~ (((scale)*(cs_lengthm)) / 2),
        TRUE                       ~ 0
      ),
      right_distance = dplyr::case_when(
        !valid_banks | !has_relief ~ (((scale)*(cs_lengthm)) / 2),
        TRUE                       ~ 0
      )
    ) %>% 
    dplyr::relocate(cs_lengthm, left_distance, right_distance) 
    # dplyr::filter(left_distance == 0, right_distance == 0)
  

  # system.time({
    
    extended_transects <- extend_transects_by_distances(
      transects    = transects_to_check,
      flowlines    = net,
      crosswalk_id = crosswalk_id,
      cs_id        = "cs_id",
      grouping_id  = crosswalk_id
    )
  
  # })
  
  # Set the is_extended flag based on if either the left OR the right side were extended
  extended_transects <- 
    extended_transects %>% 
    nhdplusTools::rename_geometry("geometry") %>%
    dplyr::mutate(
      is_extended = dplyr::case_when(
        left_is_extended | right_is_extended ~ TRUE,
        TRUE                                 ~ FALSE
      )
    ) %>% 
    dplyr::select(
      -left_distance,
      -right_distance,
      -left_is_extended, 
      -right_is_extended
      )
  
  # add back any transects that were missing banks/relief values 
  extended_transects <- dplyr::bind_rows(
                          extended_transects,
                          dplyr::select(missing_bank_or_relief_data, 
                                        dplyr::any_of(names(extended_transects))
                          )
                        )
  
  # Try and fix any transects that cross multiple 
  is_multi_intersecting <- lengths(sf::st_intersects(extended_transects)) != 1

  # replace any extended geoms that have multiple intersections with the original UNEXTENDED version of those same transects
  sf::st_geometry(extended_transects[is_multi_intersecting, ]) <- sf::st_geometry(transects_to_check[is_multi_intersecting, ])
  
  # update the lengths and is_extended flag to align with the above replacement of geometries
  extended_transects[is_multi_intersecting, ]$cs_lengthm       <- transects_to_check[is_multi_intersecting, ]$cs_lengthm
  extended_transects[is_multi_intersecting, ]$is_extended      <- transects_to_check[is_multi_intersecting, ]$is_extended
  
  # TODO: 
  # TODO:  this won't work as expected currently, in case any transects were removed by the self intersection removal above
  # TODO: if any were removed, then "transects_to_check" is not guarenteed to have the same indices so the below logical\
  # TODO: won't work as desired
  is_multi_intersecting_flowlines <- lengths(sf::st_intersects(extended_transects, net)) != 1
  
  # replace any extended geoms that have multiple intersections with any flowlines (replacing with the original set of transects)
  sf::st_geometry(extended_transects[is_multi_intersecting_flowlines, ]) <- sf::st_geometry(transects_to_check[is_multi_intersecting_flowlines, ])
  
  # update the lengths and is_extended flag to align with the above replacement of geometries
  extended_transects[is_multi_intersecting_flowlines, ]$cs_lengthm       <- transects_to_check[is_multi_intersecting_flowlines, ]$cs_lengthm
  extended_transects[is_multi_intersecting_flowlines, ]$is_extended      <- transects_to_check[is_multi_intersecting_flowlines, ]$is_extended
  
  # remove transects that intersect with OTHER TRANSECTS
  extended_transects <- 
    extended_transects[lengths(sf::st_intersects(extended_transects)) == 1, ] %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
    # dplyr::group_by(hy_id) 
    # dplyr::mutate(cs_id = 1:dplyr::n()) %>%
    dplyr::ungroup()

  # remove transects that intersect multiple flowlines
  extended_transects <- 
    extended_transects[lengths(sf::st_intersects(extended_transects, net)) == 1, ] %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
    # dplyr::mutate(cs_id = 1:dplyr::n()) %>%
    dplyr::ungroup()
  
  # check to make sure all unique hy_id/cs_id in the INPUT are in the OUTPUT, 
  # and raise an error if they're are missing hy_id/cs_ids
  input_uids    <- unique(hydrofabric3D::add_tmp_id(transects_to_check)$tmp_id)
  output_uids   <- unique(hydrofabric3D::add_tmp_id(extended_transects)$tmp_id)
  
  # missing_inputs <- 
  #   transects_to_check %>% 
  #   dplyr::filter(tmp_id %in% input_uids[!input_uids %in% output_uids])
  # missing_outputs <- 
  #   extended_transects2 %>% 
  #   dplyr::filter(tmp_id %in% output_uids[!input_uids %in% output_uids])
  # mapview::mapview(missing_inputs, color = "red") + 
  #   mapview::mapview(missing_outputs, color = "green")
  
  has_all_uids  <- all(output_uids %in% input_uids)
  
  # throw an error if NOT all hy_id/cs_ids are the same in the input and output data
  if(!has_all_uids) {
    stop("Missing unique hy_id/cs_id from input transects in the output transects")
  }
  
  return(extended_transects)
}