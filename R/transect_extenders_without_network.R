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
    "new_cs_lengthm", "polygon_index",
    "crosswalk_id", "extend_invalid_transects2",
    "anchors", "deriv_type", "edge", "extension_distance", 
    "left_is_extended", "right_is_extended", "to_node", "verbose", 
    "toindid", "indid", "toid", "is", "internal_is_braided2"
  )
)


#' @title Extend an sf linestring dataframe by a percent of the lines length
#'
#' @param x linestring sf dataframe
#' @param crosswalk_id character, unique ID column name 
#' @param pct numeric, percent of line to extend linestring by in both directions
#' @param length_col character, name of the column in "x" that has the length of the linestring (meters)
#' @importFrom dplyr group_by mutate ungroup rename across any_of sym
#' @importFrom sf st_length st_geometry st_drop_geometry st_as_sf st_crs
#' @importFrom hydroloom rename_geometry
#' @return sf dataframe with extended linestring geometries
extend_by_percent <- function(
    x, 
    crosswalk_id = NULL,
    pct        = 0.5, 
    length_col = NULL
) {
 # ---------------------------
 
  # x <- transects
  # crosswalk_id = "hy_id"
  # pct = 0.5
  # length_col = "cs_lengthm"
  # ---------------------------
  
  # rename the geometry to "geom"
  x <- hydroloom::rename_geometry(x, "geometry")
  # x <- nhdplusTools::rename_geometry(x, "geom")
  
  # length_col is NULL then set it to "cs_lengthm"
  if(is.null(length_col)) {
    length_col = "cs_lengthm"
  }
  
  #  if the length_col string is not a column in the x,
  # then create a column based on the length of the linestring using "length_col" as name of column 
  if (!length_col %in% names(x)) {
    
    # add a "length_col" column of the length of each linestring in meters
    x[length_col] <- as.numeric(sf::st_length(sf::st_geometry(x)))
    # x <- dplyr::mutate(x,  length_col = as.numeric(sf::st_length(.)))
  }
  
  # extend linestrings by pct * length of line
  extended_df <-
    x %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      extended_geom = geos_extend_line(
        geometry, 
        distance = (
          ((pct)*(!!dplyr::sym(length_col))) / 2
          # ((pct)*(.data[[length_col]])) / 2
        ),
        dir      = "both"
      ) 
    ) %>% 
    dplyr::ungroup()
  
  # drop original geometry column
  extended_df <-  sf::st_drop_geometry(extended_df)
  
  # set the extended geometry as the new geometry
  extended_df$extended_geom <- sf::st_geometry(sf::st_as_sf(extended_df$extended_geom))
  
  # make extended_df an sf object
  extended_df <- sf::st_as_sf(
    extended_df, 
    crs = sf::st_crs(x)
  )
  
  # rename "extended_geom" col to "geom"
  extended_df <- hydroloom::rename_geometry(extended_df, "geometry")
  # extended_df <- dplyr::rename(extended_df, "geometry" = "extended_geom")
  
  # recalculate length of linestring and update length_col value
  extended_df[[length_col]] <- as.numeric(sf::st_length(extended_df$geometry))
  
  return(extended_df)
  
}

#' @title Extend an sf linestring dataframe by a specified lengths vector
#'
#' @param x linestring sf dataframe
#' @param crosswalk_id character, unique ID column name 
#' @param length_vector numeric, vector of length 'x' representing the number of meters to extend 'x' from both directions (i.e. 10 means the linestring will be extended 10m from both ends of the line)
#' @param length_col character, name of the column in "x" that has the length of the linestring (meters)
#' @importFrom dplyr group_by mutate ungroup rename
#' @importFrom sf st_length st_geometry st_drop_geometry st_as_sf st_crs
#' @importFrom hydroloom rename_geometry
#' @return sf dataframe with extended linestring geometries
extend_by_length <- function(
    x, 
    crosswalk_id = NULL,
    length_vector,
    length_col = NULL
) {
  
  # rename the geometry to "geom"
  x <- hydroloom::rename_geometry(x, "geometry")
  
  # length_col is NULL then set it to "cs_lengthm"
  if(is.null(length_col)) {
    length_col = "cs_lengthm"
  }
  
  #  if the length_col string is not a column in the x,
  # then create a column based on the length of the linestring using "length_col" as name of column 
  if (!length_col %in% names(x)) {
    
    # add a "length_col" column of the length of each linestring in meters
    x[length_col] <- as.numeric(sf::st_length(sf::st_geometry(x)))
    # x <- dplyr::mutate(x,  length_col = as.numeric(sf::st_length(.)))
  }
  
  # TODO: this needs a check to make sure a column with this name does NOT already exist
  # add length vector col to extended lines out by in next step
  x$length_vector_col <- length_vector
  
  # extend linestrings by pct * length of line
  extended_df <-
    x %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      extended_geom = geos_extend_line(
        geom, 
        distance = length_vector_col,
        # distance = (pct)*(!!dplyr::sym(length_col)),
        dir      = "both"
      ) 
    ) %>% 
    dplyr::ungroup()
  
  # drop original geometry column
  extended_df <-  sf::st_drop_geometry(extended_df)
  
  # set the extended geometry as the new geometry
  extended_df$extended_geom <- sf::st_geometry(sf::st_as_sf(extended_df$extended_geom))
  
  # make extended_df an sf object
  extended_df <- sf::st_as_sf(
    extended_df, 
    crs = sf::st_crs(x)
  )
  
  # rename the geometry to "geom"
  extended_df <- hydroloom::rename_geometry(extended_df, "geometry")
  
  # recalculate length of linestring and update length_col value
  extended_df[[length_col]] <- as.numeric(sf::st_length(extended_df$geometry))
  
  # drop the added length_vector_col
  extended_df <- dplyr::select(
    extended_df, 
    -length_vector_col
  )
  
  return(extended_df)
  
}

# TODO: Delete soon, deprecated

#' @title Extend a set of transects by a percentage
#'
#' @param transects_to_extend sf linestrings, set of transects that should be extended (subset of 'transects'). Requires the following columns:  "hy_id", "cs_id", "cs_lengthm" (length of geometry in meters) 
#' @param transects sf linestrings, set of all transects in the network. Requires the following columns: "hy_id", "cs_id", "cs_lengthm" (length of geometry in meters)
#' @param net sf linestrings, flowline network that transects were generated from, requires "id" column (where "id" equals the "hy_id" columns in 'transects_to_extend' and 'transects' )
#' @param crosswalk_id character, unique ID column name 
#' @param scale numeric, percentage of current transect line length to extend transects in transects_to_extend by. Default is 0.5 (50% of the transect length)
#' @param verbose logical, whether to print messages or not. Default is TRUE
#' @return sf linestring dataframe containing the updates transects_to_extend (with a flag denoting if the geometry was extended by "scale" % or not)
#' @importFrom geos as_geos_geometry geos_intersection geos_type geos_intersects
#' @importFrom hydroloom rename_geometry
#' @importFrom sf st_geometry st_as_sf
#' @export
extend_transects <- function(
    transects_to_extend, 
    transects, 
    net, 
    crosswalk_id = NULL,
    scale = 0.5,
    verbose = TRUE
) {
  
  # rename the geometry to "geom"
  transects_to_extend  <- hydroloom::rename_geometry(transects_to_extend, "geometry")
  transects            <- hydroloom::rename_geometry(transects, "geometry")
  net                  <- hydroloom::rename_geometry(net, "geometry")
  
  # Create an "is_extended" flag to identify which transects were extended and updated 
  transects_to_extend$is_extended <- FALSE
  
  if(verbose) { message(paste0("Extending ", nrow(transects_to_extend), " transects by ",     scale * 100, "%...")) }
  
  # Extend the transects by a scale % value
  extended_trans <- extend_by_percent(x = transects_to_extend, 
                                      crosswalk_id = crosswalk_id, 
                                      pct = scale, 
                                      length_col = "cs_lengthm")
  
  # Store the identifying information to use in for loop to subset data using IDs
  fline_id_array <- net[[crosswalk_id]]
  hy_id_array    <- extended_trans[[crosswalk_id]]
  cs_id_array    <- extended_trans[[crosswalk_id]]
  
  # Convert extended transects to geos
  extended_trans  <- geos::as_geos_geometry(extended_trans)
  
  # Convert the net object into a geos_geometry
  geos_net <- geos::as_geos_geometry(net)
  
  # if(verbose) { message(paste0("Iterating through extended geometries and checking validity...")) }
  
  # Convert the original transect lines to geos_geometries and when 
  # a valid extension comes up in the below for loop, replace the old geometry with the newly extended one
  geos_list     <- geos::as_geos_geometry(transects_to_extend$geometry)
  
  # Preallocate vectors to store the "is_extended" flag and the new lengths after extensions:
  # - if an extension is VALID (checked in the loop below), then 
  #   set the "is_extended" flag to TRUE and update the cross section length 
  #   to use the new extended length
  extended_flag <- rep(FALSE, length(extended_trans))
  length_list   <- transects_to_extend$cs_lengthm
  
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
      transects[transects[[crosswalk_id]] == current_hy_id & transects$cs_id != current_cs_id, ]
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
      
      # message("Extending transect: ", i)
      
      # get the current cross section list
      current_length <- length_list[i]
      # current_length <- transects_to_extend$cs_lengthm[i]
      
      # # Calculate the updated cross section length to align with the newly extended cross section for this row
      updated_cs_length <- (current_length * scale) + current_length
      # updated_cs_length <- (output_row$cs_lengthm * scale) + output_row$cs_lengthm
      
      # copy the current cross section length
      length_list[i] <- updated_cs_length
      # length_list  <- vctrs::vec_c(length_list, updated_cs_length)
      
      # Update the transect geometry with the newly extended transect
      geos_list[i] <- current_trans
      # geos_list <- vctrs::vec_c(geos_list, current_trans)
      # transects_to_extend$geom[i] <- sf::st_geometry(sf::st_as_sf(current_trans))
      
      # Set the extended flag to TRUE for this transect
      extended_flag[i] <- TRUE
      # extended_flag  <- vctrs::vec_c(extended_flag, TRUE)
      
    } 
  }
  
  if(verbose) { message(paste0("Complete!")) }
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects_to_extend) <- sf::st_geometry(sf::st_as_sf(geos_list))
  
  transects_to_extend$is_extended <- extended_flag
  transects_to_extend$cs_lengthm  <- length_list
  
  transects_to_extend                  <- hydroloom::rename_geometry(transects_to_extend, "geometry")
  
  return(transects_to_extend)
  
}