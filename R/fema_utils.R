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

# Give a set of transecct linestrings and poylgons and get the minimum distance to extend each transect line (from both directions, to try and reach the edge of a "polygons")
# internal function for extending transect lines out to FEMA 100 year flood plain polygons
# transect_lines, set of Sf linestrigns to extend (only if the transect lines are ENTIRELLY within a polygons)
# polygons, set of sf polygons that transect lines should be exteneded 
# max_extension_distance numeric, maximum distance (meters) to extend a transect line in either direction to try and intersect one of the "polygons"

#' Give a set of transecct linestrings and poylgons and get the minimum distance to extend each transect line (from both directions, to try and reach the edge of a "polygons")
#' WIP/internal function for extending transect lines out to FEMA 100 year flood plain polygons
#' @param transect_lines Set of Sf linestrigns to extend (only if the transect lines are ENTIRELLY within a polygons)
#' @param polygons set of sf polygons that transect lines should be exteneded 
#' @param flines set of Sf linestrings
#' @param max_extension_distance numeric, maximum distance (meters) to extend a transect line in either direction to try and intersect one of the "polygons"
#'
#' @return sf linestring, with extended transect lines
#' @importFrom rmapshaper ms_simplify
#' @importFrom geos as_geos_geometry geos_intersects_matrix geos_simplify_preserve_topology geos_within_matrix geos_empty geos_point_start geos_point_end
#' @importFrom sf st_as_sf st_cast st_segmentize st_length st_drop_geometry st_geometry
#' @importFrom dplyr mutate case_when select left_join relocate
#' @importFrom lwgeom st_linesubstring
#' @importFrom wk wk_crs
#' @importFrom vctrs vec_c
#' @export
get_transect_extension_distances_to_polygons <- function(transect_lines,   polygons,  flines, max_extension_distance) {

  ###    ###    ###    ###    ###    ###    ###
  # transect_lines <- transects
  # polygons <- fema
  # # # flines <- flines
  # # max_extension_distance <- 3000
  # max_extension_distance = 3500
  # ###    ###    ###    ###    ###    ###    ###

  # keep 10% of the original points for speed
  polygons <- rmapshaper::ms_simplify(polygons, keep_shapes = F, keep = 0.10)
  
  # mapview::npts(fema)
  # mapview::npts(polygons)
  # transects <- sf::read_sf(transect_path)
  
  # polygons
  transects_geos  <- geos::as_geos_geometry(transects)
  polygons_geos   <- geos::as_geos_geometry(polygons)     
  
  # geos::geos_union( polygons_geos, polygons_geos) %>% length()
  # polygons_geos %>% length()
  # polygons
  # polygons_geos  
  transects_polygons_matrix <- geos::geos_intersects_matrix(transects_geos, polygons_geos) 
  polygons_transects_matrix <- geos::geos_intersects_matrix(polygons_geos, transects_geos) 
  
  # subset the transects and polygons to only those with intersections
  intersect_transects  <- transects[lengths(transects_polygons_matrix) != 0, ]
  intersect_polygons   <- polygons_geos[lengths(polygons_transects_matrix) != 0]
  
  # Convert our intersecting polygons to LINESTRINGS b/c we DON'T NEED polygons to calculate extension distances from our transect lines
  # This can be done with just linestrings (not sure if this is actually more performent but I'm pretty sure it is....)
  intersect_lines <- 
    intersect_polygons %>% 
    # geos::geos_make_valid() %>% 
    sf::st_as_sf() %>% 
    # sf::st_union() %>% 
    # rmapshaper::ms_explode() %>% 
    # sf::st_as_sf() %>% 
    # dplyr::mutate(fema_id = 1:dplyr::n()) %>% 
    # dplyr::select(fema_id, geom = x) %>% 
    sf::st_cast("MULTILINESTRING") %>% 
    geos::as_geos_geometry() %>% 
    geos::geos_simplify_preserve_topology(25)
  
  # mapview::npts(sf::st_as_sf(intersect_lines))
  
  #    intersect_polygons %>% 
  #      geos::geos_make_valid() %>% 
  #      geos::geos_is_valid() %>% all()
  # is.null(intersect_lines$geometry )
  # use half of the shortest transect line as the segmentation length for all transects (ensures all transects will have a midpoint...?)
  # TODO: Double check this logic.
  min_segmentation <- min(intersect_transects$cs_lengthm %/% 2)
  
  # which.min(intersect_transects$cs_lengthm %/% 2)
  
  # make each transect line have way more segments so we can take a left and right half of each transect line
  segmented_trans <- sf::st_segmentize(intersect_transects, min_segmentation)
  
  # mapview::mapview(left_trans, col.regions = "dodgerblue") +
  # mapview::mapview(intersect_transects, color = "red") +
  #   mapview::mapview(intersect_transects[42, ], color = "yellow") +
  # mapview::mapview(right_trans, color = "dodgerblue") +
  # mapview::mapview(left_trans, color = "green")
  
  # Seperate the transect lines into LEFT and RIGHT halves
  # We do this so we can check if a side of a transect is ENTIRELY WITHIN a polygon. 
  # If the half is entirely within a polygon, 
  left_trans <- 
    segmented_trans %>% 
    lwgeom::st_linesubstring(0, 0.50) %>% 
    dplyr::mutate(
      partition         = "left",
      partition_lengthm = as.numeric(sf::st_length(geom))
    ) %>% 
    hydrofabric3D::add_tmp_id() %>% 
    dplyr::select(tmp_id, hy_id, cs_source, cs_id, cs_measure,  
                  cs_lengthm, is_extended, partition, partition_lengthm, geom)
  
  # Find the distances from the right side of transect lines 
  right_trans <- 
    segmented_trans %>% 
    lwgeom::st_linesubstring(0.50, 1) %>% 
    dplyr::mutate(
      partition         = "right",
      partition_lengthm = as.numeric(sf::st_length(geom))
    ) %>% 
    hydrofabric3D::add_tmp_id() %>%
    dplyr::select(tmp_id, hy_id, cs_source, cs_id, cs_measure,  
                  cs_lengthm, is_extended, partition, partition_lengthm, geom)
  
  # convert the transect geometries to geos types
  # get the fema polygon indices for the transect halves that are completely within a fema polygon
  # add the fema polygons index as a column to the transect dataframes
  left_trans_geos     <- geos::as_geos_geometry(left_trans)
  right_trans_geos    <- geos::as_geos_geometry(right_trans)
  
  left_within_matrix  <- geos::geos_within_matrix(left_trans_geos, intersect_polygons)
  right_within_matrix <- geos::geos_within_matrix(right_trans_geos, intersect_polygons)
  
  left_within_vect    <- lapply(left_within_matrix, function(i) { if(length(i) > 0) { c(i) } else { c(NA) } })
  right_within_vect   <- lapply(right_within_matrix, function(i) { if(length(i) > 0) { c(i) } else { c(NA) } })
  
  # add the fema polygon indexes as columns
  left_trans$left_fema_index    <- left_within_vect
  right_trans$right_fema_index  <- right_within_vect
  
  # add boolean columns whether the transect is fully within the FEMA polygons
  left_trans <- 
    left_trans %>% 
    dplyr::mutate(
      left_is_within_fema = dplyr::case_when(
        !is.na(left_fema_index) ~ TRUE,
        TRUE                    ~ FALSE
      )
    ) %>% 
    dplyr::select(tmp_id, hy_id, cs_source, cs_id, cs_measure,  
                  cs_lengthm, is_extended, partition, partition_lengthm,
                  left_fema_index, left_is_within_fema, 
                  geom
    )
  
  right_trans <- 
    right_trans %>% 
    dplyr::mutate(
      right_is_within_fema = dplyr::case_when(
        !is.na(right_fema_index) ~ TRUE,
        TRUE               ~ FALSE
      )
    ) %>% 
    dplyr::select(tmp_id, hy_id, cs_source, cs_id, cs_measure,  
                  cs_lengthm, is_extended, partition, partition_lengthm,
                  right_fema_index, right_is_within_fema, 
                  geom
    ) 
  
  # max_extension_distance = 3000
  # which(transects_with_distances$hy_id == "wb-1003839")
  # 
  # left_trans[which(left_trans$hy_id == "wb-1003839"), ]$cs_lengthm
  # right_trans[which(left_trans$hy_id == "wb-1003839"), ]$cs_lengthm
  # 
  # which(right_trans$hy_id == "wb-1003839")
  
  left_distances <- calc_extension_distances(
    geos_geoms             = left_trans_geos,
    ids                    = left_trans$tmp_id,
    lines_to_cut           = intersect_lines,
    lines_to_cut_indices   = left_trans$left_fema_index,
    direction              = "head",
    max_extension_distance = max_extension_distance
  )
  
  right_distances <- calc_extension_distances(
    geos_geoms             = right_trans_geos,
    ids                    = right_trans$tmp_id,
    lines_to_cut           = intersect_lines,
    lines_to_cut_indices   = right_trans$right_fema_index,
    direction              = "tail",
    max_extension_distance = max_extension_distance
  )  
  
  left_trans$left_distance    <- left_distances
  right_trans$right_distance  <- right_distances
  
  # extensions_by_id %>% 
  #   dplyr::filter(hy_id == "wb-1003839")
  # 
  
  # distance to extend LEFT and/or RIGHT for each hy_id/cs_id
  extensions_by_id <- dplyr::left_join(
    sf::st_drop_geometry(
      dplyr::select(left_trans, 
                    hy_id, cs_id, left_distance)
    ),
    sf::st_drop_geometry(
      dplyr::select(right_trans, 
                    hy_id, cs_id, 
                    right_distance)
    ),
    by = c("hy_id", "cs_id")
  )
  
  # TODO: Add left/right extension distancces to transect data
  # TODO: this can ultimately just be the "transects" variable, dont need to make new "transects_with_distances" variable
  transects <- 
    transects %>% 
    dplyr::left_join(
      extensions_by_id,
      by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::mutate(
      left_distance = dplyr::case_when(
        is.na(left_distance) ~ 0,
        TRUE                 ~ left_distance
      ),
      right_distance = dplyr::case_when(
        is.na(right_distance) ~ 0,
        TRUE                  ~ right_distance
      )
      # left_distance = dplyr::case_when(
      #   left_distance == 0 ~ NA,
      #   TRUE               ~ left_distance
      # ),
      # right_distance = dplyr::case_when(
      #   right_distance == 0 ~ NA,
      #   TRUE                ~ right_distance
      # )
    ) %>% 
    hydrofabric3D::add_tmp_id()
  
  #     ######## ######## ######## ######## ######## ######## ######## ######## 
  #     left_extended_flag[1:20]
  #     right_extended_flag[1:20]
  #     both_extended_flag[1:20]
  #     
  #     range_vect <- 1:500
  #     
  #     fema_uids <- unique(c(unlist(fema_index_df[range_vect, ]$left_fema_index),     unlist(fema_index_df[range_vect, ]$right_fema_index)))
  #     fema_uids <- fema_uids[!is.na(fema_uids)]
  # fema_uids
  #     foi <- sf::st_as_sf(intersect_polygons[fema_uids]) %>% dplyr::mutate(
  #       fema_id = fema_uids
  #     )
  #     # ooi <- sf::st_as_sf()
  #     # toi <- sf::st_as_sf(new_transects[1:20])
  #      toi <- sf::st_as_sf(transect_geoms[range_vect])
  #     toi
  #     og_trans <- transects_with_distances[range_vect, ]
  #     mapview::mapview(foi, col.regions = "dodgerblue") +
  #       mapview::mapview(toi, color = "red") +
  #       mapview::mapview(og_trans, color = "green")
  ######## ######## ### ##### ######## ######## ######## ######## ######## 
  
  fline_id_array       <- flines$id
  
  # Convert the net object into a geos_geometry
  flines_geos          <- geos::as_geos_geometry(flines)
  
  transect_hy_id_array <- transects$hy_id
  transect_cs_id_array <- transects$cs_id
  
  transect_geoms       <- geos::as_geos_geometry(transects$geom)
  
  left_distances       <- transects$left_distance
  right_distances      <- transects$right_distance
  
  # preallocate vector that stores the extension. distances
  new_transects <- vctrs::vec_c(rep(geos::geos_empty(), length(transect_hy_id_array)))
  
  left_extended_flag   <- rep(FALSE, length(transect_hy_id_array))   
  right_extended_flag  <- rep(FALSE, length(transect_hy_id_array))
  both_extended_flag   <- rep(FALSE, length(transect_hy_id_array))
  
  
  updated_left_distances    <- rep(0, length(transect_hy_id_array))   
  updated_right_distances   <- rep(0, length(transect_hy_id_array))   
  
  # number of geometries that will be iterated over, keeping this variable to reference in message block  
  total <- length(transect_hy_id_array)
  
  # output a message every ~10% intervals
  message_interval <- total %/% 20
  number_of_skips = 0
  
  for (i in seq_along(transect_hy_id_array)) {

    # Check if the iteration is a multiple of 100
    if (i %% message_interval == 0) {
      
      # get the percent complete
      percent_done <- round(i/total, 2) * 100
      
      # Print the message every "message_interval"
      # if(verbose) { 
      message(i, " > ", percent_done, "% ") 
      message("Number of skips: ", number_of_skips)
      # }
      # message("Iteration ", i, " / ", length(extended_trans), 
      #         " - (", percent_done, "%) ")
      
    }
    # which(transects_with_distances$hy_id == "wb-1003839")
    # i = 9587
    # get the current transect, hy_id, cs_id, flowline, and extension distances
    current_trans <- transect_geoms[i]
    
    current_hy_id <- transect_hy_id_array[i]
    current_cs_id <- transect_cs_id_array[i]
    
    current_fline <- flines_geos[fline_id_array == current_hy_id]
    
    left_distance_to_extend <- left_distances[i]
    right_distance_to_extend <- right_distances[i]
    
    no_extension_required <- (left_distance_to_extend == 0 && right_distance_to_extend == 0)
    # no_extension_required <- is.na(left_distance_to_extend) && is.na(right_distance_to_extend)
    # message("Transect tmp_id: ", curr_tmp_id, " - (", i, ")")
    
    if(no_extension_required) {
      # message("Skipping -left/right extension are both 0")
      number_of_skips = number_of_skips + 1
      
      next
    }
    
    # message("Extending transect line left and right")
    # extend the lines
    left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            left_distance_to_extend, "head")
    right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            right_distance_to_extend, "tail")
    
    # initial check to make sure the extended versions of the transects are valid
    use_left_extension  <- is_valid_transect_line(left_extended_trans, transect_geoms, flines_geos)
    use_right_extension <- is_valid_transect_line(right_extended_trans, transect_geoms, flines_geos)
    # use_both_extensions <- use_left_extension && use_right_extension
    
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
      use_left_extension  <- is_valid_transect_line(left_extended_trans, transect_geoms, flines_geos)
      
      used_half_of_left <- ifelse(use_left_extension, TRUE,  FALSE)
    }
    
    # if we CAN'T use the original RIGHT extension distance, 
    # we try HALF the distance (or some distance less than we extended by in the first place)
    if (!use_right_extension) {
      
      # half_right_distance  <- ifelse(right_distance_to_extend > 0, right_distance_to_extend %/% 2, 0)
      right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
                                                              half_right_distance, "tail")
      use_right_extension <- is_valid_transect_line(right_extended_trans, transect_geoms, flines_geos)
      
      used_half_of_right  <- ifelse(use_right_extension, TRUE,  FALSE)
      
      # mapview::mapview(sf::st_as_sf(current_trans), color = "red") + 
      #   mapview::mapview(sf::st_as_sf(left_extended_trans), color = "green") + 
      #   mapview::mapview(sf::st_as_sf(right_extended_trans), color = "green") +
      #   mapview::mapview(sf::st_as_sf(left_extended_trans2), color = "dodgerblue") + 
      #   mapview::mapview(sf::st_as_sf(right_extended_trans2), color = "dodgerblue") 
      
    }
    
    use_both_extensions <- use_left_extension && use_right_extension
    
    # # message("Checking left and right intersections with flowline...")
    # # ---------------------------------------------------------------------------------
    # # TODO: UNCOMMENT BELOW ---> this was my original method 
    # # ---------------------------------------------------------------------------------
    # # Check that the extended transect lines only intersect the current flowline once
    # left_intersects_fline <- geos::geos_intersection(
    #   left_extended_trans,
    #   # current_fline
    #   flines_geos
    # )
    # 
    # right_intersects_fline <- geos::geos_intersection(
    #   right_extended_trans,
    #   # current_fline
    #   flines_geos
    # )
    # 
    # 
    # # mapview::mapview(sf::st_as_sf(flines_geos[which(geos::geos_type(left_intersects_fline) == "point")])) +
    # #   mapview::mapview(sf::st_as_sf(current_trans), color = "red") +
    # # mapview::mapview(sf::st_as_sf(left_extended_trans), color = "green") + 
    # # mapview::mapview(sf::st_as_sf(right_extended_trans), color = "green")
    # 
    # # Define conditions to decide which version of the transect to use
    # 
    # # 1. Use transect with extension in BOTH directions
    # # 2. Use transect with LEFT extension only
    # # 3. Use transect with RIGHT extension only
    # 
    # # left_intersects_fline_once  <- geos::geos_type(left_intersects_fline) == "point"
    # # right_intersects_fline_once <- geos::geos_type(right_intersects_fline) == "point"
    # left_intersects_fline_once  <- sum(geos::geos_type(left_intersects_fline) == "point") == 1 && 
    #                                sum(geos::geos_type(left_intersects_fline) == "multipoint") == 0 
    # 
    # right_intersects_fline_once <- sum(geos::geos_type(right_intersects_fline) == "point") == 1 &&
    #                                sum(geos::geos_type(right_intersects_fline) == "multipoint") == 0 
    # 
    # # sum(geos::geos_type(left_intersects_fline) == "point") == 1
    # # sum(geos::geos_type(right_intersects_fline) == "point") == 1
    # # sum(geos::geos_type(left_intersects_fline) == "multipoint") == 0 
    # 
    # 
    # 
    # # # TODO: Consider doing the opppsite of these conditions (i.e. "left_intersects_other_transects" = TRUE) 
    # # left_does_not_intersect_other_transects  <- !any(geos::geos_intersects(left_extended_trans, transect_geoms[-i]))
    # # right_does_not_intersect_other_transects <- !any(geos::geos_intersects(right_extended_trans, transect_geoms[-i]))  
    # # 
    # # use_left_extension  <- left_intersects_fline_once && left_does_not_intersect_other_transects
    # # use_right_extension <- right_intersects_fline_once && right_does_not_intersect_other_transects
    # # use_both_extensions <- use_left_extension && use_right_extension
    # 
    # 
    # # TODO: This is the opposite phrasing of these conditions, i think this is clearer to read
    # left_intersects_other_transects  <- any(geos::geos_intersects(left_extended_trans, transect_geoms[-i]))
    # right_intersects_other_transects <- any(geos::geos_intersects(right_extended_trans, transect_geoms[-i]))  
    # 
    # # # make sure the extended transects don't hit any of the newly extended transects
    # # # NOTE: I think this could be just done with a single transect list that starts with the original transects and if an update happens then we replace that transect
    # # left_intersects_new_transects  <- any(geos::geos_intersects(left_extended_trans, new_transects))
    # # right_intersects_new_transects <- any(geos::geos_intersects(right_extended_trans, new_transects))
    # 
    # # make TRUE/FALSE flags stating which transect should we use
    # # - BOTH extensions
    # # - LEFT ONLY extensions
    # # - RIGHT only extensions
    # use_left_extension  <- left_intersects_fline_once && !left_intersects_other_transects
    # use_right_extension <- right_intersects_fline_once && !right_intersects_other_transects
    # use_both_extensions <- use_left_extension && use_right_extension
    # 
    # new_use_left_extension  <- is_valid_transect_line(left_extended_trans, transect_geoms, flines_geos)
    # new_use_right_extension <- is_valid_transect_line(right_extended_trans, transect_geoms, flines_geos)
    # new_use_both_extensions <- new_use_left_extension && new_use_right_extension
    # 
    # message("--------------------------------------------")
    # message("Left intersects FLINE ONCE: ", left_intersects_fline_once)
    # message("Right intersects FLINE ONCE: ", right_intersects_fline_once)
    # message()
    # message("Left intersects OTHER TRANSECTS: ", left_intersects_other_transects)
    # message("Right intersects OTHER TRANSECTS: ", right_intersects_other_transects)
    # message()
    # message("Use LEFT extension intersects: ", use_left_extension)
    # message("Use RIGHT extension intersects: ", use_right_extension)
    # message("Use BOTH extension intersects: ", use_both_extensions)
    # message()
    # message("--------------------------------------------")
    # message()
    # # merged_trans <- geos::geos_union(left_extended_trans, right_extended_trans)
    # # sf::st_union(sf::st_cast(sf::st_as_sf(merged_trans), "LINESTRING"))
    # # mapview::mapview(sf::st_as_sf(merged_trans), color = "green") +
    # #   mapview::mapview(sf::st_as_sf(left_start), col.region = "red") +
    # #   mapview::mapview(sf::st_as_sf(left_end), col.region = "red") + 
    # #   mapview::mapview(sf::st_as_sf(right_start), col.region = "dodgerblue") +
    # #   mapview::mapview(sf::st_as_sf(right_end), col.region = "dodgerblue")
    # # ---------------------------------------------------------------------------------
    # # TODO: UNCOMMENT ABOVE ---> this was my original method 
    # # ---------------------------------------------------------------------------------
    # 
    # if(use_both_extensions) {
    
    # Get the start and end of both extended tranects
    left_start  <- geos::geos_point_start(left_extended_trans)
    left_end    <- geos::geos_point_end(left_extended_trans)
    right_start <- geos::geos_point_start(right_extended_trans)
    right_end   <- geos::geos_point_end(right_extended_trans)

    # }
    # Extend in BOTH directions
    if(use_both_extensions) {
      # message("Extend direction: BOTH")
      start  <- left_start
      end    <- right_end
      
      # extend ONLY the left side
    } else if(use_left_extension && !use_right_extension) {
      # message("Extend direction: LEFT")       
      start  <- left_start
      end    <- left_end
      
      # Extend ONLY the right side
    } else if(!use_left_extension && use_right_extension) {
      # message("Extend direction: RIGHT")       
      start  <- right_start
      end    <- right_end
      
      # DO NOT extend either direction
    } else {
      # message("No extension")   
      # TODO: Really dont need to do anything 
      # TODO: in this scenario because we just use the original transect line
      start  <- left_end
      end    <- right_start
    }
    
    # trans_needs_extension <- use_left_extension || use_right_extension
    
    # if(trans_needs_extension) {
    
    line_crs      <- wk::wk_crs(current_trans)
    updated_trans <- make_line_from_start_and_end_pts(start, end, line_crs)
    
    #   touched_flines <- flines[geos::geos_type(right_intersects_fline) != "linestring", ]
    #   mapview::mapview(touched_flines, color = "dodgerblue") + 
    #   mapview::mapview(sf::st_as_sf(current_trans), color = "red") +
    #     mapview::mapview(sf::st_as_sf(left_extended_trans), color = "green") +
    #     mapview::mapview(sf::st_as_sf(right_extended_trans), color = "green") +
    #     mapview::mapview(sf::st_as_sf(updated_trans), color = "yellow")
    # nrow(flines)
    # touched_flines <- flines[geos::geos_type(right_intersects_fline) != "linestring", ]
    # flines[lengths(right_intersects_fline) == 0, ]
    # length(right_intersects_fline)
    # }
    
    # ---------------------------------------------------
    # TODO: UNCOMMENT BELOW
    # ---------------------------------------------------
    if(use_left_extension) {
      left_extended_flag[i]  <- TRUE
    }
    
    if(use_right_extension) {
      right_extended_flag[i] <- TRUE
    }
    
    if(use_both_extensions) {
      both_extended_flag[i] <- TRUE
    }
    
    if(used_half_of_left) {
      updated_left_distances[i]  <- half_left_distance 
    }
    if(used_half_of_right) {
      updated_right_distances[i] <- half_right_distance 
    }
    
    # new_transects[i] <- updated_trans
    transect_geoms[i] <- updated_trans
    
    # ---------------------------------------------------
    # TODO: UNCOMMENT ABOVE
    # ---------------------------------------------------
    
    # start %>% class()
  }      
  
  # transects2 <- transects 
  # dplyr::mutate(
  #   new_cs_lengthm = as.numeric(sf::st_length(geom))
  # ) %>% 
  # dplyr::relocate(hy_id, cs_id, cs_lengthm, new_cs_lengthm)
  
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects) <- sf::st_geometry(sf::st_as_sf(transect_geoms))
  
  transects <- 
    transects %>% 
    dplyr::mutate(
      new_cs_lengthm = as.numeric(sf::st_length(geom))
    ) %>% 
    dplyr::relocate(hy_id, cs_id, cs_lengthm, new_cs_lengthm)
  
  # transects2 %>% 
  #   dplyr::filter(
  #     new_cs_lengthm > cs_lengthm
  #   )
  # 
  
  transects$left_is_extended  <- left_extended_flag
  transects$right_is_extended <- right_extended_flag
  
  return(transects)
  
}    

# Given 2 geos_geometry point geometries, create a line between the 2 points
# start: geos_geoemtry, point
# end: geos_geoemtry, point
# Returns geos_geometry linestring

#' Given 2 geos_geometry point geometries, create a line between the 2 points
#'
#' @param start geos_geoemtry, point
#' @param end geos_geoemtry, point
#' @param line_crs crs
#' @importFrom geos geos_y geos_x geos_make_linestring
#' @return geos_geometry linestring
make_line_from_start_and_end_pts <- function(start, end, line_crs) {
  Y_start <- geos::geos_y(start)
  X_start <- geos::geos_x(start)
  Y_end   <- geos::geos_y(end)
  X_end   <- geos::geos_x(end)
  
  # make the new transect line from the start and points 
  geos_ls <- geos::geos_make_linestring(x = c(X_start, X_end),
                                        y = c(Y_start, Y_end), 
                                        crs = line_crs)
  
  return(geos_ls)                                              
  
  
}

#' Check if an updated transect line is valid relative to the other transects and flowlines in the network
#' The 'transect_to_check' should be 'used' (i.e. function returns TRUE) if 
#' the 'transect_to_check' does NOT interesect any other transects ('transect_lines') AND it only intersects a single flowline ONCE.
#' If the 'transect_to_check' intersects ANY other transects OR intersects a flowline more
#' than once (OR more than one flowline in the network) then the function returns FALSE.
#' @param transect_to_check geos_geometry, linestring
#' @param transect_lines geos_geometry, linestring
#' @param flowlines geos_geometry, linestring
#'
#' @return TRUE if the extension should be used, FALSE if it shouldn't be used
#' @importFrom geos geos_intersection geos_type geos_intersects
is_valid_transect_line <- function(transect_to_check, transect_lines, flowlines) {
  
  # ###   ##   ##   ##   ##   ##   ##   ##   ##   ##  
  # extension_line <- left_extended_trans
  # transect_lines <- transect_geoms
  # flowlines <- flines_geos
  # ###   ##   ##   ##   ##   ##   ##   ##   ##   ##  
  
  # Define conditions to decide which version of the transect to use
  
  # 1. Use transect with extension in BOTH directions
  # 2. Use transect with LEFT extension only
  # 3. Use transect with RIGHT extension only
  
  # Check that the extended transect lines only intersect a single flowline in the network only ONCE
  intersects_with_flowlines <- geos::geos_intersection(
    transect_to_check,
    flowlines
  )
  intersects_flowline_only_once <- sum(geos::geos_type(intersects_with_flowlines) == "point") == 1 && 
    sum(geos::geos_type(intersects_with_flowlines) == "multipoint") == 0 
  
  # check that the extended transect line does NOT intersect other transect lines (other than SELF)
  intersects_other_transects <- sum(geos::geos_intersects(transect_to_check, transect_lines)) > 1
  
  # TRUE == Only one flowline is intersected a single time AND no other transect lines are intersected
  use_transect <- intersects_flowline_only_once  && !intersects_other_transects
  
  return(use_transect)
}


#' Calculate the minimum distance a line would need to extend to reach the boundary of the polygon/line that the input geometries are entirely within 
#'
#' @param geos_geoms list of geos_geometrys
#' @param ids character vector
#' @param lines_to_cut geos_linestrings
#' @param lines_to_cut_indices numeric vector
#' @param direction character, either "head", "tail" or "both"
#' @param max_extension_distance numeric
#'
#' @return numeric vecotr, distance to extend each geos_geoms
calc_extension_distances <- function(geos_geoms, ids, lines_to_cut, lines_to_cut_indices, direction = "head", max_extension_distance = 2500) {
  #####   #####   #####   #####   #####
  # geos_geoms   <- left_trans_geos
  # ids          <- left_trans$tmp_id
  # lines_to_cut <- intersect_lines
  # lines_to_cut_indices <- left_trans$left_fema_index
  # direction = "head"
  # max_extension_distance = 2500
  # geos_geoms             = left_trans_geos
  # ids                    = left_trans$tmp_id
  # lines_to_cut           = intersect_lines
  # lines_to_cut_indices   = left_trans$left_fema_index
  # direction              = "head"
  # max_extension_distance = max_extension_distance
  # geos_geoms             = left_trans_geos
  # ids                    = left_trans$tmp_id
  # lines_to_cut           = intersect_lines
  # lines_to_cut_indices   = left_trans$left_fema_index
  # direction              = "head"
  # max_extension_distance = max_extension_distance
  #
  
  #####   #####   #####   #####   #####
  
  if (!direction %in% c("head", "tail")) {
    stop("Invalid 'direction' value, must be one of 'head' or 'tail'")
  }
  
  # preallocate vector that stores the extension. distances
  extension_dists <- vctrs::vec_c(rep(0, length(ids)))
  
  # extension_dists <- vector(mode = "numeric", length = nrow(trans_data))
  for (i in seq_along(ids)) {
    # i = 118
    curr_id           <- ids[i]
    is_within_polygon <- any(!is.na(lines_to_cut_indices[[i]]))
    polygon_index     <- lines_to_cut_indices[[i]]
    # any(is_within_polygon)
    message("Transect: '", curr_id, "' - (", i, ")")
    
    if (is_within_polygon) {
      message("- Side of transect intersects with FEMA")
      message("\t > FEMA index: ", polygon_index)
      
      curr_geom  <- geos_geoms[[i]]
      index_vect <- sort(unlist(polygon_index))
      
      distance_to_extend <- geos_bs_distance(
        distances    = 1:max_extension_distance,
        line         = curr_geom,
        geoms_to_cut = lines_to_cut[index_vect],
        direction    = direction
      )
      
      extension_dists[i] <- distance_to_extend
    }
    
  }
  
  return(extension_dists)
}
