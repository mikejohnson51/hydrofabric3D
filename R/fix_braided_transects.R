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

#' Fix transects found on braided river sections (latest)
#'
#' @param network sf dataframe of hydrologic network, linestrings
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'network' the output of "cut_cross_sections()" function
#' @param crosswalk_id character, unique ID column
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#' @param method The method to determine the geometries to cut. Options are "crosswalk_id", "component", or "neighbor". Default is "crosswalk_id"
#' @param precision int, distance in meters to approximate final cross section linestring length. Value you must be greater than 0. Default is 1
#' @param rm_intersects logical, whether to remove transect linestrings that intersect with other parts of the network ('network'). Default is TRUE which will remove intersecting linestrings.
#'
#' @return sf object of transect linestrings
#' @importFrom dplyr filter group_by ungroup left_join select arrange bind_rows mutate
#' @importFrom hydroloom rename_geometry
#' @importFrom sf st_crs st_transform st_drop_geometry st_geometry st_as_sf st_intersects
#' @importFrom geos as_geos_geometry geos_intersects_any
#' @importFrom fastmap fastmap
#' @export
fix_braided_transects <- function(
    network, 
    transect_lines,
    crosswalk_id    = NULL,
    braid_threshold = NULL,
    method          = "crosswalk_id",
    precision       = 1,
    rm_intersects   = TRUE
) {
  
  # names that transect_lines starts out with to use at the end
  starting_names <- names(transect_lines)
  
  # set geometry name of network to "geometry"
  network <- hydroloom::rename_geometry(network, "geometry")
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- add_braid_ids(
    network      = network, 
    crosswalk_id = crosswalk_id,
    verbose      = FALSE
  )
  
  # first check if there are any braids 
  if (all(braids$braid_id == "no_braid")) {
    
    message("No braids identified, returning original transects")
    
    return(transect_lines)
  }
  
  message("Fixing braid transects...")
  
  # not braided flowlines
  not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  braids <- dplyr::filter(braids, braid_id != "no_braid") 
  
  # *****************************************************************************************
  # Braid length checking & removal if braid is greater than threshold 
  # *****************************************************************************************
  
  if (!is.null(braid_threshold)) {

    # remove braids that have a total flowline length greater than braid_threshold
    braids <- braid_thresholder(
      x            = braids,
      crosswalk_id = crosswalk_id,
      originals    = not_braids,
      threshold    = braid_threshold,
      verbose      = TRUE
    )

    # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
    not_braids <- braids$not_braids
    braids     <- braids$braids

  }
  
  # get the "to" version of the given 'crosswalk_id' (i.e. "to_<crosswalk_id>")
  to_crosswalk_id <- as_to_id(crosswalk_id)
  
  graph <- get_node_topology(braids, crosswalk_id)
  
  # distinguise connected and disconnected parts of the network
  components <- find_connected_components(graph = graph, 
                                          crosswalk_id = crosswalk_id,
                                          verbose = verbose
                                          )
  
  # join the component ID to the graph
  graph <- 
    graph %>% 
    dplyr::left_join(
      dplyr::select(components, 
                    dplyr::any_of(c(crosswalk_id, to_crosswalk_id)), 
                    component_id
      ),
      by = c(crosswalk_id, to_crosswalk_id)
    )

  # join the component ID to the braids
  braids <- 
    braids %>% 
    dplyr::left_join(
      dplyr::distinct(
        dplyr::select(graph, dplyr::any_of(c(crosswalk_id, to_crosswalk_id)), component_id),
        dplyr::across(dplyr::any_of(c(crosswalk_id, "component_id")))
      ),
      by = crosswalk_id
    )
  
  # Add braid vector column to use during iteration (braid_vector)
  braids$braid_vector <- strsplit(braids$braid_id, ", ")
  
  # join cross sections w/ braid flowlines
  xs <- 
    transect_lines %>%
    dplyr::filter(.data[[crosswalk_id]] %in% braids[[crosswalk_id]]) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          braids, 
          dplyr::any_of(crosswalk_id), braid_id, is_multibraid, braid_vector
        )
      ),
      by = crosswalk_id
    ) 
    
  # keep track of all original crossections
  all_xs <- add_tmp_id(xs, x = crosswalk_id)$tmp_id
  
  # column to store the relative position within the braid of the flowline we're on 
  xs$relative_position <- NA
  
  # flag determining whether transect should/has been replaced
  xs$changed <- FALSE
  
  # empty columns to store number of head/tail intersections
  xs$head_cuts     <- NA
  xs$tail_cuts     <- NA
  
  # empty columns to store distance needed to extend from head/tail of line
  xs$head_distance <- NA
  xs$tail_distance <- NA
  
  # check if any transects exist, if not, just return the original transects
  if (nrow(xs) == 0) {
    message("No transect lines intersect with braided flowlines, returning original transect lines")
    return(transect_lines)
    
  } else {
    message( "Fixing ", nrow(xs) , " transect lines intersecting with braided flowlines lines")
  }
  
  # convert sf geometry column into a geos_geometry with the same "geometry" column name
  xs         <- sf_to_geos(xs)
  braids     <- sf_to_geos(braids)
  not_braids <- sf_to_geos(not_braids)
  
  # Loop through every single cross section and determine:
  # 1. its relative position
  # 2. how far to extend the line
  # 3. in what order should transects be extended, 
  # 4. in what direction to extend the transect
  
  # Loop through all the cross sections and either:
  # - Extend the inner cross sections 
  # OR
  # - Mark them to be processed in a future step
  for(i in 1:nrow(xs)) {
    
    # 1 = braid2_components
    # 2 = braid_components
    # 3 = braid2_comids
    # 4 = braid2_neighs
    # 5 = braid_comids
    # 6 = braid_neighs
    
    # current cross section
    curr <- xs[i, ]
    
    # Get cross section geos geometry, cross section width and cross section bankful width
    cs_line  <- curr$geometry
    cs_width <- curr$cs_widths
    
    # crosswalk_id of transect line
    com <- curr[[crosswalk_id]]
    
    # braid ID of interest
    braid_of_interest <- curr$braid_id
    
    # get the component ID of current crosswalk_id
    comp_id <- braids$component_id[braids[[crosswalk_id]] == com]
    
    # other geometries to cut across with transects
    others <- get_geoms_to_cut(
      x            = braids,
      crosswalk_id = crosswalk_id,
      id           = com,
      braid_id     = braid_of_interest,
      component    = comp_id,
      method       = method
    )
    
    # # get information on extension distance and position of cross section
    # extend_maps <- geos_augment_transect(
    #   cs_line       = cs_line,
    #   cs_width      = cs_width,
    #   bf_width      = bf_width,
    #   crosswalk_id            = com,
    #   geoms_to_cut  = others$geometry,
    #   geom_ids      = others$comid,
    #   max_distance  = NULL,
    #   by            = precision, 
    #   # as_df         = FALSE,
    #   carry_geom    = FALSE
    # )
    
    extend_maps <- geos_augment_transect2(
      cs_line       = cs_line,
      crosswalk_id            = com,
      geoms_to_cut  = others$geometry,
      geom_ids      = others[[crosswalk_id]],
      max_distance  = max(cs_width * 5), 
      by            = ifelse(is.null(precision), cs_width / 2, precision), 
      carry_geom    = FALSE
    )
    
    # extract cross sections position within braids
    position <- extend_maps$head$get("position")
    
    # if a flowline on the inner portion of a braid, make extension and insert
    if(position == "inner") {
      # message("Extending ", i, " and checking if valid replacement...")
      
      # extend line out by total distance key values in head and tail maps
      res_geom <- geos_extend_transects(
        starter_line   = cs_line,
        head_distance  = extend_maps$head$get("total_distance"),
        tail_distance  = extend_maps$tail$get("total_distance"),
        extra_distance = cs_width / 2
      )
      
      # ONLY UPDATE geometry if it does NOT intersect with 
      # any of the other multibraid transects that have been changed so far (AND LEAVE OUT SELF)
      if(
        !geos::geos_intersects_any(
          res_geom,
          dplyr::filter(xs[-i,], changed)$geometry
        )
      )  
      {
        
        # update geometry with new, extended cross section
        xs$geometry[i] <- res_geom
        
        # flag determining whether transect should be replaced
        xs$changed[i] <- TRUE
        
      }
      
    } 
    
    # update relative position column
    xs$relative_position[i] <- extend_maps$head$get("position")
    
    # update head/tail distances values in dataframe w/ values from head/tail hashmaps
    xs$head_distance[i] <- extend_maps$head$get("total_distance")
    xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
    
    # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
    xs$head_cuts[i] <- extend_maps$head$get("count")
    xs$tail_cuts[i] <- extend_maps$tail$get("count")
    
  }
  
  # check intersection of keeps and NOT BRAID
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  net_intersects <- geos::geos_intersects_any(
    xs$geometry,
    not_braids$geometry
  )
  
  # remove updated cross sections that intersect with the NOT BRAIDED flowlines
  if (any(net_intersects)) {
    
    xs <- xs[!net_intersects, ]
    
  }
  
  # select the other cross sections that have NOT been changed yet and are NOT inner 
  # ---> (not changed "inner" cross sections would intersect with "changed inners", this was checked in the loop above)
  other_xs = dplyr::filter(xs, 
                           !changed,
                           relative_position != "inner")
  
  # inner transects that haven't been changed
  unchanged_inners <- dplyr::filter(xs, 
                                    !changed,
                                    relative_position == "inner")
  
  # remove excess cross sections by keeping ONLY "changed" flowlines
  xs <- dplyr::filter(xs, changed) 
  
  # intersections between updated inner cross sections ("xs") and the remaining inner cross sections that were NOT changed ("unchanged_inners")
  inner_intersects <- geos::geos_intersects_any(
    unchanged_inners$geometry,
    xs$geometry
  )
  
  # add back into "xs" the unchanged inner transects that do NOT intersect with our updated/extended inner transect lines
  xs <- dplyr::bind_rows(
    xs,
    unchanged_inners[!inner_intersects, ]
  )
  
  # indices of other_xs transects that now intersect with the updated/extended 'xs' transects. 
  # All the cross section lines in "xs" are now "inner" lines that were extended
  other_intersects <- geos::geos_intersects_any(
    other_xs$geometry,
    xs$geometry
  )
  
  # if there ARE some intersections, remove those intersecting lines from 'div_xs'
  if (any(other_intersects)) {
    
    # drop div_xs transects that are overlapping with 'xs' transects
    other_xs <- other_xs[!other_intersects, ]
  }
  
  # if there are still other (non "inner") transects, do extension processing
  if (nrow(other_xs) == 0) { 
    
    # bind together final updated transect lines
    out <- dplyr::select(xs, 
                         # -braid_id, -head_cuts, -tail_cuts
                         -is_multibraid,
                         # -has_mainstem,
                         -changed, 
                         -head_distance, -tail_distance,
    )
    
  } else {
    
    # loop through the remaining transects that were NOT "inner" lines, and do extensions
    for (i in 1:nrow(other_xs)) {
      
      # if we get to a transect that does not intersect the rest of the braid even after extension, than set "changed" to TRUE and skip the iteration
      if (other_xs$relative_position[i] == "no_intersects") {
        
        # flag determining whether transect should be replaced
        other_xs$changed[i] <- TRUE
        
        next
      }
      
      # extend line other_xs[i, ] line out by head_distance/tail_distance and provide the extra_distance of cs_width/2
      res_geom <- geos_extend_transects(
        starter_line   = other_xs$geometry[i],
        head_distance  = other_xs$head_distance[i],
        tail_distance  = other_xs$tail_distance[i],
        extra_distance = other_xs$cs_widths[i]/2
      )
      
      # - Check to make sure that the newly extended res_geom transect line does not intersect with any of the other cross sections in 'xs'
      # - Also check that the new res_geom doesn't intersect with any of the other transects in "other_xs" other than itself
      # ----> If BOTH of these are TRUE, then the new extended transect replaces the original transect in the 'other_xs' geometry column
      if(
        !any(
          geos::geos_intersects_any(
            xs,
            res_geom
          )) &
        !any(geos::geos_intersects_any(
          other_xs[-i, ],
          res_geom
        ))
      ) {
        
        # replace geometry with extended line
        other_xs$geometry[i] <- res_geom
        
        # flag determining whether transect should be replaced
        other_xs$changed[i] <- TRUE
      }
    }
    
    # keep only the transects that were changed/extended
    other_xs <- dplyr::filter(other_xs, changed)
    
    # bind together final updated transect lines
    out <- dplyr::bind_rows(
      dplyr::select(xs, 
                    -is_multibraid,
                    -changed, 
                    -head_distance, -tail_distance
      ),
      dplyr::select(other_xs,
                    -is_multibraid,
                    -changed,
                    -head_distance, -tail_distance
      )
    )
    
  }
  
  # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
  transect_lines <-  dplyr::bind_rows(
    # from original transect_lines, remove all of the cross sections on braids,
    dplyr::select(
      dplyr::filter(   
        add_tmp_id(transect_lines, 
                   x = crosswalk_id),
        !tmp_id %in% all_xs
      ),
      -tmp_id
    ),
    # updated braid cross sections
    sf::st_as_sf(out)
  )
  
  # if rm_intersects == TRUE, then remove transects that interesect with other parts of the network
  if(rm_intersects) {

    # bind braids and not_braids back together to reform original "network" but with added "braid_id" column
    network <- sf::st_as_sf(
      dplyr::bind_rows(braids, not_braids)
    )

    # if final transect_lines has an NA for the braid_id column it means that it was part of the non braided (untouched) transect_lines,
    # set braid_id to "no_braid" in those cases, otherwise keep braid_id as is
    transect_lines$braid_id <- ifelse(
      is.na(transect_lines$braid_id),
      "no_braid",
      transect_lines$braid_id
    )

    # if one of the transect lines interesects MORE than 1 line in network AND it also has a braid_id == "no_braid", then remove it from output
    transect_lines <- transect_lines[!(lengths(sf::st_intersects(transect_lines, network)) > 1 & transect_lines$braid_id == "no_braid"), ]
    
    # remove transect lines that intersect another transect line more than once AND are NOT braid transects (i.e. preserves the newly extended transects on braids but removes the non extended transects that might interesect with the braid fixed transects)
    transect_lines <- transect_lines[!(lengths(sf::st_intersects(transect_lines)) > 1 & transect_lines$braid_id == "no_braid"), ]
    
    transect_lines <- 
      transect_lines %>% 
      dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>%
      # dplyr::group_by(hy_id) %>%
      dplyr::mutate(cs_id = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(lengthm = as.numeric(sf::st_length(.)))
    
  }
  
  # # select and reorder columns back to original starting positions
  transect_lines <- transect_lines[starting_names]
  
  return(transect_lines)
  
}

#' Get Geometries to Cut/Get the nearby geometries from a crosswalk_id in a braided network
#' 
#' Internal function that tries to determine the nearby and/or connected flowlines/crosswalk_ids relative to an origin crosswalk_ids in the NHDPlus network dataset ('x'). 
#' The return crosswalk_ids are a subset of the main dataset that will be used to extend transects across because they are eligible as they either: part of the same braid_id(s), neighboring braid_id(s), or within the same connected component of the network.
#' 
#' Geometries can be selected for cutting via 3 methods:
#' 
#' 1. All crosswalk_ids within the current crosswalk_ids braid_id (s) except the origin crosswalk_id (method = "crosswalk_ids")
#' 2. crosswalk_idS of the current crosswalk_id's braid_id(s) AND the crosswalk_ids of ANY of the braids that neighbor the origin crosswalk_ids braid_id(s) (method = "neighbor")
#' 3. Uses the component ID of the origin crosswalk_id to only identify crosswalk_ids that have the same component ID AND are not the origin crosswalk_id within the current crosswalk_ids braid_id (s) (method = "component")
#' @param x A data frame containing network data.
#' @param crosswalk_id character, unique ID column
#' @param id integer or character, origin crosswalk_id or identifier.
#' @param braid_id character, The braid identifier.
#' @param component character or integer, component ID of the origin crosswalk_id identifier.
#' @param method The method to determine the geometries to cut. Options are "crosswalk_ids", "component", or "neighbor". Default is "crosswalk_ids"
#'
#' @noRd
#' @keywords internal
#' @return sf dataframe containing the subset of nearby geometries to use for cutting.
#' @importFrom dplyr filter 
get_geoms_to_cut <- function(x, 
                             crosswalk_id = NULL,
                             id          = NULL, 
                             braid_id    = NULL, 
                             component   = NULL,
                             method      = "crosswalk_id"
) {
  
  # stop the function if an invalid "method" argument is given
  if(!method %in% c("crosswalk_id", "component", "neighbor")) {
    stop("Invalid 'method' value, must be one of 'crosswalk_id', 'component', or 'neighbor'")
  }
  
  # stop the function if "id" is missing (NULL)
  if (is.null(id)) {
    stop("Missing 'id' value, provide an origin crosswalk_id")
  }
  
  # If missing 'braid_id' argument (NULL) AND there is a "braid_id" column in "x",
  # use the "braid_id" column to get the 'braid_id' value
  if (is.null(braid_id) && "braid_id" %in% names(x)) {
    
    # filter 'x' to comid of interest and get the "component_id"
    braid_id <- x$braid_id[x[[crosswalk_id]] == id]
    
  }
  
  # if missing 'component' argument (NULL) AND there is a "component_id" column in "x",
  # use the "component_id" column to get the 'component' value
  if (is.null(component) && "component_id" %in% names(x)) {
    
    # filter 'x' to comid of interest and get the "component_id"
    component <- x$component_id[x[[crosswalk_id]] == id]
    
  }
  
  # use the braid_ids of the origin COMID and get ALL of the COMIDs that are within any of those braid_ids
  # Filter "x" dataset to ONLY:
  # - COMIDs that are in the SAME braid_id OR are in a neighboring braid
  # - COMIDs that are NOT self
  if(method == "crosswalk_id") {
    
    # braid IDs of interest
    braid_id_list <- strsplit(braid_id, ", ")[[1]]
    
    # get neighboring COMIDs for our current braid
    neighbor_ids <- crosswalk_ids_in_braid_ids(
      x               = x,
      crosswalk_id    = crosswalk_id,
      braids_to_match = x$braid_vector, 
      braid_ids       = braid_id_list
    )
    
    # remove self comid
    neighbor_ids <- neighbor_ids[neighbor_ids != id]
    
    # Filter to braid flowlines other than self that are within our given braid id or
    # are a nearby neighboring, flowline of a different (but connected) braid
    others <- x[x[[crosswalk_id]] %in% neighbor_ids, ]
    
    return(others)
    
  }
  
  # Uses the component ID of the origin COMID to only identify COMIDs that have the same component ID AND are not the origin COMID within the current COMIDs braid_id (s) (method = "component")
  
  # use the component ID of the origin comid to filter down our braid dataframe down to ONLY:
  # - COMIDs with the same component_id
  # - COMIDs that are NOT self
  if(method == "component") {
    
    # if component argument is missing AND there is a "component_id" column in "x", use that
    if(is.null(component)) {
      
      # stop message informing user to give a 'component' value
      stop("Missing 'component' value, provide a 'component' value that distinguises 
               between connected/disconnected components in network 'x'")
    }
    
    # filter 'x' to rows with the same component_id AND that are the origin comid
    others <- x[x$component_id %in% component & x[[crosswalk_id]] != id, ]
    
    return(others)
    
  }
  
  # use the braid IDs of the braids that neighbor our given braid_id(s)
  if(method == "neighbor") {
    
    # braid IDs of interest
    bids <- strsplit(braid_id, ", ")[[1]]
    
    # # get neighboring braid ID for our current braid
    neighbor_braids <- get_neighbor_braids(
                                        x = x, 
                                        ids = bids, 
                                        only_unique = TRUE
                                           )
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- x[x$braid_id %in% neighbor_braids & x[[crosswalk_id]] != id, ]
    
    return(others)
  }
  
  return(others)
}


#' Convert SF geometry columns to geos geometrys and return the original dataframe 
#' Internal function used to switch sf geometry dataframes to geos for processing tasks
#' @param x sf dataframe
#' @noRd
#' @keywords internal
#' @return data.frame, tibble with geometry column containing geos_geometries
sf_to_geos <- function(x) {
  
  x_geoms <- geos::as_geos_geometry(x)
  
  x <- sf::st_drop_geometry(x) 
  
  x$geometry <- x_geoms
  
  return(x)
}

#' Determine the relative position of a transect line within a braid given the count of intersecting lines 
#' Determine the relative position of a transect line within a braid, given the count of intersections that occur after extending the transect line in both direction (from the head and tail of the transect line) 
#' @param head_count numeric, count of intersections extending from the "head" end of an extended transect line  
#' @param tail_count numeric, count of intersections extending from the "tail" end of an extended transect line  
#'
#' @noRd
#' @keywords internal
#' @return character ("no_intersects", "outer_single", "outer_multi", "inner" , or "in_between")
check_relative_position <- function(head_count, tail_count) {
  
  # given the count of interesections from the head and tail of a linestring, return whether the line has:
  # - NO INTERSECTION:: (after extending linestring out to max distance)
  # - OUTER SINGLE: extending linestring out in both directions yielded 
  # zero intersections in one direction AND exactly one intersection in the other direction
  # - OUTER MULTI: extending linestring out in both directions yielded 
  # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
  # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
  #       INNER scenario intersection count (odd and even cases):
  #         intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
  # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
  # - MIDDLE/IN BETWEEN: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
  # ----> SKIP THIS!
  # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT)
  
  # TODO: Consider renaming these as No intersections, 
  # OUTER_SINGLE = SINGLE (the outer braid flowlines)
  # OUTER_MULTI  = SINGLE (the outer braid flowlines)
  # MIDDLE = IN BETWEEN (in between the outer braid flowlines and the actual middle braid flowlines)
  # INNER  = MIDDLE (the actual middle braid flowline)
  
  # boolean that gets flipped to FALSE if any of the other scenarios are detected
  in_between = TRUE
  
  # vector of intersection counts by the extended line,
  # extending out FROM THE HEAD and then FROM THE TAIL
  counts <- c(head_count, tail_count)
  
  # 1. No intersections scenario
  if(all(counts == 0)) {
    
    # relative position of line
    line_position <- "no_intersects"
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    
    return(line_position)
    
  }
  
  # 2. OUTER SINGLE scenario
  if(all(counts == c(1, 0)) | all(counts == c(0, 1))) {
    
    # relative position of line
    line_position <- "outer_single"
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    
    return(line_position)
    
  }
  
  # 3. OUTER MULTI scenario
  # Check if one value is 0 and the other is not zero AND is NOT 1
  if (any(counts == 0) && any(counts > 1)) {
    # if (any(counts == 0) && any(counts != 0)) {  
    
    # relative position of line
    line_position <- "outer_multi"
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    
    return(line_position)
  }
  
  # 4. INNER scenario
  # Handle sitation where total intersections is odd or even, if EITHER of below conditions is TRUE (OR condition), then we have inner (middle) line
  # - ODD CASE: If both the count values equal eachother
  # - EVEN CASE: If max(counts) minus 1 EQUALS min(counts)
  # If the counts equal eachother OR max(counts) minus 1 EQUALS min(counts)
  if(counts[1] == counts[2] | max(counts) - 1 == min(counts) ){
    
    # relative position of line
    line_position <- "inner"
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    
    return(line_position)
    
  }
  # 5. IN_BETWEEN scenario
  #  IF NONE OF THE ABOVE CONDITIONS EXECUTED, then we have an IN_BETWEEN line
  if(in_between) {
    
    # relative position of line
    line_position <- "in_between"
    
    # in_between boolean
    return(line_position)
  }
  
  return(line_position)
}

#' Extend a transect line outwards by a certain distance from the head and tail directions of the line
#' Internal function
#' @param starter_line geos_geometry, original transect line to extend outwards
#' @param head_distance numeric, distance (meters) to extend from "head" of the line
#' @param tail_distance numeric, distance (meters) to extend from "tail" of the line
#' @param extra_distance numeric, any extra distance (meters) the line should be extended after the original head/tail distances (this is typically going to be the cross section width divded by 2)
#'
#' @noRd
#' @keywords internal
#' @return geos_geometry, extended by specified distance
geos_extend_transects <- function(
    starter_line, 
    head_distance  = 0, 
    tail_distance  = 0, 
    extra_distance = 0
) {
  
  # set head and tail extra values to the 'extra_distance' argument
  head_extra = tail_extra = extra_distance 
  
  # if the HEAD extending distance is 0, also set the 'head_extra' value to 0
  if(head_distance == 0) {
    head_extra = 0
  } 
  
  # if the TAIL extending distance is 0, also set the 'tail_extra' value to 0
  if(tail_distance == 0) {
    tail_extra = 0
  }
  
  # distance to extend head and tail out by
  head_extension <- head_distance + head_extra
  tail_extension <- tail_distance + tail_extra
  
  # first extend the head outwards
  res_geom <- geos_extend_line(
    starter_line,  
    head_extension, 
    "head"
  )
  
  # then extend the tail from the already head extended line 
  res_geom <- geos_extend_line(
    res_geom,  
    tail_extension, 
    "tail"
  )
  
  return(res_geom)
  
}

#' Determine the distances needed to extend a transect linestring geometry across neighboring flowlines
#' Internal function that takes a transect line, and a set of nearby geos_geometries that the transect line should be compared against to see how far the given transect line should be
#' extended in both directions in order to cross over all the avaliable nearby flowline geos_geometries. Specifically to be used for situations where a river network is braided. 
#' @param cs_line geos_geometry, transect line to try and extend to cover braided river sections. 
#' @param cs_width numeric, cross section width of cs_line (meters) 
#' @param bf_width numeric, bankful width of cs_line (meters) 
#' @param crosswalk_id integer or character, unique ID of flowline geometry (i.e. COMID)
#' @param geoms_to_cut geos_geometry, other lingestrings (flowlines) of network that "cross_section" should attempt to extend out to, and to cut across 
#' @param geom_ids character, unique identifier (comid/hy_id) of transect line 
#' @param max_distance numeric, maximum distance (meters) to extend line out by
#' @param by numeric, distance to incrementelly extend out transect line. 
#' @param carry_geom logical, whether to carry geometries and keep them in the functions return. Default is TRUE, which will return the extended geometries with the function returns. 
#' @noRd
#' @keywords internal
#' @return dataframe or list of fastmap::fastmap() objects with meta data describing how far to extend a given transect line, in which directions and the relative position of the transect line. 
#' @importFrom geos as_geos_geometry
#' @importFrom fastmap fastmap
geos_augment_transect <- function(
    cs_line,
    cs_width,
    bf_width,
    crosswalk_id,
    geoms_to_cut, 
    geom_ids,
    max_distance = NULL,
    by = NULL, 
    carry_geom = TRUE
) {
  
  # max distance from transect of interest and rest of braid flowlines 
  # TODO (need a better method of determing max possible extension of flowline)
  # max_dist <- as.numeric(max(sf::st_distance(geoms_to_cut, x)))
  
  # # cs_line <- geos::as_geos_geometry(cross_section$geometry)
  # 
  # # extract values from cross_section dataframe
  # cs_width <- cross_section$cs_widths
  # bf_width <- cross_section$bf_width
  # crosswalk_id       <- cross_section$hy_id
  
  # # convert cross_section geometry columns to geos_geometry
  # cs_line  <- geos::as_geos_geometry(cross_section$geometry)
  # cs_line  <- cross_section$geometry
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(max_distance)) {
    max_distance <- max(cs_width * 5)
  }
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(by)) {
    by = bf_width/2
  }
  
  # sequence from 0 to the max possible extension distance 
  dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
  
  # default count of intersections for line extension in a given directionis set to 1, 
  # and then check for the actual total maximum number of possible lines that could be crossed by 
  # the extended version of the cross section geometry. This is done in both directions
  
  head_check = 1
  tail_check = 1
  
  # head_check <- sum( geos::geos_intersects( 
  #                   geos_extend_line(cs_line, max(dist_vect), "head"),
  #                   geoms_to_cut),  na.rm = TRUE)
  # 
  # tail_check <- sum(geos::geos_intersects( 
  #                   geos_extend_line(cs_line, max(dist_vect), "tail"),
  #                   geoms_to_cut),  na.rm = TRUE)
  
  
  # EXTEND OUT lines 
  # extend transect line out in both directions and find the side that interests with m
  # extend line out from HEAD side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  head_map <- geos_extend_out(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    geom_ids      = geom_ids,
    ids           = c(crosswalk_id), 
    dir           = "head",
    final_count   = head_check,
    map           = TRUE
  )
  
  # extend line out from TAIL side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  tail_map <- geos_extend_out(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    geom_ids      = geom_ids,
    ids           = c(crosswalk_id), 
    dir           = "tail",
    final_count   = tail_check,
    map           = TRUE
  )
  
  # get the relative position within the braid of the linestring we are extending our transect out from
  position <- check_relative_position(
    head_count = head_map$get("count"),
    tail_count = tail_map$get("count")
  )
  
  # POSITION VALUES explanation:
  # given the count of interesections from the head and tail of a linestring, return whether the line has:
  # - NO_INTERSECTION:: (after extending linestring out to max distance)
  # - OUTER_SINGLE: extending linestring out in both directions yielded 
  # zero intersections in one direction AND exactly one intersection in the other direction
  # - OUTER_MULTI: extending linestring out in both directions yielded 
  # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
  # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
  # INNER scenario intersection count (odd and even cases):
  # intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
  # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
  # - IN_BETWEEN/MIDDLE/: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
  # ----> SKIP THESE (maybe?) !
  # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT) ???
  
  # if as_df is FALSE, return the line data hashmaps as a list of length 2, 
  # first list element is the head extension data and the second is the tail extension data
  
  # if NOT AN INNER LINE, postpone processesing
  if(position != "inner") {
    
    # set "position" values for these geometries
    head_map$set("position", position)
    tail_map$set("position", position)
    
  } else {  # if LINE IS A INNER LINE, GET READY TO EXTEND
    
    # set "position" values for these geometries
    head_map$set("position", position)
    tail_map$set("position", position)
    
  }
  
  # if carry geom is FALSE, remove geometry linestrings from maps before returning
  if(!carry_geom) {
    head_map$remove("line")
    tail_map$remove("line")
  }
  
  return(
    list(
      head = head_map,
      tail = tail_map
    )
  )
  
}

#' Determine the distances needed to extend a transect linestring geometry across neighboring flowlines (v2)
#' Internal function that takes a transect line, and a set of nearby geos_geometries that the transect line should be compared against to see how far the given transect line should be
#' extended in both directions in order to cross over all the avaliable nearby flowline geos_geometries. Specifically to be used for situations where a river network is braided. 
#' @param cs_line geos_geometry, transect line to try and extend to cover braided river sections. 
#' @param crosswalk_id integer or character, unique ID of flowline geometry (i.e. COMID)
#' @param geoms_to_cut geos_geometry, other lingestrings (flowlines) of network that "cross_section" should attempt to extend out to, and to cut across 
#' @param geom_ids character, unique identifier (comid/hy_id) of transect line 
#' @param max_distance numeric, maximum distance (meters) to extend line out by
#' @param by numeric, distance to incrementally extend out transect line. 
#' @param carry_geom logical, whether to carry geometries and keep them in the functions return. Default is TRUE, which will return the extended geometries with the function returns. 
#' @noRd
#' @keywords internal
#' @return dataframe or list of fastmap::fastmap() objects with meta data describing how far to extend a given transect line, in which directions and the relative position of the transect line. 
#' @importFrom geos as_geos_geometry
#' @importFrom fastmap fastmap
geos_augment_transect2 <- function(
    cs_line,
    crosswalk_id,
    geoms_to_cut, 
    geom_ids,
    max_distance = NULL,
    by = NULL, 
    carry_geom = TRUE
) {
  
  # max distance from transect of interest and rest of braid flowlines 
  # TODO (need a better method of determing max possible extension of flowline)
  # max_dist <- as.numeric(max(sf::st_distance(geoms_to_cut, x)))
  
  # # cs_line <- geos::as_geos_geometry(cross_section$geometry)
  # 
  # # extract values from cross_section dataframe
  # cs_width <- cross_section$cs_widths
  # bf_width <- cross_section$bf_width
  # crosswalk_id       <- cross_section$hy_id
  
  # # convert cross_section geometry columns to geos_geometry
  # cs_line  <- geos::as_geos_geometry(cross_section$geometry)
  # cs_line  <- cross_section$geometry
  
  # # if no "by" argument is given, then the default becomes bf_width/2
  # if(is.null(max_distance)) {
  #   max_distance <- max(cs_width * 5)
  # }
  # 
  # # if no "by" argument is given, then the default becomes bf_width/2
  # if(is.null(by)) {
  #   by = bf_width/2
  # }
  
  # sequence from 0 to the max possible extension distance 
  dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
  
  # default count of intersections for line extension in a given directionis set to 1, 
  # and then check for the actual total maximum number of possible lines that could be crossed by 
  # the extended version of the cross section geometry. This is done in both directions
  
  head_check = 1
  tail_check = 1
  
  # head_check <- sum( geos::geos_intersects( 
  #                   geos_extend_line(cs_line, max(dist_vect), "head"),
  #                   geoms_to_cut),  na.rm = TRUE)
  # 
  # tail_check <- sum(geos::geos_intersects( 
  #                   geos_extend_line(cs_line, max(dist_vect), "tail"),
  #                   geoms_to_cut),  na.rm = TRUE)
  
  
  # EXTEND OUT lines 
  # extend transect line out in both directions and find the side that interests with m
  # extend line out from HEAD side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  head_map <- geos_extend_out(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    geom_ids      = geom_ids,
    ids           = c(crosswalk_id), 
    dir           = "head",
    final_count   = head_check,
    map           = TRUE
  )
  
  # extend line out from TAIL side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  tail_map <- geos_extend_out(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    geom_ids      = geom_ids,
    ids           = c(crosswalk_id), 
    dir           = "tail",
    final_count   = tail_check,
    map           = TRUE
  )
  
  # get the relative position within the braid of the linestring we are extending our transect out from
  position <- check_relative_position(
    head_count = head_map$get("count"),
    tail_count = tail_map$get("count")
  )
  
  # POSITION VALUES explanation:
  # given the count of interesections from the head and tail of a linestring, return whether the line has:
  # - NO_INTERSECTION:: (after extending linestring out to max distance)
  # - OUTER_SINGLE: extending linestring out in both directions yielded 
  # zero intersections in one direction AND exactly one intersection in the other direction
  # - OUTER_MULTI: extending linestring out in both directions yielded 
  # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
  # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
  # INNER scenario intersection count (odd and even cases):
  # intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
  # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
  # - IN_BETWEEN/MIDDLE/: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
  # ----> SKIP THESE (maybe?) !
  # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT) ???
  
  # if as_df is FALSE, return the line data hashmaps as a list of length 2, 
  # first list element is the head extension data and the second is the tail extension data
  
  # if NOT AN INNER LINE, postpone processesing
  if(position != "inner") {
    
    # set "position" values for these geometries
    head_map$set("position", position)
    tail_map$set("position", position)
    
  } else {  # if LINE IS A INNER LINE, GET READY TO EXTEND
    
    # set "position" values for these geometries
    head_map$set("position", position)
    tail_map$set("position", position)
    
  }
  
  # if carry geom is FALSE, remove geometry linestrings from maps before returning
  if(!carry_geom) {
    head_map$remove("line")
    tail_map$remove("line")
  }
  
  return(
    list(
      head = head_map,
      tail = tail_map
    )
  )
}

# WHILE LOOP IMPLEMENTATION INSTEAD OF RECURSION

#' Extend a linestring outward and return the minimum linestring (or details of minimum linestring) that crosses all possible other linestrings (geoms_to_cut) for a the given direction
#' Internal Function
#' @param x start index of distances vector
#' @param line transect line that should be extended
#' @param distances numeric vector of distance values (meters) in ascending order (sorted)
#' @param geoms_to_cut geos_geometry, all other linestrings (all linestrings other than 'line') that should be cut across (typically other linestrings making up a braided section of river)
#' @param geom_ids character or numeric vector of unique identifers for each linestring in 'geoms_to_cut'
#' @param ids character or numeric vector of unique identifier of the 'line' argument
#' @param dir character, either "head" or "tail", indicating which direction to extend 'line' out
#' @param final_count integer, final total count of intersections in one direction. Default is 1
#' @param map logical, whether to return a fastmap::fastmap() containing details about
#'  the extending line (distance, number of intersections, IDs of intersected geometries, etc.) 
#'  or to just return the extended line. Default is TRUE, which will return a fastmap::fastmap() that can be used 
#'  later on to extend the line the necessary distance.  If FALSE, a geos_geometry of the extended linestring is returned
#' @noRd
#' @keywords internal
#' @return fastmap::fastmap() with details on line extension, or a geos_geometry of the extended line
#' @importFrom geos as_geos_geometry geos_intersects
#' @importFrom fastmap fastmap
geos_extend_out <- function(
    x,
    line,
    distances,
    geoms_to_cut, 
    geom_ids,
    ids, 
    dir = "head",
    final_count = 1,
    map = TRUE
) {
  
  # initialize hashmap 
  if(map) {
    dmap <- fastmap::fastmap()
  }
  
  # count interesections
  count <- 0
  dcount <- 0
  
  while (TRUE) {
    xx <- geos_bs_distance(
      distances    = distances, 
      line         = line, 
      geoms_to_cut = geoms_to_cut,
      direction    = dir
    )
    
    if (xx >= length(distances)) {
      break
    }
    
    # extend line out to midpoint of distances vector
    crosser <- geos_extend_line(line, distances[xx], dir = dir)
    
    # Get the 'new_comid' that will be added to "ids" variable and then passed to the next iteration
    # - excluding the IDs in 'ids', determine what geometries in 'geoms_to_cut' are intersecting with our extended 'crosser' line
    # - then index 'geom_ids' based on the boolean vector returned from geos_intersects(), to then get the newly intersected ID (new_comid)
    new_comid <- geom_ids[
      geos::geos_intersects(
        crosser,
        geoms_to_cut[
          !geom_ids %in% ids
        ]
      )
    ]
    
    # Update all the variables for next iteration of while loop
    
    # update 'line'
    line         <- crosser
    
    # update 'geoms_to_cut' and drop the newly added 'new_comid' 
    geoms_to_cut <- geoms_to_cut[
      !geom_ids %in% c(ids, new_comid)
    ]
    
    # update 'geom_ids', removing ids and the new_comid
    geom_ids <- geom_ids[
      !geom_ids %in% c(ids, new_comid)
    ] 
    
    # update 'ids' w/ new_comid
    ids          <- c(ids, new_comid)
    
    # update x (index) value
    x            <- xx
    
    # increment count and continue summing distances
    count        <- count + 1
    dcount       <- dcount + distances[xx]
    
  }
  
  # # if specified, return distance map of info and line
  if(map) {
    
    dmap$mset(
      index           = x, 
      distance        = distances[x], 
      total_distance  = dcount,
      line            = line,
      cut_ids         = ids,
      count           = count,
      direction       = dir
    )
    
    return(dmap)
  }
  
  # otherwise just return the line
  return(line)
  
}

# Perform Binary search on sorted distance vector to determine minimum extension distance for a line to intersect with another geometry
# distances: numeric vector sorted in ascending order
# line: linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
# geoms_to_cut: geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array 
# direction: character, either "head" or "tail", indicating which end of the line to extend out.

#' Perform Binary search on sorted distance vector to determine minimum extension distance for a line to intersect with another geometry
#'
#' @param distances numeric vector sorted in ascending order
#' @param line geos_geometry, linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
#' @param geoms_to_cut geos_geometry, geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array 
#' @param direction character, either "head" or "tail", indicating which end of the line to extend out.
#' 
#' @noRd
#' @keywords internal
#' @return index of 'distance' vector, representing the minimum extension distance for a line to intersect nearby geometries
#' @importFrom geos geos_intersects
geos_bs_distance <- function(
    distances, 
    line,
    geoms_to_cut, 
    direction = "head"
) {
  
  # Left and right pointers (start and end of distances vector)
  L = 1
  R = length(distances)
  
  # While left pointer (L) is less than or equal to the right pointer (R), run binary search. 
  # Each iteration:
  # - the midpoint value gets calculated (M)
  # - M is the index of the 'distances' vector that we will use as the distance value to extend 'line'
  # - if the new extended line ('new_line') intersects with 'geoms_to_cut', then we decrease the distance value (DECREMENT RIGHT POINTER to the MIDPOINT - 1), 
  # - if NOT we increase the distance value (INCREMENT LEFT POINTER to the MIDPOINT + 1)
  while(L <= R) {
    
    # calculate midpoint between left and right pointers
    M = (L + R) %/% 2
    
    if(M == 0 | M == length(distances)) {
      # message("EARLY STOPPING bc M = ", M)
      # message("RETURNING L = ", L)
      return(L)
    }
    
    # extend line out to midpoint of distances vector
    new_line <- geos_extend_line(line, distances[M], dir = direction)
    # new_line_sf <- st_extend_line(sf::st_as_sf(line), distances[M], end = direction)
    
    # check if any of the other braid linestrings get intersected by the extended line:
    # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
    # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
    
    # if ANY of the geometries in geoms_to_cut are intersected by the new extended line
    if(
      any(lengths(geos::geos_intersects_matrix(new_line, geoms_to_cut)) > 0)
      # any(geos::geos_intersects(geoms_to_cut, new_line))
    ) {
      
      # then DECREMENT RIGHT pointer (DECREASE DISTANCE VALUE) to the midpoint - 1
      R = M - 1
      
      # otherwise IF NO intersections occur:
    } else {
      
      # then INCREMENT LEFT pointer (INCREASE DISTANCE VALUE) to the midpoint + 1
      L = M + 1
      
    }
  }
  
  return(L)
}

### 
### Old binary search function
### 

# #Extend a linestring out and determine the minimum extension distance to cross all possible other geometries
# #Internal function, implements a binary search algorithm to determine the minimum distance the geos_geometry linestring 'line' must be extended to cross all possible geometries in 'geoms_to_cut'
# #@param distances numeric vector in ascending order
# #@param line geos_geometry linestring
# #@param geoms_to_cut geos_geomtry linestrings to try and interesect by extending 'line'
# #@param direction character, direction to extend linestring from. Either "head", "tail" or "both". Default is "head".
# #@noRd
# #@keywords internal
# #@return numeric value indicating the index of the value in 'distances' that is the minimum extension distance to intersect all possible geometries in 'geoms_to_cut'
# #@importFrom sf st_intersects
# binary_search_distance <- function(distances, line, geoms_to_cut, direction = "head") {
#   
#   # left and right pointers at the start and end of the 'distances' vector, respectively
#   L = 1
#   R = length(distances)
#   
#   # while left pointer is less than or equal to the right pointer, run binary search
#   while(L <= R) {
#   
#     # calculate midpoint
#     M = (L + R) %/% 2
#     
#     # if midpoint is at the end or start of the 'distances' vector, return left pointer
#     if(M == 0 | M == length(distances)) {
#       # message("EARLY STOPPING bc M = ", M)
#       # message("RETURNING L = ", L)
#       return(L)
#     }
#     
#     # extend linestring by distance value at midpoint (M pointer)
#     new_line <- st_extend_line(line, distances[M], dir = direction)
# 
#     # check if any of the other braid linestrings get intersected by the extended line:
#     # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
#     # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
#     if(any(lengths(sf::st_intersects(geoms_to_cut, new_line)) > 0)) {
#       # message("DECREM RIGHT.--> need smaller value")
#       # message("R = R - 1 = : ", M - 1)
#       
#       # decrement right pointer to middle - 1
#       R = M - 1
#     } else {
#       # message("DECREM RIGHT.--> need smaller value")
#       # message("L = M + 1 = : ", M + 1)
#       
#       # increment left pointer to middle + 1
#       L = M + 1
#     }
#     # message("=======================")
#   }
# 
#   return(L)
# }

#' Perform Binary search on sorted distance vector to extend a linestring (transect line) out minimum distance to another linestring geometry
#' This is a variation of the geos_bs_distance() function but this function actually returns the extended linestring instead of the distance TO EXTEND
#' @param distances numeric vector sorted in ascending order
#' @param line geos_geometry, linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
#' @param geoms_to_cut geos_geometry, geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array 
#' @param direction character, either "head" or "tail", indicating which end of the line to extend out.
#' 
#' @noRd
#' @keywords internal
#' @return extended 'line' geos linestring
#' @importFrom geos geos_intersects geos_empty geos_is_empty
geos_bs_extend_to_geom <- function(distances, line, geoms_to_cut, direction = "head") {
  
  # Left and right pointers (start and end of distances vector)
  L = 1
  R = length(distances)
  
 
  new_line <- geos::geos_empty()
  
  # While left pointer (L) is less than or equal to the right pointer (R), run binary search. 
  # Each iteration:
  # - the midpoint value gets calculated (M)
  # - M is the index of the 'distances' vector that we will use as the distance value to extend 'line'
  # - if the new extended line ('new_line') intersects with 'geoms_to_cut', then we decrease the distance value (DECREMENT RIGHT POINTER to the MIDPOINT - 1), 
  # - if NOT we increase the distance value (INCREMENT LEFT POINTER to the MIDPOINT + 1)
  while(L <= R) {
    
    # calculate midpoint between left and right pointers
    M = (L + R) %/% 2
    
    if(M == 0 | M == length(distances)) {
      
      # TODO: if the the new_line value is still an empty geos geometry, return the original line
      if(geos::geos_is_empty(new_line)) {
        return(line)
      }
      
      return(new_line)
    }
    
    # extend line out to midpoint of distances vector
    new_line <- geos_extend_line(line, distances[M], dir = direction)
    # new_line_sf <- st_extend_line(sf::st_as_sf(line), distances[M], end = direction)
    
    # check if any of the other braid linestrings get intersected by the extended line:
    # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
    # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
    
    # if ANY of the geometries in geoms_to_cut are intersected by the new extended line
    if(
      any(geos::geos_intersects(geoms_to_cut, new_line))
    ) {
      
      # then DECREMENT RIGHT pointer (DECREASE DISTANCE VALUE) to the midpoint - 1
      R = M - 1
      
      # otherwise IF NO intersections occur:
    } else {
      
      # then INCREMENT LEFT pointer (INCREASE DISTANCE VALUE) to the midpoint + 1
      L = M + 1
      
    }
  }
  
  # TODO: if the the new_line value is still an empty geos geometry, return the original line
  if(geos::geos_is_empty(new_line)) {
      message("Empty geos geometry after extending, returning original line")
      return(line)
    }
      
  return(new_line)
}


#' Find the direction of the endpoints of a linestring (v2)
#' Internal function used in geos_extend_line() function to identify the the direction of each of the ends of a linestring
#' @param line geos_geometry, linestring
#' 
#' @noRd
#' @keywords internal
#' @return numeric vector of angle directions of a given linestring
#' @importFrom geos as_geos_geometry
#' @importFrom wk wk_coords
geos_linestring_dir <- function(line) {
  
  # convert to WK coords
  coords <- wk::wk_coords(line)
  coords <- as.matrix(coords[c("x", "y", "feature_id")])
  
  x_coords <- unname(coords[, 1])
  y_coords <- unname(coords[, 2])
  
  x1 = x_coords[1]
  y1 = y_coords[1]
  
  x2 = x_coords[2]
  y2 = y_coords[2]
  
  dirs <- c(atan2(y1 - y2, x1 - x2), atan2(y2- y1, x2 - x1))
  
  return(dirs)
  
}

#' Extend a geos_geometry linestring from, one or both ends, by a given distance (meters)
#'
#' @param line sf linestring or geos_geometry linestring to extend
#' @param distance numeric value in meters or a vector of length 2 if 'end = "both"' where 
#       the first value in the vector will extend that tail by that value and the second value extends the head by that value c(tail, head).
#       If a single value is given when end = "both", the value is recycled and used to extend both ends
#' @param dir character, determines whether to extend the linestring from the 'tail', 'head' or 'both' ends
#' @param with_crs logical, whether a CRS should be prescribed to extended output geos_geometry linestring
#' 
#' @return geos_geometry linestring extended by 'distance' from either the 'head', 'tail' or 'both' ends of the original linestring
#' @importFrom geos as_geos_geometry geos_make_linestring
#' @importFrom wk wk_coords wk_crs
#' @export
geos_extend_line <- function(line, 
                             distance,
                             dir = "both", 
                             with_crs = TRUE
) {
  
  # if NOT a geos_geometry class, coerce
  if(!inherits(line, "geos_geometry")) {
    # convert to geos geometry
    line <- geos::as_geos_geometry(line)
  }
  
  if(!dir %in% c("head", "tail", "both")) {
    stop("Invalid input 'dir' must either be 'head', 'tail', or 'both'")
  }
  
  # convert to WK coords
  coords <- wk::wk_coords(line)
  coords <- as.matrix(coords[c("x", "y")])
  
  # which index to keep
  to_keep <- dir != c("tail", "head")
  
  # dir coords index we want to keep
  dirs <- c(1, nrow(coords))[to_keep]
  
  # DETERMINE THE DIRECTION OF EACH COORDINATE
  x_coords <- unname(coords[, 1])
  y_coords <- unname(coords[, 2])
  
  # X/Y coordinate pairs
  x1 = x_coords[1]
  y1 = y_coords[1]
  x2 = x_coords[2]
  y2 = y_coords[2]
  
  directions <- c(atan2(y1 - y2, x1 - x2), atan2(y2 - y1, x2 - x1))
  
  # get directions of the direction of interest
  directions <- directions[to_keep]
  # directions <- geos_linestring_dir(line)[to_keep]
  
  # if only a single distance, duplicate it, otherwise reverse the first 2 distances
  distances <- if (length(distance) == 1) {
    rep(distance, 2) 
  } else {
    rev(distance[1:2])
  }
  
  # adjust dir point coordinates 
  coords[dirs, ]  <- coords[dirs, ] + distances[to_keep] * c(cos(directions), sin(directions))
  
  # whether to return with a CRS or not
  if(with_crs) {
    
    # # make a new linestring WITH CRS
    line <- geos::geos_make_linestring(
      x   = coords[, 1],
      y   = coords[, 2],
      crs = wk::wk_crs(line)
    )
    
    return(line)
    
  } 
  
  # # make a new linestring WITHOUT CRS
  line <- geos::geos_make_linestring(
    x = coords[, 1], 
    y = coords[, 2]
  )
  
  return(line)
}

#' Find the total length of all flowlines of each braid in a NHDPlus dataset
#' 
#' Internal function that gets the lengths of each braid ID in an NHDPlus dataset (output of find_braids() function)
#' 
#' @param x sf object of braided flowlines (output of find_braids()). Requires 'braid_id' and 'comid' column.
#' @param keep_geom logical, whether to keep geometries or not. Default is FALSE to drop geometries
#' @param multibraid logical, whether to give braid lengths for each braid ID our the braid length of all flowlines in a group of braids (i.e. multibraids). If FALSE (default) the braid length is calculated for each unique braid_id. 
#' @noRd
#' @keywords internal
#' @return dataframe with 3 columns, the braid_id, length (meters) of the total flowline length, and comids within the braid, and optionally a sf geometry
#' @importFrom dplyr filter group_by summarize ungroup arrange
#' @importFrom sf st_length st_drop_geometry
braid_lengths <- function(x, 
                          keep_geom = FALSE, 
                          multibraid = FALSE
) {
  
  # input check for input 'x'
  if(is.null(x)) {
    stop("missing 'x' input argument")
  }
  
  # if multibraid == FALSE, then calculate the length of each unique braid_id
  if(!multibraid) {
    # unpack nested braid_id column0
    xlengths <- unpack_braids(x)
    
    xlengths <-
      xlengths %>% 
      dplyr::group_by(braid_id) %>%
      dplyr::summarize(
        braid_length = as.numeric(
          sum(sf::st_length(geometry), na.rm = T)
        ),
        comids = paste0(c(comid), collapse = ", ")
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::arrange(-braid_length)
    
  } else {
    
    # lengths of multibraids (groups of braids)
    xlengths <- 
      x %>% 
      dplyr::group_by(braid_id) %>%
      dplyr::mutate(
        braid_length = as.numeric(
          sum(sf::st_length(geometry), na.rm = T))
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::arrange(-braid_length)
  }
  
  # drop geometries if keep_geom is FALSE
  if(!keep_geom) {
    
    xlengths <- sf::st_drop_geometry(xlengths)
  }
  
  return(xlengths)
  
}

#' Retrieve the COMIDs for all connected flowlines in list of "braid_ids"
#' 
#' The 'x' dataframe is the output of putting NHDPlus data through the
#' find_braids() function and then the find_connected_components() function.
#' 
#' @param x must be an dataframe/tibble/sf dataframe containing comid, braid_id, component_id, and braid_vector columns
#' @param braids_to_match list of character vectors to match with braid_ids
#' @param braid_ids character vector of braid_ids (i.e. c("braid_1", "braid_2", "braid_3"))
#'
#' @noRd
#' @keywords internal
#' @return vector containing the unique COMIDs within each braid_id
#' @importFrom dplyr filter
comids_in_braid_ids <- function(x, braids_to_match, braid_ids) {
  
  # braid IDs in. the x dataframe column that have only the braid IDs from braid_ids
  is_subset <- sapply(
    braids_to_match,
    function(vec) {
      all(vec %in% braid_ids)
    }
  )
  
  # Here what ive done is used the find_connected_components on the braids 
  # object and then I've located the connected componment IDs of the braids that 
  # contains ALL and ONLY ALL of the braid_ids in "bids"
  # maybe in addition to using this i can find additional neighbors by looking for 
  # braid_ids that contains my BIDs and ONLY my bids but also extra braid_ids 
  #   (this would only be for cases when the "bid" is a single braid_id? )
  
  # subset x dataset to the rows that contain ALL of the braid_ids and then use the component_id 
  # from these rows to further filter "x" based on the "component_id"
  # # Subset x to rows where braid_ids are a subset of braid_ids
  components <-  x[x$component_id %in% unique(x[is_subset, ]$component_id), ]
  
  # # logical vector to find any braid_ids that have ZERO overlap with the braid_ids in braid_ids
  component_braid_ids <- components$braid_vector
  
  # # logical vector to find any braid_ids that have ZERO overlap with the braid_ids in braid_ids
  no_overlaps <- sapply(
    component_braid_ids,
    function(vec) {
      any(vec %in% braid_ids)
    }
  )
  
  # remove braid_id that have NO overlap with braid_ids
  components <- components[no_overlaps, ]
  
  # return the unique COMIDs 
  return(unique(components$comid))
  
}

#' Retrieve the crosswalk_ids for all connected flowlines in list of "braid_ids"
#' 
#' The 'x' dataframe is the output of find_braids() function and then  adding a component_id column via find_connected_components()
#' 
#' @param x must be an dataframe/tibble/sf dataframe containing crosswalk_id, braid_id, component_id, and braid_vector columns
#' @param crosswalk_id character, unique ID column
#' @param braids_to_match list of character vectors to match with braid_ids
#' @param braid_ids character vector of braid_ids (i.e. c("braid_1", "braid_2", "braid_3"))
#'
#' @noRd
#' @keywords internal
#' @return vector containing the unique crosswalk_ids within each braid_id
#' @importFrom dplyr filter
crosswalk_ids_in_braid_ids <- function(x, crosswalk_id, braids_to_match, braid_ids) {
  
  # braid IDs in. the x dataframe column that have only the braid IDs from braid_ids
  is_subset <- sapply(
    braids_to_match,
    function(vec) {
      all(vec %in% braid_ids)
    }
  )
  
  # Here what ive done is used the find_connected_components on the braids 
  # object and then I've located the connected componment IDs of the braids that 
  # contains ALL and ONLY ALL of the braid_ids in "bids"
  # maybe in addition to using this i can find additional neighbors by looking for 
  # braid_ids that contains my BIDs and ONLY my bids but also extra braid_ids 
  #   (this would only be for cases when the "bid" is a single braid_id? )
  
  # subset x dataset to the rows that contain ALL of the braid_ids and then use the component_id 
  # from these rows to further filter "x" based on the "component_id"
  # # Subset x to rows where braid_ids are a subset of braid_ids
  components <-  x[x$component_id %in% unique(x[is_subset, ]$component_id), ]
  
  # # logical vector to find any braid_ids that have ZERO overlap with the braid_ids in braid_ids
  component_braid_ids <- components$braid_vector
  
  # # logical vector to find any braid_ids that have ZERO overlap with the braid_ids in braid_ids
  no_overlaps <- sapply(
    component_braid_ids,
    function(vec) {
      any(vec %in% braid_ids)
    }
  )
  
  # remove braid_id that have NO overlap with braid_ids
  components <- components[no_overlaps, ]
  
  # return the unique COMIDs 
  return(
    unique(components[[crosswalk_id]])
    )
  
}

#' Get the braid_ids of all neighboring braids from a specified braid_id
#' 
#' @param x dataframe, sf object network flowlines with braid_id column
#' @param ids character vector braid_id(s)
#' @param split_ids logical, if TRUE, then the comma seperated braid_ids are separated into individual braid_ids
#' @param only_unique logical, If TRUE, then only unique braid IDs are returned, otherwise (FALSE) all are returned. Default is FALSE. 
#' 
#' @noRd
#' @keywords internal
#' @return character vector of braid IDs neighboring one or multiple braid_ids
get_neighbor_braids <- function(x, ids, split_ids = FALSE, only_unique = FALSE) {
  
  # make groups for each braided section
  braid_groups <- lapply(1:length(ids), function(i) {
    
    # braid IDs of interest
    bids <- strsplit(ids[i], ", ")[[1]]
    
    # get all linestrings that are apart of the braid_ids of interest
    bids_check <- sapply(1:length(x$braid_id), function(y) {
      any(
        strsplit(x$braid_id[y], ", ")[[1]] %in% bids
      )
    })
    
    out <- sort(
      unique(c(
        x[bids_check, ]$braid_id,
        unlist(strsplit(x[bids_check, ]$braid_id, ", "))
      )
      )
    )
    
    # split_ids is TRUE, then the coimma seperated braid_ids are seperated into individual braid_ids
    if(split_ids) {
      
      out <- sort(
        unique(unlist(strsplit(out, ", ")))
      )
    }
    
    out
    
  })
  
  # assign names
  names(braid_groups) <- ids
  
  # remove uniques if desired
  if(only_unique) {
    braid_groups <- unique(unname(unlist(braid_groups)))
  }
  return(braid_groups)
}

#Create a plot of all the different transects and there possible groupings
#
#@param net sf object of NHDplusv2 data
#@param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#@param crosswalk_id character, column name containing a unique identifier
#@param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline
#lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#Default is NULL, which will attempt to fix all the braid transects in the data
#@param method The method to determine the geometries to cut. Options are "comid", "component", or "neighbor". Default is "comid"
#@param rm_intersects logical, whether to remove transect linestrings that intersect with other parts of the network ('net'). Default is TRUE which will remove intersecting linestrings.
#@param keep_plots logical, whether to return a list of ggplot2 plots or have function return NULL. Default is FALSE, returns NULL
#@param save_path character, path to a directory to save all ggplot2 plots to.
#@return NULL or a list of ggplot2 plots if keep_plots = TRUE
plot_braid_geoms_to_cut <- function(
    net,
    transect_lines,
    crosswalk_id = NULL,
    braid_threshold = NULL,
    method          = "comid",
    rm_intersects   = TRUE,
    keep_plots = FALSE,
    save_path = NULL
) {
  
  # names that transect_lines starts out with to use at the end
  starting_names <- names(transect_lines)
  
  # set geometry name of network to "geometry"
  net <- hydroloom::rename_geometry(net, "geometry")
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- add_braid_ids(
    network      = net, 
    crosswalk_id = crosswalk_id,
    verbose      = FALSE
  ) 
  
  if(all(braids$braid_id == "no_braid")) {
    
    message("No braids identified, returning original transects")
    
    # transform CRS back to input CRS
    if(start_crs2 != 5070) {
      message("Transforming CRS back to EPSG: ", start_crs2)
      transect_lines <- sf::st_transform(transect_lines, start_crs2)
    }
    
    return(transect_lines)
  }
  
  message("Fixing braid transects...")
  
  # not braided flowlines
  not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  braids <-
    braids %>%
    dplyr::filter(braid_id != "no_braid") %>%
    dplyr::group_by(braid_id) %>%
    dplyr::ungroup()
  
  
  # temporary braid_threshold variable of "drop_max" will use all of the braids EXCEPT the max length braid (minus 1 meter)
  if(!is.null(braid_threshold) && braid_threshold == "drop_max") {
    
    braid_threshold <- max(braid_lengths(braids)$braid_length) - 1
    
  }
  
  if (!is.null(braid_threshold)) {
    
    # remove braids that have a total flowline length greater than braid_threshold
    braids <- braid_thresholder(
      x         = braids,
      crosswalk_id = crosswalk_id,
      originals = not_braids,
      threshold = braid_threshold,
      verbose   = TRUE
    )
    
    # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
    not_braids <- braids$not_braids
    braids     <- braids$braids
    
  }
  
  # add connected component "component_id" column
  braids <- find_connected_components(braids)
  
  # join cross sections w/ braid flowlines
  xs <-
    transect_lines %>%
    dplyr::filter(hy_id %in% braids$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          braids, dplyr::any_of(crosswalk_id), braid_id, is_multibraid
        )
      ),
      by = crosswalk_id
    ) %>%
    dplyr::group_by(braid_id) %>%
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-totdasqkm)
  
  # keep track of all original crossections
  all_xs <- add_tmp_id(xs, x = crosswalk_id)$tmp_id

  # column to store the relative position within the braid of the flowline we're on
  xs$relative_position <- NA
  
  # flag determining whether transect should/has been replaced
  xs$changed <- FALSE
  
  # flag determining whether transect is to be processed in a future step after middle flowlines are processed
  xs$pending <- TRUE
  
  # flag determining whether transect is to be processed in a future step after middle flowlines are processed
  xs$pending <- TRUE
  
  # empty columns to store number of head/tail intersections
  xs$head_cuts     <- NA
  xs$tail_cuts     <- NA
  
  # empty columns to store distance needed to extend from head/tail of line
  xs$head_distance <- NA
  xs$tail_distance <- NA
  
  # data.table::data.table(xs)[1, ]
  
  # check if any transects exist, if not, just return the original transects
  if (nrow(xs) == 0) {
    
    # message("===== NO 'xs' transect lines =====")
    # message("===== returning original data =====")
    message("No transect lines intersect with braided flowlines, returning original transect lines")
    return(transect_lines)
    
  } else {
    message( "Fixing ", nrow(xs) , " transect lines intersecting with braided flowlines lines")
  }
  
  # keep track of seen component_ids
  seen <- fastmap::fastmap()
  
  # Loop through every single cross section and determine:
  # 1. its relative position
  # 2. how far to extend the line
  # 3. in what order should transects be extended,
  # 4. in what direction to extend the transect
  
  # list to store plots
  plot_list <- list()
  
  # system.time({
  for(i in 1:nrow(xs)) {
    # message("i: ", i, "/", nrow(xs))
    
    
    # 1 = braid2_components
    # 2 = braid_components
    # 3 = braid2_comids
    # 4 = braid2_neighs
    # 5 = braid_comids
    # 6 = braid_neighs
    
    # comid of transect line
    com <- xs[[crosswalk_id]][i]
    
    # braid ID of interest
    bid <- xs$braid_id[i]
    
    # get the component ID of current COMID
    comp_id <- braids$component_id[braids[[crosswalk_id]] == com]
    
    # make a plot for each unique component ID that comes up
    if(!seen$has(comp_id)) {
      # save_path = "/Users/anguswatters/Desktop/test_geoms_to_cut_plot.png"
      geoms_to_cut_plot <- make_geoms_to_cut_plot(
        shp          = xs[i, ],
        x            = braids,
        crosswalk_id           = com,
        braid_id     = bid,
        component    = comp_id,
        save_path = paste0(save_path, "geoms_to_cut_component_",
                           comp_id,
                           ".png"
        )
        # save_path = paste0("/Users/anguswatters/Desktop/transect_figs/geoms_to_cut/geoms_to_cut_component_",
        #                    comp_id,
        #                    ".png")
      )
      
      message("--> Adding ", comp_id, " to seen hashmap...")
      
      # add component ID to hashmap
      seen$set(comp_id, TRUE)
      
      message("Length of seen hashmap: ", seen$size())
    }
    
    if(keep_plots) {
      
      plot_list[[i]] <- geoms_to_cut_plot
      
    }
  }
  
  if(keep_plots) {
    plot_list <- Filter(function(x) !is.null(x), plot_list)
    
    return(plot_list)
  }
  
}
