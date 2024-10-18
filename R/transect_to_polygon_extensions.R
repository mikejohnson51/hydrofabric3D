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

# ----------------------------------------------------------------------------------
# library(sf)
# library(dplyr)
# library(geos)
# polygons <- sf::read_sf("/Users/anguswatters/Desktop/lynker-spatial/FEMA_BY_VPU/VPU_06/fema_vpu_06_output.gpkg")
# transect_lines <- sf::read_sf("/Users/anguswatters/Desktop/test_transects_06.gpkg")
# flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_flines_06.gpkg")
# crosswalk_id           = "hy_id"
# grouping_id     = "mainstem"
# max_extension_distance = 3000

# polygons <- rmapshaper::ms_simplify(polygons, keep_shapes = T, keep = 0.02, sys = TRUE, sys_mem = 16)
# # mapview::mapview(polygons, col.regions = "white", color = "green") +
# # mapview::mapview(polygons2, col.regions = "white", color = "red")
# # mapview::npts(polygons, by_feature = T) %>% sort(decreasing = T) %>% .[1:100]
# # mapview::npts(polygons)
# ----------------------------------------------------------------------------------

#' Give a set of transecct linestrings and poylgons and get the minimum distance to extend each transect line (from both directions, to try and reach the edge of a "polygons")
#' WIP/internal function for extending transect lines out to FEMA 100 year flood plain polygons (VERSION 2)
#' @param transect_lines Set of Sf linestrigns to extend (only if the transect lines are ENTIRELLY within a polygons)
#' @param polygons set of sf polygons that transect lines should be exteneded 
#' @param flowlines set of Sf linestrings
#' @param crosswalk_id character, flowline ID that matches flowlines with transect lines. This crosswalk_id must appear are a column in both flowlines and transect_lines.
#' @param grouping_id character, name of a column in flowlines that should be used to group each transect with 1 or more flowlines. 
#' That is, when transects are checked to make sure they don't intersect 
#' other transects or other flowlines, this group ID will distinguise which flowlines a transect should be checked against.
#' The intersect_group_id must appear as a column in both flowlines and transect_lines dataframes
#' @param max_extension_distance numeric, maximum distance (meters) to extend a transect line 
#' in either direction to try and intersect one of the "polygons". Default is 3000m
#' @return sf linestring, with extended transect lines
#' @importFrom rmapshaper ms_simplify
#' @importFrom geos as_geos_geometry geos_intersects_matrix geos_simplify_preserve_topology geos_within_matrix geos_empty geos_point_start geos_point_end
#' @importFrom sf st_as_sf st_cast st_segmentize st_length st_drop_geometry st_geometry
#' @importFrom dplyr mutate case_when select left_join relocate n any_of
#' @importFrom lwgeom st_linesubstring
#' @importFrom wk wk_crs 
#' @importFrom vctrs vec_c
#' @export
extend_transects_to_polygons <- function(
    transect_lines, 
    polygons, 
    flowlines, 
    crosswalk_id = 'hy_id',  
    grouping_id = 'mainstem',
    max_extension_distance = 3000
) {
  
  # ----------------------------------------------------------------------------------
  # ----------- Input checking ------
  # ----------------------------------------------------------------------------------
  
  if(!crosswalk_id %in% names(flowlines)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'flowlines' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks 'flowlines' to 'transect_lines'")
  }
  
  if(!crosswalk_id %in% names(transect_lines)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'transect_lines' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks the 'transect_lines' to 'flowlines'")
  }
  
  if(!grouping_id %in% names(flowlines)) {
    stop("grouping_id '", grouping_id, "' is not a column in 'flowlines' input,\n", 
         "Please provide a valid 'grouping_id' that associates each transect line with 1 or more flowlines in 'flowlines'"
    )
  }
  
  if(!grouping_id %in% names(transect_lines)) {
    stop("grouping_id '", grouping_id, "' is not a column in 'transect_lines' input,\n", 
         "Please provide a valid 'grouping_id' that associates each transect line with 1 or more flowlines in 'flowlines'"
    )
  }
  # ----------------------------------------------------------------------------------
  
  # get only the relevent polygons/transects
  transect_subset   <- subset_transects_in_polygons(transect_lines, polygons)
  polygons_subset   <- subset_polygons_in_transects(transect_lines, polygons)

  # get a dataframe that tells you how far to extend each line in either direction
  extensions_by_id  <- get_extensions_by_id(transect_subset, polygons_subset, crosswalk_id, max_extension_distance)
  
  # TODO: Add left/right extension distancces to transect data
  # TODO: this can ultimately just be the "transects" variable, dont need to make new "transects_with_distances" variable
  transect_lines <-
    transect_lines %>% 
    dplyr::left_join(
      extensions_by_id,
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    # TODO: I think i want to keep the NAs and NOT fill w/ 0 
    dplyr::mutate(
      left_distance = dplyr::case_when(
        is.na(left_distance) ~ 0,
        TRUE                 ~ left_distance
      ),
      right_distance = dplyr::case_when(
        is.na(right_distance) ~ 0,
        TRUE                  ~ right_distance
      )
    ) %>% 
    hydrofabric3D::add_tmp_id(x = crosswalk_id, y = "cs_id") 
  
  transect_lines <- extend_transects_by_distances(
    transects    = transect_lines,
    flowlines    = flowlines,
    crosswalk_id = crosswalk_id,
    cs_id        = "cs_id",
    grouping_id  = grouping_id 
  ) 
  
  # remove transects that intersect with OTHER TRANSECTS
  transect_lines <-
    transect_lines[lengths(sf::st_intersects(transect_lines)) == 1, ] %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
    # dplyr::group_by(hy_id) 
    dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
    dplyr::ungroup()
  
  # remove transects that intersect multiple flowlines
  transect_lines <- 
    transect_lines[lengths(sf::st_intersects(transect_lines, flowlines)) == 1, ] %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
    # dplyr::group_by(hy_id) 
    dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
    dplyr::ungroup()
  
  return(transect_lines)

}



#' Calculate the minimum distance a line would need to extend to reach the boundary of the polygon/line that the input geometries are entirely within 
#' VERSION 2
#' @param geos_geoms list of geos_geometrys
#' @param ids character vector
#' @param lines_to_cut geos_linestrings
#' @param lines_to_cut_indices numeric vector
#' @param direction character, either "head", "tail" or "both"
#' @param max_extension_distance numeric
#'
#' @return numeric vector, distance to extend each geos_geoms
#' @importFrom vctrs vec_c
#' @noRd
#' @keywords internal
calc_extension_distances <- function(
    geos_geoms, 
    ids, 
    lines_to_cut, 
    lines_to_cut_indices, 
    direction = "head", 
    max_extension_distance = 2500
) {
  
  if (!direction %in% c("head", "tail")) {
    stop("Invalid 'direction' value, must be one of 'head' or 'tail'")
  }
  
  distance_range               <- 1:max_extension_distance
  
  # preallocate vector that stores the extension. distances
  extension_dists              <- vctrs::vec_c(rep(0, length(ids)))
  
  # number of geometries that will be iterated over, keeping this variable to reference in message block  
  total                        <- length(ids)
  
  # output a message every ~10% intervals
  message_interval             <- total %/% 20
  
  make_progress <- make_progress_bar(TRUE, length(ids))
  
  for (i in seq_along(ids)) {
    # log percent complete
    make_progress()
    
    # # log percent complete
    # if (message_interval != 0 && i %% message_interval == 0) {
    #   # get the percent complete
    #   percent_done <- round(i/total, 2) * 100
    #   message(i, " > ", percent_done, "% ") 
    # }
    
    index_vect <- unlist(lines_to_cut_indices[[i]])
    
    distance_to_extend <- geos_bs_distance(
      distances    = distance_range,
      line         = geos_geoms[[i]],
      geoms_to_cut = lines_to_cut[index_vect],
      direction    = direction
    )
    
    extension_dists[i] <- distance_to_extend
    
  }
  
  return(extension_dists)
}

#' Given 2 geos_geometry point geometries, create a line between the 2 points
#'
#' @param start geos_geoemtry, point
#' @param end geos_geoemtry, point
#' @param line_crs crs
#' @importFrom geos geos_y geos_x geos_make_linestring
#' @return geos_geometry linestring
#' @noRd
#' @keywords internal
make_line_from_start_and_end_pts <- function(start, end, line_crs) {
  
  # Y_start <- geos::geos_y(start)
  # X_start <- geos::geos_x(start)
  # Y_end   <- geos::geos_y(end)
  # X_end   <- geos::geos_x(end)
  # 
  # # make the new transect line from the start and points 
  # geos_ls <- geos::geos_make_linestring(x = c(X_start, X_end),
  #                                       y = c(Y_start, Y_end), 
  #                                       crs = line_crs)
  # return(geos_ls)  
  
  # make the new transect line from the start and points 
  return(
    geos::geos_make_linestring(
      x   = c(geos::geos_x(start), geos::geos_x(end)),
      y   = c(geos::geos_y(start), geos::geos_y(end)), 
      crs = line_crs
    )
  )
  
}

#' Check if an updated transect line is valid relative to the other transects and flowlines in the network
#' The 'transect_to_check' should be 'used' (i.e. function returns TRUE) if 
#' the 'transect_to_check' does NOT interesect any other transects ('transect_lines') AND it only intersects a single flowline ONCE.
#' If the 'transect_to_check' intersects ANY other transects OR intersects a flowline more
#' than once (OR more than one flowline in the network) then the function returns FALSE.
#' @param transect_to_check geos_geometry, linestring
#' @param trans geos_geometry, linestring
#' @param flines geos_geometry, linestring
#'
#' @return TRUE if the extension should be used, FALSE if it shouldn't be used
#' @importFrom geos geos_intersection geos_type geos_intersects
#' @noRd
#' @keywords internal
is_valid_transect_line <- function(transect_to_check, trans, flines) {
  
  # ###   ##   ##   ##   ##   ##   ##   ##   ##   ##  
  
  # Define conditions to decide which version of the transect to use
  
  # 1. Use transect with extension in BOTH directions
  # 2. Use transect with LEFT extension only
  # 3. Use transect with RIGHT extension only
  
  # check for NULL / empty geometries
  if (is.null(transect_to_check) || is.null(trans) || is.null(flines)) {
    stop("invalid geometry: geometries cannot be NULL")
  }
  
  # Check that the extended transect lines only intersect a single flowline in the network only ONCE
  intersects_with_flowlines <- geos::geos_intersection(
    transect_to_check,
    flines
  )
  
  intersects_flowline_only_once <- sum(geos::geos_type(intersects_with_flowlines) == "point") == 1 && 
    sum(geos::geos_type(intersects_with_flowlines) == "multipoint") == 0 
  
  # NOTE: return early if if the transects does NOT intersect the flowline ONLY once
  # little optimization, avoids extra geos_intersects() calls 
  if (!intersects_flowline_only_once) {
    return(FALSE)
  }
  
  # check that the extended transect line does NOT intersect other transect lines (other than SELF)
  intersects_other_transects <- lengths(geos::geos_intersects_matrix(transect_to_check,  trans)) > 1
  # intersects_other_transects <- sum(geos::geos_intersects(transect_to_check, trans)) > 1
  
  # TRUE == Only one flowline is intersected a single time AND no other transect lines are intersected
  use_transect <- intersects_flowline_only_once  && !intersects_other_transects
  
  return(use_transect)
}

#' Get transects that intersect with the polygons 
#' @param transect_lines Set of Sf linestrigns to extend (only if the transect lines are ENTIRELLY within a polygons)
#' @param polygons set of sf polygons that transect lines should be exteneded 
#' @return sf linestring, with extended transect lines
#' @importFrom geos as_geos_geometry geos_intersects_matrix 
#' @noRd
#' @keywords internal
subset_transects_in_polygons <- function(transect_lines, polygons)  {
  
  transects_polygons_matrix <- geos::geos_intersects_matrix(geos::as_geos_geometry(transect_lines), geos::as_geos_geometry(polygons))
  
  # return(transect_lines[lengths(transects_polygons_matrix) != 0, ])
  return(transect_lines[lengths(transects_polygons_matrix) != 0, ])
  
}

#' Get polygons that intersect with the transects 
#' @param transect_lines Set of Sf linestrigns to extend (only if the transect lines are ENTIRELLY within a polygons)
#' @param polygons set of sf polygons that transect lines should be exteneded 
#' @return sf polygon dataframe
#' @importFrom geos as_geos_geometry geos_intersects_matrix 
#' @noRd
#' @keywords internal
subset_polygons_in_transects <- function(transect_lines, polygons)  {
  
  # TODO: this should be a function argument OR removed, shouldn't probably forcibly and silently simplify the input polygons without user knowing..
  # keep 10% of the original points for speed
  # polygons <- rmapshaper::ms_simplify(polygons, keep_shapes = T, keep = 0.01)
  
  # polygons_transects_matrix <- geos::geos_intersects_matrix(polygons_geos, transects_geos)
  polygons_transects_matrix <- geos::geos_intersects_matrix(geos::as_geos_geometry(polygons), geos::as_geos_geometry(transect_lines))
  
  # return(polygons_geos[lengths(polygons_transects_matrix) != 0])
  return(polygons[lengths(polygons_transects_matrix) != 0, ])
  
}

sf_polygons_to_geos_multilinestrings <- function(polygons, tolerance = 250) {
  
  return(polygons %>% 
           geos::as_geos_geometry() %>% 
           geos::geos_node() %>% 
           geos::geos_simplify_preserve_topology(tolerance))
}

#' Helper function for cleaning up transects and adding some meta data after going through partition_transects_for_extension
#'
#' @param partition dtaframe or sf dataframe
#' @param dir character, 'left' or 'right'. Default is 'left'
#' @param crosswalk_id character, unique column ID name
#'
#' @return dataframe or sf dataframe
#' @importFrom dplyr mutate relocate any_of
#' @importFrom sf st_length
#' @noRd
#' @keywords internal
wrangle_paritioned_transects <- function(partition, 
                                         dir = "left", 
                                         crosswalk_id = "hy_id"
) {
  
  partition <- 
    partition %>% 
    dplyr::mutate(
      partition         = dir,
      partition_lengthm = as.numeric(sf::st_length(.))
    ) %>%
    add_tmp_id(x = crosswalk_id, y = "cs_id") %>%
    dplyr::relocate(tmp_id,
                    dplyr::any_of(crosswalk_id),
                    # cs_source,
                    cs_id
                    # cs_measure,
                    # cs_lengthm,
                    # is_extended,
                    # partition, partition_lengthm, geometry
    ) 
  # dplyr::select(tmp_id, dplyr::any_of(crosswalk_id), cs_source, cs_id, cs_measure, cs_lengthm,
  # partition, partition_lengthm, geometry)  
  
  return(partition)
}

# From a set of transects and a set of polygons, subset the transects based on whether their start / end point is fully within a polygon
# This function will return the points transect lines that need to be extended in a given direction
# i.e. When dir = "left" then the returned transects have their starting points entirely within a polygon

#' From a set of transects and a set of polygons, subset the transects based on whether their start / end point is fully within a polygon
#'  This function will return the points transect lines that need to be extended in a given direction. ( i.e. When dir = "left" then the returned transects have their starting points entirely within a polygon)
#' @param transects sf dataframe w/ linestrings
#' @param polygons_subset sf dataframe of polygons 
#' @param dir character, direction to partition
#'
#' @return sf dataframe subset of 'transects' that are fully within polygons
#' @importFrom geos geos_point_start geos_point_end geos_within_matrix
#' @importFrom dplyr filter 
#' @noRd
#' @keywords internal
partition_transects_for_extension <- function(transects, polygons_subset, dir = "left") {
  
  # POINT START = LEFT
  # POINT END   = RIGHT
  
  if (!dir %in% c("left", "right")) {
    stop("Invalid 'dir' value '", dir, "', 'dir' must be either 'left' or 'right'")  
  }
  
  # get the function needed for a given direction, a transects starting point is the "left" and the ending point is the right  
  dir_function      <- ifelse(dir == "left", geos::geos_point_start, geos::geos_point_end)
  
  # determine transects whose starting/end points are within a polygon
  is_within_matrix  <- geos::geos_within_matrix(dir_function(transects), polygons_subset)
  
  is_within_vect    <- lapply(is_within_matrix, function(i) { if(length(i) > 0) { c(i) } else { c(NA) } })
  
  transects$polygon_index <- is_within_vect
  
  # return only the transects whose start / end point are within a polygon
  return(dplyr::filter(transects, !is.na(polygon_index)))
  
}

#' Get the left and right extension distances for a set of transects out to a set of polygons
#'
#' @param transects sf linestring dataframe
#' @param polygons sf polygon dataframe
#' @param crosswalk_id character
#' @param max_extension_distance numeric 
#'
#' @return data.frame or tibble
#' @export
get_extensions_by_id <- function(transects, polygons, crosswalk_id, max_extension_distance) {
  
  left_partition <- partition_transects_for_extension(
    transects, 
    polygons, 
    dir = "left"
  ) %>% 
    wrangle_paritioned_transects(
      dir          = "left", 
      crosswalk_id = crosswalk_id
    )
  
  right_partition <- partition_transects_for_extension(
    transects, 
    polygons, 
    dir = "right"
  ) %>% 
    wrangle_paritioned_transects(
      dir          = "right", 
      crosswalk_id = crosswalk_id
    )
  
  # Convert the polygon to a MULTILINESTRING geometry for checking extension distances
  mls <- sf_polygons_to_geos_multilinestrings(polygons, 200)
  
  message("Generating left side distances....") 
  left_distances <- calc_extension_distances(
    geos_geoms             = geos::as_geos_geometry(left_partition),
    ids                    = left_partition$tmp_id,
    lines_to_cut           = mls,
    lines_to_cut_indices   = left_partition$polygon_index,
    direction              = "head",
    max_extension_distance = max_extension_distance
  )
  
  message("Generating right side distances...")
  right_distances <- calc_extension_distances(
    geos_geoms             = geos::as_geos_geometry(right_partition),
    ids                    = right_partition$tmp_id,
    lines_to_cut           = mls,
    lines_to_cut_indices   = right_partition$polygon_index,
    direction              = "tail",
    max_extension_distance = max_extension_distance
  )
  
  left_partition$left_distance <- left_distances
  right_partition$right_distance <- right_distances
  
  # Distance to extend LEFT and/or RIGHT for each hy_id/cs_id
  extensions_by_id <- dplyr::left_join(
    sf::st_drop_geometry(
      dplyr::select(left_partition, 
                    dplyr::any_of(crosswalk_id),
                    cs_id,
                    left_distance
      )
    ),
    sf::st_drop_geometry(
      dplyr::select(right_partition, 
                    dplyr::any_of(crosswalk_id),
                    cs_id, 
                    right_distance
      )
    ),
    by = c(crosswalk_id, "cs_id")
  )
  
  # add any missing crosswalk_id/cs_id that didnt have any extension distance w/ values of 0
  extensions_by_id <- dplyr::bind_rows(
                          extensions_by_id, 
                          transects %>% 
                            sf::st_drop_geometry() %>% 
                            hydrofabric3D::add_tmp_id(x = crosswalk_id) %>% 
                            dplyr::filter(!tmp_id %in% hydrofabric3D::add_tmp_id(extensions_by_id, x = crosswalk_id)$tmp_id) %>% 
                            dplyr::select(-tmp_id) %>% 
                            dplyr::mutate(
                              left_distance  = 0,
                              right_distance = 0
                            )
                        ) 
  
  return(extensions_by_id)
}

#' Decide the start and end points for the final transect line given two extended versions of the same transect
#' Requires two logicals indicating what to do with the extensions (these are decided by checking for intersections with the rest of the network)
#' Internal helper function
#' @param left_extension geos_geometry linestring
#' @param right_extension geos_geometry linestring 
#' @param use_left logical, do we use the left extension
#' @param use_right logical, do we use the right extension
#' @importFrom geos geos_point_start geos_point_end
#' @return geos_geometry points, the start and end point of the final extension line
#' @noRd
#' @keywords internal
pick_extension_pts <- function(
    left_extension, 
    right_extension, 
    use_left, 
    use_right
) {
  
  use_both <- use_left && use_right
  
  # Get the start and end of both extended tranects
  left_start  <- geos::geos_point_start(left_extension)
  left_end    <- geos::geos_point_end(left_extension)
  right_start <- geos::geos_point_start(right_extension)
  right_end   <- geos::geos_point_end(right_extension)
  
  # Extend in BOTH directions
  if(use_both) {
    # message("Extend direction: BOTH")
    start  <- left_start
    end    <- right_end
    
    # extend ONLY the left side
  } else if(use_left && !use_right) {
    # message("Extend direction: LEFT")       
    start  <- left_start
    end    <- left_end
    
    # Extend ONLY the right side
  } else if(!use_left && use_right) {
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
  
  return( c(start, end) )
  
}

#' Get the starting and ending points of geos_linestring 
#' Internal helper function
#' @param extension geos_geometry linestring
#' @param use_extension logical, do we use the extension
#' @importFrom geos geos_point_start geos_point_end
#' @return geos_geometry points, the start and end point of the final extension line
#' @noRd
#' @keywords internal
get_line_node_pts <- function(
    line
) {
  # Get the start and end of the geos_linestring (extended transect)
  return( c(geos::geos_point_start(line),  geos::geos_point_end(line)) )
}

#' #Calculate the minimum distance a line would need to extend to reach the boundary of the polygon/line that the input geometries are entirely within 
#' #'
#' #@param geos_geoms list of geos_geometrys
#' #@param ids character vector
#' #@param lines_to_cut geos_linestrings
#' #@param lines_to_cut_indices numeric vector
#' #@param direction character, either "head", "tail" or "both"
#' #@param max_extension_distance numeric
#' #@param verbose logical, whether to print messages or not. Default is FALSE
#' #'
#' #@return geos_geometry vector of extended linestrings where extension was needed/possible, return vector is same length as number of input 'ids'
#' #@importFrom vctrs vec_c
#' #@importFrom geos geos_empty
#' get_lines_extended_to_geoms <- function(geos_geoms, 
#'                                       ids, 
#'                                       lines_to_cut, 
#'                                       lines_to_cut_indices, 
#'                                       direction = "head", 
#'                                       max_extension_distance = 2500,
#'                                       verbose = FALSE
#'                                       ) {
#'   
#'   if (!direction %in% c("head", "tail")) {
#'     stop("Invalid 'direction' value, must be one of 'head' or 'tail'")
#'   }
#'   
#'   # # preallocate vector that stores the extended geos linestrings
#'   extended_lines_vect      <- vctrs::vec_c(rep(geos::geos_empty(), length(ids)))
#'   
#'   # Precompute flags and a distance vector for extending
#'   is_within_polygon_flags      <- sapply(lines_to_cut_indices, function(i) { any(!is.na(i)) })
#'   distance_range               <- 1:max_extension_distance
#'   
#'   if(verbose) {
#'     message("Geoms length: ", length(geos_geoms),  "\n",
#'             "IDS length: ", length(ids), "\n",
#'             "lines to cut length: ", length(lines_to_cut_indices)
#'             )
#'     }
#'   
#'   # number of geometries that will be iterated over, keeping this variable to reference in message block  
#'   total                    <- length(ids)
#'   
#'   # output a message every ~10% intervals
#'   message_interval         <- total %/% 20
#'   
#'   # extension_dists <- vector(mode = "numeric", length = nrow(trans_data))
#'   for (i in seq_along(ids)) {
#'     
#'     # log percent complete
#'     if (message_interval != 0 && i %% message_interval == 0) {
#'       # get the percent complete
#'       percent_done <- round(i/total, 2) * 100
#'       message(i, " > ", percent_done, "% ") 
#'     }
#'     
#'     curr_id           <- ids[i]
#'     is_within_polygon <- is_within_polygon_flags[i]
#'     # is_within_polygon <- any(!is.na(lines_to_cut_indices[[i]]))
#'     
#'     polygon_index     <- lines_to_cut_indices[[i]]
#'     # message("transect: '", curr_id, "' - (", i, ")")
#'     # message("in polygon? ", is_within_polygon) 
#'     
#'     if (is_within_polygon) {
#'       # message("Extending linestring within polygons...")      
#'       # curr_geom  <- geos_geoms[[i]]
#'       
#'       # # TODO: not sure why i did this sort step... probably not needed
#'       # index_vect <- unlist(polygon_index)
#'       # index_vect <- sort(unlist(polygon_index))
#'       
#'       extended_line <- geos_bs_extend_to_geom(
#'         distances    = distance_range,
#'         line         = geos_geoms[[i]],
#'         geoms_to_cut = lines_to_cut[unlist(polygon_index)],
#'         direction    = direction
#'       )
#'       
#'       extended_lines_vect[i] <- extended_line
#'     } else {
#'       extended_lines_vect[i] <- geos_geoms[[i]]
#'     }
#'     
#'   }
#'  
#'   # mapview::mapview(sf::st_as_sf(extended_lines_vect), color = 'green') + 
#'   #   mapview::mapview(sf::st_as_sf(geos_geoms), color = 'red')
#'   
#'   return(extended_lines_vect)
#' }
#' 
#' #Calculate the minimum distance a line would need to extend to reach the boundary of the polygon/line that the input geometries are entirely within 
#' #Version of get_lines_extended_to_geoms() but only iterates through the IDs/geometries that are predetermined to be WITHIN A POLYGON
#' #@param geos_geoms list of geos_geometrys
#' #@param ids character vector
#' #@param lines_to_cut geos_linestrings
#' #@param lines_to_cut_indices numeric vector
#' #@param direction character, either "head", "tail" or "both"
#' #@param max_extension_distance numeric
#' #@param verbose logical, whether to print messages or not. Default is FALSE 
#' #@return geos_geometry vector of extended linestrings for the geometries within the lines to cut
#' #@importFrom vctrs vec_c
#' #@importFrom geos geos_empty 
#' get_lines_extended_to_geoms_subset <- function(geos_geoms, 
#'                                       ids, 
#'                                       lines_to_cut, 
#'                                       lines_to_cut_indices, 
#'                                       direction = "head", 
#'                                       max_extension_distance = 2500,
#'                                       verbose = FALSE
#'                                       ) {
#'   
#'   if (!direction %in% c("head", "tail")) {
#'     stop("Invalid 'direction' value, must be one of 'head' or 'tail'")
#'   }
#'   
#'   # Precompute flags and a distance vector for extending
#'   is_within_polygon_flags      <- sapply(lines_to_cut_indices, function(i) { any(!is.na(i)) })
#'   distance_range               <- 1:max_extension_distance
#'   
#'   geos_geoms_subset            <- geos_geoms[is_within_polygon_flags]
#'   ids_subset                   <- ids[is_within_polygon_flags]
#'   lines_to_cut_indices_subset  <- lines_to_cut_indices[is_within_polygon_flags]
#'   extended_lines_vect          <- vctrs::vec_c(rep(geos::geos_empty(), length(ids_subset)))
#'   
#'   if (verbose) {
#'     message("Geoms length: ", length(geos_geoms_subset),  "\n",
#'             "IDS length: ", length(ids_subset), "\n",
#'             "lines to cut length: ", length(lines_to_cut_indices_subset)
#'             )
#'   }
#'   
#'   # number of geometries that will be iterated over, keeping this variable to reference in message block  
#'   total                    <- length(ids_subset)
#'   
#'   # output a message every ~10% intervals
#'   message_interval         <- total %/% 20
#'   
#'   for (i in seq_along(ids_subset)) {
#'     
#'     # log percent complete
#'     if (message_interval != 0 && i %% message_interval == 0) {
#'       # get the percent complete
#'       percent_done <- round(i/total, 2) * 100
#'       message(i, " > ", percent_done, "% ") 
#'     }
#'     
#'     polygon_index <- lines_to_cut_indices_subset[[i]]
#'     
#'     extended_line <- geos_bs_extend_to_geom(
#'       distances    = distance_range,
#'       line         = geos_geoms[[i]],
#'       geoms_to_cut = lines_to_cut[unlist(polygon_index)],
#'       direction    = direction
#'     )
#'     
#'     extended_lines_vect[i] <- extended_line   
#'   }
#'  
#'   # mapview::mapview(sf::st_as_sf(extended_lines_vect), color = 'green') + 
#'   #   mapview::mapview(sf::st_as_sf(geos_geoms), color = 'red')
#'   
#'   return(extended_lines_vect)
#' }



# #Give a set of transecct linestrings and poylgons and get the minimum distance to extend each transect line (from both directions, to try and reach the edge of a "polygons")
# #WIP/internal function for extending transect lines out to FEMA 100 year flood plain polygons
# #@param transect_lines Set of Sf linestrigns to extend (only if the transect lines are ENTIRELLY within a polygons)
# #@param polygons set of sf polygons that transect lines should be exteneded 
# #@param flowlines set of Sf linestrings
# #@param crosswalk_id character, flowline ID that matches flowlines with transect lines. This crosswalk_id must appear are a column in both flowlines and transect_lines.
# #@param intersect_group_id character, name of a column in flowlines that should be used to group each transect with 1 or more flowlines. 
# #That is, when transects are checked to make sure they don't intersect 
# #other transects or other flowlines, this group ID will distinguise which flowlines a transect should be checked against.
# #The intersect_group_id must appear as a column in both flowlines and transect_lines dataframes
# #@param max_extension_distance numeric, maximum distance (meters) to extend a transect line 
# #in either direction to try and intersect one of the "polygons". Default is 3000m
# #@return sf linestring, with extended transect lines
# #@importFrom rmapshaper ms_simplify
# #@importFrom geos as_geos_geometry geos_intersects_matrix geos_simplify_preserve_topology geos_within_matrix geos_empty geos_point_start geos_point_end
# #@importFrom sf st_as_sf st_cast st_segmentize st_length st_drop_geometry st_geometry
# #@importFrom dplyr mutate case_when select left_join relocate n any_of
# #@importFrom lwgeom st_linesubstring
# #@importFrom wk wk_crs 
# #@importFrom nhdplusTools rename_geometry
# #@importFrom vctrs vec_c
# #@export
# extend_transects_to_polygons <- function(transect_lines, 
#                                          polygons, 
#                                          flowlines, 
#                                          crosswalk_id,
#                                          intersect_group_id = NULL,
#                                          max_extension_distance = 3000 
#                                          ) {
#   # library(sf)
#   # library(dplyr)
#   # # library(lwgeom)
#   # # library(wk)
#   # # library(vctrs)
#   # library(geos)
#   # # library(rmapshaper)
#   # 
#   # polygons <- sf::read_sf("/Users/anguswatters/Desktop/lynker-spatial/FEMA_BY_VPU/VPU_02/fema_vpu_02_output.gpkg")
#   # transect_lines <- sf::read_sf("/Users/anguswatters/Desktop/test_transects_02.gpkg")
#   # flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_flines_02.gpkg")
#   # crosswalk_id           = "hy_id"
#   # intersect_group_id     = "mainstem"
#   # max_extension_distance = 3000 
#   # # mapview::npts(polygons) 
#   # polygons <- rmapshaper::ms_simplify(polygons, keep_shapes = T, keep = 0.01, sys = TRUE, sys_mem = 16)
#   # mapview::npts(polygons)
#   # transect_lines         = transects
#   # 
#   # polygons               = fema
#   # flowlines              = dplyr::rename(flines, hy_id = crosswalk_id)
#   # # flowlines
#   # crosswalk_id = "hy_id"
#   # intersect_group_id     = "mainstem" 
#   # max_extension_distance = 3000
#   # transect_lines <- 
#   #   transect_lines  %>%
#   #   dplyr::left_join(
#   #     dplyr::select(sf::st_drop_geometry(flowlines),
#   #                   dplyr::any_of(crosswalk_id), 
#   #                   dplyr::any_of(intersect_group_id)
#   #                   ),
#   #     by = c(crosswalk_id)
#   #   )
#     if(!crosswalk_id %in% names(flowlines)) {
#       stop("crosswalk_id '", crosswalk_id, "' is not a column in 'flowlines' input,\n", 
#          "Please provide a valid crosswalk_id that crosswalks 'flowlines' to 'transect_lines'")
#       }
# 
#     if(!crosswalk_id %in% names(transect_lines)) {
#       stop("crosswalk_id '", crosswalk_id, "' is not a column in 'transect_lines' input,\n", 
#            "Please provide a valid crosswalk_id that crosswalks the 'transect_lines' to 'flowlines'")
#     }
#   
#    if(!intersect_group_id %in% names(flowlines)) {
#       stop("intersect_group_id '", intersect_group_id, "' is not a column in 'flowlines' input,\n", 
#          "Please provide a valid intersect_group_id that associates each transect line with 1 or more flowlines in 'flowlines'"
#          )
#       }
# 
#     if(!intersect_group_id %in% names(transect_lines)) {
#       stop("intersect_group_id '", intersect_group_id, "' is not a column in 'transect_lines' input,\n", 
#          "Please provide a valid intersect_group_id that associates each transect line with 1 or more flowlines in 'flowlines'"
#          )
#     }
#   
#   transect_lines  <- nhdplusTools::rename_geometry(transect_lines, "geometry")
#   flowlines       <- nhdplusTools::rename_geometry(flowlines, "geometry")
#   
#   # if(!is.null(intersect_group_id)) {
#   #   if(!intersect_group_id %in% names(flowlines)) {
#   #     stop("Invalid 'intersect_group_id' value, '", intersect_group_id, "' is not a column in 'flowlines'.\n", 
#   #     "Provide a valid 'intersect_group_id' value representing a column in 'flowlines' that should be used to ", 
#   #     "compare neighboring flowlines and transects for proper intersection logic")
#   #   }
#   #   # TODO: if the intersect_group_id column is not attached to the transects, then join it on
#   #   if(!intersect_group_id %in% names(transect_lines)) {
#   #     transect_lines <- 
#   #       transect_lines  %>%
#   #       dplyr::left_join(
#   #         dplyr::select(sf::st_drop_geometry(flowlines),crosswalk_id, dplyr::any_of(intersect_group_id)),
#   #         by = c("hy_id" = "crosswalk_id")
#   #       )
#   #     }
#   #   }
#   
#   # TODO: this should be a function argument OR removed, shouldn't probably forcibly and silently simplify the input polygons without user knowing..
#   # keep 10% of the original points for speed
#   # polygons <- rmapshaper::ms_simplify(polygons, keep_shapes = F, keep = 0.10)
#   
#   # polygons
#   transects_geos  <- geos::as_geos_geometry(transect_lines)
#   polygons_geos   <- geos::as_geos_geometry(polygons)     
#   
#   # polygons_geos %>% 
#     # geos::geos_type() %>% 
#     # unique()
#   
#   transects_polygons_matrix <- geos::geos_intersects_matrix(transects_geos, polygons_geos) 
#   polygons_transects_matrix <- geos::geos_intersects_matrix(polygons_geos, transects_geos) 
#   
#   # subset the transects and polygons to only those with intersections
#   intersect_transects  <- transect_lines[lengths(transects_polygons_matrix) != 0, ]
#   intersect_polygons   <- polygons_geos[lengths(polygons_transects_matrix) != 0]
#   
#   # # Convert our intersecting polygons to LINESTRINGS b/c we DON'T NEED polygons to calculate extension distances from our transect lines
#   # # This can be done with just linestrings (not sure if this is actually more performent but I'm pretty sure it is....)
#   # intersect_lines <- 
#   #   intersect_polygons %>% 
#   #   # geos::geos_make_valid() %>% 
#   #   sf::st_as_sf() %>% 
#   #   sf::st_cast("MULTILINESTRING") %>% 
#   #   geos::as_geos_geometry() %>% 
#   #   geos::geos_simplify_preserve_topology(250)
#   #   # geos::geos_simplify(250)
#   
#   # # mapview::mapview(sf::st_as_sf(no_simple_intersect_lines[1]), color="gold") +
#   #   mapview::mapview(sf::st_as_sf(no_simple_intersect_lines[2]), color= "green") + 
#   #   # mapview::mapview(sf::st_as_sf(no_simple_intersect_lines[3]), color = "gold") + 
#   # # mapview::mapview(sf::st_as_sf(intersect_lines[1]), color="green") +
#   
#   # use half of the shortest transect line as the segmentation length for all transects (ensures all transects will have a midpoint...?)
#   # TODO: Double check this logic.
#   min_segmentation <- min(intersect_transects$cs_lengthm %/% 2)
#   
#   # # make each transect line have way more segments so we can take a left and right half of each transect line
#   # segmented_trans  <- sf::st_segmentize(intersect_transects, min_segmentation)
#   
#   # Seperate the transect lines into LEFT and RIGHT halves
#   # We do this so we can check if a side of a transect is ENTIRELY WITHIN a polygon. 
#   # If the half is entirely within a polygon, 
#   left_trans <- 
#     # segmented_trans %>% 
#     sf::st_segmentize(intersect_transects, min_segmentation) %>% 
#     lwgeom::st_linesubstring(0, 0.50) %>% 
#     dplyr::mutate(
#       partition         = "left",
#       partition_lengthm = as.numeric(sf::st_length(geometry))
#     ) %>% 
#     # hydrofabric3D::add_tmp_id() %>% 
#     hydrofabric3D::add_tmp_id(x = crosswalk_id, y = cs_id) %>% 
#     dplyr::select(tmp_id, 
#                   # hy_id, 
#                   dplyr::any_of(crosswalk_id),
#                   cs_source, cs_id, cs_measure,  
#                   cs_lengthm,
#                   # is_extended,
#                   partition, partition_lengthm, geometry)
#   
#   # Find the distances from the right side of transect lines 
#   right_trans <- 
#     # segmented_trans %>% 
#     sf::st_segmentize(intersect_transects, min_segmentation) %>%  
#     lwgeom::st_linesubstring(0.50, 1) %>%
#     dplyr::mutate(
#       partition         = "right",
#       partition_lengthm = as.numeric(sf::st_length(geometry))
#     ) %>% 
#     # hydrofabric3D::add_tmp_id() %>%
#     hydrofabric3D::add_tmp_id(x = crosswalk_id, y = cs_id) %>% 
#     dplyr::select(tmp_id, 
#                   # hy_id,
#                   dplyr::any_of(crosswalk_id),
#                   cs_source, 
#                   cs_id, cs_measure,  
#                   cs_lengthm,
#                   # is_extended, 
#                   partition, partition_lengthm, geometry)
#   
#   # convert the transect geometries to geos types
#   # get the fema polygon indices for the transect halves that are completely within a fema polygon
#   # add the fema polygons index as a column to the transect dataframes
#   left_trans_geos     <- geos::as_geos_geometry(left_trans)
#   right_trans_geos    <- geos::as_geos_geometry(right_trans)
#   
#   left_within_matrix  <- geos::geos_within_matrix(left_trans_geos, intersect_polygons)
#   right_within_matrix <- geos::geos_within_matrix(right_trans_geos, intersect_polygons)
#   
#   left_within_vect    <- lapply(left_within_matrix, function(i) { if(length(i) > 0) { c(i) } else { c(NA) } })
#   right_within_vect   <- lapply(right_within_matrix, function(i) { if(length(i) > 0) { c(i) } else { c(NA) } })
#   
#   # add the fema polygon indexes as columns
#   left_trans$left_fema_index    <- left_within_vect
#   right_trans$right_fema_index  <- right_within_vect
#   
#   # add boolean columns whether the transect is fully within the FEMA polygons
#   left_trans <- 
#     left_trans %>% 
#     dplyr::mutate(
#       left_is_within_fema = dplyr::case_when(
#         !is.na(left_fema_index) ~ TRUE,
#         TRUE                    ~ FALSE
#       )
#     ) %>% 
#     dplyr::select(tmp_id, 
#                   # hy_id,
#                   dplyr::any_of(crosswalk_id),
#                   cs_source, cs_id, cs_measure,  
#                   cs_lengthm,
#                   partition, 
#                   partition_lengthm,
#                   left_fema_index, 
#                   left_is_within_fema, 
#                   geometry
#                   )
#   
#   right_trans <- 
#     right_trans %>% 
#     dplyr::mutate(
#       right_is_within_fema = dplyr::case_when(
#         !is.na(right_fema_index) ~ TRUE,
#         TRUE               ~ FALSE
#       )
#     ) %>% 
#     dplyr::select(tmp_id, 
#                   # hy_id, 
#                   dplyr::any_of(crosswalk_id),
#                   cs_source, cs_id, cs_measure,  
#                   cs_lengthm, 
#                   partition,
#                   partition_lengthm,
#                   right_fema_index, 
#                   right_is_within_fema, 
#                   geometry
#                   ) 
#   
#   # Convert our intersecting polygons to LINESTRINGS b/c we DON'T NEED polygons to calculate extension distances from our transect lines
#   # This can be done with just linestrings (not sure if this is actually more performent but I'm pretty sure it is....)
#   intersect_polygons <- 
#     intersect_polygons %>% 
#     # geos::geos_make_valid() %>% 
#     sf::st_as_sf() %>% 
#     sf::st_cast("MULTILINESTRING") %>% 
#     geos::as_geos_geometry() %>% 
#     geos::geos_simplify_preserve_topology(250)
#   # geos::geos_simplify(250)
#   
#   message("Generating left side distances....") 
#   
#   # profvis::profvis({
#   
#   left_distances  <- calc_extension_distances2(
#     geos_geoms             = left_trans_geos,
#     ids                    = left_trans$tmp_id,
#     lines_to_cut           = intersect_polygons,
#     lines_to_cut_indices   = left_trans$left_fema_index,
#     direction              = "head",
#     max_extension_distance = max_extension_distance
#   )
#   
#   # })
#   
#   message("Generating right side distances...")
#   
#   right_distances <- calc_extension_distances2(
#     geos_geoms             = right_trans_geos,
#     ids                    = right_trans$tmp_id,
#     lines_to_cut           = intersect_polygons,
#     lines_to_cut_indices   = right_trans$right_fema_index,
#     direction              = "tail",
#     max_extension_distance = max_extension_distance
#   )  
#   
#   left_trans$left_distance    <- left_distances
#   right_trans$right_distance  <- right_distances
# 
#   # TODO: this way the EXTENDED transects get returned instead of the DISTANCES TO EXTEND
#   # TODO: Somepoint, this is probably the better way, it would mean 1-2 less extension calculations
#   # TODO: on the other hand, the line extension is NOT very compute or memory intensive so
#   # message("Generating left side extensions...") 
#   # left_trans_geos <- get_lines_extended_to_geoms(
#   #   geos_geoms             = left_trans_geos,
#   #   ids                    = left_trans$tmp_id,
#   #   lines_to_cut           = intersect_lines,
#   #   lines_to_cut_indices   = left_trans$left_fema_index,
#   #   direction              = "head",
#   #   max_extension_distance = max_extension_distance
#   # )
#   # 
#   # message("Generating right side extensions...")
#   # right_trans_geos <- get_lines_extended_to_geoms(
#   #   geos_geoms             = right_trans_geos,
#   #   ids                    = right_trans$tmp_id,
#   #   lines_to_cut           = intersect_lines,
#   #   lines_to_cut_indices   = right_trans$right_fema_index,
#   #   direction              = "tail",
#   #   max_extension_distance = max_extension_distance
#   # )  
#   
#   # distance to extend LEFT and/or RIGHT for each hy_id/cs_id
#   extensions_by_id <- dplyr::left_join(
#     sf::st_drop_geometry(
#       dplyr::select(left_trans, 
#                     # hy_id, 
#                     dplyr::any_of(crosswalk_id),
#                     cs_id,
#                     left_distance)
#     ),
#     sf::st_drop_geometry(
#       dplyr::select(right_trans, 
#                     # hy_id, 
#                     dplyr::any_of(crosswalk_id),
#                     cs_id, 
#                     right_distance)
#     ),
#     by = c(crosswalk_id, "cs_id")
#   )
#   
#   # TODO: Add left/right extension distancces to transect data
#   # TODO: this can ultimately just be the "transects" variable, dont need to make new "transects_with_distances" variable
#   transect_lines <- 
#     transect_lines %>% 
#     dplyr::left_join(
#       extensions_by_id,
#       by = c(crosswalk_id, "cs_id")
#     ) %>% 
#     dplyr::mutate(
#       left_distance = dplyr::case_when(
#         is.na(left_distance) ~ 0,
#         TRUE                 ~ left_distance
#       ),
#       right_distance = dplyr::case_when(
#         is.na(right_distance) ~ 0,
#         TRUE                  ~ right_distance
#       )
#     ) %>% 
#     # hydrofabric3D::add_tmp_id()
#     hydrofabric3D::add_tmp_id(x = crosswalk_id, y = cs_id) 
#   
#   # rm(fema, polygons, left_trans_geos, right_trans_geos)
#   # gc()
#   
#   # format(object.size(flowlines), 'auto')
#   # profvis::profvis({
#   
#   # # TODO: if an intersect group crosswalk_id is given, then pull those columns as vectors to use for intersection checks in loop
#   # if(!is.null(intersect_group_id)) {
#   #   fline_group_id_array     <- flowlines[[intersect_group_id]]
#   #   transect_group_id_array  <- transect_lines[[intersect_group_id]]
#   # }  
#   
#   fline_id_array   <- flowlines[[crosswalk_id]]
#   # fline_id_array   <- flowlines$crosswalk_id
#   
#   # TODO: next time, change this function to ONLY process transects that have ANY extension distance, right now we iterate through ALL transects,
#   # TODO: and 'next' the ones with the no extension distance so doesn't really matter much but 
# 
#   # Convert the net object into a geos_geometry
#   flowlines_geos       <- geos::as_geos_geometry(flowlines)
#   
#   transect_crosswalk_id_array     <- transect_lines[[crosswalk_id]]
#   # transect_crosswalk_id_array     <- transect_lines$hy_id
#   transect_cs_id_array     <- transect_lines$cs_id
#   
#   # Intersect grouping IDs
#   fline_group_id_array      <- flowlines[[intersect_group_id]]
#   transect_group_id_array   <- transect_lines[[intersect_group_id]]
#   
#   # transect_geoms       <- geos::as_geos_geometry(transect_lines$geometry)
#   
#   left_distances       <- transect_lines$left_distance
#   right_distances      <- transect_lines$right_distance
#   
#   # # preallocate vector that stores the extension. distances
#   # new_transects <- vctrs::vec_c(rep(geos::geos_empty(), length(transect_crosswalk_id_array)))
#   
#   left_extended_flag   <- rep(FALSE, length(transect_crosswalk_id_array))   
#   right_extended_flag  <- rep(FALSE, length(transect_crosswalk_id_array))
#   both_extended_flag   <- rep(FALSE, length(transect_crosswalk_id_array))
#   
#   updated_left_distances    <- rep(0, length(transect_crosswalk_id_array))   
#   updated_right_distances   <- rep(0, length(transect_crosswalk_id_array))   
#   
#   # number of geometries that will be iterated over, keeping this variable to reference in message block  
#   total <- length(transect_crosswalk_id_array)
#   
#   # output a message every ~10% intervals
#   message_interval <- total %/% 20
#   number_of_skips = 0
#   
#   for (i in seq_along(transect_crosswalk_id_array)) {
#     
#     # Check if the iteration is a multiple of 100
#     if (message_interval != 0 && i %% message_interval == 0) {
#       percent_done <- round(i/total, 2) * 100
#       message(i, " > ", percent_done, "% ") 
#       message("Number of skips: ", number_of_skips)
#     }
#     
#     # get the current transect, hy_id, cs_id, flowline, and extension distances
#     current_trans <- transects_geos[i]
#     
#     current_hy_id <- transect_crosswalk_id_array[i]
#     current_cs_id <- transect_cs_id_array[i]
#     
#     # current_intersect_group_id <- transect_group_id_array[i]
#    
#     # TODO: might need this in case I do the is_valid_transect() check on just the single flowline
#     # current_fline      <- flowlines_geos[fline_id_array == current_hy_id]
#     
#     # TODO: these are the rest of the transects for this flowline
#     # neighbor_transects <- transects_geos[transect_crosswalk_id_array == current_hy_id & transect_cs_id_array != current_cs_id]
#     
#     # mapview::mapview(sf::st_as_sf(transects_geos[transect_crosswalk_id_array == current_hy_id & transect_cs_id_array != current_cs_id]), color = "red") +
#     #   mapview::mapview(sf::st_as_sf(current_trans), color = "green")
#   
#     left_distance_to_extend  <- left_distances[i]
#     right_distance_to_extend <- right_distances[i]
#     
#     no_extension_required <- (left_distance_to_extend == 0 && right_distance_to_extend == 0)
#     # no_extension_required <- is.na(left_distance_to_extend) && is.na(right_distance_to_extend)
#     # message("Transect tmp_id: ", curr_tmp_id, " - (", i, ")")
#     
#     if(no_extension_required) {
#       # message("Skipping -left/right extension are both 0")
#       number_of_skips = number_of_skips + 1
#       
#       next
#     }
#     
#     # message("Extending transect line left and right")
#     # extend the lines
#     left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
#                                                             left_distance_to_extend, "head")
#     right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
#                                                             right_distance_to_extend, "tail")
#     
#     # initial check to make sure the extended versions of the transects are valid
#     # mapview::mapview(sf::st_as_sf(flowlines_geos[fline_group_id_array == transect_group_id_array[i]])) + 
#     #     mapview::mapview(sf::st_as_sf(transects_geos[transect_group_id_array == transect_group_id_array[i]]), color = "red") + 
#     # mapview::mapview(sf::st_as_sf(current_trans))
#     
#     # TODO: version 1 
#     # # CHECKS WHOLE NETWORK OF FLOWLINES
#     # use_left_extension  <- is_valid_transect_line(left_extended_trans, transects_geos, flowlines_geos)
#     # use_right_extension <- is_valid_transect_line(right_extended_trans, transects_geos, flowlines_geos)
#     
#     # TODO version 2:
#     # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
#     
#     
#     # if (!is.null(intersect_group_id)) {
#      use_left_extension  <- is_valid_transect_line(
#                               left_extended_trans,
#                               transects_geos[transect_group_id_array == transect_group_id_array[i]], 
#                               flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
#                               # transects_geos,
#                               # flowlines_geos
#                               ) 
#       
#     # } else {
#     #   use_left_extension  <- is_valid_transect_line(left_extended_trans, 
#     #                                                 transects_geos[transect_crosswalk_id_array == current_hy_id], 
#     #                                                 flowlines_geos[fline_id_array == current_hy_id])
#     # }
#     
#     # if (!is.null(intersect_group_id)) {
#       
#       use_right_extension <- is_valid_transect_line(
#                               right_extended_trans, 
#                               transects_geos[transect_group_id_array == transect_group_id_array[i]], 
#                               flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
#                               )
#       
#     # } else {
#     #   
#     #   use_right_extension <- is_valid_transect_line(
#     #     right_extended_trans, 
#     #     transects_geos[transect_crosswalk_id_array == current_hy_id],
#     #     flowlines_geos[fline_id_array == current_hy_id]
#     #     # transects_geos,
#     #     # flowlines_geos
#     #   )
#     # }
#     # use_both_extensions <- use_left_extension && use_right_extension
#     
#     used_half_of_left  <- FALSE
#     used_half_of_right <- FALSE
#     
#     # TODO: Probably should precompute this division BEFORE the loop...
#     half_left_distance   <- ifelse(left_distance_to_extend > 0, left_distance_to_extend %/% 2, 0)
#     half_right_distance  <- ifelse(right_distance_to_extend > 0, right_distance_to_extend %/% 2, 0)
#     
#     # if we CAN'T use the original LEFT extension distance, 
#     # we try HALF the distance (or some distane less than we extended by in the first place)
#     if (!use_left_extension) {
#       
#       # half_left_distance   <- ifelse(left_distance_to_extend > 0, left_distance_to_extend %/% 2, 0)
#       left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
#                                                               half_left_distance, "head")
#       # TODO: verison 1
#       # use_left_extension  <- is_valid_transect_line(left_extended_trans, transects_geos, flowlines_geos)
#       
#       # TODO version 2:
#       # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
#       # if (!is.null(intersect_group_id)) {
#         use_left_extension  <- is_valid_transect_line(
#           left_extended_trans,
#           transects_geos[transect_group_id_array == transect_group_id_array[i]], 
#           flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
#           # transects_geos,
#           # flowlines_geos
#         ) 
#       # } else {
#         # use_left_extension  <- is_valid_transect_line(left_extended_trans, 
#                                                       # transects_geos[transect_crosswalk_id_array == current_hy_id], 
#                                                       # flowlines_geos[fline_id_array == current_hy_id])
#       # }
#     used_half_of_left <- ifelse(use_left_extension, TRUE,  FALSE)
#     }
#     
#     # if we CAN'T use the original RIGHT extension distance, 
#     # we try HALF the distance (or some distance less than we extended by in the first place)
#     if (!use_right_extension) {
#       
#       # half_right_distance  <- ifelse(right_distance_to_extend > 0, right_distance_to_extend %/% 2, 0)
#       right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
#                                                               half_right_distance, "tail")
#       
#       # TODO: version 1
#       # use_right_extension <- is_valid_transect_line(right_extended_trans, transects_geos, flowlines_geos)
# 
#       # TODO version 3
#       # if (!is.null(intersect_group_id)) {
#         
#         use_right_extension <- is_valid_transect_line(
#           right_extended_trans, 
#           transects_geos[transect_group_id_array == transect_group_id_array[i]], 
#           flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
#           )
#         
#       # } else {
#         
#         # use_right_extension <- is_valid_transect_line(
#           # right_extended_trans, 
#           # transects_geos[transect_crosswalk_id_array == current_hy_id],
#           # flowlines_geos[fline_id_array == current_hy_id]
#           # transects_geos,
#           # flowlines_geos
#         # )
#       # }
#       
#       used_half_of_right  <- ifelse(use_right_extension, TRUE,  FALSE)
#       
#       # mapview::mapview(sf::st_as_sf(current_trans), color = "red") + 
#       #   mapview::mapview(sf::st_as_sf(left_extended_trans), color = "green") + 
#       #   mapview::mapview(sf::st_as_sf(right_extended_trans), color = "green") +
#       #   mapview::mapview(sf::st_as_sf(left_extended_trans2), color = "dodgerblue") + 
#       #   mapview::mapview(sf::st_as_sf(right_extended_trans2), color = "dodgerblue") 
#       
#     }
#     
#     use_both_extensions <- use_left_extension && use_right_extension
#     
#     # # message("Checking left and right intersections with flowline...")
#     
#     # # mapview::mapview(sf::st_as_sf(merged_trans), color = "green") +
#     # #   mapview::mapview(sf::st_as_sf(left_start), col.region = "red") +
#     # #   mapview::mapview(sf::st_as_sf(left_end), col.region = "red") + 
#     # #   mapview::mapview(sf::st_as_sf(right_start), col.region = "dodgerblue") +
#     # #   mapview::mapview(sf::st_as_sf(right_end), col.region = "dodgerblue")
#     
#     # Get the start and end of both extended tranects
#     left_start  <- geos::geos_point_start(left_extended_trans)
#     left_end    <- geos::geos_point_end(left_extended_trans)
#     right_start <- geos::geos_point_start(right_extended_trans)
#     right_end   <- geos::geos_point_end(right_extended_trans)
# 
#     # }
#     # Extend in BOTH directions
#     if(use_both_extensions) {
#       # message("Extend direction: BOTH")
#       start  <- left_start
#       end    <- right_end
#       
#       # extend ONLY the left side
#     } else if(use_left_extension && !use_right_extension) {
#       # message("Extend direction: LEFT")       
#       start  <- left_start
#       end    <- left_end
#       
#       # Extend ONLY the right side
#     } else if(!use_left_extension && use_right_extension) {
#       # message("Extend direction: RIGHT")       
#       start  <- right_start
#       end    <- right_end
#       
#       # DO NOT extend either direction
#     } else {
#       # message("No extension")   
#       # TODO: Really dont need to do anything 
#       # TODO: in this scenario because we just use the original transect line
#       start  <- left_end
#       end    <- right_start
#     }
#     
#     line_crs      <- wk::wk_crs(current_trans)
#     updated_trans <- make_line_from_start_and_end_pts(start, end, line_crs)
#     
#     #   mapview::mapview(touched_flowlines, color = "dodgerblue") + 
#     #   mapview::mapview(sf::st_as_sf(current_trans), color = "red") +
#     #     mapview::mapview(sf::st_as_sf(left_extended_trans), color = "green") +
#     #     mapview::mapview(sf::st_as_sf(right_extended_trans), color = "green") +
#     #     mapview::mapview(sf::st_as_sf(updated_trans), color = "yellow")
#     
#     if(use_left_extension) {
#       left_extended_flag[i]  <- TRUE
#     }
#     
#     if(use_right_extension) {
#       right_extended_flag[i] <- TRUE
#     }
#     
#     if(use_both_extensions) {
#       both_extended_flag[i] <- TRUE
#     }
#     
#     if(used_half_of_left) {
#       updated_left_distances[i]  <- half_left_distance 
#     }
#     if(used_half_of_right) {
#       updated_right_distances[i] <- half_right_distance 
#     }
#     
#     # new_transects[i] <- updated_trans
#     transects_geos[i] <- updated_trans
#     
#   }      
#    
#    # })
#   # transects2 <- transects 
#   # dplyr::mutate(
#   #   new_cs_lengthm = as.numeric(sf::st_length(geom))
#   # ) %>% 
#   # dplyr::relocate(hy_id, cs_id, cs_lengthm, new_cs_lengthm)
#   # mapview::mapview(sf::st_as_sf(transects_geos), color = "red")
#   # transect_lines[lengths(sf::st_intersects(transect_lines)) == 1, ] %>% 
#   #   dplyr::group_by(hy_id) 
#   
#   # Update the "transects_to_extend" with new geos geometries ("geos_list")
#   sf::st_geometry(transect_lines) <- sf::st_geometry(sf::st_as_sf(transects_geos))
#   
#   transect_lines$left_is_extended   <- left_extended_flag
#   transect_lines$right_is_extended  <- right_extended_flag
#   
#   # remove self intersecting transects or not
#   transect_lines <-
#     transect_lines[lengths(sf::st_intersects(transect_lines)) == 1, ] %>% 
#     dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
#     # dplyr::group_by(hy_id) 
#     dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
#     dplyr::ungroup()
#   
#   # remove transects that intersect multiple flowlines
#   transect_lines <- 
#     transect_lines[lengths(sf::st_intersects(transect_lines, flowlines)) == 1, ] %>% 
#     dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
#     # dplyr::group_by(hy_id) 
#     dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
#     dplyr::ungroup()
#   
#   transect_lines <-
#     transect_lines %>% 
#     dplyr::mutate(
#       cs_lengthm = as.numeric(sf::st_length(geometry))
#     ) %>% 
#     dplyr::relocate(dplyr::any_of(crosswalk_id), cs_id, cs_lengthm)
#   
#   transect_lines <- move_geometry_to_last(transect_lines)
#   
#   return(transect_lines)
#   
# }    
# 
# #Calculate the minimum distance a line would need to extend to reach the boundary of the polygon/line that the input geometries are entirely within 
# #
# #@param geos_geoms list of geos_geometrys
# #@param ids character vector
# #@param lines_to_cut geos_linestrings
# #@param lines_to_cut_indices numeric vector
# #@param direction character, either "head", "tail" or "both"
# #@param max_extension_distance numeric
# #
# #@return numeric vector, distance to extend each geos_geoms
# #@importFrom vctrs vec_c
# calc_extension_distances2 <- function(geos_geoms, ids, lines_to_cut, lines_to_cut_indices, direction = "head", max_extension_distance = 2500) {
#   
#   if (!direction %in% c("head", "tail")) {
#     stop("Invalid 'direction' value, must be one of 'head' or 'tail'")
#   }
#   
#   # Precompute flags and a distance vector for extending
#   is_within_polygon_flags      <- sapply(lines_to_cut_indices, function(i) { any(!is.na(i)) })
#   distance_range               <- 1:max_extension_distance
#   
#   # preallocate vector that stores the extension. distances
#   extension_dists              <- vctrs::vec_c(rep(0, length(ids)))
#   
#   # number of geometries that will be iterated over, keeping this variable to reference in message block  
#   total                        <- length(ids)
#   
#   # output a message every ~10% intervals
#   message_interval             <- total %/% 20
#   
#   # extension_dists <- vector(mode = "numeric", length = nrow(trans_data))
#   for (i in seq_along(ids)) {
#     
#     # log percent complete
#     if (message_interval != 0 && i %% message_interval == 0) {
#       # get the percent complete
#       percent_done <- round(i/total, 2) * 100
#       message(i, " > ", percent_done, "% ") 
#     }
#   
#     curr_id           <- ids[i]
#     is_within_polygon <- is_within_polygon_flags[i]
#     # is_within_polygon <- any(!is.na(lines_to_cut_indices[[i]]))
#     # polygon_index     <- lines_to_cut_indices[[i]]
#     
#     # message("Transect: '", curr_id, "' - (", i, ")")
#     
#     if (is_within_polygon) {
#       # message("- Side of transect intersects with FEMA")
#       # message("\t > FEMA index: ", polygon_index)
#       # polygon_index     <- lines_to_cut_indices[[i]]
#       # curr_geom  <- geos_geoms[[i]]
#       
#       index_vect <- unlist(lines_to_cut_indices[[i]])
#       # index_vect <- unlist(polygon_index)
#       # index_vect <- sort(unlist(polygon_index))
#       
#       distance_to_extend <- geos_bs_distance(
#         distances    = distance_range, 
#         line         = geos_geoms[[i]],
#         geoms_to_cut = lines_to_cut[index_vect],
#         direction    = direction
#       )
#       
#       extension_dists[i] <- distance_to_extend
#     }
#     
#   }
#   
#   return(extension_dists)
# }


