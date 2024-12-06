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

#' @title Extend an sf linestring dataframe by a percent of the lines length
#'
#' @param x linestring sf dataframe
#' @param crosswalk_id character, unique ID column name 
#' @param pct numeric, percent of line to extend linestring by in both directions
#' @param length_col character, name of the output length column name. Default is NULL which will create a length column name of "geom_length".
#' @importFrom dplyr group_by mutate ungroup rename across any_of sym
#' @importFrom sf st_length st_geometry st_drop_geometry st_as_sf st_crs
#' @importFrom hydroloom rename_geometry
#' @return sf dataframe with extended linestring geometries
#' @export
extend_by_percent <- function(
    x, 
    crosswalk_id = NULL,
    pct          = 0.5, 
    length_col   = NULL
) {
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id")
  
  # validate input graph
  is_valid <- validate_df(x, REQUIRED_COLS, "x")
  
  # rename the geometry to "geom"
  x <- hydroloom::rename_geometry(x, "geometry")
  
  is_null_length_col <- is.null(length_col)
  
  if(is_null_length_col) {
    
    length_col      <- "geom_length"
    # length_col = "cs_lengthm"
  }
  
  # length_col is NULL then set it to "cs_lengthm"
  x <- add_length_col(x = x, length_col = length_col)
  
  # if given 0% or less, just return the input data with added length column
  if (pct <= 0) {
    return(x)  
  }
  
  # extend linestrings by pct * length of line
  extended_df <-
    x %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::mutate(
      extended_geom = geos_extend_line(
        geometry, 
        distance = (
          ((pct)*(!!dplyr::sym(length_col))) / 2
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

  # add length column if specified w/ updated geometry lengths
  extended_df <- add_length_col(x = extended_df, length_col = length_col)
  
  return(extended_df)
  
}

# TODO: Change this function name to "adjust_transect_lengths()"

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
#' @noRd
#' @keywords internal
extend_by_length <- function(
    x, 
    crosswalk_id = NULL,
    length_vector,
    length_col = NULL
) {
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id")
  
  # validate input graph
  is_valid <- validate_df(x, REQUIRED_COLS, "x")
  
  # rename the geometry to "geom"
  x <- hydroloom::rename_geometry(x, "geometry")
  
  is_null_length_col <- is.null(length_col)
  
  if(is_null_length_col) {
    
    length_col      <- "geom_length"
    # length_col = "cs_lengthm"
  }
  
  # length_col is NULL then set it to "cs_lengthm"
  x <- add_length_col(x = x, length_col = length_col)
  
  # TODO: this needs a check to make sure a column with this name does NOT already exist
  # add length vector col to extended lines out by in next step
  x$length_vector_col <- length_vector
  
  # extend linestrings by pct * length of line
  extended_df <-
    x %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::mutate(
      extended_geom = geos_extend_line(
        geometry, 
        distance = length_vector_col,
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
  
  # add length column if specified w/ updated geometry lengths
  extended_df <- add_length_col(x = extended_df, length_col = length_col)
  
  # drop the added length_vector_col
  extended_df <- dplyr::select(
                    extended_df, 
                    -length_vector_col
                  )
  
  return(extended_df)
  
}


# TODO: Change this function name to "adjust_transect_lengths()"

#' @title Extend/shrink an sf linestring dataframe by a specified lengths vector
#'
#' @param x linestring sf dataframe, requires an 
#' @param crosswalk_id character, unique ID column name 
#' @param dir direction to extend/shrink transect from, either "left" or "right". Default is "left". 
#' @param length_col character, name of the column in "x" that has the length of the linestring (meters)
#' @importFrom dplyr group_by mutate ungroup rename
#' @importFrom sf st_length st_geometry st_drop_geometry st_as_sf st_crs
#' @importFrom hydroloom rename_geometry
#' @return sf dataframe with extended linestring geometries
#' @export
adjust_transect_lengths <- function(
    x, 
    crosswalk_id = NULL,
    dir = "left",
    length_col = NULL
) {
  
  direction <- switch(
    dir,
    "left"  = "head",
    "right" = "tail",
    "both" = "both"
  )
  
  extension_col_name <- switch(
    dir,
    "left"  = "left_distance",
    "right" = "right_distance",
    "both"  = "extension_distance"
  )
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", extension_col_name)
  
  # validate input graph
  is_valid <- validate_df(x, REQUIRED_COLS, "x")
  
  # rename the geometry to "geom"
  x <- hydroloom::rename_geometry(x, "geometry")
  
  is_null_length_col <- is.null(length_col)
  
  if(is_null_length_col) {
    length_col      <- "geom_length"
  }
  
  # length_col is NULL then set it to "cs_lengthm"
  x <- add_length_col(x = x, length_col = length_col)
  
  # extend linestrings by pct * length of line
  extended_df <-
    x %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      extended_geom = geos_extend_line(
        geometry, 
        distance = !!dplyr::sym(extension_col_name),
        # distance = (pct)*(!!dplyr::sym(length_col)),
        dir      = direction
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
  
  # add length column if specified w/ updated geometry lengths
  extended_df <- add_length_col(x = extended_df, length_col = length_col)
  
  return(extended_df)
  
}



#' Update a flagged set of transects by shortening them by the given left_distance and right_distance 
#' Requires 'left_distance' and 'right_distance' columns to specify how much to adjust flagged transects by 
#' @param x sf dataframe of transects 
#' @param crosswalk_id character, unique ID column
#' @param reindex_cs_ids logical, whether to generate a new 1-n set of cs_ids or to return the original identifiers
#' @importFrom hydroloom rename_geometry
#' @importFrom dplyr left_join mutate any_of select
#'
#' @return sf dataframe of transects with updated geometries 
#' @export
adjust_flagged_transects <- function(
    x, 
    crosswalk_id = NULL,
    reindex_cs_ids = FALSE
) {
  
  # set geometry column names at beginning
  x    <- hydroloom::rename_geometry(x, "geometry")
  
  # validate input datas
  is_valid_df        <- validate_df(x, 
                                    c(crosswalk_id, "cs_id", "cs_lengthm", "cs_measure", 
                                      "flagged", 
                                      "left_distance", "right_distance", 
                                      "geometry"), 
                                    "x"
  )
  
  # shorten the unimproved set of transects
  x <- shorten_flagged_transects(
    transects = x,
    crosswalk_id = crosswalk_id
  )
  
  # select relevant columns
  x <- 
    x %>%  
    dplyr::select(
      dplyr::any_of(
        c(
          crosswalk_id,
          "cs_id",
          "cs_lengthm", 
          "cs_measure",
          "ds_distance",
          "sinuosity",
          "cs_source",
          "geometry"
        )
      )
    )

  # re-index the cs_ids to make sure there are 1-number of transects for each crosswalk_id and that there are NO gaps between cs_ids
  if (reindex_cs_ids) {
    warning("Re-indexing cs_ids may result in a mismatch between unique crosswalk_id/cs_ids in input 'transects' and the output unique crosswalk_id/cs_ids")
    x <- renumber_cs_ids(x, crosswalk_id = crosswalk_id)
  }
  
  return(x)
  
}

# # Update a flagged set of transects by shortening them by the given extension_distance
# #
# # @param x sf dataframe of transects 
# # @param crosswalk_id character, unique ID column
# # @param reindex_cs_ids logical, whether to generate a new 1-n set of cs_ids or to return the original identifiers
# # @importFrom hydroloom rename_geometry
# # @importFrom dplyr left_join mutate any_of select
# #
# # @return sf dataframe of transects with updated geometries 
# # @export
# adjust_flagged_transects2 <- function(
#     x, 
#     crosswalk_id = NULL,
#     reindex_cs_ids = FALSE
# ) {
  
#   # set geometry column names at beginning
#   x    <- hydroloom::rename_geometry(x, "geometry")
  
#   # validate input datas
#   is_valid_df        <- validate_df(x, 
#                                     c(crosswalk_id, "cs_id", "cs_lengthm", "cs_measure", 
#                                       "flagged", "extension_distance", "geometry"), 
#                                     "x"
#   )
  
#   # shorten the unimproved set of transects
#   x <- shorten_flagged_transects2(
#     transects = x,
#     crosswalk_id = crosswalk_id
#   )
  
#   # select relevent columns
#   x <- 
#     x %>%  
#     dplyr::select(
#       dplyr::any_of(
#         c(
#           crosswalk_id,
#           "cs_id",
#           "cs_lengthm", 
#           "cs_measure",
#           "ds_distance",
#           "sinuosity",
#           "cs_source",
#           "geometry"
#         )
#       )
#     )
  
#   # re-index the cs_ids to make sure there are 1-number of transects for each crosswalk_id and that there are NO gaps between cs_ids
#   if (reindex_cs_ids) {
#     warning("Re-indexing cs_ids may result in a mismatch between unique crosswalk_id/cs_ids in input 'transects' and the output unique crosswalk_id/cs_ids")
#     x <- renumber_cs_ids(x, crosswalk_id = crosswalk_id)
#   }
  
#   return(x)
  
# }


#' Takes any transects with multiple intersections that was extended, and shortens them by the distance specified in the "extension_distance" column
#'
#' @param x sf dataframe of transects, requires a crosswalk_id, cs_id, cs_lengthm, extension_distance, and geometry column
#' @param crosswalk_id character, unique ID column
#' 
#' @importFrom sf st_intersects st_geometry
#' @return sf dataframe of transects with any transects that intersect multiple other transects being shortened by -extension_distance
shorten_multi_transect_intersecting_extended_transects <- function(x, crosswalk_id = NULL) {
  
  suppressWarnings({
    
    is_valid_df <- validate_df(x, 
                               c(crosswalk_id, "cs_id", "cs_lengthm", 
                               "is_extended", 
                               "left_distance", "right_distance",
                               "geometry"), 
                               "x")
    
    # Try and fix any transects that cross multiple transects AND were extended
    is_multi_intersecting <- (lengths(sf::st_intersects(x)) != 1) & x$is_extended
    # is_multi_intersecting <- lengths(sf::st_intersects(transects)) != 1
    
    has_no_multi_intersects <- !any(is_multi_intersecting)
    
    # return early if NO multi intersections exist
    if (has_no_multi_intersects) {
      return(x)
    }
    
    # x[is_multi_intersecting, ]$left_distance
    shortened_transects <- x[is_multi_intersecting, ] %>% 
                                    dplyr::mutate(
                                      left_distance = -abs(left_distance),
                                      right_distance = -abs(right_distance)
                                    )
    
    # shorten the left side
    shortened_transects  <- adjust_transect_lengths(
                                      x             = shortened_transects, 
                                      crosswalk_id  = crosswalk_id, 
                                      dir           = "left",
                                      length_col    = "cs_lengthm"
                                    )
    
    # shorten the right side
    shortened_transects  <- adjust_transect_lengths(
                                      x             = shortened_transects, 
                                      crosswalk_id  = crosswalk_id, 
                                      dir           = "right",
                                      length_col    = "cs_lengthm"
                                    )
    
    # set is_extended to FALSE for clarity  
    shortened_transects$is_extended     <- FALSE 
    shortened_transects$left_distance   <- 0
    shortened_transects$right_distance  <- 0
    
    if ("left_is_extended" %in% names(shortened_transects)) {
      shortened_transects$left_is_extended <- FALSE
    }
    
    if ("right_is_extended" %in% names(shortened_transects)) {
      shortened_transects$right_is_extended <- FALSE
    }
    
    
    # replace the geometries with the shorter transects
    sf::st_geometry(x[is_multi_intersecting, ])  <- sf::st_geometry(shortened_transects)
    
    # update the lengths and is_extended flag to align with the above replacement of geometries
    x <- add_length_col(x, "cs_lengthm") 
    
    # transects[is_multi_intersecting, ]$cs_lengthm       <- shortened_transects$cs_lengthm
    x[is_multi_intersecting, ]$is_extended        <- shortened_transects$is_extended
    x[is_multi_intersecting, ]$left_distance      <- shortened_transects$left_distance
    x[is_multi_intersecting, ]$right_distance     <- shortened_transects$right_distance
    
    if ("left_is_extended" %in% names(x)) {
      x[is_multi_intersecting, ]$left_is_extended      <- shortened_transects$left_is_extended
    }
    
    if ("right_is_extended" %in% names(x)) {
      x[is_multi_intersecting, ]$right_is_extended     <- shortened_transects$right_is_extended
    }
    
    return(x)
    
  })
}


#' Takes any transects that was extended and with multiple flowline intersections, and shortens them by the distance specified in the "extension_distance" column
#'
#' @param x sf dataframe of transects, requires a crosswalk_id, cs_id, cs_lengthm, extension_distance, and geometry column
#' @param flowlines sf dataframe of flowline LINESTRINGS to compare to  
#' @param crosswalk_id character, unique ID column
#' 
#' @importFrom sf st_intersects st_geometry
#' @return sf dataframe of transects with any transects that intersect multiple other transects being shortened by -extension_distance
shorten_multi_flowline_intersecting_extended_transects <- function(x, 
                                                          flowlines, 
                                                          crosswalk_id = NULL) {
  # flowlines 
  suppressWarnings({
    
    is_valid_df <- validate_df(x, 
                               c(crosswalk_id, "cs_id", "cs_lengthm", 
                               "is_extended", "left_distance", "right_distance", 
                               # "extension_distance", 
                               "geometry"), 
                               "x")
    
    # Try and fix any transects that cross multiple
    is_multi_intersecting_flowlines <- (lengths(sf::st_intersects(x, flowlines)) != 1) & x$is_extended 
  
    has_no_multi_intersects <- !any(is_multi_intersecting_flowlines)
    
    # return early if NO multi intersections exist
    if (has_no_multi_intersects) {
      return(x)
    }
    
    shortened_transects <- x[is_multi_intersecting_flowlines, ] %>% 
      dplyr::mutate(
        left_distance = -abs(left_distance),
        right_distance = -abs(right_distance)
      )
    
    # shorten the left side
    shortened_transects  <- adjust_transect_lengths(
      x             = shortened_transects, 
      crosswalk_id  = crosswalk_id, 
      dir           = "left",
      length_col    = "cs_lengthm"
    )
    # shorten the right side
    shortened_transects  <- adjust_transect_lengths(
      x             = shortened_transects, 
      crosswalk_id  = crosswalk_id, 
      dir           = "right",
      length_col    = "cs_lengthm"
    )
    
    # set is_extended to FALSE for clarity  
    shortened_transects$is_extended <- FALSE 
    shortened_transects$left_distance   <- 0
    shortened_transects$right_distance  <- 0
    
    if ("left_is_extended" %in% names(shortened_transects)) {
      shortened_transects$left_is_extended <- FALSE
    }
    
    if ("right_is_extended" %in% names(shortened_transects)) {
      shortened_transects$right_is_extended <- FALSE
    }
    
    # replace the geometries with the shorter transects
    sf::st_geometry(x[is_multi_intersecting_flowlines, ]) <- sf::st_geometry(shortened_transects)
    
    # update the lengths and is_extended flag to align with the above replacement of geometries
    x <- add_length_col(x, "cs_lengthm") 
    
    x[is_multi_intersecting_flowlines, ]$is_extended      <- shortened_transects$is_extended
    x[is_multi_intersecting_flowlines, ]$left_distance    <- shortened_transects$left_distance
    x[is_multi_intersecting_flowlines, ]$right_distance   <- shortened_transects$right_distance
    
    if ("left_is_extended" %in% names(x)) {
      x[is_multi_intersecting_flowlines, ]$left_is_extended      <- shortened_transects$left_is_extended
    }
    
    if ("right_is_extended" %in% names(x)) {
      x[is_multi_intersecting_flowlines, ]$right_is_extended     <- shortened_transects$right_is_extended
    }
    
    return(x)
    
  })
}

#' Shorten specific flagged transects by specified distance
#' Shorten transects by 'left_distance' and 'right_distance' if they have a 'flagged' column value of TRUE 
#'
#' @param transects sf dataframe of LINESTRINGS, requires 'left_distance' and 'right_distance' columns to specify how much to flagged transects by
#' @param crosswalk_id character, unique ID column
#' 
#' @importFrom sf st_intersects st_geometry
#' @return sf dataframe of transects with shortened transects where flagged is TRUE 
#' @noRd
#' @keywords internal
shorten_flagged_transects <- function(transects, crosswalk_id = NULL) {
  
  suppressWarnings({
    
    is_valid_df <- validate_df(transects, 
                               
                               c(crosswalk_id, "cs_id", "cs_lengthm", 
                                 "flagged", 
                                 "left_distance", "right_distance",
                                 "geometry"),
                               
                               "transects")
    
    # which rows are flagged for shortening?  
    is_flagged      <- transects$flagged
    
    has_no_flags    <- !any(is_flagged)
    
    # return early if NO rows were flagged for shortening 
    if (has_no_flags) {
      return(transects)
    }
    
    # get the negative of the left and right distance values
    shortened_transects <- transects[is_flagged, ] %>% 
      dplyr::mutate(
        left_distance  = -abs(left_distance),
        right_distance = -abs(right_distance)
      )
    
    # shorten the left side
    shortened_transects  <- adjust_transect_lengths(
      x             = shortened_transects, 
      crosswalk_id  = crosswalk_id, 
      dir           = "left",
      length_col    = "cs_lengthm"
    )
    
    # shorten the right side
    shortened_transects  <- adjust_transect_lengths(
      x             = shortened_transects, 
      crosswalk_id  = crosswalk_id, 
      dir           = "right",
      length_col    = "cs_lengthm"
    )
    
    # set is_extended to FALSE for clarity  
    shortened_transects$left_distance   <- 0
    shortened_transects$right_distance  <- 0
    
    # replace the geometries with the shorter transects
    sf::st_geometry(transects[is_flagged, ])  <- sf::st_geometry(shortened_transects)
    
    # update the lengths to align with the above replacement of geometries
    transects <- add_length_col(transects, "cs_lengthm") 
    
    transects[is_flagged, ]$left_distance   <- 0
    transects[is_flagged, ]$right_distance  <- 0 
    
    return(transects)
    
  })
}


# # Shorten specific flagged transects by specified distance
# # Shorten transects by 'extension_distance' if they have a 'flagged' column value of TRUE 
# #
# # @param transects sf dataframe of LINESTRINGS, requires an 'extension_distance' column to specify how much to flagged transects by
# # @param crosswalk_id character, unique ID column
# # 
# # @importFrom sf st_intersects st_geometry
# # @return sf dataframe of transects with shortened transects where flagged is TRUE 
# # @noRd
# # @keywords internal
# shorten_flagged_transects2 <- function(transects, crosswalk_id = NULL) {
  
#   suppressWarnings({
    
#     is_valid_df <- validate_df(transects, 
#                                c(crosswalk_id, "cs_id", "cs_lengthm", "flagged", "extension_distance", "geometry"), 
#                                "transects")
    
#     # wehich rows are flagged for shortening?  
#     is_flagged      <- transects$flagged
    
#     has_no_flags    <- !any(is_flagged)
    
#     # return early if NO rows were flagged for shortening 
#     if (has_no_flags) {
#       return(transects)
#     }
    
#     # reduce the length of each transect by extension_distance (from BOTH sides)
#     shortened_transects  <- extend_by_length(
#       x             = transects[is_flagged, ], 
#       crosswalk_id  = crosswalk_id, 
#       length_vector = -transects[is_flagged, ]$extension_distance, 
#       length_col    = "cs_lengthm"
#     ) 
    
    
#     # replace the geometries with the shorter transects
#     sf::st_geometry(transects[is_flagged, ])  <- sf::st_geometry(shortened_transects)
    
#     # update the lengths to align with the above replacement of geometries
#     transects <- add_length_col(transects, "cs_lengthm") 
    
#     return(transects)
    
#   })
# }