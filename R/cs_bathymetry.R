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


#' Get the AHG estimated parabolas for each hy_id/cs_id cross section given a set of cross section points
#'
#' @param cs_pts dataframe or sf dataframe with "hy_id", "cs_id", "bottom" columns and 
#'  specififed "top_width", "depth", "dingman_r" columns  (see top_width, depth, and dingman_r arguments)
#' @param crosswalk_id character, ID column 
#' @importFrom dplyr bind_rows rowwise select group_by slice ungroup filter mutate
#' @importFrom AHGestimation cross_section
#' @importFrom tidyr unnest
#' @importFrom rlang as_name enquo
#' @return dataframe with a set of AHG points for each hy_id/cs_id in the input data, with AHG estimated X, Y, A point values that form a parabola
#' @noRd
#' @keywords internal
get_ahg_parabolas <- function(
    cs_pts = NULL,
    crosswalk_id = NULL
) {
  
  .data <- NULL
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "bottom", 
                     "TW", "DEPTH", "DINGMAN_R")
  
  # validate input graph
  is_valid <- validate_df(cs_pts, REQUIRED_COLS, "cs_pts")
  
  if (is.null(cs_pts)) {
    stop(
      paste0("'cs_pts' is NULL, provide a dataframe with the following columns:\n > ",
             paste0(REQUIRED_COLS, collapse = "\n > "))
    )
  }
  

  # this function will take in one of the AHG estimated parabolas, a bottom depth, and the estimated inchannel depth 
  # and if there are infinite/NaN/NA values in the AHG estimated parabolas depth ("Y"),
  # we pin the the left and right side of the parabola to the input bottomZ and set 
  # the bottom depth to the input bottom minus the given inchannel depth estimate
  # TODO: Probably won't be a permanent solution if the AHG estimation package ends up never returning NaN/Infinite values
  make_rectangle_if_needed <- function(parabola_df,  bottomZ, inchannel_depth) {
    contains_nan_or_inf_y_values <- any(is.nan(parabola_df$Y)) || any(is.na(parabola_df$Y)) || any(is.infinite(parabola_df$Y))
    
    if (contains_nan_or_inf_y_values) {
      parabola_df[1, 'Y']                        <- bottomZ
      parabola_df[nrow(parabola_df), 'Y']        <- bottomZ
      parabola_df[2:(nrow(parabola_df)-1), 'Y']  <- bottomZ - inchannel_depth
    }
    
    return(parabola_df)
    
  }
  
  # set the parabola to align with the cross section points bottom most points
  offset_and_partition_parabola <- function(parabola, bottomZ) {
    
    # indices of the left and right parabola halves
    left_half  = 1:(nrow(parabola) / 2)
    right_half = (1 + (nrow(parabola) / 2)):nrow(parabola)
    
    # get the left and right side of the parabolas
    left_parabola = parabola[left_half, ]
    right_parabola = parabola[right_half, ]
    
    # shift the Z values to have there max points be at the "bottom" of the "cs_pts" points
    left_parabola$Y <- left_parabola$Y + (bottomZ - max(left_parabola$Y))
    right_parabola$Y <- right_parabola$Y + (bottomZ - max(right_parabola$Y))
    
    left_parabola$partition  <- "left"
    right_parabola$partition <- "right"
   
    parabola <- dplyr::bind_rows(
      left_parabola, 
      right_parabola
    )
    
    return(parabola)
  } 
  
  # keep only a single row for each cross section
  ahg_parameters <- 
    cs_pts %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, bottom, TW, DEPTH, DINGMAN_R) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # remove any cross sections that are missing top_width, depth, or dingman_r
  set_aside <- 
    ahg_parameters %>% 
    dplyr::filter(is.na(TW) | is.na(DEPTH) | is.na(DINGMAN_R))
  
  ahg_parameters <- 
    ahg_parameters %>%
    hydrofabric3D::add_tmp_id() %>% 
    dplyr::filter(!tmp_id %in% hydrofabric3D::add_tmp_id(set_aside, x = crosswalk_id)$tmp_id) %>% 
    dplyr::select(-tmp_id) %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(
      parabola = list(
        AHGestimation::cross_section(
          r    = DINGMAN_R,
          TW   = TW,
          Ymax = DEPTH
        ) 
      )
    )
  
  ahg_parameters <- 
    ahg_parameters  %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      parabola = list(
        make_rectangle_if_needed(
          parabola,
          bottom,
          # NOTE: Not sure why i had this like this, should use the bottom as the anchor...? 
          DEPTH
        )
      )
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      parabola = list(
        offset_and_partition_parabola(
          parabola,
          bottom
        )
      )
    )
  
  # unnest the parabola dataframes and rename columns then return
  ahg_parameters <-
    ahg_parameters %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, parabola) %>% 
    tidyr::unnest(c(parabola)) %>% 
    dplyr::select(
        dplyr::any_of(crosswalk_id),
        cs_id, 
        ahg_index = ind, 
        ahg_x = x, 
        ahg_y = Y, 
        ahg_a = A, 
        partition
    ) 
  
  
  return(ahg_parameters)
}


#' Generate X/Y coordinates between a set of known points within a cross section
#' Used after inserting AHG estimated parabolas in between DEM cross sections points 
#'
#' @param cs_pts cross section points dataframe with missing X/Y coordinates between sets of known X/Y coordinates
#' @param crosswalk_id character, ID column 
#' @importFrom dplyr filter group_by summarize ungroup left_join select ends_with mutate n bind_rows
#' @importFrom tidyr unnest
#' @return dataframe, input dataframe with X/Y coordinates filled in for missing hy_id/cs_id X/Y values
#' @noRd
#' @keywords internal
fill_missing_ahg_coords <- function(cs_pts, 
                                    crosswalk_id = NULL
                                    ) {
  
  #Fix the missing X/Y coordinates (NAs) from the inserted AHG Parabola points in a set of cross section points
  seq_between_start_and_end <- function(start, end, n) {
    
    if (n == 0) {
      return(NULL)
    }
    
    # Generate new X / Y coordinates
    coords <- seq(start, end, length.out = n + 2)
    
    return(coords[2:(length(coords) - 1)])
  }
  
  # get the first and last coordinates beforee the missing NA X/Y points
  start_and_end_coords <- get_coords_around_parabola(cs_pts, crosswalk_id = crosswalk_id)
  
  start_and_end_pts_ids <- 
    cs_pts %>% 
    dplyr::filter(!is_dem_point) %>%  
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::summarize(
      start_pt_id = min(pt_id, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup()
  
  # get only the rows with the parabola w/ missing X/Y coords
  parabola_pts <- 
    cs_pts %>% 
    dplyr::filter(!is_dem_point) %>%  
    dplyr::left_join(
      start_and_end_coords,
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::select(
        dplyr::any_of(crosswalk_id), 
        cs_id, pt_id, 
        X, Y, dplyr::ends_with("_start"), dplyr::ends_with("end")
      ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      n = dplyr::n()
    ) %>% 
    dplyr::ungroup()
  
  # generate coordinates for each parabola
  parabola_coords <- 
    parabola_pts %>% 
    dplyr::left_join(
      start_and_end_pts_ids, 
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::select(
        dplyr::any_of(crosswalk_id), 
        cs_id, start_pt_id, 
        dplyr::ends_with("_start"), dplyr::ends_with("end"), n
      ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::slice(1)  %>% 
    dplyr::mutate(
      X = list(seq_between_start_and_end(X_start, X_end, n)),
      Y = list(seq_between_start_and_end(Y_start, Y_end, n))
    ) %>% 
    tidyr::unnest(c(X, Y)) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      pt_id = (start_pt_id - 1) + (1:dplyr::n())
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      dplyr::any_of(crosswalk_id), 
      cs_id, pt_id, X, Y
      ) 
  
  # join the new parabola X/Y points with the rest of the original data, dropping the old NA X/Y coordinates
  parabolas_with_coords <- 
    cs_pts %>% 
    dplyr::filter(!is_dem_point) %>% 
    dplyr::select(-X, -Y) %>% 
    dplyr::left_join(
      parabola_coords,
      by = c(crosswalk_id, "cs_id", "pt_id")
    )
  
  pts_with_fixed_coords <- 
    dplyr::bind_rows(
      dplyr::filter(cs_pts, is_dem_point),
      parabolas_with_coords
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::arrange(relative_distance, .by_group = TRUE) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      pt_id = 1:dplyr::n()
    ) %>%
    dplyr::ungroup()
  
  return(pts_with_fixed_coords)
}

#' Get the coordinates surrounding a set of missing AHG X/Y coordinates.
#' 
#' @param cs_pts dataframe with cross section points, (required cols, "hy_id", "cs_id", "X", "Y", "is_dem_point")
#' @param crosswalk_id character, ID column 
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select group_by mutate lag lead case_when filter ungroup left_join
#' @return dataframe with each hy_id/cs_id cross section containing a value for X_start, X_end, Y_start, Y_end, representing the points surrounding the AHG inserted points
#' @noRd
#' @keywords internal
get_coords_around_parabola <- function(cs_pts, crosswalk_id = NULL) {
  
  fill_value <- -999999999
  
  X_coords <-
    cs_pts %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, X, is_dem_point) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      X = ifelse(is.na(X), fill_value, X)
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      next_X_is_missing  = dplyr::case_when(
        dplyr::lead(X) == fill_value       ~ TRUE,
        TRUE                                  ~ FALSE
      ),
      prev_X_is_missing  = dplyr::case_when(
        dplyr::lag(X) == fill_value          ~ TRUE,
        TRUE                                 ~ FALSE
      )
    ) %>% 
    dplyr::filter((is_dem_point & next_X_is_missing) | (is_dem_point & prev_X_is_missing)) %>% 
    dplyr::mutate(
      start_or_end  = dplyr::case_when(
        (is_dem_point & next_X_is_missing)   ~ "X_start",
        (is_dem_point & prev_X_is_missing)   ~ "X_end"
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, X, is_dem_point, start_or_end)
  
  # pivot so each cross sections is a single row with a "X_start" and "X_end" point
  X_coords <-
    X_coords %>% 
    tidyr::pivot_wider(
      id_cols = c(dplyr::any_of(crosswalk_id), cs_id),
      names_from = start_or_end,
      values_from = X
    )
  
  Y_coords <-
    cs_pts %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, Y, is_dem_point) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      Y = ifelse(is.na(Y), fill_value, Y)
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      next_Y_is_missing  = dplyr::case_when(
        dplyr::lead(Y) == fill_value       ~ TRUE,
        TRUE                               ~ FALSE
      ),
      prev_Y_is_missing  = dplyr::case_when(
        dplyr::lag(Y) == fill_value        ~ TRUE,
        TRUE                               ~ FALSE
      )
    ) %>% 
    dplyr::filter((is_dem_point & next_Y_is_missing) | (is_dem_point & prev_Y_is_missing)) %>% 
    dplyr::mutate(
      start_or_end  = dplyr::case_when(
        (is_dem_point & next_Y_is_missing)   ~ "Y_start",
        (is_dem_point & prev_Y_is_missing)   ~ "Y_end"
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, Y, is_dem_point, start_or_end)
  
  # pivot so each cross sections is a single row with a "X_start" and "X_end" point 
  Y_coords <-
    Y_coords %>% 
    tidyr::pivot_wider(
      id_cols = c(dplyr::any_of(crosswalk_id), cs_id),
      names_from = start_or_end,
      values_from = Y
    )
  
  coords_around_parabola <- dplyr::left_join(
    X_coords,
    Y_coords,
    by = c(crosswalk_id, "cs_id")
  )
  
  return(coords_around_parabola)
}

#' Check that all cross sections points have a prescribed top width less than the total cross section length
#' @description
#' If a set of cross section points has a top width length that is longer than the cross sections length, then a new top width and Y max (depth) value
#' are given so that the estimated shape is able to properly fit into the cross sections. 
#' The cross sections length (meters) minus 1 meter is used as the new top width and 
#' the new Y max (depth) value is derived from the original ratio between the prescribed top width and Y max
#' @param cs_pts dataframe or sf dataframe with "hy_id", "cs_id", "pt_id", "Z", "relative_distance", "cs_lengthm", "class", "point_type", "TW", "DEPTH", "DINGMAN_R"
#' @param crosswalk_id character, ID column 
#' @importFrom rlang as_name enquo
#' @importFrom dplyr group_by mutate case_when sym ungroup select filter slice
#' @return cs_pts dataframe with updated "top_width" and "depth" column values
#' @noRd
#' @keywords internal
fix_oversized_topwidths <- function(
    cs_pts = NULL,
    crosswalk_id = NULL
) {
  
  # use this to get a new scaled Ymax value given a known Top width, Ymax (ML generated in this case)
  # and an expected new Top width (new_TW), this function is used to handle 
  # the case when the ML estimated top width is GREATER than the DEM cross section length
  scale_DEPTH_to_TW <- function(TW, Ymax, new_TW) {
    
    new_DEPTH <- Ymax * (new_TW / TW)
    
    return(new_DEPTH)
    
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "Z", "relative_distance", "cs_lengthm", 
                     "class", 
                     "point_type", "TW", "DEPTH", "DINGMAN_R")
  
  # validate input graph
  is_valid <- validate_df(cs_pts, REQUIRED_COLS, "cs_pts")
  
  if (is.null(cs_pts)) {
    stop(
      paste0("'cs_pts' is NULL, provide a dataframe with the following columns:\n > ",
             paste0(REQUIRED_COLS, collapse = "\n > "))
    )
  }
  
  # keep track of the original column order for reordering at the end
  starting_col_order  <- names(cs_pts)
  
  # Determine the distance interval for each cross section
  # we're going to use this value to 
  # derive a new Top width for each cross section if 
  # the cross section length is less than the prescribed top width, 
  # we round the distance interval UP sure we are not underestimating the interval
  distance_between_pts <- 
    cs_pts %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, relative_distance) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
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
  
  # add the distance interval values to the cross section points
  cs_pts <- 
    cs_pts %>% 
    dplyr::left_join(
      distance_between_pts, 
      by = c(crosswalk_id, "cs_id")
    )
  
  updated_TW_and_Ymax <- 
    cs_pts %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::mutate(
      new_DEPTH = dplyr::case_when(
        TW >= cs_lengthm ~ scale_DEPTH_to_TW(TW, 
                                            DEPTH, 
                                            cs_lengthm - distance_interval
                                             # !!dplyr::sym(length_colname) - 1 # TODO: arbitrarily remove 1 meter from the length to 
                                             # TODO: make sure topwidth/depth is SMALLER than cross section length 
                                             # TODO: so the AHG parabola points can fit within cross section 
                                             # !!dplyr::sym(length_colname) 
                                             # TODO: Original method ^^^ (no subtraction)
        ), 
        TRUE                        ~ DEPTH
      ),
      new_TW = dplyr::case_when(
        TW >= cs_lengthm ~ cs_lengthm - distance_interval, # TODO: Same arbitrary subtraction of 1 meter as above note ^^^
        TRUE                          ~ TW
      ),
      has_new_DEPTH = new_DEPTH != DEPTH,
      has_new_TW    = new_TW != TW,
      fixed_TW      = has_new_DEPTH | has_new_TW
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(
      -has_new_DEPTH,
      -has_new_TW,
      -TW,
      -DEPTH
    )
  
  # number of cross sections that had their TW/Depths changed to fit into the cross section properly
  number_fixed_TWs <- 
    updated_TW_and_Ymax %>% 
    dplyr::filter(fixed_TW) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    nrow()
  
  if (number_fixed_TWs > 0) {
    warning(
      "Had to fix ", number_fixed_TWs, " cross section(s) top width/depth values to make sure", 
      "\nthe prescribed topwidth is not greater than or equal to the length of",
      "\nthe entire cross section (meters), the cross section(s) total length is", 
      "\nused as the new TW and the ratio of the prescribed TW to the prescribed depth", 
      "\nis used to calculate a new depth (Y) value."
    )
  }
  
  # Drop the flag column that says if the top width had to be fixed
  updated_TW_and_Ymax <- dplyr::select(updated_TW_and_Ymax, 
                                       -distance_interval,
                                       -fixed_TW
                                       )
  
  # any starting columns in the original data
  ending_col_order  <- names(updated_TW_and_Ymax)
  
  # change the new_TW and new_DEPTH columns to match the original input TW/Depth column names
  ending_col_order[ending_col_order == "new_TW"] <- "TW"
  ending_col_order[ending_col_order == "new_DEPTH"] <- "DEPTH"

  # # update the names
  names(updated_TW_and_Ymax) <- ending_col_order
  
  return(updated_TW_and_Ymax)
}


#' Calculate the length between the leftmost and rightmost bottom point in each cross section 
#'
#' @param cs_pts dataframe, or sf dataframe of cross section points
#' @param crosswalk_id character, ID column 
#' @importFrom dplyr select mutate case_when group_by lag ungroup filter summarise left_join across any_of
#' @return summarized dataframe of input cs_pts dataframe with a bottom_length value for each hy_id/cs_id
#' @export
get_cs_bottom_length <- function(cs_pts, 
                                 crosswalk_id = NULL) {
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "relative_distance", "point_type")
  
  # validate input graph
  is_valid <- validate_df(cs_pts, REQUIRED_COLS, "cs_pts")
  
  # get the distance between cross section pts in each cross section,
  # this will be used as a default for bottom length in case bottom length is 0
  interval_distances <- 
    cs_pts %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, relative_distance) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
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
    cs_pts %>% 
    dplyr::filter(point_type == "bottom") %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, relative_distance) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::summarise(
      bottom_start = min(relative_distance, na.rm = TRUE),
      bottom_end   = max(relative_distance, na.rm = TRUE)
    ) %>% 
    dplyr::left_join(
      interval_distances, 
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
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
  
  return(bottom_lengths)
  
}

#' Given provide inchannel widths and depths to a set of cross section points and derive estimated shapes
#' @description
#' Takes in a point of cross section points with added top width (TW), depth (DEPTH), and dingman_r (DINGMAN_R) columns  
#' @param cs_pts dataframe or sf dataframe. Default is NULL
#' @param crosswalk_id character, ID column 
#' @importFrom dplyr bind_rows select mutate n case_when summarise ungroup group_by filter relocate left_join slice slice_max rename arrange
#' @importFrom AHGestimation cross_section
#' @importFrom stats median
#' @importFrom rlang as_name enquo
#' @return dataframe or sf dataframe with AHG estimated points injected into the input cross section points
#' @export
add_cs_bathymetry <- function(
    cs_pts = NULL,
    crosswalk_id = NULL
) {
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "Z", "relative_distance", "cs_lengthm", 
                     "class", 
                     "point_type", "TW", "DEPTH", "DINGMAN_R")
  
  # validate input graph
  is_valid <- validate_df(cs_pts, REQUIRED_COLS, "cs_pts")
  
  if (is.null(cs_pts)) {
    stop(
      paste0("'cs_pts' is NULL, provide a dataframe with the following columns:\n> ",
             paste0(c(crosswalk_id, 'cs_id', 'Z', 'bottom', 'relative_distance', 
                      'point_type', 'class', 
                      'top_width - (specify via "top_width" argument)',  
                      'depth - (specify via "depth" argument)', 
                      'dingman_r - (specify via "dingman_r" argument)'
             ), 
             collapse = "\n> "))
    )
  }
  
  # Replace any topwidth values that are GREATER than the actual cross section length (meters)
  cs_pts <- fix_oversized_topwidths(
    cs_pts = cs_pts,
    crosswalk_id = crosswalk_id
  )

  # generate AHG parabolas for each hy_id/cs_id in the cross section points 
  # using the provided top_widths, depths, and dingman's R
  ahg_parabolas <- get_ahg_parabolas(
    cs_pts = cs_pts,
    crosswalk_id = crosswalk_id
  )
  
  # store the maximum X on the left side of the parabola for later use
  ahg_left_max <- 
    ahg_parabolas %>% 
    dplyr::filter(partition == "left") %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::summarise(left_max = max(ahg_x, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  # ------------------------------------------------------------------------------------------------
  # ---- Partition input cross section points (left/right) ----
  # ------------------------------------------------------------------------------------------------
  
  # split the cross section into a left and right half, from the midpoint of the bottom
  # and then join on the maximum X point of the LEFT half of the AHG parabolas 
  # this paritioned set of cross sections will ultimately get the AHG parabolas inserted in between
  # the left and right partitions
  partioned_cs <- 
    cs_pts %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      bottom_midpoint = dplyr::case_when(
        point_type == "bottom" ~ relative_distance,
        TRUE                   ~ NA
      ),
      bottom_midpoint = stats::median(bottom_midpoint, na.rm = TRUE)
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      cs_partition = dplyr::case_when(
        relative_distance < bottom_midpoint ~ "left_cs",
        TRUE                                ~ "right_cs"
      )
    )  %>% 
    dplyr::left_join(
      ahg_left_max,
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::ungroup()
  
  #  get the midpoint value for each hy_id/cs_id so we can use them during the shifting process 
  midpoints <- 
    partioned_cs %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, bottom_midpoint) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # ------------------------------------------------------------------------------------------------
  # ---- Process LEFT side of cross section and parabola ----
  # ------------------------------------------------------------------------------------------------
  
  # grab just the left cross sections and remove any points that will be swallowed by the newly inserted AHG estimates 
  # And also determine the offset of the left parabolas X points, the left_start will be joined back onto the AHG parabolas
  left_cs <- 
    partioned_cs %>% 
    dplyr::filter(cs_partition == "left_cs") %>%  
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::filter(
      relative_distance < (bottom_midpoint - max(left_max)) | relative_distance == 0 # TODO: testing this new condition out
    ) %>% 
    dplyr::mutate(
      left_start = bottom_midpoint - max(left_max)
    ) %>% 
    dplyr::ungroup()
  
  left_starts <- 
    left_cs %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, left_start) %>%  
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # offset the left parabolas X points using the left_start value
  left_parabolas <-
    ahg_parabolas %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::filter(partition == "left") %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(
      left_starts,
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      ahg_x = ahg_x + left_start
    ) %>% 
    dplyr::ungroup()
  
  # ------------------------------------------------------------------------------------------------
  # ---- Process RIGHT side of cross section and parabola ----
  # ------------------------------------------------------------------------------------------------
  
  # subset cross section to the RIGHT of the midpoint
  right_cs <-
    partioned_cs %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::filter(relative_distance > bottom_midpoint) %>% 
    dplyr::ungroup()
  
  right_parabolas <- 
    ahg_parabolas %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::filter(partition == "right") %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(
      ahg_left_max,
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::left_join(
      midpoints,
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      right_start = bottom_midpoint + ((ahg_x) - left_max)
    ) %>% 
    dplyr::ungroup()
  
  # ----------------------------------------------------------------------------------------------------------------
  # ------- Still reviewing this additon ---------
  # --- This ties back to the fix_oversized_topwidths() function applied at the beginning -----
  # ----------------------------------------------------------------------------------------------------------------
  # TODO: Newly added to deal with situations where the right side of the parabola is TOO LONG,
  # TODO: and will go past the outside of the predefined cross section length 
  # TODO: This all needs to evaluated and double checked to make sure it makes 
  # TODO: sense hydrologically and won't break the standard "good" case
  # for each cross section, we isolate the total length of the cross section 
  # to make sure that the parabola is not going past the edge of the cross section
  total_cross_section_length <-
    right_cs %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::slice_max(relative_distance, n = 1) %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), 
                  cs_id, 
                  max_right_position = relative_distance) %>% 
    dplyr::ungroup() 
  
  # from the right side of the parabola, 
  # we remove any parabola points that would be past
  # the last right side cross section points
  right_parabolas <- 
    right_parabolas %>% 
    dplyr::left_join(
      total_cross_section_length,
      by = c(crosswalk_id, "cs_id")
    )  %>% 
    dplyr::filter(right_start < max_right_position) 
  
  # ----------------------------------------------------------------------------------------------------------------
  # TODO: Above still needs review ^^^ 
  # ----------------------------------------------------------------------------------------------------------------
  
  # getting the starting X value for the RIGHT side of the parabola
  max_right_starting_pts <- 
    right_parabolas %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::summarise(
      right_start_max = max(right_start, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup()
  
  # removing cross section point that will be replaced by right_parabola points
  right_cs <- 
    right_cs %>% 
    dplyr::left_join(
      max_right_starting_pts, 
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::filter(
      relative_distance > right_start_max
    ) %>% 
    dplyr::ungroup()
  
  # ---------------------------------------------------------------------------------------------------
  # ---- MERGE the left and right sides of the parabolas -----
  # ---------------------------------------------------------------------------------------------------
  
  right_parabolas <- 
    right_parabolas %>% 
    dplyr::select(-ahg_x) %>% 
    dplyr::rename(ahg_x = right_start) %>% 
    dplyr::select(
      dplyr::any_of(crosswalk_id),
      cs_id, 
      ahg_index, ahg_x, ahg_y, ahg_a,
      partition
    )
  
  left_parabolas <- 
    left_parabolas %>% 
    dplyr::select(
      dplyr::any_of(crosswalk_id),
      cs_id, 
      ahg_index, ahg_x, ahg_y, ahg_a,
      partition
    )
  
  # merge
  parabolas <- dplyr::bind_rows(left_parabolas, right_parabolas)
  
  # reorder to parabolas by X values so they are in order from left to right for each hy_id/cs_id
  parabolas <- 
    parabolas %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::arrange(ahg_x, .by_group = TRUE) %>% 
    dplyr::ungroup()
  
  # select relevant columns and adjust the names so 
  # the AHG parabola can be inserted nicely with the original cross sections
  # NOTE: 
  # AHG X values == "relative_distance" in cs_pts
  # AHG Y values == "Z" in cs_pts
  parabolas <- 
    parabolas %>% 
    dplyr::select(
      dplyr::any_of(crosswalk_id),
      cs_id,
      relative_distance = ahg_x,
      Z                 = ahg_y
    )
  
  # ---------------------------------------------------------------------------------------------------
  # ---- Insert the parabolas in between the LEFT and RIGHT cross section partitions -----
  # ---------------------------------------------------------------------------------------------------
  
  # drop unneeded columns
  left_cs <- dplyr::select(left_cs, 
                           -left_start, -left_max, -bottom_midpoint, -cs_partition)
  right_cs <- dplyr::select(right_cs, 
                            -right_start_max, -left_max, -bottom_midpoint, -cs_partition)
  
  # combine left cross section points, parabola, and right cross section points
  # and then reorder each cross section (hy_id/cs_id) by the relative distance 
  # so all the points are in correct order
  out_cs <-
    dplyr::bind_rows(
      dplyr::mutate(left_cs, is_dem_point = TRUE),
      dplyr::mutate(parabolas, is_dem_point = FALSE),
      dplyr::mutate(right_cs, is_dem_point = TRUE),
    ) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::filter(relative_distance >= 0) %>% # TODO: testing out this condition as well
    dplyr::arrange(relative_distance, .by_group = TRUE) %>% 
    dplyr::ungroup()
  
  # Assign / renumber the "pt_ids" and 
  # set the "point_types" of the inserted parabola points to "bottom" type
  out_cs <- 
    out_cs %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%  
    dplyr::mutate(
      pt_id      = 1:dplyr::n(),
      class = dplyr::case_when(
        is.na(class)      ~ "bottom",
        TRUE              ~ class
      ),
      point_type = dplyr::case_when(
        is.na(point_type) ~ "bottom",
        TRUE              ~ point_type
      )
    ) %>% 
    dplyr::ungroup()
  
  tryCatch({
    # message("Generate XY coordinates for AHG estimated points...")
    out_cs <- fill_missing_ahg_coords(cs_pts = out_cs, crosswalk_id = crosswalk_id)

  }, error = function(cond) {

    message("Failed to fix X/Y coordinates for estimated bathymetry points, returning cross section points with inserted bathymetry with missing X/Y values")
    message(conditionMessage(cond))

    # Choose a return value in case of error
    return(out_cs)

  })
  
  return(out_cs)
  
}