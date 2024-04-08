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
    "prev_Y_is_missing", "right_start", "right_start_max", "start_or_end", "start_pt_id"
  )
)

###############################################################################
##### ---- VERSION 2 ------
###############################################################################

#' Get the AHG estimated parabolas for each hy_id/cs_id cross section given a set of cross section points
#'
#' @param cross_section_pts dataframe or sf dataframe with "hy_id", "cs_id", "bottom" columns and 
#'  specififed "top_width", "depth", "dingman_r" columns  (see top_width, depth, and dingman_r arguments)
#' @importFrom dplyr bind_rows rowwise select group_by slice ungroup filter mutate
#' @importFrom AHGestimation cross_section
#' @importFrom tidyr unnest
#' @importFrom rlang as_name enquo
#' @return dataframe with a set of AHG points for each hy_id/cs_id in the input data, with AHG estimated X, Y, A point values that form a parabola
get_ahg_parabolas <- function(
    cross_section_pts = NULL
) {
  
  # cross_section_pts <-
  #   cs_ml %>%
  #   dplyr::slice(1:20)
  # cross_section_pts = cross_section_pts
  # # top_width         = {{top_width}},
  # # depth             = {{depth}},
  # top_width         = "owp_tw_inchan"
  # depth             = "owp_y_inchan"
  # length_col        = "cs_lengthm"
  # cross_section_pts = dplyr::slice(inchannel_cs, 1:10)
  # top_width         = "owp_tw_inchan"
  # depth             = "owp_y_inchan"
  # dingman_r         = "owp_dingman_r"
  
  req_cols <- c("hy_id", "cs_id", "bottom", 
                "TW", "DEPTH", "DINGMAN_R")
  
  if (is.null(cross_section_pts)) {
    stop(
      paste0("'cross_section_pts' is NULL, provide a dataframe with the following columns:\n > ",
             paste0(req_cols, collapse = "\n > "))
    )
  }
  
  if (!all(req_cols %in% names(cross_section_pts))) {
    stop(paste0('"cross_section_pts" is missing columns, must include all of:\n > ', paste0(req_cols, collapse = "\n > ") ))
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
    
    # shift the Z values to have there max points be at the "bottom" of the "cross_section_pts" points
    left_parabola$Y <- left_parabola$Y + (bottomZ - max(left_parabola$Y))
    right_parabola$Y <- right_parabola$Y + (bottomZ - max(right_parabola$Y))
    
    left_parabola$partition  <- "left"
    right_parabola$partition <- "right"
    # left_parabola <- 
    #   left_parabola %>% 
    #   dplyr::mutate(
    #     left_max = max(x, na.rm = TRUE)
    #   )
    parabola <- dplyr::bind_rows(
      left_parabola, 
      right_parabola
    )
    
    return(parabola)
  } 
  
  # keep only a single row for each cross section
  ahg_parameters <- 
    cross_section_pts %>% 
    dplyr::select(hy_id, cs_id, bottom, 
                  TW, DEPTH, DINGMAN_R
                  # dplyr::any_of(c(top_width, depth, dingman_r))
                  # .data[[top_width]],
                  # .data[[depth]],
                  # .data[[dingman_r]]
                  # !!dplyr::sym(top_width),
                  # !!dplyr::sym(depth),
                  # !!dplyr::sym(dingman_r),
                  # {{top_width}}, {{depth}}, {{dingman_r}}
    ) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # remove any cross sections that are missing top_width, depth, or dingman_r
  set_aside <- 
    ahg_parameters %>% 
    dplyr::filter(is.na(TW) | is.na(DEPTH) | is.na(DINGMAN_R))
    # dplyr::filter(is.na(.data[[top_width]]) | is.na(.data[[depth]]) | is.na(.data[[dingman_r]]) )
  # dplyr::filter(is.na(get(top_width)) | is.na(get(depth)) | is.na(get(dingman_r)) )
  # dplyr::filter(is.na({{top_width}}) | is.na({{depth}}) | is.na({{dingman_r}}) )
  
  ahg_parameters <- 
    ahg_parameters %>%
    hydrofabric3D::add_tmp_id() %>% 
    dplyr::filter(!tmp_id %in% hydrofabric3D::add_tmp_id(set_aside)$tmp_id) %>% 
    dplyr::select(-tmp_id) %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(
      parabola = list(
        AHGestimation::cross_section(
          r    = DINGMAN_R,
          TW   = TW,
          Ymax = DEPTH
          # r    = get(dingman_r),
          # TW   = get(top_width),
          # Ymax = get(depth)
          # r    = .data[[dingman_r]],
          # TW   = .data[[top_width]],
          # Ymax = .data[[depth]]
          # r    = {{dingman_r}},
          # TW   = {{top_width}},
          # Ymax = {{depth}}
        ) 
        # dplyr::mutate(
        #   Y = dplyr::case_when(
        #     ind == 2 ~ NaN,
        #     TRUE ~ Y
        #   )
        # )
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
          # {{top_width}},
          # {{depth}}
          # get(top_width),
          # get(depth)
          # .data[[depth]]
          DEPTH
        )
        # parabola = purrr::map2(parabola, bottom, {{depth}}, make_rectangle_if_needed)
      )
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      parabola = list(
        offset_and_partition_parabola(
          parabola,
          bottom
        )
        # parabola = purrr::map2(parabola, bottom, {{depth}}, make_rectangle_if_needed)
      )
    )
  
  # unnest the parabola dataframes and rename columns then return
  ahg_parameters <-
    ahg_parameters %>% 
    dplyr::select(hy_id, cs_id, parabola) %>% 
    tidyr::unnest(c(parabola)) %>% 
    # dplyr::group_by(hy_id, cs_id, partition) %>% 
    # dplyr::group_by(hy_id, cs_id) %>% 
    # dplyr::mutate(
    #   left_max = max(x, na.rm = TRUE)
    # ) %>% 
    # dplyr::ungroup() %>% 
    dplyr::select(hy_id, cs_id, 
                  ahg_index = ind, 
                  ahg_x = x, 
                  ahg_y = Y, 
                  ahg_a = A, 
                  # left_max,
                  partition
    ) 
  
  # plot(ahg_parameters$ahg_y)
  
  return(ahg_parameters)
}


#' Generate X/Y coordinates between a set of known points within a cross section
#' Used after inserting AHG estimated parabolas in between DEM cross sections points 
#'
#' @param cross_section_pts cross section points dataframe with missing X/Y coordinates between sets of known X/Y coordinates
#' @importFrom dplyr filter group_by summarize ungroup left_join select ends_with mutate n bind_rows
#' @importFrom tidyr unnest
#' @return dataframe, input dataframe with X/Y coordinates filled in for missing hy_id/cs_id X/Y values
#' @export
fill_missing_ahg_coords <- function(cross_section_pts) {
  
  # cross_section_pts <-
  #   cs_bathy_inchannel %>% 
  #   # dplyr::slice(1:159)
  #   dplyr::slice(1:171375)
  
  # cross_section_pts <- cs_bathy_inchannel
  
  #Fix the missing X/Y coordinates (NAs) from the inserted AHG Parabola points in a set of cross section points
  seq_between_start_and_end <- function(start, end, n) {
    # df = fix_coords
    
    if (n == 0) {
      return(NULL)
    }
    
    # Generate new X / Y coordinates
    # coords <- seq(start, end, length.out = n )
    coords <- seq(start, end, length.out = n + 2)
    
    # return(coords)
    return(coords[2:(length(coords) - 1)])
  }
  
  
  # get the first and last coordinates beforee the missing NA X/Y points
  start_and_end_coords <- get_coords_around_parabola(cross_section_pts)
  
  # 
  start_and_end_pts_ids <- 
    cross_section_pts %>% 
    dplyr::filter(!is_dem_point) %>%  
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::summarize(
      start_pt_id = min(pt_id, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup()
  
  # get only the rows with the parabola w/ missing X/Y coords
  parabola_pts <- 
    cross_section_pts %>% 
    dplyr::filter(!is_dem_point) %>%  
    dplyr::left_join(
      start_and_end_coords,
      by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::select(hy_id, cs_id, pt_id, X, Y, 
                  dplyr::ends_with("_start"), dplyr::ends_with("end")) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      n = dplyr::n()
    ) %>% 
    dplyr::ungroup()
  
  # generate coordinates for each parabola
  parabola_coords <- 
    parabola_pts %>% 
    dplyr::left_join(
      start_and_end_pts_ids, 
      by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::select(hy_id, cs_id, start_pt_id, dplyr::ends_with("_start"), dplyr::ends_with("end"), n) %>% 
    dplyr::group_by(hy_id, cs_id)  %>% 
    dplyr::slice(1)  %>% 
    dplyr::mutate(
      X = list(seq_between_start_and_end(X_start, X_end, n)),
      Y = list(seq_between_start_and_end(Y_start, Y_end, n))
    ) %>% 
    tidyr::unnest(c(X, Y)) %>% 
    dplyr::group_by(hy_id, cs_id)  %>%
    dplyr::mutate(
      pt_id = (start_pt_id - 1) + (1:dplyr::n())
      # pt_id = start_pt_id + (0:(dplyr::n()-1))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(hy_id, cs_id, pt_id, X, Y) 
  # dplyr::select(hy_id, cs_id, X, Y)
  
  # join the new parabola X/Y points with the rest of the original data, dropping the old NA X/Y coordinates
  parabolas_with_coords <- 
    cross_section_pts %>% 
    dplyr::filter(!is_dem_point) %>% 
    dplyr::select(-X, -Y) %>% 
    dplyr::left_join(
      parabola_coords,
      by = c("hy_id", "cs_id", "pt_id")
    )
  
  pts_with_fixed_coords <- 
    dplyr::bind_rows(
      dplyr::filter(cross_section_pts, is_dem_point),
      parabolas_with_coords
    ) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::arrange(relative_distance, .by_group = TRUE) %>%
    # dplyr::arrange(pt_id, .by_group = TRUE) %>%
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      pt_id = 1:dplyr::n()
    ) %>%
    dplyr::ungroup()
  
  # pts_with_fixed_coords %>% 
  # dplyr::group_by(hy_id, cs_id) 
  # 
  # pts_with_fixed_coords %>% 
  #   dplyr::slice(1:15000) %>% 
  #   sf::st_as_sf(coords = c("X", "Y"), crs = 5070) %>% 
  #   mapview::mapview()
  return(pts_with_fixed_coords)
}

#' Get the coordinates surrounding a set of missing AHG X/Y coordinates.
#' 
#' @param cross_section_pts dataframe with cross section points, (required cols, "hy_id", "cs_id", "X", "Y", "is_dem_point")
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr select group_by mutate lag lead case_when filter ungroup left_join
#' @return dataframe with each hy_id/cs_id cross section containing a value for X_start, X_end, Y_start, Y_end, representing the points surrounding the AHG inserted points
#' @export
get_coords_around_parabola <- function(cross_section_pts) {
  
  fill_value <- -999999999
  
  X_coords <-
    cross_section_pts %>% 
    dplyr::select(hy_id, cs_id, X, is_dem_point) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      X = ifelse(is.na(X), fill_value, X)
    ) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      next_X_is_missing  = dplyr::case_when(
        dplyr::lead(X) == fill_value       ~ TRUE,
        # is_dem_point & is.na(dplyr::lead(X))  ~ TRUE,
        TRUE                                  ~ FALSE
      ),
      prev_X_is_missing  = dplyr::case_when(
        dplyr::lag(X) == fill_value          ~ TRUE,
        # is_dem_point & is.na(dplyr::lead(X))  ~ TRUE,
        TRUE                                 ~ FALSE
      )
    ) %>% 
    dplyr::filter((is_dem_point & next_X_is_missing) | (is_dem_point & prev_X_is_missing)) %>% 
    dplyr::mutate(
      start_or_end  = dplyr::case_when(
        # is_dem_point & is.na(dplyr::lead(X))  ~ TRUE,
        (is_dem_point & next_X_is_missing)   ~ "X_start",
        (is_dem_point & prev_X_is_missing)   ~ "X_end"
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(hy_id, cs_id, X, is_dem_point, start_or_end)
  
  # X_coords %>% 
  #   dplyr::group_by(hy_id, cs_id)
  
  # pivot so each cross sections is a single row with a "X_start" and "X_end" point 
  X_coords <- 
    X_coords %>% 
    # dplyr::select(-is_dem_point) %>% 
    tidyr::pivot_wider(
      id_cols = c(hy_id, cs_id),
      names_from = start_or_end,
      values_from = X
    )
  
  Y_coords <-
    cross_section_pts %>% 
    dplyr::select(hy_id, cs_id, Y, is_dem_point) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      Y = ifelse(is.na(Y), fill_value, Y)
    ) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
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
    dplyr::select(hy_id, cs_id, Y, is_dem_point, start_or_end)
  
  # pivot so each cross sections is a single row with a "X_start" and "X_end" point 
  Y_coords <-
    Y_coords %>% 
    tidyr::pivot_wider(
      id_cols = c(hy_id, cs_id),
      names_from = start_or_end,
      values_from = Y
    )
  
  coords_around_parabola <- dplyr::left_join(
    X_coords,
    Y_coords,
    by = c("hy_id", "cs_id")
  )
  
  return(coords_around_parabola)
}

#' Check that all cross sections points have a prescribed top width less than the total cross section length
#' @description
#' If a set of cross section points has a top width length that is longer than the cross sections length, then a new top width and Y max (depth) value
#' are given so that the estimated shape is able to properly fit into the cross sections. 
#' The cross sections length (meters) minus 1 meter is used as the new top width and 
#' the new Y max (depth) value is derived from the original ratio between the prescribed top width and Y max
#' @param cross_section_pts dataframe or sf dataframe with "hy_id", "cs_id", "pt_id", "Z", "relative_distance", "cs_lengthm", "class", "point_type", "TW", "DEPTH", "DINGMAN_R"
#' @importFrom rlang as_name enquo
#' @importFrom dplyr group_by mutate case_when sym ungroup select filter slice
#' @return cross_section_pts dataframe with updated "top_width" and "depth" column values
fix_oversized_topwidths <- function(
    cross_section_pts = NULL
) {
  # cross_section_pts = cross_section_pts
  # top_width         = top_width
  # depth             = depth
  # length_col        = "cs_lengthm"
  
  # cross_section_pts = dplyr::slice(inchannel_cs, 1:100000)
  # top_width         = cross_section_pts$owp_y_inchan
  # depth             = cross_section_pts$owp_y_inchan
  # dingman_r         = cross_section_pts$owp_dingman_r
  
  # use this to get a new scaled Ymax value given a known Top width, Ymax (ML generated in this case)
  # and an expected new Top width (new_TW), this function is used to handle 
  # the case when the ML estimated top width is GREATER than the DEM cross section length
  scale_DEPTH_to_TW <- function(TW, Ymax, new_TW) {
    
    new_DEPTH <- Ymax * (new_TW / TW)
    
    return(new_DEPTH)
    
  }
  
  req_cols <- c("hy_id", "cs_id", "pt_id", "Z", "relative_distance", "cs_lengthm", "class", "point_type", "TW", "DEPTH", "DINGMAN_R")
  
  if (is.null(cross_section_pts)) {
    stop(
      paste0("'cross_section_pts' is NULL, provide a dataframe with the following columns:\n > ",
             paste0(req_cols, collapse = "\n > "))
    )
  }
  
  if (!all(req_cols %in% names(cross_section_pts))) {
    stop(paste0('"cross_section_pts" is missing columns, must include all of:\n > ', paste0(req_cols, collapse = "\n > ") ))
  }

  # original_cs %>% 
  #   hydrofabric3D::plot_cs_pts(x = "relative_distance")
  
  
  ##############################################
  ##############################################
  # cross_section_pts <-
  #   inchannel_cs %>%
  #   # dplyr::filter(hy_id == "wb-1002477", cs_id == "2")
  #   # dplyr::filter(hy_id == "wb-1002477", cs_id %in% c("3"))
  #   dplyr::filter(hy_id == "wb-1002477", cs_id %in% c("2", "3"))
  # #
  # top_width     = "owp_tw_inchan"
  # depth         = "owp_y_inchan"
  # length_col    = "cs_lengthm"
  
  # # cross_section_pts %>%
  #   # hydrofabric3D::plot_cs_pts(x = "relative_distance")
  
  ##############################################
  ##############################################
  
  # keep track of the original column order for reordering at the end
  starting_col_order  <- names(cross_section_pts)
  
  # Determine the distance interval for each cross section
  # we're going to use this value to 
  # derive a new Top width for each cross section if 
  # the cross section length is less than the prescribed top width, 
  # we round the distance interval UP sure we are not underestimating the interval
  distance_between_pts <- 
    cross_section_pts %>% 
    dplyr::select(hy_id, cs_id, relative_distance) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
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
  cross_section_pts <- 
    cross_section_pts %>% 
    dplyr::left_join(
      distance_between_pts, 
      by = c("hy_id", "cs_id")
    )
  
  # message("Ending col order:\n> ", paste0(names(cross_section_pts), collapse = "\n> "))
  # cross_section_pts$tmp_top_width <- top_width
  # cross_section_pts$tmp_depth     <- depth
  
  updated_TW_and_Ymax <- 
    cross_section_pts %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
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
        # owp_tw_inchan >= cs_lengthm ~ scale_DEPTH_to_TW(owp_tw_inchan, owp_y_inchan, cs_lengthm),
        # TRUE                        ~ owp_y_inchan
      ),
      new_TW = dplyr::case_when(
        TW >= cs_lengthm ~ cs_lengthm - distance_interval, # TODO: Same arbitrary subtraction of 1 meter as above note ^^^
        # get(top_width) >= get(length_colname) ~ !!dplyr::sym(length_colname) - 1, # TODO: Same arbitrary subtraction of 1 meter as above note ^^^
        # get(top_width) >= get(length_colname) ~ !!dplyr::sym(length_colname),   # TODO: Original method (no subtraction)
        TRUE                          ~ TW
        # owp_tw_inchan >= cs_lengthm ~ cs_lengthm,
        # TRUE                        ~ owp_tw_inchan
      ),
      has_new_DEPTH = new_DEPTH != DEPTH,
      has_new_TW    = new_TW != TW,
      fixed_TW      = has_new_DEPTH | has_new_TW
    ) %>% 
    dplyr::ungroup() %>% 
    # dplyr::relocate(hy_id, cs_id, cs_lengthm, owp_tw_inchan, 
    #                 new_TW, new_TW2, owp_y_inchan, new_DEPTH, new_DEPTH2, fixed_TW)
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
    dplyr::group_by(hy_id, cs_id) %>% 
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
  
  # # any starting columns in the original data
  ending_col_order  <- names(updated_TW_and_Ymax)
  
  # message("Ending col order:\n> ", paste0(names(updated_TW_and_Ymax), collapse = "\n> "))
  
  # # change the new_TW and new_DEPTH columns to match the original input TW/Depth column names
  ending_col_order[ending_col_order == "new_TW"] <- "TW"
  ending_col_order[ending_col_order == "new_DEPTH"] <- "DEPTH"

  # # update the names
  names(updated_TW_and_Ymax) <- ending_col_order
  
  # # reorder columns to original order
  # updated_TW_and_Ymax <- updated_TW_and_Ymax[starting_col_order]
  
  return(updated_TW_and_Ymax)
}

# cross_section_pts <- 
#   inchannel_cs %>% 
#   # dplyr::filter(hy_id == "wb-1002477", cs_id == "2")
#   dplyr::filter(hy_id == "wb-1002477", cs_id %in% c("2", "3"))
# top_width = "owp_tw_inchan"
# depth     = "owp_y_inchan"
# dingman_r = "owp_dingman_r"
# cross_section_pts$owp_tw_bf

#' Given provide inchannel widths and depths to a set of cross section points and derive estimated shapes
#' @description
#' Still in early development phases
#' @param cross_section_pts dataframe or sf dataframe. Default is NULL
#' @importFrom dplyr bind_rows select mutate n case_when summarise ungroup group_by filter relocate left_join slice slice_max rename arrange
#' @importFrom AHGestimation cross_section
#' @importFrom stats median
#' @importFrom rlang as_name enquo
#' @return dataframe or sf dataframe with AHG estimated points injected into the input cross section points
#' @export
add_cs_bathymetry <- function(
    cross_section_pts = NULL
) {
  
  if (is.null(cross_section_pts)) {
    stop(
      paste0("'cross_section_pts' is NULL, provide a dataframe with the following columns:\n> ",
             paste0(c('hy_id', 'cs_id', 'Z', 'bottom', 'relative_distance', 
                      'point_type', 'class', 
                      'top_width - (specify via "top_width" argument)',  
                      'depth - (specify via "depth" argument)', 
                      'dingman_r - (specify via "dingman_r" argument)'
             ), 
             collapse = "\n> "))
    )
  }
  
  ########################################################## 
  ##########################################################
  # cross_section_pts <-
  #   inchannel_cs %>%
  #   # dplyr::filter(hy_id == "wb-1002477", cs_id == "2")
  #   dplyr::filter(hy_id == "wb-1002477", cs_id %in% c("2", "3"))
  # top_width = "owp_tw_inchan"
  # depth     = "owp_y_inchan"
  # dingman_r = "owp_dingman_r"
  # cross_section_pts <- 
  #   inchannel_cs %>% 
  #   dplyr::slice(1:100000) %>% 
  #   dplyr::select(-owp_y_bf, -owp_tw_bf) %>% 
  #   dplyr::rename(
  #     TW        = owp_tw_inchan, 
  #     DEPTH     = owp_y_inchan,
  #     DINGMAN_R = owp_dingman_r
  #     )
  # # cross_section_pts = dplyr::slice(inchannel_cs, 1:100000) 
  # top_width         = cross_section_pts$TW
  # depth             = cross_section_pts$DEPTH
  # dingman_r         = cross_section_pts$DINGMAN_R
  
  # cross_section_pts = NULL
  # top_width = NULL
  # depth     = NULL
  # dingman_r = NULL
  # 
  ########################################################## 
  

  req_cols <- c("hy_id", "cs_id", "pt_id", "Z", "relative_distance", "cs_lengthm", "class", "point_type", "TW", "DEPTH", "DINGMAN_R")
  
  if (!all(req_cols %in% names(cross_section_pts))) {
    stop(paste0('"cross_section_pts" is missing columns, must include all of:\n > ', paste0(req_cols, collapse = "\n > ") ))
  }
  
  
  
  
  ##########################################################

  # Replace any topwidth values that are GREATER than the actual cross section length (meters)
  cross_section_pts <- fix_oversized_topwidths(
    cross_section_pts = cross_section_pts
  )

  # generate AHG parabolas for each hy_id/cs_id in the cross section points 
  # using the provided top_widths, depths, and dingman's R
  ahg_parabolas <- get_ahg_parabolas(
    cross_section_pts = cross_section_pts
  )

  # ##############################################

  # plot(ahg_parabolas$ahg_y)
  
  # store the maximum X on the left side of the parabola for later use
  ahg_left_max <- 
    ahg_parabolas %>% 
    dplyr::filter(partition == "left") %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::summarise(left_max = max(ahg_x, na.rm = TRUE)) %>% 
    dplyr::ungroup()
  
  # ------------------------------------------------------------------------------------------------
  # ---- Partition input cross section points (left/right) ----
  # ------------------------------------------------------------------------------------------------
  
  # split the cross section into a left and right half, from the midpoint of the bottom
  # and then join on the maximum X point of the LEFT half of the AHG parabolas 
  # this paritioned set of cross sections will ultimately get the AHG parabolas inserted in between the left and right partitions
  partioned_cs <- 
    cross_section_pts %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      # relative_distance_of_bottom = point_type == "bottom" 
      bottom_midpoint = dplyr::case_when(
        point_type == "bottom" ~ relative_distance,
        TRUE                   ~ NA
      ),
      bottom_midpoint = stats::median(bottom_midpoint, na.rm = TRUE)
    ) %>% 
    # dplyr::relocate(bottom_midpoint) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      # relative_distance_of_bottom = point_type == "bottom" 
      cs_partition = dplyr::case_when(
        relative_distance < bottom_midpoint ~ "left_cs",
        TRUE                                ~ "right_cs"
      )
      # bottom_midpoint = stats::median(bottom_midpoint, na.rm = TRUE)
    )  %>% 
    dplyr::left_join(
      ahg_left_max,
      by = c("hy_id", "cs_id")
    ) %>% 
    # dplyr::relocate(left_max, bottom_midpoint, cs_partition) %>% 
    dplyr::ungroup()
  
  #  get the midpoint value for each hy_id/cs_id so we can use them during the shifting process 
  midpoints <- 
    partioned_cs %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::select(hy_id, cs_id, bottom_midpoint) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # ------------------------------------------------------------------------------------------------
  # ---- Process LEFT side of cross section and parabola ----
  # ------------------------------------------------------------------------------------------------
  # # lefty <- 
  #   partioned_cs %>% 
  #   # dplyr::filter(cs_partition == "left_cs") %>%  
  #   dplyr::group_by(hy_id, cs_id) %>% 
  #   # dplyr::mutate(
  #   #   res = bottom_midpoint - max(left_max),
  #   #   mark = relative_distance < bottom_midpoint - max(left_max) | relative_distance == 0
  #   # ) %>% 
  #   # dplyr::relocate(res, mark, relative_distance)
  #   dplyr::filter(
  #     relative_distance < (bottom_midpoint - max(left_max)) | relative_distance == 0
  #     # relative_distance < 0
  #     )
  # left_new <- partioned_cs %>% 
  #   dplyr::filter(cs_partition == "left_cs") %>%  
  #   dplyr::group_by(hy_id, cs_id) %>% 
  #   dplyr::filter(
  #     # relative_distance < (bottom_midpoint - max(left_max))
  #     relative_distance < (bottom_midpoint - max(left_max)) | relative_distance == 0 # TODO: testing this new condition out
  #   ) 
  
  # # TODO: look back at this tomorrow
  # left_side_pt_counts <- 
  #   partioned_cs %>% 
  #   dplyr::filter(cs_partition == "left_cs") %>%  
  #   dplyr::group_by(hy_id, cs_id) %>% 
  #   dplyr::mutate(
  #     marked = relative_distance < (bottom_midpoint - max(left_max))
  #   ) %>% 
  #   dplyr::select(hy_id, cs_id, marked) %>% 
  #   dplyr::summarise(total_left_side_pts = sum(marked, na.rm = TRUE)) %>% 
  #   dplyr::ungroup()
  # partioned_cs %>% 
  #   dplyr::filter(cs_partition == "left_cs") %>% 
  #   dplyr::left_join(
  #     left_side_pt_counts,
  #     by = c("hy_id", "cs_id")
  #   ) 
  
  # grab just the left cross sections and remove any points that will be swallowed by the newly inserted AHG estimates 
  # And also determine the offset of the left parabolas X points, the left_start will be joined back onto the AHG parabolas
  left_cs <- 
    partioned_cs %>% 
    dplyr::filter(cs_partition == "left_cs") %>%  
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::filter(
      # relative_distance < (bottom_midpoint - max(left_max))
      relative_distance < (bottom_midpoint - max(left_max)) | relative_distance == 0 # TODO: testing this new condition out
      # relative_distance < (bottom_midpoint - max(left_max)) | (relative_distance == 0 & total_left_side_pts == 0)  # TODO: testing this new condition out
    ) %>% 
    dplyr::mutate(
      left_start = bottom_midpoint - max(left_max)
    ) %>% 
    dplyr::ungroup()
  
  left_starts <- 
    left_cs %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::select(hy_id, cs_id, left_start) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  # offset the left parabolas X points using the left_start value
  left_parabolas <-
    ahg_parabolas %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::filter(partition == "left") %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(
      left_starts,
      by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
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
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::filter(relative_distance > bottom_midpoint) %>% 
    # dplyr::filter(cs_partition == "right_cs")
    dplyr::ungroup()
  
  right_parabolas <- 
    ahg_parabolas %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::filter(partition == "right") %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(
      ahg_left_max,
      by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::left_join(
      midpoints,
      by = c("hy_id", "cs_id")
    ) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
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
    dplyr::group_by(hy_id, cs_id) %>% 
    # dplyr::filter(relative_distance == max(relative_distance)) %>% 
    dplyr::slice_max(relative_distance, n = 1) %>% 
    dplyr::select(hy_id, cs_id, 
                  max_right_position = relative_distance) %>% 
    dplyr::ungroup() 
  
  # from the right side of the parabola, 
  # we remove any parabola points that would be past
  # the last right side cross section points
  right_parabolas <- 
    right_parabolas %>% 
    dplyr::left_join(
      total_cross_section_length,
      by = c("hy_id", "cs_id")
    )  %>% 
    # dplyr::relocate(hy_id, cs_id, right_start, max_right_position) %>% 
    dplyr::filter(right_start < max_right_position) 
  
  # ----------------------------------------------------------------------------------------------------------------
  # TODO: Above still needs review ^^^ 
  # ----------------------------------------------------------------------------------------------------------------
  
  # getting the starting X value for the RIGHT side of the parabola
  max_right_starting_pts <- 
    right_parabolas %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::summarise(
      right_start_max = max(right_start, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup()
  
  # removing cross section point that will be replaced by right_parabola points
  right_cs <- 
    right_cs %>% 
    dplyr::left_join(
      max_right_starting_pts, 
      by = c("hy_id", "cs_id")
    ) %>% 
    # dplyr::relocate(right_start_max) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
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
      hy_id, cs_id, ahg_index, ahg_x, ahg_y, ahg_a,
      partition
      # left_max, bottom_midpoint
    )
  
  left_parabolas <- 
    left_parabolas %>% 
    dplyr::select(
      hy_id, cs_id, ahg_index, ahg_x, ahg_y, ahg_a,
      partition
    )
  
  # merge
  parabolas <- dplyr::bind_rows(left_parabolas, right_parabolas)
  
  # reorder to parabolas by X values so they are in order from left to right for each hy_id/cs_id
  parabolas <- 
    parabolas %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::arrange(ahg_x, .by_group = TRUE) %>% 
    dplyr::ungroup()
  
  # select relevant columns and adjust the names so 
  # the AHG parabola can be inserted nicely with the original cross sections
  # NOTE: 
  # AHG X values == "relative_distance" in cross_section_pts
  # AHG Y values == "Z" in cross_section_pts
  parabolas <- 
    parabolas %>% 
    dplyr::select(
      hy_id, cs_id,
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
      # left_cs,
      # parabolas,
      # right_cs
      dplyr::mutate(left_cs, is_dem_point = TRUE),
      dplyr::mutate(parabolas, is_dem_point = FALSE),
      dplyr::mutate(right_cs, is_dem_point = TRUE),
    ) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::filter(relative_distance >= 0) %>% # TODO: testing out this condition as well
    dplyr::arrange(relative_distance, .by_group = TRUE) %>% 
    dplyr::ungroup()
  
  # Assign / renumber the "pt_ids" and 
  # set the "point_types" of the inserted parabola points to "bottom" type
  out_cs <- 
    out_cs %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
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
  
  # parabolas %>%
  #   hydrofabric3D::add_tmp_id() %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z)) +
  #   ggplot2::facet_wrap(~tmp_id)
  # out_cs %>%
  #   hydrofabric3D::plot_cs_pts(x = "relative_distance", color = "is_dem_point")
  
  tryCatch({
    message("Generate XY coordinates for AHG estimated points...")
    out_cs <- fill_missing_ahg_coords(out_cs)

  }, error = function(cond) {

    message("Failed to fix X/Y coordinates for estimated bathymetry points, returning cross section points with inserted bathymetry with missing X/Y values")
    message(conditionMessage(cond))

    # Choose a return value in case of error
    return(out_cs)

  })
  
  return(out_cs)
  
}

#' ###############################################################################
#' ##### ---- VERSION 1 ------
#' ###############################################################################
#' 
#' #' Get the AHG estimated parabolas for each hy_id/cs_id cross section given a set of cross section points
#' #'
#' #' @param cross_section_pts dataframe or sf dataframe with "hy_id", "cs_id", "bottom" columns and 
#' #'  specififed "top_width", "depth", "dingman_r" columns  (see top_width, depth, and dingman_r arguments)
#' #' @param top_width character or tidy selector column name of top width value column. Default is "owp_tw_inchan"
#' #' @param depth character or tidy selector column name of Y depth value column. Default is "owp_y_inchan"
#' #' @param dingman_r numeric, Dingman's R coeffiecient.  Default is "owp_dingman_r".
#' #' @importFrom dplyr bind_rows rowwise select group_by slice ungroup filter mutate
#' #' @importFrom AHGestimation cross_section
#' #' @importFrom tidyr unnest
#' #' @importFrom rlang as_name enquo
#' #' @return dataframe with a set of AHG points for each hy_id/cs_id in the input data, with AHG estimated X, Y, A point values that form a parabola
#' get_ahg_parabolas <- function(
#'     cross_section_pts,
#'     top_width = "owp_tw_inchan",
#'     depth     = "owp_y_inchan",
#'     dingman_r = "owp_dingman_r"
#' ) {
#' 
#'   # cross_section_pts <-
#'   #   cs_ml %>%
#'   #   dplyr::slice(1:20)
#'   # cross_section_pts = cross_section_pts
#'   # # top_width         = {{top_width}},
#'   # # depth             = {{depth}},
#'   # top_width         = "owp_tw_inchan"
#'   # depth             = "owp_y_inchan"
#'   # length_col        = "cs_lengthm"
#'   # cross_section_pts = dplyr::slice(inchannel_cs, 1:10)
#'   # top_width         = "owp_tw_inchan"
#'   # depth             = "owp_y_inchan"
#'   # dingman_r         = "owp_dingman_r"
#'   
#'   # this function will take in one of the AHG estimated parabolas, a bottom depth, and the estimated inchannel depth 
#'   # and if there are infinite/NaN/NA values in the AHG estimated parabolas depth ("Y"),
#'   # we pin the the left and right side of the parabola to the input bottomZ and set 
#'   # the bottom depth to the input bottom minus the given inchannel depth estimate
#'   # TODO: Probably won't be a permanent solution if the AHG estimation package ends up never returning NaN/Infinite values
#'   make_rectangle_if_needed <- function(parabola_df,  bottomZ, inchannel_depth) {
#'     contains_nan_or_inf_y_values <- any(is.nan(parabola_df$Y)) || any(is.na(parabola_df$Y)) || any(is.infinite(parabola_df$Y))
#'     
#'     if (contains_nan_or_inf_y_values) {
#'       parabola_df[1, 'Y']                        <- bottomZ
#'       parabola_df[nrow(parabola_df), 'Y']        <- bottomZ
#'       parabola_df[2:(nrow(parabola_df)-1), 'Y']  <- bottomZ - inchannel_depth
#'     }
#'     
#'     return(parabola_df)
#'     
#'   }
#'   
#'   # set the parabola to align with the cross section points bottom most points
#'   offset_and_partition_parabola <- function(parabola, bottomZ) {
#'     
#'     # indices of the left and right parabola halves
#'     left_half  = 1:(nrow(parabola) / 2)
#'     right_half = (1 + (nrow(parabola) / 2)):nrow(parabola)
#'     
#'     # get the left and right side of the parabolas
#'     left_parabola = parabola[left_half, ]
#'     right_parabola = parabola[right_half, ]
#'     
#'     # shift the Z values to have there max points be at the "bottom" of the "cross_section_pts" points
#'     left_parabola$Y <- left_parabola$Y + (bottomZ - max(left_parabola$Y))
#'     right_parabola$Y <- right_parabola$Y + (bottomZ - max(right_parabola$Y))
#'     
#'     left_parabola$partition  <- "left"
#'     right_parabola$partition <- "right"
#'     # left_parabola <- 
#'     #   left_parabola %>% 
#'     #   dplyr::mutate(
#'     #     left_max = max(x, na.rm = TRUE)
#'     #   )
#'     parabola <- dplyr::bind_rows(
#'       left_parabola, 
#'       right_parabola
#'     )
#'     
#'     return(parabola)
#'   } 
#'   
#'   top_width_str  <- rlang::as_name(rlang::enquo(top_width))
#'   depth_str      <- rlang::as_name(rlang::enquo(depth))
#'   dingman_r_str  <- rlang::as_name(rlang::enquo(dingman_r))
#'   
#'   # message("top_width_str: ", top_width_str)
#'   # message("depth_str: ", depth_str)
#'   # message("dingman_r_str: ", dingman_r_str)
#'   # message("class top_width_str: ", class(top_width_str))
#'   # message("class depth_str: ", class(depth_str))
#'   # message("class dingman_r_str: ", class(dingman_r_str))
#'   
#'   # check that the top_width, depth, and dingman_r values are columns in the input dataframe
#'   if (!top_width_str %in% names(cross_section_pts)) {
#'     stop(paste0("'top_width' column '", top_width_str, "' does not exist in cross_section_pts dataframe"))
#'   }
#'   
#'   if (!depth_str %in% names(cross_section_pts)) {
#'     stop(paste0("'depth' column '", depth_str, "' does not exist in cross_section_pts dataframe"))
#'   }
#'   
#'   if (!dingman_r_str %in% names(cross_section_pts)) {
#'     stop(paste0("'dingman_r' column '", dingman_r_str, "' does not exist in cross_section_pts dataframe"))
#'   }
#'   
#'   ahg_parameters <- 
#'     cross_section_pts %>% 
#'     dplyr::select(hy_id, cs_id, bottom, 
#'                   dplyr::any_of(c(top_width, depth, dingman_r))
#'                   # .data[[top_width]],
#'                   # .data[[depth]],
#'                   # .data[[dingman_r]]
#'                   # !!dplyr::sym(top_width),
#'                   # !!dplyr::sym(depth),
#'                   # !!dplyr::sym(dingman_r),
#'                   # {{top_width}}, {{depth}}, {{dingman_r}}
#'                   ) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::slice(1) %>% 
#'     dplyr::ungroup()
#'   
#'   # remove any cross sections that are missing top_width, depth, or dingman_r
#'   set_aside <- 
#'     ahg_parameters %>% 
#'     dplyr::filter(is.na(.data[[top_width]]) | is.na(.data[[depth]]) | is.na(.data[[dingman_r]]) )
#'     # dplyr::filter(is.na(get(top_width)) | is.na(get(depth)) | is.na(get(dingman_r)) )
#'     # dplyr::filter(is.na({{top_width}}) | is.na({{depth}}) | is.na({{dingman_r}}) )
#'   
#'   ahg_parameters <- 
#'     ahg_parameters %>%
#'     hydrofabric3D::add_tmp_id() %>% 
#'     dplyr::filter(!tmp_id %in% hydrofabric3D::add_tmp_id(set_aside)$tmp_id) %>% 
#'     dplyr::select(-tmp_id) %>% 
#'     dplyr::rowwise() %>%
#'     dplyr::mutate(
#'       parabola = list(
#'         AHGestimation::cross_section(
#'           # r    = get(dingman_r),
#'           # TW   = get(top_width),
#'           # Ymax = get(depth)
#'           r    = .data[[dingman_r]],
#'           TW   = .data[[top_width]],
#'           Ymax = .data[[depth]]
#'           # r    = {{dingman_r}},
#'           # TW   = {{top_width}},
#'           # Ymax = {{depth}}
#'         ) 
#'         # dplyr::mutate(
#'         #   Y = dplyr::case_when(
#'         #     ind == 2 ~ NaN,
#'         #     TRUE ~ Y
#'         #   )
#'         # )
#'       )
#'     )
#'   
#'   ahg_parameters <- 
#'     ahg_parameters  %>% 
#'     dplyr::rowwise() %>% 
#'     dplyr::mutate(
#'       parabola = list(
#'         make_rectangle_if_needed(
#'           parabola,
#'           bottom,
#'           # NOTE: Not sure why i had this like this, should use the bottom as the anchor...? 
#'           # {{top_width}},
#'           # {{depth}}
#'           # get(top_width),
#'           # get(depth)
#'           .data[[depth]]
#'         )
#'         # parabola = purrr::map2(parabola, bottom, {{depth}}, make_rectangle_if_needed)
#'       )
#'     ) %>% 
#'     dplyr::rowwise() %>% 
#'     dplyr::mutate(
#'       parabola = list(
#'         offset_and_partition_parabola(
#'           parabola,
#'           bottom
#'         )
#'         # parabola = purrr::map2(parabola, bottom, {{depth}}, make_rectangle_if_needed)
#'       )
#'     )
#'   
#'   # unnest the parabola dataframes and rename columns then return
#'   ahg_parameters <-
#'     ahg_parameters %>% 
#'     dplyr::select(hy_id, cs_id, parabola) %>% 
#'     tidyr::unnest(c(parabola)) %>% 
#'     # dplyr::group_by(hy_id, cs_id, partition) %>% 
#'     # dplyr::group_by(hy_id, cs_id) %>% 
#'     # dplyr::mutate(
#'     #   left_max = max(x, na.rm = TRUE)
#'     # ) %>% 
#'     # dplyr::ungroup() %>% 
#'     dplyr::select(hy_id, cs_id, 
#'                   ahg_index = ind, 
#'                   ahg_x = x, 
#'                   ahg_y = Y, 
#'                   ahg_a = A, 
#'                   # left_max,
#'                   partition
#'     ) 
#'   
#'   # plot(ahg_parameters$ahg_y)
#'   
#'   return(ahg_parameters)
#' }
#' 
#' 
#' #' Generate X/Y coordinates between a set of known points within a cross section
#' #' After 
#' #'
#' #' @param cross_section_pts cross section points dataframe with missing X/Y coordinates between sets of known X/Y coordinates
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' fill_missing_ahg_coords <- function(cross_section_pts) {
#'   
#'   # cross_section_pts <-
#'   #   cs_bathy_inchannel %>% 
#'   #   # dplyr::slice(1:159)
#'   #   dplyr::slice(1:171375)
#'   
#'   # cross_section_pts <- cs_bathy_inchannel
#'   
#'   #Fix the missing X/Y coordinates (NAs) from the inserted AHG Parabola points in a set of cross section points
#'   seq_between_start_and_end <- function(start, end, n) {
#'     # df = fix_coords
#'     
#'     if (n == 0) {
#'       return(NULL)
#'     }
#'     
#'     # Generate new X / Y coordinates
#'     # coords <- seq(start, end, length.out = n )
#'     coords <- seq(start, end, length.out = n + 2)
#'     
#'     # return(coords)
#'     return(coords[2:(length(coords) - 1)])
#'   }
#'   
#'   
#'   # get the first and last coordinates beforee the missing NA X/Y points
#'   start_and_end_coords <- get_coords_around_parabola(cross_section_pts)
#'   
#'   # 
#'   start_and_end_pts_ids <- 
#'     cross_section_pts %>% 
#'     dplyr::filter(!is_dem_point) %>%  
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::summarize(
#'       start_pt_id = min(pt_id, na.rm = TRUE)
#'     ) %>% 
#'     dplyr::ungroup()
#'   
#'   # get only the rows with the parabola w/ missing X/Y coords
#'   parabola_pts <- 
#'     cross_section_pts %>% 
#'     dplyr::filter(!is_dem_point) %>%  
#'     dplyr::left_join(
#'       start_and_end_coords,
#'       by = c("hy_id", "cs_id")
#'     ) %>% 
#'     dplyr::select(hy_id, cs_id, pt_id, X, Y, 
#'                   dplyr::ends_with("_start"), dplyr::ends_with("end")) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       n = dplyr::n()
#'     ) %>% 
#'     dplyr::ungroup()
#'   
#'     # generate coordinates for each parabola
#'     parabola_coords <- 
#'       parabola_pts %>% 
#'       dplyr::left_join(
#'         start_and_end_pts_ids, 
#'         by = c("hy_id", "cs_id")
#'       ) %>% 
#'       dplyr::select(hy_id, cs_id, start_pt_id, dplyr::ends_with("_start"), dplyr::ends_with("end"), n) %>% 
#'       dplyr::group_by(hy_id, cs_id)  %>% 
#'       dplyr::slice(1)  %>% 
#'       dplyr::mutate(
#'         X = list(seq_between_start_and_end(X_start, X_end, n)),
#'         Y = list(seq_between_start_and_end(Y_start, Y_end, n))
#'       ) %>% 
#'       tidyr::unnest(c(X, Y)) %>% 
#'       dplyr::group_by(hy_id, cs_id)  %>%
#'       dplyr::mutate(
#'           pt_id = (start_pt_id - 1) + (1:dplyr::n())
#'           # pt_id = start_pt_id + (0:(dplyr::n()-1))
#'       ) %>%
#'       dplyr::ungroup() %>%
#'       dplyr::select(hy_id, cs_id, pt_id, X, Y) 
#'       # dplyr::select(hy_id, cs_id, X, Y)
#'     
#'     # join the new parabola X/Y points with the rest of the original data, dropping the old NA X/Y coordinates
#'     parabolas_with_coords <- 
#'       cross_section_pts %>% 
#'       dplyr::filter(!is_dem_point) %>% 
#'       dplyr::select(-X, -Y) %>% 
#'       dplyr::left_join(
#'         parabola_coords,
#'         by = c("hy_id", "cs_id", "pt_id")
#'       )
#'     
#'     pts_with_fixed_coords <- 
#'       dplyr::bind_rows(
#'       dplyr::filter(cross_section_pts, is_dem_point),
#'       parabolas_with_coords
#'       ) %>% 
#'       dplyr::group_by(hy_id, cs_id) %>% 
#'       dplyr::arrange(relative_distance, .by_group = TRUE) %>%
#'       # dplyr::arrange(pt_id, .by_group = TRUE) %>%
#'       dplyr::group_by(hy_id, cs_id) %>% 
#'       dplyr::mutate(
#'         pt_id = 1:dplyr::n()
#'       ) %>%
#'       dplyr::ungroup()
#'     
#'     # pts_with_fixed_coords %>% 
#'     # dplyr::group_by(hy_id, cs_id) 
#'     # 
#'     # pts_with_fixed_coords %>% 
#'     #   dplyr::slice(1:15000) %>% 
#'     #   sf::st_as_sf(coords = c("X", "Y"), crs = 5070) %>% 
#'     #   mapview::mapview()
#'     return(pts_with_fixed_coords)
#' }
#' 
#' #' Get the coordinates surrounding a set of missing AHG X/Y coordinates.
#' #' 
#' #' @param cross_section_pts dataframe with cross section points, (required cols, "hy_id", "cs_id", "X", "Y", "is_dem_point")
#' #' @importFrom tidyr pivot_wider
#' #' @importFrom select group_by mutate lag lead case_when filter ungroup left_join
#' #' @return dataframe with each hy_id/cs_id cross section containing a value for X_start, X_end, Y_start, Y_end, representing the points surrounding the AHG inserted points
#' #' @export
#' get_coords_around_parabola <- function(cross_section_pts) {
#'   
#'   fill_value <- -999999999
#' 
#'   X_coords <-
#'     cross_section_pts %>% 
#'     dplyr::select(hy_id, cs_id, X, is_dem_point) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       X = ifelse(is.na(X), fill_value, X)
#'     ) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       next_X_is_missing  = dplyr::case_when(
#'         dplyr::lead(X) == fill_value       ~ TRUE,
#'         # is_dem_point & is.na(dplyr::lead(X))  ~ TRUE,
#'         TRUE                                  ~ FALSE
#'       ),
#'       prev_X_is_missing  = dplyr::case_when(
#'         dplyr::lag(X) == fill_value          ~ TRUE,
#'         # is_dem_point & is.na(dplyr::lead(X))  ~ TRUE,
#'         TRUE                                 ~ FALSE
#'       )
#'     ) %>% 
#'     dplyr::filter((is_dem_point & next_X_is_missing) | (is_dem_point & prev_X_is_missing)) %>% 
#'     dplyr::mutate(
#'       start_or_end  = dplyr::case_when(
#'         # is_dem_point & is.na(dplyr::lead(X))  ~ TRUE,
#'         (is_dem_point & next_X_is_missing)   ~ "X_start",
#'         (is_dem_point & prev_X_is_missing)   ~ "X_end"
#'       )
#'     ) %>% 
#'     dplyr::ungroup() %>% 
#'     dplyr::select(hy_id, cs_id, X, is_dem_point, start_or_end)
#'   
#'   # X_coords %>% 
#'   #   dplyr::group_by(hy_id, cs_id)
#'   
#'   # pivot so each cross sections is a single row with a "X_start" and "X_end" point 
#'   X_coords <- 
#'     X_coords %>% 
#'     # dplyr::select(-is_dem_point) %>% 
#'     tidyr::pivot_wider(
#'       id_cols = c(hy_id, cs_id),
#'       names_from = start_or_end,
#'       values_from = X
#'     )
#'   
#'   Y_coords <-
#'     cross_section_pts %>% 
#'     dplyr::select(hy_id, cs_id, Y, is_dem_point) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       Y = ifelse(is.na(Y), fill_value, Y)
#'     ) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       next_Y_is_missing  = dplyr::case_when(
#'         dplyr::lead(Y) == fill_value       ~ TRUE,
#'         TRUE                               ~ FALSE
#'       ),
#'       prev_Y_is_missing  = dplyr::case_when(
#'         dplyr::lag(Y) == fill_value        ~ TRUE,
#'         TRUE                               ~ FALSE
#'       )
#'     ) %>% 
#'     dplyr::filter((is_dem_point & next_Y_is_missing) | (is_dem_point & prev_Y_is_missing)) %>% 
#'     dplyr::mutate(
#'       start_or_end  = dplyr::case_when(
#'         (is_dem_point & next_Y_is_missing)   ~ "Y_start",
#'         (is_dem_point & prev_Y_is_missing)   ~ "Y_end"
#'       )
#'     ) %>% 
#'     dplyr::ungroup() %>% 
#'     dplyr::select(hy_id, cs_id, Y, is_dem_point, start_or_end)
#'     
#'     # pivot so each cross sections is a single row with a "X_start" and "X_end" point 
#'     Y_coords <-
#'       Y_coords %>% 
#'       tidyr::pivot_wider(
#'         id_cols = c(hy_id, cs_id),
#'         names_from = start_or_end,
#'         values_from = Y
#'       )
#' 
#'     coords_around_parabola <- dplyr::left_join(
#'                                         X_coords,
#'                                         Y_coords,
#'                                         by = c("hy_id", "cs_id")
#'                                         )
#'     
#'     return(coords_around_parabola)
#' }
#' 
#' #' Check that all cross sections points have a prescribed top width less than the total cross section length
#' #' @description
#' #' If a set of cross section points has a top width length that is longer than the cross sections length, then a new top width and Y max (depth) value
#' #' are given so that the estimated shape is able to properly fit into the cross sections. 
#' #' The cross sections length (meters) minus 1 meter is used as the new top width and 
#' #' the new Y max (depth) value is derived from the original ratio between the prescribed top width and Y max
#' #' @param cross_section_pts dataframe or sf dataframe with hy_id, cs_id, 
#' #' @param top_width character or tidy selector column name of top width value column. Default is "owp_tw_inchan"
#' #' @param depth character or tidy selector column name of Y depth value column. Default is "owp_y_inchan"
#' #' @param length_col character or tidy selector column name of the numeric total cross section length column (meters). Default is "cs_lengthm"
#' #' @importFrom rlang as_name enquo
#' #' @importFrom dplyr group_by mutate case_when sym ungroup select filter slice
#' #' @return cross_section_pts dataframe with updated "top_width" and "depth" column values
#' fix_oversized_topwidths <- function(
#'     cross_section_pts = NULL,
#'     top_width  = "owp_tw_inchan",
#'     depth      = "owp_y_inchan",
#'     length_col = "cs_lengthm"
#' ) {
#'   
#'   # use this to get a new scaled Ymax value given a known Top width, Ymax (ML generated in this case)
#'   # and an expected new Top width (new_TW), this function is used to handle 
#'   # the case when the ML estimated top width is GREATER than the DEM cross section length
#'   scale_Ymax_to_TW <- function(TW, Ymax, new_TW) {
#'     
#'     new_Ymax <- Ymax * (new_TW / TW)
#'     
#'     return(new_Ymax)
#'     
#'   }
#'   
#'   if (is.null(cross_section_pts)) {
#'     stop(
#'       paste0("'cross_section_pts' is NULL, provide a dataframe with the following columns:\n> ",
#'              paste0(c('hy_id', 'cs_id', 'Z', 'bottom', 'relative_distance', 
#'                       'point_type', 'class', 
#'                       'top_width - (specify via "top_width" argument)',  
#'                       'depth - (specify via "depth" argument)', 
#'                       'dingman_r - (specify via "dingman_r" argument)'
#'              ), 
#'              collapse = "\n> "))
#'     )
#'   }
#'   
#'   # original_cs %>% 
#'   #   hydrofabric3D::plot_cs_pts(x = "relative_distance")
#' 
#'   
#'   ##############################################
#'   ##############################################
#'   # cross_section_pts <-
#'   #   inchannel_cs %>%
#'   #   # dplyr::filter(hy_id == "wb-1002477", cs_id == "2")
#'   #   # dplyr::filter(hy_id == "wb-1002477", cs_id %in% c("3"))
#'   #   dplyr::filter(hy_id == "wb-1002477", cs_id %in% c("2", "3"))
#'   # #
#'   # top_width     = "owp_tw_inchan"
#'   # depth         = "owp_y_inchan"
#'   # length_col    = "cs_lengthm"
#' 
#'   # # cross_section_pts %>%
#'   #   # hydrofabric3D::plot_cs_pts(x = "relative_distance")
#'   
#'   ##############################################
#'   ##############################################
#' 
#'   top_width_str  <- rlang::as_name(rlang::enquo(top_width))
#'   depth_str      <- rlang::as_name(rlang::enquo(depth))
#'   length_col_str <- rlang::as_name(rlang::enquo(length_col))
#'   
#'   # check that the top_width, depth, and dingman_r values are columns in the input dataframe
#'   if (!top_width_str %in% names(cross_section_pts)) {
#'     stop(paste0("'top_width' column '", top_width_str, "' does not exist in cross_section_pts dataframe"))
#'   }
#'   
#'   if (!depth_str %in% names(cross_section_pts)) {
#'     stop(paste0("'depth' column '", depth_str, "' does not exist in cross_section_pts dataframe"))
#'   }
#'   if (!depth_str %in% names(cross_section_pts)) {
#'     stop(paste0("'depth' column '", depth_str, "' does not exist in cross_section_pts dataframe"))
#'   }
#'   
#'   # keep track of the original column order for reordering at the end
#'   starting_col_order  <- names(cross_section_pts)
#'   
#'   # Determine the distance interval for each cross section
#'   # we're going to use this value to 
#'   # derive a new Top width for each cross section if 
#'   # the cross section length is less than the prescribed top width, 
#'   # we round the distance interval UP sure we are not underestimating the interval
#'   distance_between_pts <- 
#'     cross_section_pts %>% 
#'     dplyr::select(hy_id, cs_id, relative_distance) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       distance_interval = relative_distance - dplyr::lag(relative_distance)
#'     ) %>% 
#'     dplyr::summarise(
#'       distance_interval = ceiling(mean(distance_interval, na.rm = TRUE)) # TODO: round up to make sure we are not underestimating 
#'                                                                          # the interval, we're going to use this value to 
#'                                                                          # derive a new Top width for each cross section if 
#'                                                                          # the cross section length is less than the prescribed top width
#'     ) %>% 
#'     dplyr::ungroup()
#'   
#'   # add the distance interval values to the cross section points
#'   cross_section_pts <- 
#'       cross_section_pts %>% 
#'       dplyr::left_join(
#'         distance_between_pts, 
#'         by = c("hy_id", "cs_id")
#'       )
#'   # message("Ending col order:\n> ", paste0(names(cross_section_pts), collapse = "\n> "))
#' 
#'   updated_TW_and_Ymax <- 
#'     cross_section_pts %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       new_Ymax = dplyr::case_when(
#'         get(top_width) >= get(length_col) ~ scale_Ymax_to_TW(!!dplyr::sym(top_width), 
#'                                                        !!dplyr::sym(depth), 
#'                                                        !!dplyr::sym(length_col) - distance_interval
#'                                                        # !!dplyr::sym(length_col) - 1 # TODO: arbitrarily remove 1 meter from the length to 
#'                                                                                     # TODO: make sure topwidth/depth is SMALLER than cross section length 
#'                                                                                     # TODO: so the AHG parabola points can fit within cross section 
#'                                                        # !!dplyr::sym(length_col) 
#'                                                        # TODO: Original method ^^^ (no subtraction)
#'                                                        ), 
#'         TRUE                        ~ !!dplyr::sym(depth)
#'         # owp_tw_inchan >= cs_lengthm ~ scale_Ymax_to_TW(owp_tw_inchan, owp_y_inchan, cs_lengthm),
#'         # TRUE                        ~ owp_y_inchan
#'       ),
#'       new_TW = dplyr::case_when(
#'         get(top_width) >= get(length_col) ~ !!dplyr::sym(length_col) - distance_interval, # TODO: Same arbitrary subtraction of 1 meter as above note ^^^
#'         # get(top_width) >= get(length_col) ~ !!dplyr::sym(length_col) - 1, # TODO: Same arbitrary subtraction of 1 meter as above note ^^^
#'         # get(top_width) >= get(length_col) ~ !!dplyr::sym(length_col),   # TODO: Original method (no subtraction)
#'         TRUE                              ~ !!dplyr::sym(top_width)
#'         # owp_tw_inchan >= cs_lengthm ~ cs_lengthm,
#'         # TRUE                        ~ owp_tw_inchan
#'       ),
#'       has_new_Ymax = new_Ymax != get(depth),
#'       has_new_TW = new_TW != get(top_width),
#'       fixed_TW = has_new_Ymax | has_new_TW
#'     ) %>% 
#'     dplyr::ungroup() %>% 
#'     # dplyr::relocate(hy_id, cs_id, cs_lengthm, owp_tw_inchan, 
#'     #                 new_TW, new_TW2, owp_y_inchan, new_Ymax, new_Ymax2, fixed_TW)
#'     dplyr::select(
#'       -has_new_Ymax,
#'       -has_new_TW,
#'       -{{top_width}},
#'       -{{depth}}
#'       )
#'   
#'   # number of cross sections that had their TW/Depths changed to fit into the cross section properly
#'   number_fixed_TWs <- 
#'     updated_TW_and_Ymax %>% 
#'     dplyr::filter(fixed_TW) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::slice(1) %>% 
#'     dplyr::ungroup() %>% 
#'     nrow()
#'   
#'   if (number_fixed_TWs > 0) {
#'     warning(
#'       "Had to fix ", number_fixed_TWs, " cross section(s) top width/depth values to make sure", 
#'       "\nthe prescribed topwidth is not greater than or equal to the length of",
#'       "\nthe entire cross section (meters), the cross section(s) total length is", 
#'       "\nused as the new TW and the ratio of the prescribed TW to the prescribed depth", 
#'       "\nis used to calculate a new depth (Y) value."
#'     )
#'   }
#'  
#'   # Drop the flag column that says if the top width had to be fixed
#'   updated_TW_and_Ymax <- dplyr::select(updated_TW_and_Ymax, 
#'                                        -fixed_TW)
#'   
#'   # any starting columns in the original data
#'   ending_col_order  <- names(updated_TW_and_Ymax)
#' 
#'   # message("Ending col order:\n> ", paste0(names(updated_TW_and_Ymax), collapse = "\n> "))
#' 
#'   # change the new_TW and new_Ymax columns to match the original input TW/Depth column names
#'   ending_col_order[ending_col_order == "new_TW"] <- top_width_str
#'   ending_col_order[ending_col_order == "new_Ymax"] <- depth_str
#' 
#'   # update the names
#'   names(updated_TW_and_Ymax) <- ending_col_order
#' 
#'   # reorder columns to original order
#'   updated_TW_and_Ymax <- updated_TW_and_Ymax[starting_col_order]
#' 
#'   return(updated_TW_and_Ymax)
#' }
#' 
#' # cross_section_pts <- 
#' #   inchannel_cs %>% 
#' #   # dplyr::filter(hy_id == "wb-1002477", cs_id == "2")
#' #   dplyr::filter(hy_id == "wb-1002477", cs_id %in% c("2", "3"))
#' # top_width = "owp_tw_inchan"
#' # depth     = "owp_y_inchan"
#' # dingman_r = "owp_dingman_r"
#' # cross_section_pts$owp_tw_bf
#' 
#' #' Given provide inchannel widths and depths to a set of cross section points and derive estimated shapes
#' #' @description
#' #' Still in early development phases
#' #' @param cross_section_pts dataframe or sf dataframe. Default is NULL
#' #' @param top_width character or tidy selector column name of top width value column. Default is "owp_tw_inchan"
#' #' @param depth character or tidy selector column name of Y depth value column. Default is "owp_y_inchan"
#' #' @param dingman_r numeric, Dingman's R coeffiecient.  Default is "owp_dingman_r".
#' #' @importFrom dplyr bind_rows select mutate n case_when summarise ungroup group_by filter relocate left_join slice slice_max rename arrange
#' #' @importFrom AHGestimation cross_section
#' #' @importFrom stats median
#' #' @importFrom rlang as_name enquo
#' #' @return dataframe or sf dataframe with AHG estimated points injected into the input cross section points
#' #' @export
#' add_cs_bathymetry <- function(
#'     cross_section_pts = NULL,
#'     top_width = "owp_tw_inchan",
#'     depth     = "owp_y_inchan",
#'     dingman_r = "owp_dingman_r"
#' ) {
#' 
#'   if (is.null(cross_section_pts)) {
#'     stop(
#'       paste0("'cross_section_pts' is NULL, provide a dataframe with the following columns:\n> ",
#'              paste0(c('hy_id', 'cs_id', 'Z', 'bottom', 'relative_distance', 
#'                       'point_type', 'class', 
#'                       'top_width - (specify via "top_width" argument)',  
#'                       'depth - (specify via "depth" argument)', 
#'                       'dingman_r - (specify via "dingman_r" argument)'
#'              ), 
#'              collapse = "\n> "))
#'       )
#'   }
#'   
#'   ########################################################## 
#'   ##########################################################
#'   # cross_section_pts <-
#'   #   inchannel_cs %>%
#'   #   # dplyr::filter(hy_id == "wb-1002477", cs_id == "2")
#'   #   dplyr::filter(hy_id == "wb-1002477", cs_id %in% c("2", "3"))
#'   # top_width = "owp_tw_inchan"
#'   # depth     = "owp_y_inchan"
#'   # dingman_r = "owp_dingman_r"
#'   
#'   ########################################################## 
#'   ##########################################################
#'   
#'   top_width_str  <- rlang::as_name(rlang::enquo(top_width))
#'   depth_str      <- rlang::as_name(rlang::enquo(depth))
#'   dingman_r_str  <- rlang::as_name(rlang::enquo(dingman_r))
#'   
#'   # check that the top_width, depth, and dingman_r values are columns in the input dataframe
#'   if (!top_width_str %in% names(cross_section_pts)) {
#'     stop(paste0("'top_width' column '", top_width_str, "' does not exist in cross_section_pts dataframe"))
#'   }
#'   
#'   if (!depth_str %in% names(cross_section_pts)) {
#'     stop(paste0("'depth' column '", depth_str, "' does not exist in cross_section_pts dataframe"))
#'   }
#'   
#'   if (!dingman_r_str %in% names(cross_section_pts)) {
#'     stop(paste0("'dingman_r' column '", dingman_r_str, "' does not exist in cross_section_pts dataframe"))
#'   }
#'   
#'   # Replace any topwidth values that are GREATER than the actual cross section length (meters)
#'   cross_section_pts <- fix_oversized_topwidths(
#'                         cross_section_pts = cross_section_pts,
#'                         top_width         = {{top_width}},
#'                         depth             = {{depth}},
#'                         length_col        = "cs_lengthm"
#'                         )
#'   
#'   # cross_section_pts <- fix_oversized_topwidths(
#'   #   cross_section_pts = cross_section_pts,
#'   #   # top_width         = {{top_width}},
#'   #   # depth             = {{depth}},
#'   #   top_width         = "owp_tw_inchan",
#'   #   depth             = "owp_y_inchan",
#'   #   length_col        = "cs_lengthm"
#'   # )
#'   
#'   # generate AHG parabolas for each hy_id/cs_id in the cross section points 
#'   # using the provided top_widths, depths, and dingman's R
#'   ahg_parabolas <- get_ahg_parabolas(
#'                         cross_section_pts = cross_section_pts,
#'                         top_width         = {{top_width}},
#'                         depth             = {{depth}},
#'                         dingman_r         = {{dingman_r}}
#'                         )
#' 
#'   # ahg_parabolas %>%
#'   #   hydrofabric3D::add_tmp_id() %>%
#'   #   ggplot2::ggplot() +
#'   #   ggplot2::geom_point(ggplot2::aes(x = ahg_x, y = ahg_y)) +
#'   #   ggplot2::facet_wrap(hy_id~cs_id)
#'   
#'   # bads <- cs_bathy_inchannel %>%  
#'   #   dplyr::group_by(hy_id, cs_id) %>% 
#'   #   dplyr::filter(owp_tw_inchan >= cs_lengthm) %>% 
#'   #   dplyr::mutate( new_Ymax = scale_Ymax_to_TW(owp_tw_inchan, owp_y_inchan, cs_lengthm)) %>% 
#'   #   dplyr::select(hf_id, hy_id, cs_id, cs_lengthm, owp_tw_inchan, owp_y_inchan, new_Ymax) 
#'   
#'   # ##############################################
#'   # ################# Testing area ###############
#'   # ##############################################
#'   # 
#'   # cross_section_pts <-
#'   #   inchannel_cs %>%
#'   #   # dplyr::filter(hy_id == "wb-1002477", cs_id == "2")
#'   #   dplyr::filter(hy_id == "wb-1002477", cs_id %in% c("2", "3"))
#'   # top_width = "owp_tw_inchan"
#'   # depth     = "owp_y_inchan"
#'   # dingman_r = "owp_dingman_r"
#'   
#'   # cross_section_pts <- bad_inchannels
#'   # top_width         = "owp_tw_inchan"
#'   # depth             = "owp_y_inchan" 
#'   # dingman_r         = "owp_dingman_r"
#'   
#'   # cross_section_pts <- fix_oversized_topwidths(
#'   #   cross_section_pts = cross_section_pts,
#'   #   top_width     = "owp_tw_inchan",
#'   #   depth         = "owp_y_inchan",
#'   #   length_col    = "cs_lengthm"
#'   # )
#'   # 
#'   # ahg_parabolas <- get_ahg_parabolas(
#'   #                       cross_section_pts = cross_section_pts,
#'   #                       top_width         = "owp_tw_inchan",
#'   #                       depth             = "owp_y_inchan",
#'   #                       dingman_r         = "owp_dingman_r"
#'   #                     )
#'   
#'   # ahg_parabolas[ahg_parabolas$cs_id == 2, ]$ahg_y %>% plot()
#'   # ahg_parabolas[ahg_parabolas$cs_id == 3, ]$ahg_y %>% plot()
#'   # 
#'   # ##############################################
#'   # ##############################################
#'   
#'   # plot(ahg_parabolas$ahg_y)
#'   
#'   # store the maximum X on the left side of the parabola for later use
#'   ahg_left_max <- 
#'     ahg_parabolas %>% 
#'     dplyr::filter(partition == "left") %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::summarise(left_max = max(ahg_x, na.rm = TRUE)) %>% 
#'     dplyr::ungroup()
#'   
#'   # ------------------------------------------------------------------------------------------------
#'   # ---- Partition input cross section points (left/right) ----
#'   # ------------------------------------------------------------------------------------------------
#'   
#'   # split the cross section into a left and right half, from the midpoint of the bottom
#'   # and then join on the maximum X point of the LEFT half of the AHG parabolas 
#'   # this paritioned set of cross sections will ultimately get the AHG parabolas inserted in between the left and right partitions
#'   partioned_cs <- 
#'     cross_section_pts %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       # relative_distance_of_bottom = point_type == "bottom" 
#'       bottom_midpoint = dplyr::case_when(
#'         point_type == "bottom" ~ relative_distance,
#'         TRUE                   ~ NA
#'       ),
#'       bottom_midpoint = stats::median(bottom_midpoint, na.rm = TRUE)
#'     ) %>% 
#'     # dplyr::relocate(bottom_midpoint) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       # relative_distance_of_bottom = point_type == "bottom" 
#'       cs_partition = dplyr::case_when(
#'         relative_distance < bottom_midpoint ~ "left_cs",
#'         TRUE                                ~ "right_cs"
#'       )
#'       # bottom_midpoint = stats::median(bottom_midpoint, na.rm = TRUE)
#'     )  %>% 
#'     dplyr::left_join(
#'       ahg_left_max,
#'       by = c("hy_id", "cs_id")
#'     ) %>% 
#'     # dplyr::relocate(left_max, bottom_midpoint, cs_partition) %>% 
#'     dplyr::ungroup()
#'   
#'   #  get the midpoint value for each hy_id/cs_id so we can use them during the shifting process 
#'   midpoints <- 
#'     partioned_cs %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::select(hy_id, cs_id, bottom_midpoint) %>% 
#'     dplyr::slice(1) %>% 
#'     dplyr::ungroup()
#'   
#'   # ------------------------------------------------------------------------------------------------
#'   # ---- Process LEFT side of cross section and parabola ----
#'   # ------------------------------------------------------------------------------------------------
#'   # # lefty <- 
#'   #   partioned_cs %>% 
#'   #   # dplyr::filter(cs_partition == "left_cs") %>%  
#'   #   dplyr::group_by(hy_id, cs_id) %>% 
#'   #   # dplyr::mutate(
#'   #   #   res = bottom_midpoint - max(left_max),
#'   #   #   mark = relative_distance < bottom_midpoint - max(left_max) | relative_distance == 0
#'   #   # ) %>% 
#'   #   # dplyr::relocate(res, mark, relative_distance)
#'   #   dplyr::filter(
#'   #     relative_distance < (bottom_midpoint - max(left_max)) | relative_distance == 0
#'   #     # relative_distance < 0
#'   #     )
#'   # left_new <- partioned_cs %>% 
#'   #   dplyr::filter(cs_partition == "left_cs") %>%  
#'   #   dplyr::group_by(hy_id, cs_id) %>% 
#'   #   dplyr::filter(
#'   #     # relative_distance < (bottom_midpoint - max(left_max))
#'   #     relative_distance < (bottom_midpoint - max(left_max)) | relative_distance == 0 # TODO: testing this new condition out
#'   #   ) 
#'   
#'   # # TODO: look back at this tomorrow
#'   # left_side_pt_counts <- 
#'   #   partioned_cs %>% 
#'   #   dplyr::filter(cs_partition == "left_cs") %>%  
#'   #   dplyr::group_by(hy_id, cs_id) %>% 
#'   #   dplyr::mutate(
#'   #     marked = relative_distance < (bottom_midpoint - max(left_max))
#'   #   ) %>% 
#'   #   dplyr::select(hy_id, cs_id, marked) %>% 
#'   #   dplyr::summarise(total_left_side_pts = sum(marked, na.rm = TRUE)) %>% 
#'   #   dplyr::ungroup()
#'   # partioned_cs %>% 
#'   #   dplyr::filter(cs_partition == "left_cs") %>% 
#'   #   dplyr::left_join(
#'   #     left_side_pt_counts,
#'   #     by = c("hy_id", "cs_id")
#'   #   ) 
#' 
#'   # grab just the left cross sections and remove any points that will be swallowed by the newly inserted AHG estimates 
#'   # And also determine the offset of the left parabolas X points, the left_start will be joined back onto the AHG parabolas
#'   left_cs <- 
#'     partioned_cs %>% 
#'     dplyr::filter(cs_partition == "left_cs") %>%  
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::filter(
#'       # relative_distance < (bottom_midpoint - max(left_max))
#'       relative_distance < (bottom_midpoint - max(left_max)) | relative_distance == 0 # TODO: testing this new condition out
#'       # relative_distance < (bottom_midpoint - max(left_max)) | (relative_distance == 0 & total_left_side_pts == 0)  # TODO: testing this new condition out
#'       ) %>% 
#'     dplyr::mutate(
#'       left_start = bottom_midpoint - max(left_max)
#'     ) %>% 
#'     dplyr::ungroup()
#'   
#'   left_starts <- 
#'     left_cs %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::select(hy_id, cs_id, left_start) %>% 
#'     dplyr::slice(1) %>% 
#'     dplyr::ungroup()
#'   
#'   # offset the left parabolas X points using the left_start value
#'   left_parabolas <-
#'     ahg_parabolas %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::filter(partition == "left") %>% 
#'     dplyr::ungroup() %>% 
#'     dplyr::left_join(
#'       left_starts,
#'       by = c("hy_id", "cs_id")
#'     ) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       ahg_x = ahg_x + left_start
#'     ) %>% 
#'     dplyr::ungroup()
#'   
#'   # ------------------------------------------------------------------------------------------------
#'   # ---- Process RIGHT side of cross section and parabola ----
#'   # ------------------------------------------------------------------------------------------------
#'   
#'   # subset cross section to the RIGHT of the midpoint
#'   right_cs <-
#'     partioned_cs %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::filter(relative_distance > bottom_midpoint) %>% 
#'     # dplyr::filter(cs_partition == "right_cs")
#'     dplyr::ungroup()
#'   
#'   right_parabolas <- 
#'     ahg_parabolas %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::filter(partition == "right") %>% 
#'     dplyr::ungroup() %>% 
#'     dplyr::left_join(
#'       ahg_left_max,
#'       by = c("hy_id", "cs_id")
#'     ) %>% 
#'     dplyr::left_join(
#'       midpoints,
#'       by = c("hy_id", "cs_id")
#'     ) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       right_start = bottom_midpoint + ((ahg_x) - left_max)
#'     ) %>% 
#'     dplyr::ungroup()
#'   
#'   # ----------------------------------------------------------------------------------------------------------------
#'   # ------- Still reviewing this additon ---------
#'   # --- This ties back to the fix_oversized_topwidths() function applied at the beginning -----
#'   # ----------------------------------------------------------------------------------------------------------------
#'   # TODO: Newly added to deal with situations where the right side of the parabola is TOO LONG,
#'   # TODO: and will go past the outside of the predefined cross section length 
#'   # TODO: This all needs to evaluated and double checked to make sure it makes 
#'   # TODO: sense hydrologically and won't break the standard "good" case
#'   # for each cross section, we isolate the total length of the cross section 
#'   # to make sure that the parabola is not going past the edge of the cross section
#'   total_cross_section_length <-
#'     right_cs %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     # dplyr::filter(relative_distance == max(relative_distance)) %>% 
#'     dplyr::slice_max(relative_distance, n = 1) %>% 
#'     dplyr::select(hy_id, cs_id, 
#'                   max_right_position = relative_distance) %>% 
#'     dplyr::ungroup() 
#'  
#'   # from the right side of the parabola, 
#'   # we remove any parabola points that would be past
#'   # the last right side cross section points
#'   right_parabolas <- 
#'     right_parabolas %>% 
#'     dplyr::left_join(
#'       total_cross_section_length,
#'       by = c("hy_id", "cs_id")
#'     )  %>% 
#'     # dplyr::relocate(hy_id, cs_id, right_start, max_right_position) %>% 
#'     dplyr::filter(right_start < max_right_position) 
#'   
#'   # ----------------------------------------------------------------------------------------------------------------
#'   # TODO: Above still needs review ^^^ 
#'   # ----------------------------------------------------------------------------------------------------------------
#'   
#'   # getting the starting X value for the RIGHT side of the parabola
#'   max_right_starting_pts <- 
#'     right_parabolas %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::summarise(
#'       right_start_max = max(right_start, na.rm = TRUE)
#'     ) %>% 
#'     dplyr::ungroup()
#'   
#'   # removing cross section point that will be replaced by right_parabola points
#'   right_cs <- 
#'     right_cs %>% 
#'     dplyr::left_join(
#'       max_right_starting_pts, 
#'       by = c("hy_id", "cs_id")
#'     ) %>% 
#'     # dplyr::relocate(right_start_max) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::filter(
#'       relative_distance > right_start_max
#'     ) %>% 
#'     dplyr::ungroup()
#'   
#'   # ---------------------------------------------------------------------------------------------------
#'   # ---- MERGE the left and right sides of the parabolas -----
#'   # ---------------------------------------------------------------------------------------------------
#'   
#'   right_parabolas <- 
#'     right_parabolas %>% 
#'     dplyr::select(-ahg_x) %>% 
#'     dplyr::rename(ahg_x = right_start) %>% 
#'     dplyr::select(
#'       hy_id, cs_id, ahg_index, ahg_x, ahg_y, ahg_a,
#'       partition
#'       # left_max, bottom_midpoint
#'     )
#'   
#'   left_parabolas <- 
#'     left_parabolas %>% 
#'     dplyr::select(
#'       hy_id, cs_id, ahg_index, ahg_x, ahg_y, ahg_a,
#'       partition
#'     )
#'   
#'   # merge
#'   parabolas <- dplyr::bind_rows(left_parabolas, right_parabolas)
#'   
#'   # reorder to parabolas by X values so they are in order from left to right for each hy_id/cs_id
#'   parabolas <- 
#'     parabolas %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::arrange(ahg_x, .by_group = TRUE) %>% 
#'     dplyr::ungroup()
#'   
#'   # select relevant columns and adjust the names so 
#'   # the AHG parabola can be inserted nicely with the original cross sections
#'     # NOTE: 
#'       # AHG X values == "relative_distance" in cross_section_pts
#'       # AHG Y values == "Z" in cross_section_pts
#'   parabolas <- 
#'     parabolas %>% 
#'     dplyr::select(
#'       hy_id, cs_id,
#'       relative_distance = ahg_x,
#'       Z                 = ahg_y
#'     )
#'   
#'   # ---------------------------------------------------------------------------------------------------
#'   # ---- Insert the parabolas in between the LEFT and RIGHT cross section partitions -----
#'   # ---------------------------------------------------------------------------------------------------
#'   
#'   # drop unneeded columns
#'   left_cs <- dplyr::select(left_cs, 
#'                            -left_start, -left_max, -bottom_midpoint, -cs_partition)
#'   right_cs <- dplyr::select(right_cs, 
#'                             -right_start_max, -left_max, -bottom_midpoint, -cs_partition)
#'   
#'   # combine left cross section points, parabola, and right cross section points
#'   # and then reorder each cross section (hy_id/cs_id) by the relative distance 
#'   # so all the points are in correct order
#'   out_cs <-
#'     dplyr::bind_rows(
#'       # left_cs,
#'       # parabolas,
#'       # right_cs
#'       dplyr::mutate(left_cs, is_dem_point = TRUE),
#'       dplyr::mutate(parabolas, is_dem_point = FALSE),
#'       dplyr::mutate(right_cs, is_dem_point = TRUE),
#'     ) %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::filter(relative_distance >= 0) %>% # TODO: testing out this condition as well
#'     dplyr::arrange(relative_distance, .by_group = TRUE) %>% 
#'     dplyr::ungroup()
#'   
#'   # Assign / renumber the "pt_ids" and 
#'   # set the "point_types" of the inserted parabola points to "bottom" type
#'   out_cs <- 
#'     out_cs %>% 
#'     dplyr::group_by(hy_id, cs_id) %>% 
#'     dplyr::mutate(
#'       pt_id      = 1:dplyr::n(),
#'       class = dplyr::case_when(
#'         is.na(class)      ~ "bottom",
#'         TRUE              ~ class
#'       ),
#'       point_type = dplyr::case_when(
#'         is.na(point_type) ~ "bottom",
#'         TRUE              ~ point_type
#'       )
#'     ) %>% 
#'     dplyr::ungroup()
#'   
#'   # parabolas %>%
#'   #   hydrofabric3D::add_tmp_id() %>%
#'   #   ggplot2::ggplot() +
#'   #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z)) +
#'   #   ggplot2::facet_wrap(~tmp_id)
#'   # out_cs %>%
#'   #   hydrofabric3D::plot_cs_pts(x = "relative_distance", color = "is_dem_point")
#'   
#'   tryCatch({
#'     message("Generate XY coordinates for AHG estimated points...")
#'     out_cs <- fill_missing_ahg_coords(out_cs)
#' 
#'   }, error = function(cond) {
#' 
#'     message("Failed to fix X/Y coordinates for estimated bathymetry points, returning cross section points with inserted bathymetry with missing X/Y values")
#'     message(conditionMessage(cond))
#' 
#'     # Choose a return value in case of error
#'     return(out_cs)
#' 
#'   })
#'   
#'   return(out_cs)
#'   
#' }
#' 
#' 
#' #Fix the missing X/Y coordinates (NAs) from the inserted AHG Parabola points in a set of cross section points
#' fix_xy <- function(df) {
#'   # df = fix_coords
#'   
#'   missing_coords_indices <- which(is.na(df$X))
#'   
#'   if (length(missing_coords_indices) == 0) {
#'     return(df)
#'   }
#'   
#'   first_NA         <- missing_coords_indices[1]
#'   last_NA          <- missing_coords_indices[length(missing_coords_indices)]
#'   number_of_points <- length(missing_coords_indices)
#'   
#'   # Get the start / end X / Y points
#'   start_X <- df$X[first_NA - 1]
#'   end_X   <- df$X[last_NA + 1]
#'   start_Y <- df$Y[first_NA - 1]
#'   end_Y   <- df$Y[last_NA + 1]
#'   
#'   # Generate new X / Y coordinates
#'   X_coords <- seq(start_X, end_X, length.out = number_of_points + 2)
#'   Y_coords <- seq(start_Y, end_Y, length.out = number_of_points + 2)
#'   
#'   # Insert the new coordinates into the original missing rows
#'   df[first_NA:last_NA, ]$X <- X_coords[2:(length(X_coords) - 1)]
#'   df[first_NA:last_NA, ]$Y <- Y_coords[2:(length(Y_coords) - 1)]
#'   
#'   return(df)
#' }
#' 
#' # TODO: DELETE DEPRECATED
#' 
#' #' Given provide inchannel widths and depths to a set of cross section points and derive estimated shapes
#' #' @description
#' #' Still in early development phases
#' #' @param cross_section_pts dataframe or sf dataframe
#' #' @param r numeric, R coefficient
#' #' @param inchannel_width numeric
#' #' @param inchannel_depth numeric
#' #' @param drop_negative_depths logical, whether to remove any depths that are negative, default is FALSE (probably Deprecated at this point)
#' #' @importFrom dplyr bind_rows select mutate n case_when
#' #' @importFrom AHGestimation cross_section
#' #' @importFrom stats median
#' #' @return dataframe or sf dataframe with AHG estimated points injected into the input cross section points
#' #' @export
#' cs_inchannel_estimate <- function(
#'     cross_section_pts,
#'     r = 3,
#'     inchannel_width,
#'     inchannel_depth,
#'     drop_negative_depths = FALSE
#' ) {
#' 
#'   #####################################
#' 
#'   #####################################
#' 
#'   primary_z     <- cross_section_pts$Z
#'   rel_distance  <- cross_section_pts$relative_distance
#'   channel_point_type <- cross_section_pts$point_type
#' 
#'   # hydrofabric3D::plot_cs_pts(cross_section_pts, color = "point_type")
#'   # plot(primary_z)
#' 
#'   # cross_section_pts %>%
#'   #   ggplot2::ggplot() +
#'   #   ggplot2::scale_y_continuous(limits = c(0, max(cross_section_pts$Z)),
#'   #                               breaks = seq(0, max(cross_section_pts$Z),
#'   #                               by = (max(cross_section_pts$Z) - min(cross_section_pts$Z)) / 4)) +
#'   #   # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = point_type) )+
#'   #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = point_type) )+
#'   #   ggplot2::facet_grid(hy_id~cross_section_pts_id, scale = "free_y")
#' 
#'   # # relative distance of the bottoms
#'   bottom   <- rel_distance[channel_point_type == "bottom"]
#'   bottomZ  <- primary_z[channel_point_type == "bottom"]
#' 
#'   # # find the middle of the bottom
#'   midpoint <- stats::median(bottom)
#' 
#'   # generate AHG estimates
#'   ahg_est <- AHGestimation::cross_section(
#'     r    = r,
#'     TW   = inchannel_width,
#'     Ymax = inchannel_depth
#'   )
#' 
#'   # plot(ahg_est$Y)
#' 
#'   # indices of the left and right parabola halves
#'   left_half  = 1:(nrow(ahg_est) / 2)
#'   right_half = (1 + (nrow(ahg_est) / 2)):nrow(ahg_est)
#' 
#'   # get the left and right side of the parabolas
#'   left_parabola = ahg_est[left_half, ]
#'   right_parabola = ahg_est[right_half, ]
#' 
#'   # shift the Z values to have there max points be at the "bottom" of the "cross_section_pts" points
#'   left_parabola$Y <- left_parabola$Y + (bottomZ[1] - max(left_parabola$Y))
#'   right_parabola$Y <- right_parabola$Y + (bottomZ[1] - max(right_parabola$Y))
#' 
#'   #  set any negative values in parabola to 0
#'   if (drop_negative_depths) {
#'     left_parabola$Y [left_parabola$Y  < 0] = 0
#'     right_parabola$Y[right_parabola$Y < 0] = 0
#'   }
#' 
#'   # Offset LEFT parabola (X values)
#' 
#'   # original maximum left X value (to use for offsetting right_parabola in a future step)
#'   left_max <- max(left_parabola$x)
#' 
#'   # subset cross section to the LEFT of the midpoint
#'   left_cs <- cross_section_pts[rel_distance < midpoint, ]
#' 
#'   # removing cross section point that will be replaced by left_parabola points
#'   left_cs <- left_cs[left_cs$relative_distance < (midpoint - max(left_parabola$x)), ]
#' 
#'   # getting the starting X value for the LEFT side of the parabola
#'   left_start <- midpoint - max(left_parabola$x)
#' 
#'   # offset X values to fit within cross_section_pts points
#'   left_parabola$x <- left_start + left_parabola$x
#' 
#' 
#'   # Offset RIGHT parabola (X values)
#' 
#'   # subset cross section to the RIGHT of the midpoint
#'   right_cs <- cross_section_pts[rel_distance > midpoint, ]
#' 
#'   # getting the starting X value for the RIGHT side of the parabola
#'   right_start <- midpoint + ((right_parabola$x) - left_max)
#' 
#'   # removing cross section point that will be replaced by right_parabola points
#'   right_cs <- right_cs[right_cs$relative_distance > max(right_start), ]
#'   # right_cs <- right_cs[right_cs$relative_distance > (midpoint + max(right_parabola$x)), ]
#' 
#'   # offset X values to fit within cs points
#'   right_parabola$x <- right_start
#' 
#'   # # get the last point on the LEFT side of parabola
#'   # last_left <- dplyr::slice_tail(left_parabola)
#'   #
#'   # # get the first point on the RIGHT side of parabola
#'   # first_right <- dplyr::slice_head(right_parabola)
#'   #
#'   # # create an additional point in the middle between the left and right parabolas
#'   # extra_midpoint <- data.frame(
#'   #                     ind = last_left$ind + 1,
#'   #                     x   = median(c(last_left$x, first_right$x)),
#'   #                     Y   = median(c(last_left$Y, first_right$Y)),
#'   #                     A   = median(c(last_left$A, first_right$A))
#'   #                   )
#' 
#'   # combine all parts of the parabola back together
#'   parabola <- dplyr::bind_rows(left_parabola, right_parabola)
#'   # parabola <- dplyr::bind_rows(left_parabola, extra_midpoint, right_parabola)
#' 
#'   # select relevant columns and adjust the names
#'   parabola <-
#'     parabola %>%
#'     dplyr::select(
#'       relative_distance = x,
#'       Z                 = Y
#'     )
#' 
#'   # combine left cross section points, parabola, and right cross section points
#'   out_cs <- dplyr::bind_rows(
#'     left_cs,
#'     parabola,
#'     right_cs
#'   )
#' 
#'   # Add new pt_id to account for inserted parabola points, and assign all parabola points to have a point_type of "bottom"
#'   out_cs <-
#'     out_cs %>%
#'     dplyr::mutate(
#'       pt_id = 1:dplyr::n(),
#'       point_type = dplyr::case_when(
#'         is.na(point_type) ~ "bottom",
#'         TRUE         ~ point_type
#'       )
#'     )
#' 
#'   # Add back ID data
#'   out_cs$hy_id      <- cross_section_pts$hy_id[1]
#'   out_cs$cs_id      <- cross_section_pts$cs_id[1]
#'   out_cs$cs_lengthm <- cross_section_pts$cs_lengthm[1]
#'   # out_cs$Z_source   <- cross_section_pts$Z_source[1]
#' 
#'   # ahg_est
#'   # plot(ahg_est$Y)
#'   # plot(out_cs$Z)
#' 
#'   # plot_df <- dplyr::bind_rows(
#'   #             dplyr::mutate(cs,
#'   #                           source = "DEM"
#'   #             ),
#'   #             dplyr::mutate(out_cs,
#'   #                           source = "AHG Estimate (In channel)"
#'   #             )
#'   #           )
#'   # plot_df %>%
#'   #   ggplot2::ggplot() +
#'   #   ggplot2::scale_y_continuous(limits = c(-1, 2),
#'   #                               breaks = seq(-5, 2,
#'   #                                            by = 0.5)) +
#'   #   # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = point_type) )+
#'   #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = point_type) )+
#'   #   ggplot2::facet_wrap(source~., scale = "free_y")
#'   #
#'   # out_cs %>%
#'   #   ggplot2::ggplot() +
#'   #   ggplot2::scale_y_continuous(limits = c(-1, 2),
#'   #                               breaks = seq(-5, 2,
#'   #                                            by = 0.5)) +
#'   #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = point_type) )+
#'   #   ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
#' 
#'   return(out_cs)
#' 
#' }

# #########W  #########W  #########W  #########W#########W  #########W  #########W  #########W#########W  #########W  #########W  #########W
# #########W  #########W  #########W  #########W UNCOMMENT ABOVE #########W  #########W  #########W  #########W#########W  #########W  #########W  #########W
# #########W  #########W  #########W  #########W#########W  #########W  #########W  #########W#########W  #########W  #########W  #########W
# 
# cs_bf_estimate <- function(cs, bf_width, bf_depth) {
#   library(dplyr)
#   #########W  #########W  #########W  #########W
#   #########W  #########W  #########W  #########W
#   #########W  #########W  #########W  #########W
#   cs_pts = arrow::read_parquet("/Users/anguswatters/Desktop/lynker-spatial/02_cs_pts/nextgen_12_cross_sections.parquet")
#   ml_widths <- arrow::read_parquet("/Users/anguswatters/Downloads/conus_width_depth_ml.parquet")
#   # ml_widths %>%
#   #   dplyr::filter(FEATUREID == 5587412)
# 
#   net = arrow::open_dataset('s3://lynker-spatial/v20.1/conus_net.parquet') %>%
#     dplyr::filter(vpu == 12)
#   aoi <-
#     net %>%
#     dplyr::collect()
# 
#   # fab <- sf::read_sf("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg/nextgen_12.gpkg", layer = "flowpaths")
#   # sf::st_layers("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg/nextgen_12.gpkg")
# 
#   # tmp_i <- 32771
#   tmp_i <- 33437
#   # tmp_i <- 33298
#   which(aoi$id == "wb-2398332")
#   # which(aoi$id == "wb-2398289")
# 
#   mlw <-
#     ml_widths %>%
#     dplyr::filter(FEATUREID ==  aoi$hf_id[tmp_i])
#   # which(aoi$id == "wb-2398284")
#   # cs_points %>%
#   cs_pts %>%
#     dplyr::filter(hy_id == aoi$id[tmp_i]) %>%
#     ggplot2::ggplot() +
#     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
#     ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
#     ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# 
#   cs <-
#     cs_pts %>%
#     dplyr::filter(hy_id ==  aoi$id[tmp_i], cs_id == 3)
# 
#   bf_width <- mlw$owp_tw_bf
#   bf_depth <- mlw$owp_y_bf
# 
#   #########W  #########W  #########W  #########W
#   #########W  #########W  #########W  #########W
#   #########W  #########W  #########W  #########W
# 
# 
#   primary_z     <- cs$Z
#   rel_distance  <- cs$relative_distance
#   channel_class <- cs$class
#   # max_z = round(max(cs_points$Z), 2)
#   # total_length  <- cs_points$cs_lengthm[1]
# 
#   ahg_est <- AHGestimation::cross_section(
#     r    = 3,
#     TW   = bf_width,
#     Ymax = bf_depth
#   )
#   plot(ahg_est$Y)
#   plot(cs$Z~cs$relative_distance)
#   # channel_class
#   # rel_distance
# 
#   # relative distance of the bottoms
#   bottoms <- rel_distance[channel_class == "bottom"]
#   # bottoms <- rel_distance[channel_class %in% c("channel", "bottom")]
# 
#   # find the middle of the bottom
#   middle_pt <- median(bottoms)
# 
#   message("middle_pt: ", round(middle_pt, 3))
# 
#   # AHG Estimated X and Y values
#   ahg_x <- ahg_est$x
#   ahg_z <- ahg_est$Y
# 
#   # plot(ahg_est$Y)
# 
#   # distnace in both directios from the middle of the channel bottom
#   middle_to_left  <- abs(rel_distance - (middle_pt - (max(ahg_x)/2)))
#   middle_to_right <- abs(rel_distance - (middle_pt + (max(ahg_x)/2)))
# 
#   # indices to "pin" AHG estimate into original Z values
#   left_bank  <- which.min(middle_to_left)
#   # right_bank <- which.min(middle_to_right)
#   right_bank <- which.min(middle_to_right)
# 
#   # extract the relative distance (x) values to the left and right of the channel bottom,
#   # these X values will be the distance along the cross section for each point,
#   # we'll be inserting the AHG X values between the left and right relative distances
#   left_side <- rel_distance[1:(left_bank-1)]
#   right_side <- rel_distance[(1 + right_bank):length(rel_distance)]
#   # left_side <- rel_distance[1:(left_bank)]
#   # right_side <- rel_distance[(right_bank):length(rel_distance)]
#   #
#   # insert the AHG X distances between the original X relative distances
#   final_x <- c(left_side, left_side[length(left_side)] + ahg_x, right_side)
#   final_x <- round(final_x, 2)
# 
#   # cs_points %>%
#   #   dplyr::filter(class == "left_bank") %>%
#   #   dplyr::slice_min(Z) %>%
#   #   dplyr::slice_max(pt_id)
#   #   # dplyr::slice_max(pt_id)
#   #
#   # cs_points %>%
#   #   dplyr::filter(class == "right_bank") %>%
#   #   dplyr::slice_min(Z) %>%
#   #   dplyr::slice_min(pt_id)
#   # cs_points$Z %>% which.max()
# 
#   # extract the Z values to the left and right of the channel bottom, these Z values will "surround" the AHG estimate Z values
#   left_z  <- primary_z[1:(left_bank-1)]
#   right_z <- primary_z[(1 + right_bank):length(primary_z)]
# 
#   shift_ahg_z <- ahg_z + (left_z[length(left_z)]  - max(ahg_z))
# 
#   final_z <- c(left_z, shift_ahg_z, right_z)
#   # final_z <- c(left_z,  ahg_z, right_z)
# 
#   plot(final_z)
# 
#   final_z <- round(final_z, 2)
#   ahg_z + left_z[length(left_z)]
#   final_class <- c(
#     rep("left_bank", length(left_z)),
#     rep("bottom", length(ahg_z)),
#     rep("right_bank", length(right_z))
#   )
# 
#   output <- data.frame(
#     x = final_x,
#     z = final_z,
#     class = final_class
#   ) %>%
#     dplyr::tibble() %>%
#     dplyr::mutate(
#       is_ahg_estimate = dplyr::case_when(
#         class == "bottom" ~ TRUE,
#         TRUE              ~ FALSE
#       )
#     )
#   plot(cs$Z~cs$relative_distance)
#   plot(output$z~output$x)
#   ggplot2::ggplot() +
#     ggplot2::scale_y_continuous(limits = c(0, 25), breaks = seq(0, 15, by = 5)) +
#     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
#     # ggplot2::geom_point(data = cs, ggplot2::aes(x = relative_distance, y = Z, color = class),
#     #                     size = 8,
#     #                     alpha = 0.4,
#     #                     shape = 18
#     # ) +
#     ggplot2::geom_point(data = output,
#                         ggplot2::aes(x = x, y = z, color = is_ahg_estimate),
#                         size = 3
#     ) +
#     # ggplot2::scale_y_continuous(limits = c(0, 20), breaks = seq(0, 15, by = 5)) +
#     ggplot2::theme(legend.position = "bottom")
#   ggplot2::ggplot() +
#     ggplot2::scale_y_continuous(limits = c(0, 25), breaks = seq(0, 15, by = 5)) +
#     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
#     ggplot2::geom_point(data = cs, ggplot2::aes(x = relative_distance, y = Z, color = class),
#                         size = 8,
#                         alpha = 0.4,
#                         shape = 18
#     ) +
#     ggplot2::geom_point(data = output,
#                         ggplot2::aes(x = x, y = z, color = class),
#                         size = 3
#     ) +
#     # ggplot2::scale_y_continuous(limits = c(0, 20), breaks = seq(0, 15, by = 5)) +
#     ggplot2::theme(legend.position = "bottom")
# 
# }
# # # cs_bf_estimate <- function(cs, bf_width, bf_depth) {
# # #   library(dplyr)
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # #   cs_pts = arrow::read_parquet("/Users/anguswatters/Desktop/lynker-spatial/02_cs_pts/nextgen_12_cross_sections.parquet")
# # #   ml_widths <- arrow::read_parquet("/Users/anguswatters/Downloads/conus_width_depth_ml.parquet")
# # #   # ml_widths %>%
# # #   #   dplyr::filter(FEATUREID == 5587412)
# # # 
# # #   net = arrow::open_dataset('s3://lynker-spatial/v20.1/conus_net.parquet') %>%
# # #     dplyr::filter(vpu == 12)
# # #   aoi <-
# # #     net %>%
# # #     dplyr::collect()
# # # 
# # #   # fab <- sf::read_sf("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg/nextgen_12.gpkg", layer = "flowpaths")
# # #   # sf::st_layers("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg/nextgen_12.gpkg")
# # # 
# # #   # tmp_i <- 32771
# # #   tmp_i <- 33437
# # #   # tmp_i <- 33298
# # #   which(aoi$id == "wb-2398332")
# # #   # which(aoi$id == "wb-2398289")
# # # 
# # #   mlw <-
# # #     ml_widths %>%
# # #     dplyr::filter(FEATUREID ==  aoi$hf_id[tmp_i])
# # #   # which(aoi$id == "wb-2398284")
# # #   # cs_points %>%
# # #   cs_pts %>%
# # #     dplyr::filter(hy_id == aoi$id[tmp_i]) %>%
# # #     ggplot2::ggplot() +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # #     ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # # 
# # #   cs <-
# # #     cs_pts %>%
# # #     dplyr::filter(hy_id ==  aoi$id[tmp_i], cs_id == 3)
# # # 
# # #   bf_width <- mlw$owp_tw_bf
# # #   bf_depth <- mlw$owp_y_bf
# # # 
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # # 
# # # 
# # #   primary_z     <- cs$Z
# # #   rel_distance  <- cs$relative_distance
# # #   channel_class <- cs$class
# # #   # max_z = round(max(cs_points$Z), 2)
# # #   # total_length  <- cs_points$cs_lengthm[1]
# # # 
# # #   ahg_est <- AHGestimation::cross_section(
# # #     r    = 3,
# # #     TW   = bf_width,
# # #     Ymax = bf_depth
# # #   )
# # #   plot(ahg_est$Y)
# # #   plot(cs$Z~cs$relative_distance)
# # #   # channel_class
# # #   # rel_distance
# # # 
# # #   # relative distance of the bottoms
# # #   bottoms <- rel_distance[channel_class == "bottom"]
# # #   # bottoms <- rel_distance[channel_class %in% c("channel", "bottom")]
# # # 
# # #   # find the middle of the bottom
# # #   middle_pt <- median(bottoms)
# # # 
# # #   message("middle_pt: ", round(middle_pt, 3))
# # # 
# # #   # AHG Estimated X and Y values
# # #   ahg_x <- ahg_est$x
# # #   ahg_z <- ahg_est$Y
# # # 
# # #   # plot(ahg_est$Y)
# # # 
# # #   # distnace in both directios from the middle of the channel bottom
# # #   middle_to_left  <- abs(rel_distance - (middle_pt - (max(ahg_x)/2)))
# # #   middle_to_right <- abs(rel_distance - (middle_pt + (max(ahg_x)/2)))
# # # 
# # #   # indices to "pin" AHG estimate into original Z values
# # #   left_bank  <- which.min(middle_to_left)
# # #   # right_bank <- which.min(middle_to_right)
# # #   right_bank <- which.min(middle_to_right)
# # # 
# # #   # extract the relative distance (x) values to the left and right of the channel bottom,
# # #   # these X values will be the distance along the cross section for each point,
# # #   # we'll be inserting the AHG X values between the left and right relative distances
# # #   left_side <- rel_distance[1:(left_bank-1)]
# # #   right_side <- rel_distance[(1 + right_bank):length(rel_distance)]
# # #   # left_side <- rel_distance[1:(left_bank)]
# # #   # right_side <- rel_distance[(right_bank):length(rel_distance)]
# # #   #
# # #   # insert the AHG X distances between the original X relative distances
# # #   final_x <- c(left_side, left_side[length(left_side)] + ahg_x, right_side)
# # #   final_x <- round(final_x, 2)
# # # 
# # #   # cs_points %>%
# # #   #   dplyr::filter(class == "left_bank") %>%
# # #   #   dplyr::slice_min(Z) %>%
# # #   #   dplyr::slice_max(pt_id)
# # #   #   # dplyr::slice_max(pt_id)
# # #   #
# # #   # cs_points %>%
# # #   #   dplyr::filter(class == "right_bank") %>%
# # #   #   dplyr::slice_min(Z) %>%
# # #   #   dplyr::slice_min(pt_id)
# # #   # cs_points$Z %>% which.max()
# # # 
# # #   # extract the Z values to the left and right of the channel bottom, these Z values will "surround" the AHG estimate Z values
# # #   left_z  <- primary_z[1:(left_bank-1)]
# # #   right_z <- primary_z[(1 + right_bank):length(primary_z)]
# # # 
# # #   shift_ahg_z <- ahg_z + (left_z[length(left_z)]  - max(ahg_z))
# # # 
# # #   final_z <- c(left_z, shift_ahg_z, right_z)
# # #   # final_z <- c(left_z,  ahg_z, right_z)
# # # 
# # #   plot(final_z)
# # # 
# # #   final_z <- round(final_z, 2)
# # #   ahg_z + left_z[length(left_z)]
# # #   final_class <- c(
# # #     rep("left_bank", length(left_z)),
# # #     rep("bottom", length(ahg_z)),
# # #     rep("right_bank", length(right_z))
# # #   )
# # # 
# # #   output <- data.frame(
# # #     x = final_x,
# # #     z = final_z,
# # #     class = final_class
# # #   ) %>%
# # #     dplyr::tibble() %>%
# # #     dplyr::mutate(
# # #       is_ahg_estimate = dplyr::case_when(
# # #         class == "bottom" ~ TRUE,
# # #         TRUE              ~ FALSE
# # #       )
# # #     )
# # #   plot(cs$Z~cs$relative_distance)
# # #   plot(output$z~output$x)
# # #   ggplot2::ggplot() +
# # #     ggplot2::scale_y_continuous(limits = c(0, 25), breaks = seq(0, 15, by = 5)) +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     # ggplot2::geom_point(data = cs, ggplot2::aes(x = relative_distance, y = Z, color = class),
# # #     #                     size = 8,
# # #     #                     alpha = 0.4,
# # #     #                     shape = 18
# # #     # ) +
# # #     ggplot2::geom_point(data = output,
# # #                         ggplot2::aes(x = x, y = z, color = is_ahg_estimate),
# # #                         size = 3
# # #     ) +
# # #     # ggplot2::scale_y_continuous(limits = c(0, 20), breaks = seq(0, 15, by = 5)) +
# # #     ggplot2::theme(legend.position = "bottom")
# # #   ggplot2::ggplot() +
# # #     ggplot2::scale_y_continuous(limits = c(0, 25), breaks = seq(0, 15, by = 5)) +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::geom_point(data = cs, ggplot2::aes(x = relative_distance, y = Z, color = class),
# # #                         size = 8,
# # #                         alpha = 0.4,
# # #                         shape = 18
# # #     ) +
# # #     ggplot2::geom_point(data = output,
# # #                         ggplot2::aes(x = x, y = z, color = class),
# # #                         size = 3
# # #     ) +
# # #     # ggplot2::scale_y_continuous(limits = c(0, 20), breaks = seq(0, 15, by = 5)) +
# # #     ggplot2::theme(legend.position = "bottom")
# # # 
# # # }
# # # 
# # # #########W  #########W  #########W  #########W
# # # #   ######### EXAMPLE DATA  #########
# # # #########W  #########W  #########W  #########W
# # # #########W  #########W  #########W  #########W
# # # cs_pts = arrow::read_parquet("/Users/anguswatters/Desktop/lynker-spatial/02_cs_pts/nextgen_12_cross_sections.parquet")
# # # ml_widths <- arrow::read_parquet("/Users/anguswatters/Downloads/conus_width_depth_ml.parquet")
# # # # ml_widths %>%
# # # #   dplyr::filter(FEATUREID == 5587412)
# # # ml_widths$owp_y_inchan/ml_widths$owp_y_bf %>% hist(breaks = 10)
# # # 
# # # ml_widths %>%
# # #   dplyr::mutate(
# # #     ratio = owp_y_bf/owp_y_inchan
# # #   ) %>%
# # #   # .$ratio %>%
# # #   # max()
# # #   dplyr::filter(ratio < 5) %>%
# # #   .$ratio %>%
# # #   hist()
# # # 
# # # 
# # # bottom_distances <-
# # #   cs_pts %>%
# # #   dplyr::filter(class == "bottom") %>%
# # #   # dplyr::filter(hy_id == "wb-2398282", class == "bottom") %>%
# # #   dplyr::group_by(hy_id, cs_id) %>%
# # #   dplyr::mutate(
# # #     bottom_width = max(relative_distance) - min(relative_distance)
# # #   ) %>%
# # #   dplyr::select(hy_id, cs_id, relative_distance, X, Y, Z, bottom_width) %>%
# # #   dplyr::slice(1) %>%
# # #   dplyr::ungroup()
# # # 
# # # bottom_distances <-
# # #   bottom_distances %>%
# # #   dplyr::left_join(
# # #     dplyr::select(aoi, id, hf_id),
# # #     by = c("hy_id" = "id")
# # #   )
# # # 
# # # bottom_distances <-
# # #   bottom_distances %>%
# # #   dplyr::mutate(hf_id = as.character(hf_id)) %>%
# # #   dplyr::left_join(
# # #     ml_widths,
# # #     by = c("hf_id" = "FEATUREID")
# # #   )
# # # 
# # # plot_df <-
# # #   bottom_distances %>%
# # #   dplyr::mutate(
# # #     ratio = (bottom_width) / owp_tw_inchan
# # #   )
# # # plot_df
# # # plot_df %>%
# # #   dplyr::filter(hy_id == "wb-2412339")
# # # cs_bottoms <-
# # #   cs_pts %>%
# # #   dplyr::mutate(
# # #     tmp_id = paste0(hy_id, "_", cs_id)
# # #     ) %>%
# # #   dplyr::left_join(
# # #     dplyr::select(dplyr::mutate(plot_df,
# # #                     tmp_id = paste0(hy_id, "_", cs_id)
# # #                     ),
# # #                   tmp_id, bottom_width, owp_tw_inchan, ratio),
# # #     by = "tmp_id"
# # #   )
# # #   # dplyr::filter(ratio < 1)
# # #   # dplyr::filter(hy_id %in% dplyr::filter(plot_df, ratio < 1)$hy_id)
# # # cs_bottoms %>%
# # #   dplyr::filter(ratio < 1, ratio > 0) %>%
# # #   dplyr::slice(1:1000) %>%
# # #   sf::st_as_sf(coords = c("X", "Y"), crs = 5070) %>%
# # #   mapview::mapview()
# # # plot_df %>%
# # #   dplyr::filter(ratio > 100) %>%
# # #   sf::st_as_sf(coords = c("X", "Y"), crs = 5070) %>%
# # #   mapview::mapview()
# # # plot_df$ratio %>% max(na.rm = T)
# # # 
# # # # plot_df$owp_tw_inchan %>% is.na()
# # # plot_df %>%
# # #   dplyr::slice(1:50000) %>%
# # #   ggplot2::ggplot() +
# # #   ggplot2::geom_point(ggplot2::aes(x = bottom_width, y = owp_tw_bf))
# # # 
# # # dplyr::filter(FEATUREID ==  aoi$hf_id[tmp_i])
# # # 
# # # 
# # # 
# # # net = arrow::open_dataset('s3://lynker-spatial/v20.1/conus_net.parquet') %>%
# # #   dplyr::filter(vpu == 12)
# # # aoi <-
# # #   net %>%
# # #   dplyr::collect()
# # # 
# # # aoi %>%
# # #   dplyr::filter(hf_id == 5523848) %>%
# # #   .$hf_areasqkm
# # # 
# # #   net_vpu11 = arrow::open_dataset('s3://lynker-spatial/v20.1/conus_net.parquet') %>%
# # #   dplyr::filter(vpu == 11)
# # # 
# # # aoi_vpu11 <-
# # #   net_vpu11 %>%
# # #   dplyr::collect()
# # # aoi_vpu11
# # # # fab <- sf::read_sf("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg/nextgen_12.gpkg", layer = "flowpaths")
# # # # sf::st_layers("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg/nextgen_12.gpkg")
# # # 
# # # 
# # # aoi$id %>% unique()
# # # which(aoi$id == "wb-2398332")
# # # which(aoi$id == "wb-2408548")
# # # which(aoi$id == "wb-2408566")
# # # which(aoi$id == "wb-2398282")
# # # 
# # # tmp <-
# # #   cs_pts %>%
# # #   dplyr::group_by(hy_id, cs_id, class) %>%
# # #   dplyr::summarize(total_rows = n()) %>%
# # #   dplyr::ungroup() %>%
# # #   dplyr::filter(class %in% c("channel", "bottom")) %>%
# # #   dplyr::filter(total_rows > 1) %>%
# # #   # dplyr::filter(total_rows == 5) %>%
# # #   dplyr::arrange(-total_rows)
# # # 
# # # tmp_i <-which(aoi$id == "wb-2398332")[1]
# # # tmp_i <-which(aoi$id == "wb-2408548")[1]
# # # # tmp_i <-which(aoi$id == "wb-2408566")[1]
# # # tmp_i <-which(aoi$id == "wb-2398282")[1]
# # # tmp_i <- which(aoi$id == "wb-2429329")[1]
# # # tmp_i <- which(aoi$id == "wb-2398282")[1]
# # # # 2398282
# # # # tmp_i <- which(aoi$id == "wb-2398686")[1]
# # # 
# # # tmp_i <- which(aoi$id == "wb-2398687")[1]
# # # # tmp_i <- which(aoi$id == "wb-2398287")[1]
# # # 
# # # # "wb-2408566"
# # # # which(aoi$id == "wb-2398289")
# # # 
# # # # tmp_i <- 32771
# # # # tmp_i <- 33437
# # # # tmp_i <- 1129
# # # # tmp_i <- 1239
# # # # tmp_i <- 32769
# # # # tmp_i <- 33298
# # # # tmp_id <- which(aoi$id == "wb-2398282")
# # # # aoi$id[32770]
# # # 
# # # model_widths <-
# # #   ml_widths %>%
# # #   dplyr::filter(FEATUREID ==  aoi$hf_id[tmp_i])
# # #   # dplyr::filter(FEATUREID ==  aoi_vpu11$hf_id[tmp_i])
# # # # which(aoi$id == "wb-2398284")
# # # # cs_points %>%
# # # 
# # # cs_pts %>%
# # #   dplyr::filter(hy_id == aoi$id[tmp_i])
# # # cs_pts %>%
# # #   dplyr::filter(hy_id == aoi$id[tmp_i])  %>%
# # #   .$cs_id %>%
# # #   unique()
# # # cs_pts %>%
# # #   dplyr::filter(hy_id == aoi$id[tmp_i]) %>%
# # #   # dplyr::filter(hy_id == aoi_vpu11$id[tmp_i]) %>%
# # #   ggplot2::ggplot() +
# # #   # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # #   ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # # 
# # # cs <-
# # #   cs_pts %>%
# # #   dplyr::filter(hy_id ==  aoi$id[tmp_i], cs_id == 7)
# # #   # dplyr::filter(hy_id ==  aoi_vpu11$id[tmp_i], cs_id == 2)
# # # cs %>%
# # #   ggplot2::ggplot() +
# # #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class)) +
# # #   ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # # 
# # # bf_width <- model_widths$owp_tw_bf
# # # bf_depth <- model_widths$owp_y_bf
# # # 
# # # inchannel_width <- mlw$owp_tw_inchan
# # # inchannel_depth <- mlw$owp_y_inchan
# # # 
# # # # drop_negative_depths = TRUE
# # # drop_negative_depths = FALSE
# # # 
# # # bf_cs_base <- cs_bankfull_estimate(
# # #   cs = cs,
# # #   r  = 3,
# # #   bf_width = bf_width,
# # #   bf_depth = bf_depth,
# # #   drop_negative_depths = FALSE,
# # #   keep_observed = FALSE,
# # #   only_bottom = FALSE
# # # )
# # # 
# # # bf_cs_keep_observed <- cs_bankfull_estimate(
# # #   cs = cs,
# # #   r  = 3,
# # #   bf_width = bf_width,
# # #   bf_depth = bf_depth,
# # #   drop_negative_depths = FALSE,
# # #   keep_observed = TRUE,
# # #   only_bottom = FALSE
# # # )
# # # 
# # # bf_cs_keep_only_bottom <- cs_bankfull_estimate(
# # #   cs = cs,
# # #   r  = 3,
# # #   bf_width = bf_width,
# # #   bf_depth = bf_depth,
# # #   drop_negative_depths = FALSE,
# # #   keep_observed = TRUE,
# # #   only_bottom = TRUE
# # # )
# # # 
# # # inchannel_cs <- cs_inchannel_estimate(
# # #   cs = cs,
# # #   r  = 3,
# # #   inchannel_width = inchannel_width,
# # #   inchannel_depth = inchannel_depth,
# # #   drop_negative_depths = FALSE
# # # )
# # # 
# # # cs %>%
# # #   sf::st_as_sf(coords = c("X", "Y"), crs = 5070) %>%
# # #   mapview::mapview()
# # # 
# # # plot_df <- dplyr::bind_rows(
# # #             dplyr::mutate(cs,
# # #                           source = "DEM"
# # #             ),
# # #             dplyr::mutate(inchannel_cs,
# # #                           source = "IN CHANNEL"
# # #             ),
# # #             dplyr::mutate(bf_cs_base,
# # #                           source = "BANKFUL - (drop DEM POINTS)"
# # #             ),
# # #             dplyr::mutate(bf_cs_keep_observed,
# # #                           source = "BANKFUL - (kept DEM points)"
# # #             ),
# # #             dplyr::mutate(bf_cs_keep_only_bottom,
# # #                           source = "BANKFUL - (replace ONLY bottom)"
# # #             )
# # #           )
# # # 
# # # plot_df %>%
# # #   ggplot2::ggplot() +
# # #   # ggplot2::scale_y_continuous(limits = c(-1, 2),
# # #   #                             breaks = seq(-5, 2,
# # #   #                                          by = 0.5)) +
# # #   # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class), size = 3)+
# # #   # ggplot2::facet_wrap(source~., scale = "free_y")
# # #   # ggplot2::facet_wrap(source~., nrow = 1)
# # #   ggplot2::facet_grid(source~.)
# # # 
# # # 
# # # 
# # # plot_df %>%
# # #   ggplot2::ggplot() +
# # #   # ggplot2::scale_y_continuous(limits = c(-1, 2),
# # #   #                             breaks = seq(-5, 2,
# # #   #                                          by = 0.5)) +
# # #   # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class), size = 3)+
# # #   # ggplot2::facet_wrap(source~., scale = "free_y")
# # #   ggplot2::facet_wrap(source~., nrow = 1)
# # # 
# # # model_widths
# # # 
# # # 
# # # 
# # # 
# # # 
# # # # out_cs %>%
# # # #   ggplot2::ggplot() +
# # # #   ggplot2::scale_y_continuous(limits = c(-1, 2),
# # # #                               breaks = seq(-5, 2,
# # # #                                            by = 0.5)) +
# # # #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # # #   ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # # 
# # # 
# # # #########W  #########W  #########W  #########W
# # # #   ######### EXAMPLE DATA  #########
# # # #########W  #########W  #########W  #########W
# # # #########W  #########W  #########W  #########W
# # # cs_bankfull_estimate <- function(cs,
# # #                                  r = 3,
# # #                                  bf_width,
# # #                                  bf_depth,
# # #                                  drop_negative_depths = TRUE,
# # #                                  keep_observed = FALSE,
# # #                                  only_bottom = FALSE
# # # ) {
# # # 
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # # 
# # #   # cs = cs
# # #   # r  = 3
# # #   # bf_width = bf_width
# # #   # bf_depth = bf_depth
# # #   # drop_negative_depths = FALSE
# # #   # keep_observed = TRUE
# # # 
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # # 
# # #   primary_z     <- cs$Z
# # #   rel_distance  <- cs$relative_distance
# # #   channel_class <- cs$class
# # # 
# # #   # plot(primary_z)
# # # 
# # #   # cs %>%
# # #   #   ggplot2::ggplot() +
# # #   #   ggplot2::scale_y_continuous(limits = c(0, max(cs$Z)),
# # #   #                               breaks = seq(0, max(cs$Z),
# # #   #                               by = (max(cs$Z) - min(cs$Z)) / 4)) +
# # #   #   # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #   #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # #   #   ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # # 
# # #   start_z <-
# # #     cs %>%
# # #     dplyr::filter(class %in% c("right_bank", "left_bank")) %>%
# # #     dplyr::slice_min(Z, with_ties = FALSE) %>%
# # #     .$Z
# # # 
# # #   # # relative distance of the bottoms
# # #   bottom   <- rel_distance[channel_class == "bottom"]
# # #   bottomZ  <- primary_z[channel_class == "bottom"]
# # # 
# # #   # # find the middle of the bottom
# # #   midpoint <- median(bottom)
# # # 
# # #   # generate AHG estimates
# # #   ahg_est <- AHGestimation::cross_section(
# # #     r    = r,
# # #     TW   = bf_width,
# # #     Ymax = bf_depth
# # #   )
# # # 
# # # 
# # #   # indices of the left and right parabola halves
# # #   left_half  = 1:(nrow(ahg_est) / 2)
# # #   right_half = (1 + (nrow(ahg_est) / 2)):nrow(ahg_est)
# # # 
# # #   # get the left and right side of the parabolas
# # #   left_parabola = ahg_est[left_half, ]
# # #   right_parabola = ahg_est[right_half, ]
# # # 
# # #   # shift the Z values to have there max points be at the "bottom" of the "cs" points
# # #   left_parabola$Y <- left_parabola$Y + (start_z - max(left_parabola$Y))
# # #   right_parabola$Y <- right_parabola$Y + (start_z - max(right_parabola$Y))
# # #   # left_parabola$Y <- left_parabola$Y + (bottomZ[1] - max(left_parabola$Y))
# # #   # right_parabola$Y <- right_parabola$Y + (bottomZ[1] - max(right_parabola$Y))
# # # 
# # #   #  set any negative values in parabola to 0
# # #   if (drop_negative_depths) {
# # #     left_parabola$Y [left_parabola$Y  < 0] = 0
# # #     right_parabola$Y[right_parabola$Y < 0] = 0
# # #   }
# # # 
# # #   # Offset LEFT parabola (X values)
# # # 
# # #   # original maximum left X value (to use for offsetting right_parabola in a future step)
# # #   # left_max <- max(left_parabola$x)
# # #   left_max <- left_parabola$x[nrow(left_parabola)]
# # # 
# # #   # # getting the starting X value for the LEFT side of the parabola
# # #   # (get the last element from the left_parabola, this assumes the "x" column is sorted in non decreasing order, otherwise we can use the max() value....)
# # #   left_start <- midpoint - left_parabola$x[nrow(left_parabola)]
# # #   # left_start <- midpoint - max(left_parabola$x)
# # # 
# # #   # # offset the left starting position
# # #   # left_start <- left_start * 2
# # # 
# # #   # offset X values to fit within cs points
# # #   left_parabola$x <- left_start + left_parabola$x
# # # 
# # # 
# # #   # Offset RIGHT parabola (X values)
# # # 
# # #   # getting the starting X value for the RIGHT side of the parabola
# # #   right_start <- midpoint + ((right_parabola$x) - left_max)
# # # 
# # #   # offset X values to fit within cs points
# # #   right_parabola$x <- right_start
# # # 
# # #   # combine all parts of the parabola back together
# # #   parabola <- dplyr::bind_rows(
# # #                 dplyr::mutate(
# # #                   left_parabola,
# # #                   side = "left"
# # #                   ),
# # #                 dplyr::mutate(
# # #                   right_parabola,
# # #                   side = "right"
# # #                   )
# # #                 )
# # # 
# # #   # plot(parabola$Y~parabola$x)
# # # 
# # #   # subset original cross section points to the left and right of midpoint and
# # #   # that are greater than the minimum start_z value determined from the beginning
# # #   lower_bound <-
# # #     cs %>%
# # #     dplyr::filter(Z >= start_z, relative_distance < midpoint) %>%
# # #     # dplyr::filter(class == "bottom") %>%
# # #     dplyr::slice_max(relative_distance)
# # # 
# # #   # if there are any points (rows), use the "relative_distance" value, otherwise assign lower_bound of 0
# # #   lower_bound <- ifelse(nrow(lower_bound) > 0, lower_bound$relative_distance, 0)
# # # 
# # #   # keep_right <-
# # #   upper_bound <-
# # #     cs %>%
# # #     dplyr::filter(Z >= start_z, relative_distance > midpoint) %>%
# # #     # dplyr::filter(class == "bottom") %>%
# # #     dplyr::slice_min(relative_distance)
# # # 
# # #   # if there are any points (rows), use the "relative_distance" value, otherwise assign upper_bound of the length of the cross section
# # #   upper_bound <- ifelse(nrow(upper_bound) > 0, upper_bound$relative_distance, upper_bound$cs_lengthm)
# # # 
# # # 
# # #   # upper_bound <-
# # #   #   cs %>%
# # #   #   dplyr::filter(class == "bottom") %>%
# # #   #   dplyr::slice_max(relative_distance) %>%
# # #   #   .$relative_distance
# # #   #
# # #   # lower_bound <-
# # #   #   cs %>%
# # #   #   dplyr::filter(class == "bottom") %>%
# # #   #   dplyr::slice_min(relative_distance) %>%
# # #   #   .$relative_distance
# # # 
# # #   # subset the parabola values to just those within the original cross section bounds
# # #   parabola <-
# # #     parabola %>%
# # #     dplyr::select(
# # #       relative_distance = x,
# # #       Z                 = Y,
# # #       side
# # #     ) %>%
# # #     dplyr::filter(
# # #       relative_distance >= lower_bound,
# # #       relative_distance <= upper_bound
# # #     )
# # # 
# # #   # minimum point distance of LEFT parabola
# # #   left_parabola_min <- dplyr::slice_head(
# # #                             dplyr::filter(
# # #                               parabola,
# # #                               side == "left"
# # #                               )
# # #                           )$relative_distance
# # # 
# # #   # maximum point distance of RIGHT parabola
# # #   right_parabola_max <- dplyr::slice_tail(
# # #                             dplyr::filter(
# # #                               parabola,
# # #                               side == "right"
# # #                               )
# # #                           )$relative_distance
# # # 
# # #   # Get the original set of cross section points to the LEFT of the midpoint and to the LEFT of the LEFT MOST parabola point
# # #   left_cs  <- dplyr::filter(cs,
# # #                             # Z >= start_z,
# # #                             relative_distance < left_parabola_min,
# # #                             relative_distance < midpoint
# # #                             )
# # # 
# # #   # Get the original set of cross section points to the RIGHT of the midpoint and to the RIGHT of the RIGHT MOST parabola point
# # #   right_cs <- dplyr::filter(cs,
# # #                             # Z >= start_z,
# # #                             relative_distance > right_parabola_max,
# # #                             relative_distance > midpoint
# # #                             )
# # # 
# # #   # filter out any parts of the parabola that have Z values that are
# # #   # greater than the minimum DEM Z values of the original left and right cross section points
# # #   parabola <- dplyr::bind_rows(
# # #                 dplyr::filter(parabola,
# # #                               side == "left",
# # #                               Z < min(left_cs$Z)
# # #                 ),
# # #                 dplyr::filter(parabola,
# # #                               side == "right",
# # #                               Z < min(right_cs$Z)
# # #                   )
# # #               )
# # # 
# # #   # parabola <- dplyr::select(parabola, -side)
# # # 
# # #   # plot(para$Z~para$relative_distance)
# # # 
# # #   # combine the kept left cross section points, parabola points, and right cross section points together
# # #   out_cs <- dplyr::bind_rows(
# # #                   left_cs,
# # #                   parabola,
# # #                   # dplyr::select(parabola, -side),
# # #                   right_cs
# # #                   )
# # #   # plot(out_cs$Z)
# # # 
# # #   # if keep_observed is TRUE, then any original cross section points are kept and
# # #   # only the "bottom" classified points are filled in using the AHG estimate points that
# # #   if(keep_observed) {
# # # 
# # #     # indices of the "bottom" points
# # #     bottom_indices <- which(cs$class == "bottom")
# # # 
# # #     # min index is the left most bottom point and max index is the right most bottom point
# # #     left_index <- min( bottom_indices )
# # #     right_index <- max( bottom_indices )
# # # 
# # #     # Whether to keep bankful width parabola points out to the points just beyond the "bottom" points,
# # #     # this will extend the number of "bottom" points from the AHG estimates that are included in output
# # #     if(!only_bottom) {
# # #       # min index is the left most bottom point and max index is the right most bottom point
# # #       left_index  <- left_index - 1
# # #       right_index <- right_index + 1
# # #     }
# # # 
# # #     # # min index is the left most bottom point and max index is the right most bottom point
# # #     # left_index <- min( bottom_indices ) - 1
# # #     # right_index <- max( bottom_indices ) + 1
# # # 
# # #     # get the relative distance of the left and right bottom points
# # #     left_bound <- cs$relative_distance[left_index]
# # #     right_bound <- cs$relative_distance[right_index]
# # # 
# # #     para_left <-
# # #       out_cs %>%
# # #       # dplyr::filter(!is.na(side)) %>%
# # #       dplyr::filter(side == "left") %>%
# # #       dplyr::filter(relative_distance >= left_bound)
# # # 
# # #     para_right <-
# # #       out_cs %>%
# # #       # dplyr::filter(!is.na(side)) %>%
# # #       dplyr::filter(side == "right") %>%
# # #       dplyr::filter(relative_distance <= right_bound)
# # # 
# # #     # subset LEFT SIDE of cross section that has DEM points and is NOT a "bottom" point
# # #     left_cs <- dplyr::filter(cs,
# # #                   relative_distance <= left_bound,
# # #                   class != "bottom"
# # #                   )
# # # 
# # #     # subset RIGHT SIDE of cross section that has DEM points and is NOT a "bottom" point
# # #     right_cs <- dplyr::filter(cs,
# # #                   relative_distance >= right_bound,
# # #                   class != "bottom"
# # #                   )
# # # 
# # # 
# # #     out_cs <- dplyr::bind_rows(
# # #                   left_cs,
# # #                   para_left,
# # #                   para_right,
# # #                   right_cs
# # #                   )
# # # 
# # #     # plot(out_cs$Z)
# # # 
# # #   }
# # # 
# # # 
# # #   # reassign new 'pt_id' to account for inserted parabola points and
# # #   # assign all NA points to "bottom" (assigning "bottom" to all NA points is probably temporary and NOT what the final result should be)
# # #   out_cs <-
# # #     out_cs %>%
# # #     dplyr::select(-side) %>%
# # #     dplyr::mutate(
# # #       pt_id = 1:dplyr::n(),
# # #       class = dplyr::case_when(
# # #         is.na(class) ~ "bottom",
# # #         TRUE         ~ class
# # #       )
# # #   )
# # # 
# # #   # Add back ID data
# # #   out_cs$hy_id      <- cs$hy_id[1]
# # #   out_cs$cs_id      <- cs$cs_id[1]
# # #   out_cs$cs_lengthm <- cs$cs_lengthm[1]
# # #   out_cs$Z_source   <- cs$Z_source[1]
# # # 
# # #   # plot_df <- dplyr::bind_rows(
# # #   #             dplyr::mutate(cs,
# # #   #                           source = "DEM"
# # #   #             ),
# # #   #             dplyr::mutate(out_cs,
# # #   #                           source = "AHG Estimate (In channel)"
# # #   #             )
# # #   #           )
# # #   # plot_df %>%
# # #   #   ggplot2::ggplot() +
# # #   #   # ggplot2::scale_y_continuous(limits = c(-1, 2),
# # #   #   #                             breaks = seq(-5, 2,
# # #   #   #                                          by = 0.5)) +
# # #   #   # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #   #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # #   #   # ggplot2::facet_wrap(source~.)
# # #   #   ggplot2::facet_grid(source~.)
# # #   #
# # #   # out_cs %>%
# # #   #   ggplot2::ggplot() +
# # #   #   ggplot2::scale_y_continuous(limits = c(-1, 2),
# # #   #                               breaks = seq(-5, 2,
# # #   #                                            by = 0.5)) +
# # #   #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # #   #   ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # # 
# # #   return(out_cs)
# # # 
# # # }
# # # 
# # # cs_inchannel_estimate <- function(cs,
# # #                                   r = 3,
# # #                                   inchannel_width,
# # #                                   inchannel_depth,
# # #                                   drop_negative_depths = TRUE
# # #                                   ) {
# # # 
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # # 
# # # 
# # #   primary_z     <- cs$Z
# # #   rel_distance  <- cs$relative_distance
# # #   channel_class <- cs$class
# # # 
# # #   # plot(primary_z)
# # # 
# # #   # cs %>%
# # #   #   ggplot2::ggplot() +
# # #   #   ggplot2::scale_y_continuous(limits = c(0, max(cs$Z)),
# # #   #                               breaks = seq(0, max(cs$Z),
# # #   #                               by = (max(cs$Z) - min(cs$Z)) / 4)) +
# # #   #   # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #   #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # #   #   ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # # 
# # #   # # relative distance of the bottoms
# # #   bottom   <- rel_distance[channel_class == "bottom"]
# # #   bottomZ  <- primary_z[channel_class == "bottom"]
# # # 
# # #   # # find the middle of the bottom
# # #   midpoint <- median(bottom)
# # # 
# # #   # generate AHG estimates
# # #   ahg_est <- AHGestimation::cross_section(
# # #                 r    = r,
# # #                 TW   = inchannel_width,
# # #                 Ymax = inchannel_depth
# # #               )
# # # 
# # # 
# # #   # indices of the left and right parabola halves
# # #   left_half  = 1:(nrow(ahg_est) / 2)
# # #   right_half = (1 + (nrow(ahg_est) / 2)):nrow(ahg_est)
# # # 
# # #   # get the left and right side of the parabolas
# # #   left_parabola = ahg_est[left_half, ]
# # #   right_parabola = ahg_est[right_half, ]
# # # 
# # #   # shift the Z values to have there max points be at the "bottom" of the "cs" points
# # #   left_parabola$Y <- left_parabola$Y + (bottomZ[1] - max(left_parabola$Y))
# # #   right_parabola$Y <- right_parabola$Y + (bottomZ[1] - max(right_parabola$Y))
# # # 
# # #   #  set any negative values in parabola to 0
# # #   if (drop_negative_depths) {
# # #     left_parabola$Y [left_parabola$Y  < 0] = 0
# # #     right_parabola$Y[right_parabola$Y < 0] = 0
# # #   }
# # # 
# # #   # Offset LEFT parabola (X values)
# # # 
# # #   # original maximum left X value (to use for offsetting right_parabola in a future step)
# # #   left_max <- max(left_parabola$x)
# # # 
# # #   # subset cross section to the LEFT of the midpoint
# # #   left_cs <- cs[rel_distance < midpoint, ]
# # # 
# # #   # removing cross section point that will be replaced by left_parabola points
# # #   left_cs <- left_cs[left_cs$relative_distance < (midpoint - max(left_parabola$x)), ]
# # # 
# # #   # getting the starting X value for the LEFT side of the parabola
# # #   left_start <- midpoint - max(left_parabola$x)
# # # 
# # #   # offset X values to fit within cs points
# # #   left_parabola$x <- left_start + left_parabola$x
# # # 
# # # 
# # #   # Offset RIGHT parabola (X values)
# # # 
# # #   # subset cross section to the RIGHT of the midpoint
# # #   right_cs <- cs[rel_distance > midpoint, ]
# # # 
# # #   # getting the starting X value for the RIGHT side of the parabola
# # #   right_start <- midpoint + ((right_parabola$x) - left_max)
# # # 
# # #   # removing cross section point that will be replaced by right_parabola points
# # #   right_cs <- right_cs[right_cs$relative_distance > max(right_start), ]
# # #   # right_cs <- right_cs[right_cs$relative_distance > (midpoint + max(right_parabola$x)), ]
# # # 
# # #   # offset X values to fit within cs points
# # #   right_parabola$x <- right_start
# # # 
# # #   # # get the last point on the LEFT side of parabola
# # #   # last_left <- dplyr::slice_tail(left_parabola)
# # #   #
# # #   # # get the first point on the RIGHT side of parabola
# # #   # first_right <- dplyr::slice_head(right_parabola)
# # #   #
# # #   # # create an additional point in the middle between the left and right parabolas
# # #   # extra_midpoint <- data.frame(
# # #   #                     ind = last_left$ind + 1,
# # #   #                     x   = median(c(last_left$x, first_right$x)),
# # #   #                     Y   = median(c(last_left$Y, first_right$Y)),
# # #   #                     A   = median(c(last_left$A, first_right$A))
# # #   #                   )
# # # 
# # #   # combine all parts of the parabola back together
# # #   parabola <- dplyr::bind_rows(left_parabola, right_parabola)
# # #   # parabola <- dplyr::bind_rows(left_parabola, extra_midpoint, right_parabola)
# # # 
# # #   # select relevant columns and adjust the names
# # #   parabola <-
# # #     parabola %>%
# # #     dplyr::select(
# # #       relative_distance = x,
# # #       Z                 = Y
# # #       )
# # # 
# # #   # combine left cross section points, parabola, and right cross section points
# # #   out_cs <- dplyr::bind_rows(
# # #                 left_cs,
# # #                 parabola,
# # #                 right_cs
# # #                 )
# # # 
# # #   # Add new pt_id to account for inserted parabola points, and assign all parabola points to have a class of "bottom"
# # #   out_cs <-
# # #     out_cs %>%
# # #     dplyr::mutate(
# # #       pt_id = 1:dplyr::n(),
# # #       class = dplyr::case_when(
# # #         is.na(class) ~ "bottom",
# # #         TRUE         ~ class
# # #       )
# # #     )
# # # 
# # #   # Add back ID data
# # #   out_cs$hy_id      <- cs$hy_id[1]
# # #   out_cs$cs_id      <- cs$cs_id[1]
# # #   out_cs$cs_lengthm <- cs$cs_lengthm[1]
# # #   out_cs$Z_source   <- cs$Z_source[1]
# # # 
# # #   # ahg_est
# # #   # plot(ahg_est$Y)
# # # 
# # #   # plot_df <- dplyr::bind_rows(
# # #   #             dplyr::mutate(cs,
# # #   #                           source = "DEM"
# # #   #             ),
# # #   #             dplyr::mutate(out_cs,
# # #   #                           source = "AHG Estimate (In channel)"
# # #   #             )
# # #   #           )
# # #   # plot_df %>%
# # #   #   ggplot2::ggplot() +
# # #   #   ggplot2::scale_y_continuous(limits = c(-1, 2),
# # #   #                               breaks = seq(-5, 2,
# # #   #                                            by = 0.5)) +
# # #   #   # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #   #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # #   #   ggplot2::facet_wrap(source~., scale = "free_y")
# # #   #
# # #   # out_cs %>%
# # #   #   ggplot2::ggplot() +
# # #   #   ggplot2::scale_y_continuous(limits = c(-1, 2),
# # #   #                               breaks = seq(-5, 2,
# # #   #                                            by = 0.5)) +
# # #   #   ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # #   #   ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # # 
# # #   return(out_cs)
# # # 
# # # }
# # # 
# # # cs_bf_estimate <- function(cs, bf_width, bf_depth) {
# # # 
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # #   #########W  #########W  #########W  #########W
# # # 
# # # 
# # #   primary_z     <- cs$Z
# # #   rel_distance  <- cs$relative_distance
# # #   channel_class <- cs$class
# # #   # max_z = round(max(cs_points$Z), 2)
# # #   # total_length  <- cs_points$cs_lengthm[1]
# # # 
# # #   # get the left most bottom point
# # #   left <-
# # #     cs %>%
# # #     dplyr::filter(class == "bottom") %>%
# # #     dplyr::slice_min(pt_id) %>%
# # #     .$pt_id
# # # 
# # #   # get the index to the left of the left most bottom point
# # #   left = left - 1
# # # 
# # #   # get the right most bottom point
# # #   right <-
# # #     cs %>%
# # #     dplyr::filter(class == "bottom") %>%
# # #     dplyr::slice_max(pt_id) %>%
# # #     .$pt_id
# # # 
# # #   # get the index to the right of the right most bottom point
# # #   right = right + 1
# # # 
# # #   ahg_est <- AHGestimation::cross_section(
# # #     r    = 3,
# # #     TW   = bf_width,
# # #     Ymax = bf_depth
# # #   )
# # # 
# # #   plot(ahg_est$Y)
# # #   plot(cs$Z~cs$relative_distance)
# # #   # channel_class
# # #   # rel_distance
# # # 
# # #   # # relative distance of the bottoms
# # #   # bottoms <- rel_distance[channel_class == "bottom"]
# # #   # # bottoms <- rel_distance[channel_class %in% c("channel", "bottom")]
# # #   #
# # #   # # find the middle of the bottom
# # #   # middle_pt <- median(bottoms)
# # #   #
# # #   # message("middle_pt: ", round(middle_pt, 3))
# # #   #
# # #   # AHG Estimated X and Y values
# # #   ahg_x <- ahg_est$x
# # #   ahg_z <- ahg_est$Y
# # # 
# # #   # # plot(ahg_est$Y)
# # #   #
# # #   # # distnace in both directios from the middle of the channel bottom
# # #   # middle_to_left  <- abs(rel_distance - (middle_pt - (max(ahg_x)/2)))
# # #   # middle_to_right <- abs(rel_distance - (middle_pt + (max(ahg_x)/2)))
# # #   #
# # #   # # indices to "pin" AHG estimate into original Z values
# # #   # left_bank  <- which.min(middle_to_left)
# # #   # # right_bank <- which.min(middle_to_right)
# # #   # right_bank <- which.min(middle_to_right)
# # # 
# # #   # extract the relative distance (x) values to the left and right of the channel bottom,
# # #   # these X values will be the distance along the cross section for each point,
# # #   # we'll be inserting the AHG X values between the left and right relative distances
# # #   # left_side <- rel_distance[1:(left_bank-1)]
# # #   # right_side <- rel_distance[(1 + right_bank):length(rel_distance)]
# # #   # left_side <- rel_distance[1:(left_bank)]
# # #   # right_side <- rel_distance[(right_bank):length(rel_distance)]
# # #   #
# # # 
# # #   left_side <- rel_distance[1:(left)]
# # #   right_side <- rel_distance[(right):length(rel_distance)]
# # # 
# # #   plot(ahg_est$Y)
# # #   # insert the AHG X distances between the original X relative distances
# # #   final_x <- c(left_side, left_side[length(left_side)] + ahg_x, right_side)
# # #   final_x <- round(final_x, 2)
# # # 
# # #   # cs_points %>%
# # #   #   dplyr::filter(class == "left_bank") %>%
# # #   #   dplyr::slice_min(Z) %>%
# # #   #   dplyr::slice_max(pt_id)
# # #   #   # dplyr::slice_max(pt_id)
# # #   #
# # #   # cs_points %>%
# # #   #   dplyr::filter(class == "right_bank") %>%
# # #   #   dplyr::slice_min(Z) %>%
# # #   #   dplyr::slice_min(pt_id)
# # #   # cs_points$Z %>% which.max()
# # # 
# # #   # extract the Z values to the left and right of the channel bottom, these Z values will "surround" the AHG estimate Z values
# # #   # left_z  <- primary_z[1:(left_bank-1)]
# # #   # right_z <- primary_z[(1 + right_bank):length(primary_z)]
# # # 
# # #   left_z  <- primary_z[1:(left)]
# # #   right_z <- primary_z[(right):length(primary_z)]
# # # 
# # #   shift_ahg_z <- ahg_z + (left_z[length(left_z)]  - max(ahg_z))
# # # 
# # #   final_z <- c(left_z, shift_ahg_z, right_z)
# # #   # final_z <- c(left_z,  ahg_z, right_z)
# # #   plot(primary_z)
# # #   final_z <- round(final_z, 2)
# # #   plot(final_z)
# # # 
# # #   # rename with the classications
# # #   final_class <- c(
# # #     rep("left_bank", length(left_z)),
# # #     rep("bottom", length(ahg_z)),
# # #     rep("right_bank", length(right_z))
# # #   )
# # # 
# # #   output <- data.frame(
# # #     x = final_x,
# # #     z = final_z,
# # #     class = final_class
# # #   ) %>%
# # #     dplyr::tibble() %>%
# # #     dplyr::mutate(
# # #       is_ahg_estimate = dplyr::case_when(
# # #         class == "bottom" ~ TRUE,
# # #         TRUE              ~ FALSE
# # #       )
# # #     )
# # #   plot(cs$Z~cs$relative_distance)
# # #   plot(output$z~output$x)
# # #   ggplot2::ggplot() +
# # #     # ggplot2::scale_y_continuous(limits = c(0, 25), breaks = seq(0, 15, by = 5)) +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     # ggplot2::geom_point(data = cs, ggplot2::aes(x = relative_distance, y = Z, color = class),
# # #     #                     size = 8,
# # #     #                     alpha = 0.4,
# # #     #                     shape = 18
# # #     # ) +
# # #     ggplot2::geom_point(data = output,
# # #                         ggplot2::aes(x = x, y = z, color = is_ahg_estimate),
# # #                         size = 3
# # #     ) +
# # #     # ggplot2::scale_y_continuous(limits = c(0, 20), breaks = seq(0, 15, by = 5)) +
# # #     ggplot2::theme(legend.position = "bottom")
# # #   ggplot2::ggplot() +
# # #     ggplot2::scale_y_continuous(limits = c(0, 25), breaks = seq(0, 15, by = 5)) +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::geom_point(data = cs, ggplot2::aes(x = relative_distance, y = Z, color = class),
# # #                         size = 8,
# # #                         alpha = 0.4,
# # #                         shape = 18
# # #     ) +
# # #     # ggplot2::geom_point(data = output,
# # #     #                     ggplot2::aes(x = x, y = z, color = class),
# # #     #                     size = 3
# # #     # ) +
# # #     # ggplot2::scale_y_continuous(limits = c(0, 20), breaks = seq(0, 15, by = 5)) +
# # #     ggplot2::theme(legend.position = "bottom")
# # # 
# # # }
# # # 
# # # 
# # # cs_to_ahg <- function() {
# # #   library(dplyr)
# # # 
# # #   cs_pts = arrow::read_parquet("/Users/anguswatters/Desktop/lynker-spatial/02_cs_pts/nextgen_12_cross_sections.parquet")
# # # 
# # #   cs_pts = sf::read_sf("/Users/anguswatters/Desktop/lynker-spatial/01_transects/nextgen_10L_cross_sections.parquet")
# # # 
# # #   ml_widths <- arrow::read_parquet("/Users/anguswatters/Downloads/conus_width_depth_ml.parquet")
# # #   ml_widths %>%
# # #     dplyr::filter(FEATUREID == 5587412)
# # # 
# # #   net = arrow::open_dataset('s3://lynker-spatial/v20.1/conus_net.parquet') %>%
# # #     dplyr::filter(vpu == 12)
# # # 
# # #   aoi <-
# # #     net %>%
# # #     dplyr::collect()
# # # 
# # #   fab <- sf::read_sf("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg/nextgen_12.gpkg", layer = "flowpaths")
# # #   sf::st_layers("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg/nextgen_12.gpkg")
# # #   fab
# # #   # aoi$hf_id[1]
# # #   # aoi$id[1]
# # # 
# # #   aoi$hf_id
# # # 
# # #   aoi[5000, ]
# # #   # tmp_i <- 32771
# # #   tmp_i <- 33437
# # #   # tmp_i <- 33298
# # #   which(aoi$id == "wb-2398332")
# # #   # which(aoi$id == "wb-2398289")
# # #   # cs_pts %>%
# # #   # dplyr::filter(cs_lengthm > 100)
# # #   # which(cs_pts$hy_id == "wb-2398284")
# # #   mlw <-
# # #     ml_widths %>%
# # #     dplyr::filter(FEATUREID ==  aoi$hf_id[tmp_i])
# # #   # which(aoi$id == "wb-2398284")
# # #   # cs_points %>%
# # #   cs_pts %>%
# # #     dplyr::filter(hy_id == aoi$id[tmp_i]) %>%
# # #     ggplot2::ggplot() +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # #     ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # # 
# # #   cs_points <-
# # #     cs_pts %>%
# # #     dplyr::filter(hy_id ==  aoi$id[tmp_i], cs_id == 3)
# # #   plot(cs_points$Z)
# # #   mlw
# # # 
# # # 
# # # 
# # #   ahg_est <- AHGestimation::cross_section(r = 3,
# # #                                           # TW =   max(tester$bf_width),
# # #                                           # TW =   max(tester$bf_width),
# # #                                           # TW = 50,
# # #                                           TW = mlw$owp_tw_inchan,
# # #                                           # Ymax = mlw$owp_y_bf
# # #                                           # Ymax = max(cs_points$Z)
# # #                                           Ymax = mlw$owp_y_inchan
# # # 
# # #   )
# # # 
# # #   plot(ahg_est$Y)
# # #   plot(cs_points$Z)
# # # 
# # #   cs_points %>%
# # #     ggplot2::ggplot() +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::geom_point(ggplot2::aes(x = relative_distance, y = Z, color = class) )+
# # #     # ggplot2::ylim(c(195, 200)) +
# # #     ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # # 
# # # 
# # #   total_length <- cs_points$cs_lengthm[1]
# # #   primary_z    <- cs_points$Z
# # #   max_z = round(max(cs_points$Z), 2)
# # #   rel_distance <- cs_points$relative_distance
# # #   channel_class <- cs_points$class
# # #   # channel_class
# # #   # rel_distance
# # # 
# # #   # relative distance of the bottoms
# # #   bottoms <- rel_distance[channel_class == "bottom"]
# # #   # bottoms <- rel_distance[channel_class %in% c("channel", "bottom")]
# # # 
# # #   # find the middle of the bottom
# # #   middle_pt <- median(bottoms)
# # # 
# # #   message("middle_pt: ", round(middle_pt, 3))
# # # 
# # #   # AHG Estimated X and Y values
# # #   ahg_x <- ahg_est$x
# # #   ahg_z <- ahg_est$Y
# # # 
# # #   # plot(ahg_est$Y)
# # #   # ahg_est
# # #   # ahg_x
# # #   #
# # #   # distnace in both directios from the middle of the channel bottom
# # #   middle_to_left  <- abs(rel_distance - (middle_pt - (max(ahg_x)/2)))
# # #   middle_to_right <- abs(rel_distance - (middle_pt + (max(ahg_x)/2)))
# # # 
# # #   # indices to "pin" AHG estimate into original Z values
# # #   left_bank  <- which.min(middle_to_left)
# # #   # right_bank <- which.min(middle_to_right)
# # #   right_bank <- which.min(middle_to_right)
# # # 
# # #   rel_distance[-left_bank:-right_bank]
# # # 
# # #   left_bank:right_bank
# # # 
# # #   # extract the relative distance (x) values to the left and right of the channel bottom,
# # #   # these X values will be the distance along the cross section for each point,
# # #   # we'll be inserting the AHG X values between the left and right relative distances
# # #   left_side <- rel_distance[1:(left_bank-1)]
# # #   right_side <- rel_distance[(1 + right_bank):length(rel_distance)]
# # #   # left_side <- rel_distance[1:(left_bank)]
# # #   # right_side <- rel_distance[(right_bank):length(rel_distance)]
# # #   #
# # #   # insert the AHG X distances between the original X relative distances
# # #   final_x <- c(left_side, left_side[length(left_side)] + ahg_x, right_side)
# # #   final_x <- round(final_x, 2)
# # #   final_x
# # # 
# # #   cs_points[3, ]
# # #   ahg_z %>% round(2)
# # # 
# # #   # cs_points %>%
# # #   #   dplyr::filter(class == "left_bank") %>%
# # #   #   dplyr::slice_min(Z) %>%
# # #   #   dplyr::slice_max(pt_id)
# # #   #   # dplyr::slice_max(pt_id)
# # #   #
# # #   # cs_points %>%
# # #   #   dplyr::filter(class == "right_bank") %>%
# # #   #   dplyr::slice_min(Z) %>%
# # #   #   dplyr::slice_min(pt_id)
# # #   # cs_points$Z %>% which.max()
# # # 
# # #   # extract the Z values to the left and right of the channel bottom, these Z values will "surround" the AHG estimate Z values
# # #   left_z  <- primary_z[1:(left_bank-1)]
# # #   right_z <- primary_z[(1 + right_bank):length(primary_z)]
# # #   # left_z  <- primary_z[1:(left_bank)]
# # #   # right_z <- primary_z[(right_bank):length(primary_z)]
# # # 
# # #   channel_class
# # #   rep("left_bank", length(left_z))
# # #   left_z + ahg_z
# # #   left_z[length(left_z)]
# # #   final_z <- c(left_z,    left_z[length(left_z)] + ahg_z, right_z)
# # #   final_z <- round(final_z, 2)
# # #   final_z
# # #   final_class <- c(
# # #     rep("left_bank", length(left_z)),
# # #     rep("bottom", length(ahg_z)),
# # #     rep("right_bank", length(right_z))
# # #   )
# # # 
# # #   length(final_x)
# # #   length(final_z)
# # # 
# # #   pts <-  cs_points %>% sf::st_as_sf(coords = c("X", "Y"), crs = 5070)
# # #   bb <- pts[1, ] %>%
# # #     sf::st_buffer(10000)
# # # 
# # #   flines <- fab %>%
# # #     sf::st_filter(bb)
# # #   mapview::mapview(bb) + sf::st_as_sf(cs_points, coords = c("X", "Y"), crs = 5070) + flines
# # #   cs_points %>% sf::st_as_sf(coords = c("X", "Y"), crs = 5070) %>% mapview::mapview()
# # # 
# # #   output <- data.frame(
# # #     x = final_x,
# # #     z = final_z,
# # #     class = final_class
# # #   ) %>%
# # #     dplyr::tibble() %>%
# # #     dplyr::mutate(
# # #       is_ahg_estimate = dplyr::case_when(
# # #         class == "bottom" ~ TRUE,
# # #         TRUE              ~ FALSE
# # #       )
# # #     )
# # # 
# # #   ggplot2::ggplot() +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::geom_point(data = cs_points, ggplot2::aes(x = relative_distance, y = Z, color = class),
# # #                         size = 8,
# # #                         alpha = 0.4,
# # #                         shape = 18
# # #     ) +
# # #     ggplot2::geom_point(data = output, ggplot2::aes(x = x, y = z, color = class),
# # #                         size = 3
# # #     ) +
# # #     ggplot2::scale_y_continuous(limits = c(0, 20), breaks = seq(0, 15, by = 5)) +
# # #     ggplot2::theme(legend.position = "bottom")
# # # 
# # #   ahgX <- dplyr::filter(output, is_ahg_estimate)$x
# # #   ahgZ <- dplyr::filter(output, is_ahg_estimate)$z
# # # 
# # #   leftX <- dplyr::filter(output, class == "left_bank")$x
# # #   leftZ <- dplyr::filter(output, class == "left_bank")$z
# # # 
# # #   rightX <- dplyr::filter(output, class == "right_bank")$x
# # #   rightZ <- dplyr::filter(output, class == "right_bank")$z
# # # 
# # #   # Raise AHG estimate points as far as possible while still remaining between the left bank point
# # #   # and the next point to the right of it (and same for the right bank but for the point to the left of it)
# # #   # left_bound  = left_bank
# # #   # right_bound = right_bank - 1
# # # 
# # # 
# # #   left_bound  = cs_points$relative_distance[left_bank]
# # #   right_bound = cs_points$relative_distance[right_bank - 1]
# # # 
# # #   left_bankX <-
# # # 
# # #     newZ = ahgZ + 1
# # # 
# # #   to_keep <- newZ < max_z
# # # 
# # #   newX <- ahgX[to_keep]
# # #   newZ <- newZ[to_keep]
# # # 
# # # 
# # #   ahgX[newZ < max_z]
# # # 
# # #   # output$
# # #   output$z + 1
# # #   dplyr::mutate(output,
# # #                 z = z + 1
# # #   )
# # #   added_z = 1
# # #   dplyr::mutate(output,
# # #                 z = dplyr::case_when(
# # #                   is_ahg_estimate ~ z + added_z,
# # #                   TRUE            ~ z
# # #                 )
# # #   )
# # # 
# # #   ggplot2::ggplot() +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::geom_point(data = cs_points, ggplot2::aes(x = relative_distance, y = Z),
# # #                         size = 8,
# # #                         alpha = 0.4,
# # #                         shape = 18
# # #     ) +
# # #     ggplot2::geom_point(data =dplyr::mutate(output,
# # #                                             z = dplyr::case_when(
# # #                                               is_ahg_estimate ~ z + added_z,
# # #                                               TRUE            ~ z
# # #                                             )),
# # #                         ggplot2::aes(x = x, y = z,
# # #                                      color = is_ahg_estimate
# # #                         ),
# # #                         size = 3
# # #     ) +
# # #     ggplot2::scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 5)) +
# # #     ggplot2::theme(legend.position = "bottom")
# # # 
# # #   ggplot2::ggplot() +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::geom_point(data = cs_points, ggplot2::aes(x = relative_distance, y = Z, color = class),
# # #                         # color = "slategray",
# # #                         size = 8,
# # #                         alpha = 0.4,
# # #                         shape = 18
# # #     ) +
# # #     # ggplot2::scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 5)) +
# # #     ggplot2::theme(legend.position = "bottom")
# # # 
# # #   ggplot2::ggplot() +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::geom_point(data = cs_points, ggplot2::aes(x = relative_distance, y = Z, color = class),
# # #                         size = 8,
# # #                         alpha = 0.4,
# # #                         shape = 18
# # #     ) +
# # #     ggplot2::geom_point(data = output, ggplot2::aes(x = x, y = z, color = class),
# # #                         size = 3
# # #     ) +
# # #     # ggplot2::scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 5)) +
# # #     ggplot2::theme(legend.position = "bottom")
# # # 
# # #   ggplot2::ggplot() +
# # #     # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::geom_point(data = cs_points, ggplot2::aes(x = relative_distance, y = Z),
# # #                         size = 8,
# # #                         alpha = 0.4,
# # #                         shape = 18
# # #     ) +
# # #     ggplot2::geom_point(data = output, ggplot2::aes(x = x, y = z,
# # #                                                     color = is_ahg_estimate
# # #     ),
# # #     size = 3
# # #     ) +
# # #     # ggplot2::scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 5)) +
# # #     ggplot2::theme(legend.position = "bottom")
# # # 
# # #   cs_points$Z
# # #   output$z
# # # 
# # #   # ggplot2::ylim(c(195, 200)) +
# # #   ###########################################################################
# # #   ###########################################################################
# # #   ###########################################################################
# # # 
# # #   plot(ahg_est$Y)
# # #   plot(cs_points$Z)
# # #   cs_pts$
# # #     library(dplyr)
# # #   cs_points <-
# # #     cs_pts %>%
# # #     # dplyr::filter(hy_id %in% c("wb-1560496", "wb-1560496", "wb-1560498", "wb-1560498", "wb-1529103"))
# # #     dplyr::filter(hy_id %in% c( "wb-1560498"), cs_id == 7)
# # #   unique(  cs_pts$hy_id)
# # #   cs_points %>%
# # #     ggplot2::ggplot() +
# # #     ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class) )+
# # #     ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # #   cs_points
# # # 
# # #   cs_points$Z %>% max()
# # #   # ahg_est <- AHGestimation::cross_section(r = 2,
# # #   #                                         # TW =   max(tester$bf_width),
# # #   #                                         # TW =   max(tester$bf_width),
# # #   #                                         TW = 20,
# # #   #                                         Ymax =   max(cs_points$Z)
# # #   # )
# # # 
# # #   ahg_est <- AHGestimation::cross_section(r = 2,
# # #                                           # TW =   max(tester$bf_width),
# # #                                           # TW =   max(tester$bf_width),
# # #                                           # TW = 50,
# # #                                           TW = mlw$owp_tw_bf,
# # #                                           Ymax = max(cs_points$Z)
# # #   )
# # # 
# # #   total_length <- cs_points$cs_lengthm[1]
# # #   primary_z    <- cs_points$Z
# # #   rel_distance <- cs_points$relative_distance
# # #   channel_class <- cs_points$class
# # #   # channel_class
# # #   # rel_distance
# # # 
# # #   # relative distance of the bottoms
# # #   bottoms <- rel_distance[channel_class == "bottom"]
# # # 
# # #   # find the middle of the bottom
# # #   middle_pt <- median(bottoms)
# # # 
# # #   # AHG Estimated X and Y values
# # #   ahg_x <- ahg_est$x
# # #   ahg_z <- ahg_est$Y
# # # 
# # #   # ahg_est
# # #   # ahg_x
# # #   #
# # #   # distnace in both directios from the middle of the channel bottom
# # #   middle_to_left  <- abs(rel_distance - (middle_pt - (max(ahg_x)/2)))
# # #   middle_to_right <- abs(rel_distance - (middle_pt + (max(ahg_x)/2)))
# # # 
# # #   # indices to "pin" AHG estimate into original Z values
# # #   left_bank  <- which.min(middle_to_left)
# # #   right_bank <- which.min(middle_to_right)
# # # 
# # #   rel_distance[-left_bank:-right_bank]
# # # 
# # #   left_bank:right_bank
# # # 
# # #   # extract the relative distance (x) values to the left and right of the channel bottom,
# # #   # these X values will be the distance along the cross section for each point,
# # #   # we'll be inserting the AHG X values between the left and right relative distances
# # #   left_side <- rel_distance[1:(left_bank-1)]
# # #   right_side <- rel_distance[(1 + right_bank):length(rel_distance)]
# # # 
# # #   # insert the AHG X distances between the original X relative distances
# # #   final_x <- c(left_side, left_side[length(left_side)] + ahg_x, right_side)
# # # 
# # #   # extract the Z values to the left and right of the channel bottom, these Z values will "surround" the AHG estimate Z values
# # #   left_z  <- primary_z[1:(left_bank-1)]
# # #   right_z <- primary_z[(1 + right_bank):length(primary_z)]
# # # 
# # #   final_z <- c(left_z, ahg_z, right_z)
# # # 
# # # 
# # #   min(right_side) -  max(left_side)
# # #   ahg_x
# # # 
# # #   which.min(left_distance)
# # #   which.min(right_distance)
# # #   rel_distance - right_bank
# # #   min_index <- which.min(absolute_differences)
# # #   which.min(abs(rel_distance - left_bank))
# # # 
# # #   plot(ahg_est$Y)
# # #   plot(cs_points$Z)
# # #   ahg_est$Y
# # #   cs_points$Z
# # #   data.frame(
# # #     pt_id = 1:length(ahg_est$Y),
# # #     ahg   = ahg_est$Y
# # #   )
# # #   dplyr::mutate(ahg_est, pt_id = 1:dplyr::n())
# # #   ggplot2::ggplot() +
# # #     ggplot2::geom_point(data =dplyr::mutate(
# # #       ahg_est, pt_id = 1:dplyr::n()
# # #     ),
# # #     ggplot2::aes(x = pt_id, y = Y), color = "red") +
# # #     ggplot2::geom_point(data = cs_points, ggplot2::aes(x = pt_id, y = Z), color = "green")
# # #   # ggplot2::geom_point(data =dplyr::mutate(
# # #   #                               ahg_est, pt_id = 1:dplyr::n()
# # #   #                               ),
# # #   #                               ggplot2::aes(x = pt_id, y = Y), color = "red")
# # #   # # install.packages("devtools")
# # #   # /Users/anguswatters/Desktop/lynker-spatial/02_cs_pts
# # #   # devtools::install_github("mikejohnson51/AHGestimation")
# # #   # devtools::install_github("anguswg-ucsb/hydrofabric3D")
# # #   # /Users/anguswatters/Desktop/lynker-spatial/02_cs_pts
# # #   net = sf::read_sf("/Users/anguswatters/Desktop/test_net.gpkg")
# # #   cs = sf::read_sf("/Users/anguswatters/Desktop/test_cs.gpkg")
# # #   cs_pts = sf::read_sf("/Users/anguswatters/Desktop/test_cs_pts.gpkg")
# # # 
# # #   update_pts1 <- rectify_flat_cs(
# # #     net            = net,
# # #     cs             = cs,
# # #     cs_pts         = cs_pts,
# # #     points_per_cs  = NULL,
# # #     min_pts_per_cs = 10,
# # #     # dem            = DEM_URL,
# # #     scale          = 0.5,
# # #     threshold      = 0
# # #   )
# # #   # Remove any cross section that has ANY missing (NA) Z values.
# # #   update_pts1 <-
# # #     update_pts1 %>%
# # #     # dplyr::filter(hy_id %in% c("wb-849054", "wb-845736")) %>%
# # #     dplyr::group_by(hy_id, cs_id) %>%
# # #     dplyr::filter(!any(is.na(Z))) %>%
# # #     dplyr::ungroup()
# # # 
# # #   profvis::profvis({
# # # 
# # #     # classify the cross section points
# # #     out_pts <-
# # #       update_pts1 %>%
# # #       dplyr::rename(cs_widths = cs_lengthm) %>%
# # #       hydrofabric3D::classify_points() %>%
# # #       dplyr::mutate(
# # #         X = sf::st_coordinates(.)[,1],
# # #         Y = sf::st_coordinates(.)[,2]
# # #       ) %>%
# # #       dplyr::select(
# # #         hy_id, cs_id, pt_id,
# # #         cs_lengthm = cs_widths,
# # #         relative_distance,
# # #         X, Y, Z,
# # #         class
# # #       )
# # #   })
# # # 
# # #   # Drop point geometries, leaving just X, Y, Z values
# # #   out <- sf::st_drop_geometry(out_pts)
# # # 
# # #   # add Z_source column for source of elevation data
# # #   out <-
# # #     out %>%
# # #     dplyr::mutate(
# # #       Z_source = "hydrofabric3D"
# # #     ) %>%
# # #     dplyr::relocate(hy_id, cs_id, pt_id, cs_lengthm, relative_distance, X, Y, Z, Z_source, class)
# # #   tw_net <-
# # #     net %>%
# # #     dplyr::mutate(
# # #       bf_width = exp(0.700    + 0.365* log(tot_drainage_areasqkm))
# # #     ) %>%
# # #     dplyr::select(hy_id = id, bf_width)
# # #   # dplyr::select(hy_id = id, tot_drainage_areasqkm, bf_width)
# # # 
# # #   out2 <-
# # #     out %>%
# # #     dplyr::left_join(
# # #       sf::st_drop_geometry(tw_net),
# # #       by = "hy_id"
# # #     )
# # #   uids <- out2$hy_id %>% unique()
# # #   picks <- uids[754:755]
# # #   out2 %>%
# # #     dplyr::filter(hy_id %in% picks) %>%
# # #     ggplot2::ggplot() +
# # #     ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z) )+
# # #     ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # #   tester <- out2 %>%
# # #     dplyr::filter(hy_id =="wb-2639244", cs_id == 6)
# # #   # dplyr::filter(hy_id =="wb-2636395", cs_id == 3)
# # #   max(tester$bf_width)
# # #   max(tester$Z)
# # #   max(tester$Y)
# # #   tester$Z %>% plot()
# # # 
# # #   ahg_est <- AHGestimation::cross_section(r = 2,
# # #                                           # TW =   max(tester$bf_width),
# # #                                           # TW =   max(tester$bf_width),
# # #                                           TW = 50,
# # #                                           Ymax =   max(tester$Z)
# # #   )
# # #   ahg_est
# # # 
# # #   tester %>%
# # #     ggplot2::ggplot() +
# # #     ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z, color = class))
# # #   tester$relative_distance
# # #   floor(tester$relative_distance )
# # #   ahg_est[floor(tester$relative_distance ), ]
# # # 
# # #   ahg_est
# # #   tester %>% dplyr::select(pt_id, total_length = cs_lengthm, relative_distance, Z)
# # #   tester$Z
# # #   tester$Z %>% plot()
# # #   ahg_est$Y
# # #   tester$Z
# # #   ahg_est$Y %>% plot()
# # #   pinned_cs <- c(tester$Z[1:2],   ahg_est$Y, tester$Z[7:length(tester$Z)])
# # #   pinned_cs %>% plot()
# # #   ahg_est
# # #   ahg_est$Y %>% plot()
# # #   uids <- out2$hy_id %>% unique()
# # #   picks <- uids[888:890]
# # #   out2 %>%
# # #     dplyr::filter(hy_id %in% picks) %>%
# # #     ggplot2::ggplot() +
# # #     ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z) )+
# # #     ggplot2::facet_grid(hy_id~cs_id, scale = "free_y")
# # #   cs
# # #   data = AHGestimation::nwis
# # #   filter_data <-
# # #     data %>%
# # #     AHGestimation::date_filter(10, keep_max = TRUE) %>%
# # #     AHGestimation::nls_filter() %>%
# # #     dplyr::select(-date, -siteID)
# # # 
# # #   ahg_fit = AHGestimation::ahg_estimate(filter_data)[1,]
# # #   shape = AHGestimation::compute_hydraulic_params(ahg_fit)
# # # 
# # #   max(filter_data$TW)
# # #   max(filter_data$Y)
# # # 
# # #   cs3 <- AHGestimation::cross_section(r = shape$r,
# # #                                       TW = max(filter_data$TW),
# # #                                       Ymax = max(filter_data$Y)
# # #   )
# # #   dplyr::glimpse(cs)
# # # 
# # #   plot(cs3$Y)
# # # 
# # # }