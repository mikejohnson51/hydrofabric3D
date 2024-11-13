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


#' @title Extend a set of transects by a percentage based on banks and relief
#' Given a set of transect lines with valid_banks and has_relief columns (derived from DEM extracted cross section points), extend any transects 
#' by a percentage of the transects length if the transect does NOT have valid banks (valid_banks == FALSE) OR it does NOT have relief (has_relief == FALSE).
#' @param transects_to_check sf linestrings, set of all transects in the network. Requires the following columns: "hy_id", "cs_id", "cs_lengthm" (length of geometry in meters), "valid_banks", and "has_relief"
#' @param net sf linestrings, flowline network that transects were generated from, requires "crosswalk_id" column (where "crosswalk_id" equals the "hy_id" columns in 'transects_to_check' and 'transects' )
#' @param crosswalk_id character, column name that connects features in transects to net
#' @param scale numeric, percentage of current transect line length to extend transects in transects_to_extend by. Default is 0.5 (50% of the transect length)
#' @param direction character, strategy to take for extending transects. 
#' Either 'both' or 'any'. 'both' means both sides of each transects are extended if and only both sides are 
#' valid extensions. 'any' means a transect can be extended from either the 'left', 'right', or both 'left' and 'right' sides, 
#' depending on which sides show valid extensions. 'any' will require at least 2x more intersection checks with the network and thus can result in longer processing times.
#' Default is 'both'. 
#' @param verbose logical, whether to show a progress bar and progress messages or not. Default is TRUE.
#' @return sf linestring dataframe containing the the original transects with extensions performed on transects without valid_banks OR has_relief (a "is_extended" flag denotes if the geometry was extended by "scale" % or not)
#' @importFrom geos as_geos_geometry geos_intersection geos_type geos_intersects
#' @importFrom sf st_geometry st_as_sf
#' @importFrom dplyr filter bind_rows mutate case_when
#' @importFrom hydroloom rename_geometry 
#' @export 
extend_invalid_transect_sides <- function(
    transects_to_check, 
    net, 
    crosswalk_id = NULL,
    scale     = 0.5,
    direction = "both",
    verbose   = TRUE
) {
  # ----------------------------------------
  # ----------------------------------------
  
  # transects_to_check  = transects
  # net                 = net
  # crosswalk_id        = crosswalk_id
  # scale               = scale
  # direction           = "both"
  # verbose             = verbose
  
  # transects_to_check  = transects
  # net                 = net
  # crosswalk_id          = "hy_id"
  # 
  # scale               = scale
  # verbose             = verbose
  
  # transects_to_check  = transects
  # net                 = net
  # crosswalk_id        = "hy_id"
  # scale               = scale
  # verbose             = verbose
  
  # transects_to_check  = transects %>% dplyr::slice(52000:54000)
  
  # net                 = net
  # net2 <- net %>% dplyr::filter(hy_id %in% transects_to_check$hy_id)
  # crosswalk_id        = crosswalk_id
  # scale               = scale
  # direction           = "both"
  # verbose             = verbose
  
  # ----------------------------------------
  # ----------------------------------------
  
  # transects_to_check  = transects
  # net                 = net
  # crosswalk_id        = crosswalk_id
  # scale               = scale
  # direction           = "both"
  # verbose             = verbose
  
  
  # ----------------------------------------------------------------------------------
  # ----------- Input checking ------
  # ----------------------------------------------------------------------------------
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  # set geometry column names at beginning 
  net                 <- hydroloom::rename_geometry(net, "geometry") 
  transects_to_check  <- hydroloom::rename_geometry(transects_to_check, "geometry") 
 
  # validate input datas 
  is_net_valid        <- validate_df(net, c(crosswalk_id, "geometry"), "net")
  is_transects_valid  <- validate_df(transects_to_check, c(crosswalk_id, "cs_id", "cs_lengthm", 
                                                          "valid_banks", "has_relief", "geometry"), 
                                    "transects_to_check")
  start_cols          <- names(transects_to_check)
  
  # if(!crosswalk_id %in% names(net)) {
  #   stop("crosswalk_id '", crosswalk_id, "' is not a column in 'net' input,\n", 
  #        "Please provide a valid 'crosswalk_id' that crosswalks 'net' to 'transects_to_check'")
  # }
  # 
  # if(!crosswalk_id %in% names(transects_to_check)) {
  #   stop("crosswalk_id '", crosswalk_id, "' is not a column in 'transects_to_check' input,\n", 
  #        "Please provide a valid 'crosswalk_id' that crosswalks the 'transects_to_check' to 'net'")
  # }
  # 
  # # set geometry column name as beginning 
  # transects_to_check <- hydroloom::rename_geometry(transects_to_check, "geometry") 
  # 
  # # check for necessary columns
  # req_cols    <- c(crosswalk_id, "cs_id", "cs_lengthm", 
  #                  "valid_banks", "has_relief", "geometry")
  # start_cols  <- names(transects_to_check)
  # 
  # if (!all(req_cols %in% start_cols)) {
  #   missing_cols <- req_cols[which(!req_cols %in% start_cols)]
  #   stop("'transects_to_check' is missing the following required columns: \n > ", 
  #        paste0(missing_cols, collapse = "\n > "))
  # }
  
  # Create an "is_extended" flag to identify which transects were extended and updated 
  transects_to_check$is_extended <- FALSE
  
  # # split input transects into invalid and valid sets (valid == has valid banks AND has relief)
  # invalid_transects  <- dplyr::filter(transects_to_check, !valid_banks | !has_relief)
  # valid_transects    <- dplyr::filter(transects_to_check, valid_banks & has_relief)
  
  # TODO: problematic hy_ids in VPU 13 (have NA valid_banks / has_relief)
  # which(transects_to_check$hy_id %in% c("wb-2131572", "wb-2146599"))
  
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
  
  # add distances to extend for the left and right side of a transect
  # for any of the the already "valid transects", we just set an extension distance of 0 
  # on both sides and these transects will be KEPT AS IS
  # also set any missing valid_banks or has_relief values to 0
  transects_to_check <- add_attribute_based_extension_distances(transects = transects_to_check, 
                                                                scale = scale, 
                                                                length_col = "cs_lengthm"
  )
  # # TODO: this should be reviewed 
  # # NOTE:  --> setting a default of FALSE for NA valid_banks and NA has_relief values
  # transects_to_check <-
  #   transects_to_check %>%
  #     dplyr::mutate(
  #       valid_banks = dplyr::case_when(
  #         is.na(valid_banks) ~ FALSE,
  #         TRUE               ~ valid_banks
  #       ),
  #       has_relief = dplyr::case_when(
  #         is.na(has_relief)  ~ FALSE,
  #         TRUE               ~ has_relief
  #       )
  #     )
  # 
  # # add distances to extend for the left and right side of a transect
  # # for any of the the already "valid transects", we just set an extension distance of 0 
  # # on both sides and these transects will be KEPT AS IS
  # transects_to_check <- 
  #   transects_to_check %>% 
  #   dplyr::mutate(
  #     extension_distance = dplyr::case_when(
  #       !valid_banks | !has_relief ~ (((scale)*(cs_lengthm)) / 2),
  #       TRUE                       ~ 0
  #     )
  #   ) 
  #   # dplyr::relocate(cs_lengthm, 
  #   #                 extension_distance) 
  
  if(verbose) { message(paste0("Extending ", nrow(transects_to_check), 
                               " transects without valid banks or relief by ",     
                               scale * 100, "%...")) }
  
  # extend the transects based on the 'extension_distance' column (meters)
  extended_transects <- extend_transects_sides(
    transects    = transects_to_check,
    flowlines    = net,
    crosswalk_id = crosswalk_id,
    cs_id        = "cs_id",
    grouping_id  = crosswalk_id,
    direction    = direction
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
      -left_distance,
      -right_distance,
      -extension_distance,
      -left_is_extended, 
      -right_is_extended
    )
  
  # TODO: if we do it this way where we add back the CS that have missing banks/relief, we also need to add them back
  # TODO: to the 'transects_to_check' 
  # # # # add back any transects that were missing banks/relief values 
  # extended_transects <- dplyr::bind_rows(
  #   extended_transects,
  #   dplyr::select(missing_bank_or_relief_data,
  #                 dplyr::any_of(names(extended_transects))
  #   )
  # )
  
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
  sf::st_geometry(extended_transects[is_multi_intersecting_flowlines, ])  <- sf::st_geometry(transects_to_check[is_multi_intersecting_flowlines, ])
  
  # update the lengths and is_extended flag to align with the above replacement of geometries
  extended_transects[is_multi_intersecting_flowlines, ]$cs_lengthm        <- transects_to_check[is_multi_intersecting_flowlines, ]$cs_lengthm
  extended_transects[is_multi_intersecting_flowlines, ]$is_extended       <- transects_to_check[is_multi_intersecting_flowlines, ]$is_extended
  
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
  input_uids    <- unique(hydrofabric3D::add_tmp_id(transects_to_check, x = crosswalk_id)$tmp_id)
  output_uids   <- unique(hydrofabric3D::add_tmp_id(extended_transects, x = crosswalk_id)$tmp_id)
  
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

#' Given a set of transect lines, a flowline network, extend the transect lines out given distances from the left and right
#' Flowlines are required to ensure valid transect intersection relationship is maintained
#'
#' @param transects sf dataframe of linestrings, requires crosswalk_id, cs_id, grouping_id columns and numeric 'extension_distance' column indicating the distance to extend 
#' @param flowlines sf dataframe of linestrings
#' @param crosswalk_id character, column name that connects features in transects to flowlines
#' @param cs_id character, column name that uniquely identifies transects within a flowline
#' @param grouping_id character, column name in both transects and flowlines that denotes which flowlines are grouped with which transects.
#' @param direction character, whether to extend transects individually from left and right sides, or to strictly extend a transect if BOTH the left and right extension are valid. Valid inputs are be either "any" or "both".
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
  
  valid_directions <- c("any", "both")
  
  if (!direction %in% valid_directions) {
    stop("Invalid 'direction' argument '", direction, 
         "'\n'direction' must be one of ", 
         paste(valid_directions, collapse = ", "))
  }
  
  extension_function <- switch(
    direction,
    "any"  = extend_transects_any_side,
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
#' @export
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
  # fline_id_array   <- flowlines$id
  
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
  
  # output a message every ~10% intervals
  number_of_skips  <- 0
  
  make_progress <- make_progress_bar(TRUE, length(transect_crosswalk_id_array))
  
  for (i in seq_along(transect_crosswalk_id_array)) {
    
    make_progress()
    
    # get the current transect, hy_id, cs_id, flowline, and extension distances
    current_trans <- transects_geos[i]
    
    current_hy_id <- transect_crosswalk_id_array[i]
    current_cs_id <- transect_cs_id_array[i]
    current_uid   <- transect_uid_array[i]
    
    # transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
    # # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
    
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
      number_of_skips = number_of_skips + 1
      
      next
    }
    
    
    # extend the transects by the prescribed distances
    left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            left_distance_to_extend, "head")
    right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            right_distance_to_extend, "tail")
    
    # initial check to make sure the extended versions of the transects are valid
    # mapview::mapview(sf::st_as_sf(flowlines_geos[fline_group_id_array == transect_group_id_array[i]])) +
    #     mapview::mapview(sf::st_as_sf(transects_geos[transect_group_id_array == transect_group_id_array[i]]), color = "red") +
    #   mapview::mapview(sf::st_as_sf(left_extended_trans), color = "green") + 
    # mapview::mapview(sf::st_as_sf(right_extended_trans), color = "green") + 
    # mapview::mapview(sf::st_as_sf(current_trans), color = "red") 
    
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
        # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
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
        # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
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
    
    # TODO: in case the above code is making a copy, below should NOT ( i dont think creating start/end is a copy but just a pointer to the data)
    # updated_trans  <- make_line_from_start_and_end_pts(extension_pts[1], extension_pts[2], line_crs) 
    
    # use_left_extension <- TRUE
    # used_half_of_left <- F
    # left_distance_to_extend <- 100
    # half_left_distance <- left_distance_to_extend / 2
    
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
    
    # # last step is to replace the original transect with the updated transect (extended)
    # transects_geos[i] <- updated_trans
    # 
    # # flag if left extension happened
    # if(use_left_extension) {
    #   left_extended_flag[i]  <- TRUE
    # }
    # 
    # # flag if right extension happened
    # if(use_right_extension) {
    #   right_extended_flag[i] <- TRUE
    # }
    # 
    # # update the left extension distance if half the original distance was used
    # if (used_half_of_left) {
    #   left_distances[i]  <- half_left_distance 
    #   # updated_left_distances[i]  <- half_left_distance 
    # }
    # 
    # # update the right extension distance if half the original distance was used   
    # if (used_half_of_right) {
    #   right_distances[i] <- half_right_distance
    #   # updated_right_distances[i] <- half_right_distance 
    # }
    # 
    # # TODO: review this, didnt have this check before so theoretically, an iteration could happen where neither left or right extensions were used, but we still 
    # # TODO: set the transect to the updated transect
    # if (use_left_extension || use_right_extension) {
    #   # last step is to replace the original transect with the updated transect (extended)
    #   transects_geos[i] <- updated_trans
    # }
    # 
    # # last step is to replace the original transect with the updated transect (extended)
    # transects_geos[i] <- updated_trans
    
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
#' @export
extend_transects_both_sides <- function(
    transects,
    flowlines,
    crosswalk_id,
    cs_id       = "cs_id",
    grouping_id = "mainstem"
) {
  
  # ----------------------------------------------------------------------------------
  # Test data
  # ----------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------
  # transects    = transects
  # flowlines    = net
  # crosswalk_id = crosswalk_id
  # cs_id        = "cs_id"
  # grouping_id  = crosswalk_id
  # direction    = "both"
  # ----------------------------------------------------------------------------------
  
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
  # both_extended_flag   <- rep(FALSE, length(transect_crosswalk_id_array))
  
  # number of geometries that will be iterated over, keeping this variable to reference in message block  
  total <- length(transect_crosswalk_id_array)
  
  # output a message every ~10% intervals
  number_of_skips  <- 0
  
  make_progress <- make_progress_bar(TRUE, length(transect_crosswalk_id_array))
  
  # TODO: Dev variables to track how often the "half distance extensions" happen
  # half_extension_count            <- 0
  # successful_half_extension_count <- 0
  # which(transect_crosswalk_id_array == "wb-2425750")
  # transect_crosswalk_id_array[transect_crosswalk_id_array == "wb-2425750"]
  # which(transect_crosswalk_id_array == "wb-2425750" & transect_cs_id_array == 7)
  # transect_cs_id_array[transect_crosswalk_id_array == "wb-2425750" & transect_cs_id_array == 7]
  
  for (i in seq_along(transect_crosswalk_id_array)) {
    
    make_progress()
    # i = 69
    # i = 8
    # i
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
      number_of_skips <- number_of_skips + 1
      
      next
    }
    
    # extend the transects by the prescribed distances
    extended_trans <- hydrofabric3D::geos_extend_line(
      current_trans,
      distance_to_extend,
      dir = "both"
    )
    # transects_geos[transect_group_id_array == transect_group_id_array[i]]
    # 
    # transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_cs_id_array != current_cs_id]
    
    # # NOTE: plotting during developoment / testing 
    # plot(extended_trans, add = F, col = "red")
    # plot(current_trans, add = T, col = "green")
    # 
    # mapview::mapview(sf::st_as_sf(flowlines_geos[fline_group_id_array == transect_group_id_array[i]])) +
    # mapview::mapview(sf::st_as_sf(transects_geos[transect_group_id_array == transect_group_id_array[i]]), color = "red") +
    # mapview::mapview(sf::st_as_sf(current_trans), color = "red") + 
    # mapview::mapview(sf::st_as_sf(extended_trans), color = "green")
    
    
    # plot(transects_geos[transect_group_id_array == transect_group_id_array[i]], col = "red")
    # plot(extended_trans, col = "green", lwd = 3, add = T)
    # plot(flowlines_geos[fline_group_id_array == transect_group_id_array[i]], add = T)
    # 
    
    # check whether the extension is valid and should be used 
    # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
    use_extension <- is_valid_transect_line2(
      extended_trans,
      transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
      # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
      flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
    )
    
    used_half_of_extension <- FALSE
    
    # TODO: Probably should precompute this division BEFORE the loop...
    half_distance   <- ifelse(distance_to_extend > 0, distance_to_extend %/% 2, 0)
    
    if (!use_extension) {
      
      # half_extension_count = half_extension_count + 1
      
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
        # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      ) 
      
      # plot(transects_geos[transect_group_id_array == transect_group_id_array[i]], col = "red")
      # plot(extended_trans, col = "green", lwd = 3, add = T)
      # plot(flowlines_geos[fline_group_id_array == transect_group_id_array[i]], add = T)
      # # 
      
      
      # if (use_extension) {
      #   successful_half_extension_count = successful_half_extension_count + 1
      #   message("Successful half extension > ", successful_half_extension_count)
      # }
      
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
    
    # # flag if extension happened
    # if(use_extension) {
    #   is_extended_flag[i]  <- TRUE
    #   
    #   # TODO: Review this, i think this is right but not sure
    #   # last step is to replace the original transect with the updated transect (extended)
    #   transects_geos[i] <- extended_trans
    # }
    # 
    # # update the extension distance if half the original distance was used
    # if (used_half_of_extension) {
    #   extension_distances[i]  <- half_distance 
    # }
    # 
    # # TODO: Review this, i think this is wrong and the transect should ONLY be replaced IF it was used (duh) 
    # # # last step is to replace the original transect with the updated transect (extended)
    # transects_geos[i] <- extended_trans
    
  }      
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects)   <- sf::st_geometry(sf::st_as_sf(transects_geos))
  
  # replace the distance values so that any transects that were extended HALFWAY will be accounted for
  transects$left_distance      <- extension_distances
  transects$right_distance     <- extension_distances
  # transects$extension_distance <- extension_distances 
  
  # Flags indicating if extensions happened or not (probably can just be dropped)
  transects$left_is_extended   <- is_extended_flag
  transects$right_is_extended  <- is_extended_flag
  # transects$left_and_right_is_extended <- is_extended_flag
  
  transects <- hydroloom::rename_geometry(transects, "geometry")
  
  transects <-
    transects %>% 
    dplyr::mutate(
      cs_lengthm = as.numeric(sf::st_length(.))
    ) %>% 
    dplyr::relocate(
      dplyr::any_of(c(crosswalk_id, cs_id)),
      # dplyr::any_of(crosswalk_id),
      # dplyr::any_of(cs_id),
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
#' @export
extend_transects_both_sides2 <- function(
    transects,
    flowlines,
    crosswalk_id,
    cs_id       = "cs_id",
    grouping_id = "mainstem"
) {
  
  # ----------------------------------------------------------------------------------
  # Test data
  # ----------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------
  
  # transects    = transects
  # flowlines    = net
  # crosswalk_id = crosswalk_id
  # cs_id        = "cs_id"
  # grouping_id  = crosswalk_id
  # direction    = "both"
  
  # ----------------------------------------------------------------------------------
  
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
  # both_extended_flag   <- rep(FALSE, length(transect_crosswalk_id_array))
  
  # number of geometries that will be iterated over, keeping this variable to reference in message block  
  total <- length(transect_crosswalk_id_array)
  
  # output a message every ~10% intervals
  number_of_skips  <- 0
  
  make_progress <- make_progress_bar(TRUE, length(transect_crosswalk_id_array))
  
  # TODO: Dev variables to track how often the "half distance extensions" happen
  # half_extension_count            <- 0
  # successful_half_extension_count <- 0
  # which(transect_crosswalk_id_array == "wb-2425750")
  # transect_crosswalk_id_array[transect_crosswalk_id_array == "wb-2425750"]
  # which(transect_crosswalk_id_array == "wb-2425750" & transect_cs_id_array == 7)
  # transect_cs_id_array[transect_crosswalk_id_array == "wb-2425750" & transect_cs_id_array == 7]
  
  for (i in seq_along(transect_crosswalk_id_array)) {
    # i = 2
    make_progress()
    # i = 69
    # i = 8 
    # i
    # get the current transect, hy_id, cs_id, flowline, and extension distances
    current_trans <- transects_geos[i]
    
    current_hy_id <- transect_crosswalk_id_array[i]
    current_cs_id <- transect_cs_id_array[i]
    current_uid   <- transect_uid_array[i]
    
    # distance to try extending
    distance_to_extend <- extension_distances[i]
    
    # skip the iteration if no extension required
    no_extension_required <- distance_to_extend == 0 || is.na(distance_to_extend) || is.null(distance_to_extend)
    
    # Skip the iteration if NO extension distance is prescribed 
    if (no_extension_required) {
      number_of_skips <- number_of_skips + 1
      
      next
    }
    
    # extend the transects by the prescribed distances
    extended_trans <- hydrofabric3D::geos_extend_line(
      current_trans,
      distance_to_extend,
      dir = "both"
    )
    # transects_geos[transect_group_id_array == transect_group_id_array[i]]
    # 
    # transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_cs_id_array != current_cs_id]
    
    # # NOTE: plotting during developoment / testing 
    # plot(extended_trans, add = F, col = "red")
    # plot(current_trans, add = T, col = "green")
    # 
    # mapview::mapview(sf::st_as_sf(flowlines_geos[fline_group_id_array == transect_group_id_array[i]])) +
    # mapview::mapview(sf::st_as_sf(transects_geos[transect_group_id_array == transect_group_id_array[i]]), color = "red") +
    # mapview::mapview(sf::st_as_sf(current_trans), color = "red") + 
    # mapview::mapview(sf::st_as_sf(extended_trans), color = "green")
    
    
    # plot(transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid], col = "red")
    # # plot(transects_geos[transect_group_id_array == transect_group_id_array[i]], col = "red")
    # plot(extended_trans, col = "green", lwd = 3, add = T)
    # plot(flowlines_geos[fline_group_id_array == transect_group_id_array[i]], add = T)
    # 
    
    # check whether the extension is valid and should be used 
    # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
    use_extension <- is_valid_transect_line2(
      extended_trans,
      transects_geos[transect_group_id_array == transect_group_id_array[i] & transect_uid_array != current_uid],
      # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
      flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
    )
    
    used_half_of_extension <- FALSE
    
    # TODO: Probably should precompute this division BEFORE the loop...
    half_distance   <- ifelse(distance_to_extend > 0, distance_to_extend %/% 2, 0)
    
    if (!use_extension) {
      
      # half_extension_count = half_extension_count + 1
      
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
        # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      ) 
      
      # plot(transects_geos[transect_group_id_array == transect_group_id_array[i]], col = "red")
      # plot(extended_trans, col = "green", lwd = 3, add = T)
      # plot(flowlines_geos[fline_group_id_array == transect_group_id_array[i]], add = T)
      # # 
      
      
      # if (use_extension) {
      #   successful_half_extension_count = successful_half_extension_count + 1
      #   message("Successful half extension > ", successful_half_extension_count)
      # }
      
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
    
    # plot(transects_geos[transect_group_id_array == transect_group_id_array[i]], col = "red")
    # plot(extended_trans, col = "green", lwd = 3, add = T)
    # plot(flowlines_geos[fline_group_id_array == transect_group_id_array[i]], add = T)
    
    # # flag if extension happened
    # if(use_extension) {
    #   is_extended_flag[i]  <- TRUE
    #   
    #   # TODO: Review this, i think this is right but not sure
    #   # last step is to replace the original transect with the updated transect (extended)
    #   transects_geos[i] <- extended_trans
    # }
    # 
    # # update the extension distance if half the original distance was used
    # if (used_half_of_extension) {
    #   extension_distances[i]  <- half_distance 
    # }
    # 
    # # TODO: Review this, i think this is wrong and the transect should ONLY be replaced IF it was used (duh) 
    # # # last step is to replace the original transect with the updated transect (extended)
    # transects_geos[i] <- extended_trans
    
  }      
 
  # transects_geos  %>% plot()
  # mapview::mapview(sf::st_as_sf(transects_geos))
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects)   <- sf::st_geometry(sf::st_as_sf(transects_geos))
  
  # replace the distance values so that any transects that were extended HALFWAY will be accounted for
  transects$left_distance      <- extension_distances
  transects$right_distance     <- extension_distances
  # transects$extension_distance <- extension_distances 
  
  # Flags indicating if extensions happened or not (probably can just be dropped)
  transects$left_is_extended   <- is_extended_flag
  transects$right_is_extended  <- is_extended_flag
  # transects$left_and_right_is_extended <- is_extended_flag
  
  transects <- hydroloom::rename_geometry(transects, "geometry")
  
  transects <-
    transects %>% 
    dplyr::mutate(
      cs_lengthm = as.numeric(sf::st_length(.))
    ) %>% 
    dplyr::relocate(
      dplyr::any_of(c(crosswalk_id, cs_id)),
      # dplyr::any_of(crosswalk_id),
      # dplyr::any_of(cs_id),
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
#' @export
extend_transects_by_distances <- function(
    transects,
    flowlines,
    crosswalk_id,
    cs_id       = "cs_id",
    grouping_id = "mainstem"
) {
  
  # ---------------------------------------
  # transects <- transect_lines2
  # flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_flines_06.gpkg")
  # crosswalk_id = "hy_id"
  # cs_id = "cs_id"
  # grouping_id = 'mainstem'
  
  # transects    = transect_lines
  # flowlines    = flowlines
  # crosswalk_id = crosswalk_id
  # cs_id        = "cs_id"
  # grouping_id  = grouping_id
  
  # ---------------------------------------
  
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
  # fline_id_array   <- flowlines$id
  
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
  
  # transect_crosswalk_id_array     <- transect_lines$hy_id
  # transect_cs_id_array     <- transects$cs_id
  
  
  # Intersect grouping IDs
  fline_group_id_array      <- flowlines[[grouping_id]]
  transect_group_id_array   <- transects[[grouping_id]]
  
  # distance vectors 
  left_distances       <- transects$left_distance
  right_distances      <- transects$right_distance
  
  # # preallocate vector that stores the extension. distances
  # new_transects <- vctrs::vec_c(rep(geos::geos_empty(), length(transect_crosswalk_id_array)))
  
  # preallocate vectors for storing if transect was extended and from which directions
  left_extended_flag   <- rep(FALSE, length(transect_crosswalk_id_array))   
  right_extended_flag  <- rep(FALSE, length(transect_crosswalk_id_array))
  # both_extended_flag   <- rep(FALSE, length(transect_crosswalk_id_array))
  
  # updated_left_distances    <- rep(0, length(transect_crosswalk_id_array))   
  # updated_right_distances   <- rep(0, length(transect_crosswalk_id_array))   
  
  # number of geometries that will be iterated over, keeping this variable to reference in message block  
  total <- length(transect_crosswalk_id_array)
  
  # output a message every ~10% intervals
  # message_interval <- total %/% 20
  number_of_skips  <- 0
  
  make_progress <- make_progress_bar(TRUE, length(transect_crosswalk_id_array))
  
  # which(transect_crosswalk_id_array == "wb-1002059")
  # which(transect_crosswalk_id_array == "wb-1002059" & transect_cs_id_array == 4)
  # which(transect_crosswalk_id_array == "wb-1012096")
  # which(transect_crosswalk_id_array == "wb-1012096" & transect_cs_id_array == 3)
  # profvis::profvis({
 
  for (i in seq_along(transect_crosswalk_id_array)) {

    make_progress()
    
    # message("i: ", i)
    
    # # Check if the iteration is a multiple of 100
    # if (message_interval != 0 && i %% message_interval == 0) {
    #   percent_done <- round(i/total, 2) * 100
    #   message(i, " > ", percent_done, "% ") 
    #   message("Number of skips: ", number_of_skips)
    # }
    
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
    # no_extension_required <- (left_distance_to_extend == 0 && right_distance_to_extend == 0)
    
    # TODO: use this if we switch from using 0s to NAs
    # no_extension_required <- (is.na(left_distance_to_extend) && is.na(right_distance_to_extend))
    # current_intersect_group_id <- transect_group_id_array[i]
    # TODO: might need this in case I do the is_valid_transect() check on just the single flowline
    # current_fline      <- flowlines_geos[fline_id_array == current_hy_id]
    # TODO: these are the rest of the transects for this flowline
    # neighbor_transects <- transects_geos[transect_crosswalk_id_array == current_hy_id & transect_cs_id_array != current_cs_id]
    # mapview::mapview(sf::st_as_sf(transects_geos[transect_crosswalk_id_array == current_hy_id & transect_cs_id_array != current_cs_id]), color = "red") +
    #   mapview::mapview(sf::st_as_sf(current_trans), color = "green")
    
    # Skip the iteration if NO extension distance in either direction
    if(no_extension_required) {
      # message("Skipping -left/right extension are both 0")
      number_of_skips <- number_of_skips + 1
      
      next
    }
    
    # message("Extending transect line left and right")
    
    # extend the transects by the prescribed distances
    left_extended_trans  <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            left_distance_to_extend, "head")
    
    right_extended_trans <- hydrofabric3D::geos_extend_line(current_trans, 
                                                            right_distance_to_extend, "tail")
    
    # # initial check to make sure the extended versions of the transects are valid
    # mapview::mapview(sf::st_as_sf(flowlines_geos[fline_group_id_array == transect_group_id_array[i]])) +
    #     mapview::mapview(sf::st_as_sf(transects_geos[transect_group_id_array == transect_group_id_array[i]]), color = "red") +
    #   mapview::mapview(sf::st_as_sf(left_extended_trans), color = "green") +
    # mapview::mapview(sf::st_as_sf(right_extended_trans), color = "green") +
    # mapview::mapview(sf::st_as_sf(current_trans), color = "red")
    
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
        # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      ) 
      
      # this is dumb, true if true else false
      # this flag is just helpful for reading, whether the half distance was used or not further down in the loop
      used_half_of_left <- ifelse(use_left_extension, TRUE,  FALSE)
      # used_half_of_left <- use_left_extension
      
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
        # transects_geos[transect_group_id_array == transect_group_id_array[i]], 
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      )
      
      # this is dumb, true if true else false
      # this flag is just helpful for reading, whether the half distance was used or not further down in the loop
      used_half_of_right  <- ifelse(use_right_extension, TRUE,  FALSE)
      # used_half_of_right  <- use_right_extension
      
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
    
    # mapview::mapview(sf::st_as_sf(flowlines_geos[fline_group_id_array == transect_group_id_array[i]])) +
    #   mapview::mapview(sf::st_as_sf(transects_geos[transect_group_id_array == transect_group_id_array[i]]), color = "red") +
    #   mapview::mapview(sf::st_as_sf(left_extended_trans), color = "green") +
    #   mapview::mapview(sf::st_as_sf(right_extended_trans), color = "green") +
    #   mapview::mapview(sf::st_as_sf(updated_trans), color = "yellow") +
    #   mapview::mapview(sf::st_as_sf(current_trans), color = "red")
    
    # TODO: in case the above code is making a copy, below should NOT ( i dont think creating start/end is a copy but just a pointer to the data)
    # updated_trans  <- make_line_from_start_and_end_pts(extension_pts[1], extension_pts[2], line_crs) 
    
    # flag if left extension happened AND it was greater than 0 
    left_extended_flag[i]   <-  use_left_extension && left_distance_to_extend > 0
    # if(use_left_extension) {
      # left_extended_flag[i]  <- TRUE
    # }
    
    # flag if right extension happened AND it was greater than 0 
    right_extended_flag[i]  <-  use_right_extension && right_distance_to_extend > 0
    # if(use_right_extension) {
      # right_extended_flag[i] <- TRUE
    # }
    
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
     
    # update the left extension distance if half the original distance was used
    # if (used_half_of_left) {
    #   left_distances[i]  <- half_left_distance 
    # }
    
    # update the right extension distance if half the original distance was us
    # if (used_half_of_right) {
    #   right_distances[i] <- half_right_distance
    # }
    
    # last step is to replace the original transect with the updated transect (extended)
    transects_geos[i] <- updated_trans
    
  }      
  
  # })
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects) <- sf::st_geometry(sf::st_as_sf(transects_geos))
  
  # replace the distance values so that any transects that were extended HALFWAY will be accounted for
  transects$left_distance      <- left_distances 
  transects$right_distance     <- right_distances
  
  # Flags indicating if extensions happened or not (probably can just be dropped)
  transects$left_is_extended   <- left_extended_flag
  transects$right_is_extended  <- right_extended_flag
  
  # message("==========================")
  # message("Structure of transects object ^^^^^^^", utils::str(transects))
  # message("==========================")
  
  transects <- nhdplusTools::rename_geometry(transects, "geometry")
  
  # message("Structure of transects object AFTER RENAME ^^^^^^^", utils::str(transects))
  # message("==========================")
  
  transects <-
    transects %>% 
    dplyr::mutate(
      cs_lengthm = as.numeric(sf::st_length(.))
    ) %>% 
    dplyr::relocate(
      dplyr::any_of(c(crosswalk_id, cs_id)),
      # dplyr::any_of(crosswalk_id),
      # dplyr::any_of(cs_id),
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
    reindex_cs_ids = FALSE,
    verbose        = TRUE
) {
  
  # ----------------------------------------------------------------------------------
  # TEST DATA
  # ----------------------------------------------------------------------------------
  # library(sf)
  # library(dplyr)
  # library(geos)
  # library(terra)
  # 
  # crosswalk_id = "hy_id"
  # scale          = 0.5
  # keep_lengths   = TRUE
  # verbose        = TRUE
  # 
  # flowlines <- sf::read_sf("/Users/anguswatters/Desktop/extend_transects_by_cs_attributes_flowlines_test.gpkg") %>%
  #   dplyr::filter(hy_id %in% c("wb-778430", "wb-1060460")) %>% 
  #   hydroloom::rename_geometry("geometry")
  # 
  # transects <- sf::read_sf("/Users/anguswatters/Desktop/extend_transects_by_cs_attributes_transects_test.gpkg") %>%
  #   dplyr::filter(hy_id %in% c("wb-778430", "wb-1060460")) %>% 
  #   dplyr::select(hy_id, cs_id, cs_measure, cs_lengthm, valid_banks, has_relief, geom) %>% 
  #   hydroloom::rename_geometry("geometry")
  # # 
  # mapview::mapview(dplyr::filter(transects, vpu %in% c("07", "05")))
  # mapview::mapview(dplyr::filter(transects, vpu %in% c("07", "05")))
  
  # cs_pts <- arrow::read_parquet("/Users/anguswatters/Desktop/extend_transects_by_cs_attributes_cs_pts2_test.parquet")
  # cs_pts <- arrow::read_parquet("/Users/anguswatters/Desktop/extend_transects_by_cs_attributes_cs_pts_test.parquet")
  
  # transects = transects
  # flowlines = flowlines
  # crosswalk_id = CROSSWALK_ID
  # scale = 0.5
  # keep_lengths = TRUE
  # reindex_cs_ids = FALSE
  # verbose =T
  # ----------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------
  
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
    # x             <- add_hydrofabric_id(x)
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
  
  # start_cols          <- names(transects)
  
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
  
  # count_check <- nrow(valid_transects) + nrow(invalid_transects) == nrow(transects_to_check)
  # count_check <- nrow(valid_transects) + nrow(invalid_transects) == nrow(transects_to_check) - nrow(missing_bank_or_relief_data)
  
  if(!count_check) {
    warning(paste0(nrow(missing_bank_or_relief_data), " transects have NA values in either 'valid_banks' or 'has_relief' columns"))
    # warning(paste0("Different number of transects after splitting data by 'valid_banks' and 'has_relief' columns, ", nrow(missing_bank_or_relief_data), " transects have NA values in either 'valid_banks' or 'has_relief' columns"))
    # stop("Mismatch in number of points after splitting data by the 'valid_banks' and 'has_relief' columns, likely a missing value in either 'valid_banks' or 'has_relief' columns")
  }
  
  # add distances to extend for the left and right side of a transect
  # for any of the the already "valid transects", we just set an extension distance of 0
  # on both sides and these transects will be KEPT AS IS
  # also set any missing valid_banks or has_relief values to 0
  transects <- add_attribute_based_extension_distances(transects = transects,
                                                       scale = scale,
                                                       length_col = "cs_lengthm"
  )
  
  # count of transects to improve
  invalid_count <-
    transects %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(valid_banks, has_relief) %>% 
    dplyr::summarise(
      count = sum(!valid_banks | !has_relief, na.rm = T)
    ) %>%
    dplyr::pull(count)
  
  if(verbose) { message(paste0("Extending ", 
                               invalid_count,
                               " transects without valid banks or relief by ",
                               scale * 100, "%...")) }
  
  # extend the transects based on the 'extension_distance' column (meters)
  extended_transects <- extend_transects_sides(
    transects    = transects,
    flowlines    = flowlines,
    crosswalk_id = crosswalk_id,
    cs_id        = "cs_id",
    grouping_id  = crosswalk_id,
    direction    = "both"
  )

  # extended_transects$is_extended
  
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
      # -left_distance,
      # -right_distance,
      # -extension_distance,
      -left_is_extended,
      -right_is_extended
    ) 
  
  
  # tmp_trans <- 
  #   extended_transects %>% 
  #   dplyr::filter(
  #     hy_id %in% c("wb-778430", "wb-1060460")
  #                 )
  # mapview::mapview(tmp_trans) + 
  # mapview::mapview(  rm_self_intersections(tmp_trans) , color = "red") +
  # mapview::mapview(  rm_multi_intersects(tmp_trans) , color = "green")
  
  # extended_transects2  <- extended_transects
  
  # shorten any transects that intersect multiple transects back to their original lengths
  extended_transects  <- shorten_multi_intersecting_transects(x = extended_transects, 
                                                              crosswalk_id = crosswalk_id)
  
  # shorten any transects that intersect multiple flowlines (or a flowline more than once) back to their original lengths
  extended_transects  <- shorten_multi_flowline_intersecting_transects(x = extended_transects, 
                                                                       flowlines = flowlines,
                                                                       crosswalk_id = crosswalk_id)
  
  # remove self intersections and flowline multi intersections
  extended_transects <-
    extended_transects %>% 
    rm_multi_intersects() %>% 
    rm_self_intersections() %>%
    rm_multiflowline_intersections(flowlines)
  
  # select relevant IDs
  extended_transects <- 
    extended_transects %>% 
    dplyr::select(
      dplyr::any_of(c(crosswalk_id, "cs_id", "cs_source")),
      cs_lengthm, cs_measure,
      valid_banks, has_relief,
      geometry
    )
  
  # extended_transects3 <-
  #   extended_transects %>% 
  #   rm_self_intersections() %>%
  #   rm_multiflowline_intersections(flowlines)
  
  # mapview::mapview(transects, color = "cyan") +
  # mapview::mapview(extended_transects , color = "red") +
  # mapview::mapview(extended_transects2, color = "green") + 
  # mapview::mapview(extended_transects3, color = "red")
  
  # check to make sure all unique hy_id/cs_id in the INPUT are in the OUTPUT,
  # and raise an error if they're are missing hy_id/cs_ids
  input_uids    <- get_unique_tmp_ids(transects, x = crosswalk_id)
  output_uids   <- get_unique_tmp_ids(extended_transects, x = crosswalk_id)
  
  # check all of the output_uids exist in the input UIDs
  has_all_uids           <- all(output_uids %in% input_uids)
  # has_all_original_uids  <- all(input_uids %in% output_uids)
  # no_extra_output_uids <- length(output_uids[!output_uids %in% input_uids]) == 0
  
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
      ) %>% 
      dplyr::select(
        dplyr::any_of(c(crosswalk_id, "cs_id", "cs_source")),
        cs_lengthm, cs_measure,
        valid_banks, has_relief,
        initial_length,
        geometry
      )
  }
  
  # re-index the cs_ids to make sure there are 1-number of transects for each crosswalk_id and that there are NO gaps between cs_ids
  if (reindex_cs_ids) {
    warning("Re-indexing cs_ids may result in a mismatch between unique crosswalk_id/cs_ids in input 'transects' and the output unique crosswalk_id/cs_ids")
    extended_transects <- renumber_cs_ids(extended_transects, crosswalk_id = crosswalk_id)
  }
  
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
#' @export
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
  
  # to_extend2 <- dplyr::slice(to_extend, 1:10)
  # extended_trans2 <- extend_by_percent(to_extend2, scale, "cs_lengthm")
  # geos_trans <- geos::as_geos_geometry(extended_trans2)
  
  # if(verbose) { message(paste0("Converting sf geometries to geos geometries...")) }
  
  # Convert extended transects to geos
  extended_trans  <- geos::as_geos_geometry(extended_trans)
  
  # Convert the net object into a geos_geometry
  geos_net <- geos::as_geos_geometry(net)
  
  # if(verbose) { message(paste0("Iterating through extended geometries and checking validity...")) }
  
  # Convert the original transect lines to geos_geometries and when 
  # a valid extension comes up in the below for loop, replace the old geometry with the newly extended one
  geos_list     <- geos::as_geos_geometry(transects_to_extend$geom)
  
  # Preallocate vectors to store the "is_extended" flag and the new lengths after extensions:
  # - if an extension is VALID (checked in the loop below), then 
  #   set the "is_extended" flag to TRUE and update the cross section length 
  #   to use the new extended length
  extended_flag <- rep(FALSE, length(extended_trans))
  # length_list   <- transects_to_extend$cs_lengthm
  
  # length(geos_net)
  # length(fline_id_array)
  # length(hy_id_array)
  
  # geos_list <- geos::geos_empty(rep("linestring", length(extended_trans)))
  # geos_list <- extended_trans
  # extended_flag  <- vctrs::vec_c()
  # length_list    <- vctrs::vec_c()
  
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
      # message("Iteration ", i, " / ", length(extended_trans), 
      #         " - (", percent_done, "%) ")
      
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
  
  transects_to_extend$is_extended  <- extended_flag
  transects_to_extend$cs_lengthm   <- as.numeric(sf::st_length(transects_to_extend))
  # transects_to_extend$cs_lengthm   <- length_list
  
  # transects_to_extend$geom[1]  %>% sf::st_length()
  # geos::geos_length(geos_list[1])
  
  return(transects_to_extend)
}

# TODO: DELETE SOON

# # @title Extend a set of transects by a percentage based on banks and relief
# # Given a set of transect lines with valid_banks and has_relief columns (derived from DEM extracted cross section points), extend any transects 
# # by a percentage of the transects length if the transect does NOT have valid banks (valid_banks == FALSE) OR it does NOT have relief (has_relief == FALSE).
# # @param transects_to_check sf linestrings, set of all transects in the network. Requires the following columns: "hy_id", "cs_id", "cs_lengthm" (length of geometry in meters), "valid_banks", and "has_relief"
# # @param net sf linestrings, flowline network that transects were generated from, requires "id" column (where "crosswalk_id" equals the "hy_id" columns in 'transects_to_check' and 'transects' )
# # @param scale numeric, percentage of current transect line length to extend transects in transects_to_extend by. Default is 0.5 (50% of the transect length)
# # @param verbose logical, whether to show a progress bar and progress messages or not. Default is TRUE.
# # @return sf linestring dataframe containing the the original transects with extensions performed on transects without valid_banks OR has_relief (a "is_extended" flag denotes if the geometry was extended by "scale" % or not)
# # @importFrom geos as_geos_geometry geos_intersection geos_type geos_intersects
# # @importFrom sf st_geometry st_as_sf
# # @importFrom dplyr filter bind_rows
# # @export
# extend_invalid_transects3 <- function(
    #     transects_to_check, 
#     net, 
#     scale = 0.5,
#     verbose = TRUE
# ) {
#   # ----------------------------------------
#   # ----------------------------------------
#   
#   # transects_to_check  = transects
#   # net                 = net
#   # scale               = scale
#   # verbose             = verbose
#   
#   # ----------------------------------------
#   # ----------------------------------------
#   
#   # Create an "is_extended" flag to identify which transects were extended and updated 
#   transects_to_check$is_extended <- FALSE
#   
#   # split input transects into invalid and valid sets (valid == has valid banks AND has relief)
#   invalid_transects  <- dplyr::filter(transects_to_check, !valid_banks | !has_relief)
#   valid_transects    <- dplyr::filter(transects_to_check, valid_banks & has_relief)
#   
#   # keep track of any transects that having missing values in either valid_banks/has_relief columns, 
#   # these get added back to the updated data at the end
#   missing_bank_or_relief_data <- 
#     transects_to_check %>% 
#     dplyr::filter(is.na(valid_banks) | is.na(has_relief))
#   
#   # TODO: Probably remove this
#   count_check <- nrow(valid_transects) + nrow(invalid_transects) == nrow(transects_to_check)
#   # count_check <- nrow(valid_transects) + nrow(invalid_transects) == nrow(transects_to_check) - nrow(missing_bank_or_relief_data)
#   
#   if(!count_check) {
#     warning(paste0(nrow(missing_bank_or_relief_data), " transects have NA values in either 'valid_banks' or 'has_relief' columns"))
#     # warning(paste0("Different number of transects after splitting data by 'valid_banks' and 'has_relief' columns, ", nrow(missing_bank_or_relief_data), " transects have NA values in either 'valid_banks' or 'has_relief' columns"))
#     # stop("Mismatch in number of points after splitting data by the 'valid_banks' and 'has_relief' columns, likely a missing value in either 'valid_banks' or 'has_relief' columns")
#   }
#   
#   if(verbose) { message(paste0("Extending ", nrow(invalid_transects), " transects without valid banks or relief by ",     scale * 100, "%...")) }
#   
#   # Extend the transects by a scale % value
#   extended_trans <- extend_by_percent(invalid_transects, scale, "cs_lengthm")
#   
#   # Store the identifying information to use in for loop to subset data using IDs
#   fline_id_array <- net$id
#   hy_id_array    <- extended_trans$hy_id
#   cs_id_array    <- extended_trans$cs_id
#   
#   # Convert extended transects to geos
#   extended_trans  <- geos::as_geos_geometry(extended_trans)
#   
#   # Convert the net object into a geos_geometry
#   geos_net        <- geos::as_geos_geometry(net)
#   
#   # if(verbose) { message(paste0("Iterating through extended geometries and checking validity...")) }
#   
#   # Convert the original transect lines to geos_geometries and when 
#   # a valid extension comes up in the below for loop, replace the old geometry with the newly extended one
#   geos_list       <- geos::as_geos_geometry(invalid_transects$geom)
#   
#   # Preallocate vectors to store the "is_extended" flag and the new lengths after extensions:
#   # - if an extension is VALID (checked in the loop below), then 
#   #   set the "is_extended" flag to TRUE and update the cross section length 
#   #   to use the new extended length
#   extended_flag <- rep(FALSE, length(extended_trans))
#   length_list   <- invalid_transects$cs_lengthm
#   
#   make_progress <- make_progress_bar(verbose, length(extended_trans))
# 
#     # loop through geometries that might need to be extended, try to extend, and then update 
#     # the 'to_extend' values IF the extended transectr does NOT violate any intersection rules
#     for (i in 1:length(extended_trans)) {
# 
#       # Get the current transect, hy_id, cs_id
#       current_trans <- extended_trans[i]
#       current_hy_id <- hy_id_array[i]
#       current_cs_id <- cs_id_array[i]
#       
#       # use the hy_id from the current transect line to index the 
#       # full network of flowlines to get the specific flowline for this transect (geos_geometry)
#       current_fline <- geos_net[fline_id_array == current_hy_id]
#       
#       # # filter down to the rest of the transects on the given "hy_id", EXCLUDING SELF
#       # neighbor_transects <- geos::as_geos_geometry(dplyr::filter(transects, 
#       # hy_id == current_hy_id,  cs_id != current_cs_id))
#       
#       # Get all of the other transects on this flowline using "hy_id" and "cs_id" (EXCLUDING SELF)
#       neighbor_transects <- geos::as_geos_geometry(
#         transects_to_check[transects_to_check$hy_id == current_hy_id & transects_to_check$cs_id != current_cs_id, ]
#       )
#       
#       # Make sure that newly extended transect line only intersects its origin flowline at MOST 1 time
#       # AND that the newly extended transect does NOT intersect with any previously computed transect lines
#       fline_intersect <- geos::geos_intersection(
#         current_trans,     
#         current_fline
#       )
#       
#       # If all of these conditions are TRUE then the currently extended transect will get inserted into "to_extend"
#       # - Newly extended transect intersects with its flowlines AT MOST 1 time
#       # - Newly extended transect does NOT intersect with any of the other NEWLY EXTENDED transect lines
#       # - Newly extended transect does NOT intersect with any of the ORIGINAL transect lines
#       if (
#         # Check that newly extended cross section only intersects its origin flowline at MOST 1 time 
#         # (This value will be a "MULTIPOINT" if it intersects more than once and will evaluate to FALSE)
#         geos::geos_type(fline_intersect) == "point" &&
#         # Check that extended transect doesn't intersect with any of the NEWLY EXTENDED cross sections
#         !any(geos::geos_intersects(current_trans, extended_trans[-i])) &&
#         # Check that extended transect doesn't intersect with any of the original cross sections on this "hy_id"
#         !any(geos::geos_intersects(current_trans, neighbor_transects))
#       ) {
#         
#         # message("Extending transect: ", i)
#         
#         # get the current cross section list
#         current_length <- length_list[i]
#         # current_length <- invalid_transects$cs_lengthm[i]
#         
#         # # Calculate the updated cross section length to align with the newly extended cross section for this row
#         updated_cs_length <- (current_length * scale) + current_length
#         # updated_cs_length <- (output_row$cs_lengthm * scale) + output_row$cs_lengthm
#         
#         # copy the current cross section length
#         length_list[i] <- updated_cs_length
#         # length_list  <- vctrs::vec_c(length_list, updated_cs_length)
#         
#         # Update the transect geometry with the newly extended transect
#         geos_list[i] <- current_trans
#         # geos_list <- vctrs::vec_c(geos_list, current_trans)
#         
#         # Set the extended flag to TRUE for this transect
#         extended_flag[i] <- TRUE
#         # extended_flag  <- vctrs::vec_c(extended_flag, TRUE)
#         
#       } 
#       
#       make_progress()
#     }
#     
#   if(verbose) { message(paste0("Complete!")) }
#   
#   # Update the "invalid_transects" with new geos geometries ("geos_list")
#   sf::st_geometry(invalid_transects) <- sf::st_geometry(sf::st_as_sf(geos_list))
#   
#   # update the "is_extended" flag and the cross section lengths to reflect any extensions
#   invalid_transects$is_extended <- extended_flag
#   invalid_transects$cs_lengthm  <- length_list
#   
#   # Combine the valid_transects with the UPDATED invalid_transects (updated by attempting extension) to get the final output dataset
#   extended_transects <- dplyr::bind_rows(
#     valid_transects,
#     invalid_transects
#   )
#   
#   # add back any transects that were missing banks/relief values 
#   extended_transects <- dplyr::bind_rows(
#     extended_transects,
#     dplyr::select(missing_bank_or_relief_data, 
#                   dplyr::any_of(names(extended_transects)))
#   )
#   
#   # check to make sure all unique hy_id/cs_id in the INPUT are in the OUTPUT, 
#   # and raise an error if they're are missing hy_id/cs_ids
#   input_uids  <- unique(hydrofabric3D::add_tmp_id(transects_to_check)$tmp_id)
#   output_uids <- unique(hydrofabric3D::add_tmp_id(extended_transects)$tmp_id)
#   
#   has_all_uids  <- all(output_uids %in% input_uids)
#   
#   # throw an error if NOT all hy_id/cs_ids are the same in the input and output data
#   if(!has_all_uids) {
#     stop("Missing unique hy_id/cs_id from input transects in the output transects")
#   }
#   
#   return(extended_transects)
# }