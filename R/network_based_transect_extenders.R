#' @title Extend a set of transects by a percentage based on banks and relief
#' Given a set of transect lines with valid_banks and has_relief columns (derived from DEM extracted cross section points), extend any transects 
#' by a percentage of the transects length if the transect does NOT have valid banks (valid_banks == FALSE) OR it does NOT have relief (has_relief == FALSE).
#' @param transects_to_check sf linestrings, set of all transects in the network. Requires the following columns: "hy_id", "cs_id", "cs_lengthm" (length of geometry in meters), "valid_banks", and "has_relief"
#' @param net sf linestrings, flowline network that transects were generated from, requires "id" column (where "id" equals the "hy_id" columns in 'transects_to_check' and 'transects' )
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
#' @importFrom dplyr filter bind_rows
#' @export
extend_invalid_transect_sides <- function(
    transects_to_check, 
    net, 
    crosswalk_id,
    scale     = 0.5,
    direction = "both",
    verbose   = TRUE
) {
  # ----------------------------------------
  # ----------------------------------------
  
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
  req_cols    <- c(crosswalk_id, "cs_id", "cs_lengthm", 
                   "valid_banks", "has_relief", "geometry")
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
      extension_distance = dplyr::case_when(
        !valid_banks | !has_relief ~ (((scale)*(cs_lengthm)) / 2),
        TRUE                       ~ 0
      )
    ) %>% 
    dplyr::relocate(cs_lengthm, 
                    extension_distance) 
  
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
      -extension_distance,
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
  input_uids    <- unique(hydrofabric3D::add_tmp_id(transects_to_check, x = get(crosswalk_id))$tmp_id)
  output_uids   <- unique(hydrofabric3D::add_tmp_id(extended_transects, x = get(crosswalk_id))$tmp_id)
  
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
#' @param transects sf dataframe of linestrings, requires crosswalk_id, cs_id, grouping_id columns and numeric 'left_distance' and 'right_distance' columns
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
  
  transect_crosswalk_id_array     <- transects[[crosswalk_id]]
  
  transect_cs_id_array      <- transects[[cs_id]]
  
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
    use_left_extension  <- is_valid_transect_line(
      left_extended_trans,
      transects_geos[transect_group_id_array == transect_group_id_array[i]], 
      flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
    ) 
    
    use_right_extension <- is_valid_transect_line(
      right_extended_trans, 
      transects_geos[transect_group_id_array == transect_group_id_array[i]], 
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
      use_left_extension  <- is_valid_transect_line(
        left_extended_trans,
        transects_geos[transect_group_id_array == transect_group_id_array[i]], 
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
      
      use_right_extension <- is_valid_transect_line(
        right_extended_trans, 
        transects_geos[transect_group_id_array == transect_group_id_array[i]], 
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
    
    # flag if left extension happened
    if(use_left_extension) {
      left_extended_flag[i]  <- TRUE
    }
    
    # flag if right extension happened
    if(use_right_extension) {
      right_extended_flag[i] <- TRUE
    }
    
    # update the left extension distance if half the original distance was used
    if (used_half_of_left) {
      left_distances[i]  <- half_left_distance 
      # updated_left_distances[i]  <- half_left_distance 
    }
    
    # update the right extension distance if half the original distance was used   
    if (used_half_of_right) {
      right_distances[i] <- half_right_distance
      # updated_right_distances[i] <- half_right_distance 
    }
    
    # last step is to replace the original transect with the updated transect (extended)
    transects_geos[i] <- updated_trans
    
  }      
  
  # Update the "transects_to_extend" with new geos geometries ("geos_list")
  sf::st_geometry(transects)   <- sf::st_geometry(sf::st_as_sf(transects_geos))
  
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
  
  for (i in seq_along(transect_crosswalk_id_array)) {
    
    make_progress()
    
    # get the current transect, hy_id, cs_id, flowline, and extension distances
    current_trans <- transects_geos[i]
    
    current_hy_id <- transect_crosswalk_id_array[i]
    current_cs_id <- transect_cs_id_array[i]
    
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
    
    # NOTE: plotting during developoment / testing 
    # plot(extended_trans, add = F, col = "red")
    # plot(current_trans, add = T, col = "green")
    # mapview::mapview(sf::st_as_sf(flowlines_geos[fline_group_id_array == transect_group_id_array[i]])) +
    # mapview::mapview(sf::st_as_sf(transects_geos[transect_group_id_array == transect_group_id_array[i]]), color = "red") +
    # mapview::mapview(sf::st_as_sf(current_trans), color = "red") 
    
    # check whether the extension is valid and should be used 
    # ONLY CHECKING FOR INTERSECTIONS ON CURRENT FLOWLINE NOT WHOLE NETWORK 
    use_extension <- is_valid_transect_line(
      extended_trans,
      transects_geos[transect_group_id_array == transect_group_id_array[i]], 
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
      use_extension          <- is_valid_transect_line(
        extended_trans,
        transects_geos[transect_group_id_array == transect_group_id_array[i]], 
        flowlines_geos[fline_group_id_array == transect_group_id_array[i]]
      ) 
      
      # if (use_extension) {
      #   successful_half_extension_count = successful_half_extension_count + 1
      #   message("Successful half extension > ", successful_half_extension_count)
      # }
      
      used_half_of_extension <- ifelse(use_extension, TRUE,  FALSE)
    } 
    
    # flag if extension happened
    if(use_extension) {
      is_extended_flag[i]  <- TRUE
    }
    
    # update the extension distance if half the original distance was used
    if (used_half_of_extension) {
      extension_distances[i]  <- half_distance 
    }
    
    # last step is to replace the original transect with the updated transect (extended)
    transects_geos[i] <- extended_trans
    
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
  
  transects <- nhdplusTools::rename_geometry(transects, "geometry")
  
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
