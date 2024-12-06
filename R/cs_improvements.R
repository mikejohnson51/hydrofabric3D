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

# TODO: Finalize version 3 that extends from BOTH directions instead of individually extending LEFT and RIGHT 

#' Check if dataset X has all the same unique tmp_ids as dataset Y
#' Internal helper function for keeping track of IDs when they should be identical between 2 datasets
#' @param x tibble, dataframe, or sf dataframe
#' @param y tibble, dataframe, or sf dataframe
#' @param crosswalk_id character, unique ID column
#'
#' @return logical, TRUE if all id / cs_id combos are contained in both dataset x and y
#' @noRd
#' @keywords internal
has_same_unique_tmp_ids <- function(x, y, crosswalk_id = NULL) {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  start_ids <- get_unique_tmp_ids(df = x, x = crosswalk_id, y = "cs_id")
  end_ids   <- get_unique_tmp_ids(df = y, x = crosswalk_id, y = "cs_id")
  
  # all IDs are in x AND y and same number of ids
  same_unique_ids <- all(start_ids %in% end_ids) && all(end_ids %in% start_ids) && length(start_ids) == length(end_ids)
  
  return(same_unique_ids)
}

#' Check if dataset X has all the same unique tmp_ids as dataset Y 
#' Internal helper function for keeping track of IDs when they should be identical between 2 datasets
#' @param x tibble, dataframe, or sf dataframe
#' @param y tibble, dataframe, or sf dataframe
#' @param crosswalk_id character, unique ID column
#'
#' @return logical, TRUE if all id / cs_id combos are contained in both dataset x and y
#' @noRd
#' @keywords internal
has_same_uids <- function(x, y, crosswalk_id = NULL) {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  x_uids <- get_unique_tmp_ids(df = x, x = crosswalk_id, y = "cs_id")
  y_uids <- get_unique_tmp_ids(df = y, x = crosswalk_id, y = "cs_id")
  
  # all IDs are in x AND y and same number of ids
  same_unique_ids <- all(x_uids %in% y_uids) && all(y_uids %in% x_uids) && length(x_uids) == length(y_uids)
  
  return(same_unique_ids)
  
}

#' Join 'valid_banks' and 'has_relief' columns to transects dataset from corresponding cross section points 
#'
#' @param transects dataframe or sf dataframe of transects
#' @param cs_pts dataframe or sf dataframe cross section points corresponding to transects
#' @param crosswalk_id character, unique ID column
#' @importFrom dplyr left_join select group_by across any_of slice ungroup 
#' @importFrom sf st_drop_geometry 
#' @return dataframe or sf dataframe
#' @noRd
#' @keywords internal
add_cs_attributes_to_transects <- function(transects, 
                                           cs_pts, 
                                           crosswalk_id = NULL) {
  # validate input datas
  is_transects_valid  <- validate_df(transects, 
                                     c(crosswalk_id, "cs_id"),
                                     "transects")
  
  is_cs_pts_valid        <- validate_df(cs_pts, 
                                        c(crosswalk_id, "cs_id", "valid_banks", "has_relief"), 
                                        "cs_pts")
  
  # join 'valid_banks' and 'has_relief' columns to transects from cs_pts
  transects <- 
    transects %>% 
    dplyr::left_join(
      cs_pts %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(dplyr::any_of(crosswalk_id), cs_id, valid_banks, has_relief),
      by = c(crosswalk_id, "cs_id")
    )
  
  return(transects)
  
}

#' Add an extension_distance column based off valid_banks and has_relief attributes
#'
#' @param transects dataframe, tibble or sf dataframe with length_col,  "valid_banks", and "has_relief" columns
#' @param scale numeric, percentage of current transect line length to extend transects in transects_to_extend by. Default is 0.5 (50% of the transect length)
#' @param length_col character, name of the column with the numeric cross section length 
#'
#' @return dataframe, tibble or sf dataframe
#' @importFrom dplyr mutate case_when
#' @noRd
#' @keywords internal
add_attribute_based_extension_distances <- function(transects, 
                                                    scale      = 0.5, 
                                                    length_col = NULL
) {
  
  if(!inherits(scale, "numeric")) {
    stop("Invalid 'scale' value, scale must be an integer or float numeric value")
  }
  
  if (scale < 0) {
    stop("Invalid 'scale' value, scale must be numeric value greater than or equal to 0")
  }
  
  if(is.null(length_col)) {
    stop("Missing 'length_col' character input indicating which column in 'transects' is a numeric vector of the lengths of each transect")
  }
  
  REQUIRED_COLS <- c(length_col, "valid_banks", "has_relief")
  
  # validate input graph
  is_valid <- validate_df(transects, REQUIRED_COLS, "transects")
  
  # TODO: this should be reviewed 
  # NOTE:  --> setting a default of FALSE for NA valid_banks and NA has_relief values
  transects <-
    transects %>%
    dplyr::mutate(
      valid_banks = dplyr::case_when(
        is.na(valid_banks) ~ FALSE,
        TRUE               ~ valid_banks
      ),
      has_relief = dplyr::case_when(
        is.na(has_relief)  ~ FALSE,
        TRUE               ~ has_relief
      )
    )
  
  # add distances to extend for the left and right side of a transect
  # for any of the the already "valid transects", we just set an extension distance of 0 
  # on both sides and these transects will be KEPT AS IS
  transects <-
    transects %>% 
    dplyr::mutate(
      extension_distance = dplyr::case_when(
        !valid_banks | !has_relief ~ (((scale)*(.data[[length_col]])) / 2),
        TRUE                       ~ 0
      )
    ) 
  
  return(transects)
  
}

#' Set valid_banks and has_relief values to TRUE if an NA exists in either column
#'
#' @param x dataframe or sf dataframe with valid_banks and has_relief columns
#'
#' @importFrom dplyr mutate case_when
#'
#' @return dataframe or sf dataframe with valid_banks and has_relief set to TRUE 
#' @noRd
#' @keywords internal
fill_missing_cs_attributes <- function(x) {
  
  # validate input datas
  is_valid        <- validate_df(x, 
                                 c("valid_banks", "has_relief"), 
                                 "x")
  x <- 
    x %>% 
    dplyr::mutate(
      valid_banks = dplyr::case_when(
        is.na(valid_banks) | is.na(has_relief) ~ TRUE,
        TRUE                                   ~ valid_banks
      ),
      has_relief = dplyr::case_when(
        is.na(valid_banks) | is.na(has_relief) ~ TRUE,
        TRUE                                   ~ has_relief
      )
    )
  
  
  return(x)
  
}

#' Add a flagged and extension distance columns to set of transects with CS attributes based on new cross section points data
#'
#' @param x sf dataframe of transects 
#' @param crosswalk_id character, unique ID column
#' @param points_per_cs numeric
#' @param min_pts_per_cs numeric
#' @param dem character
#' @param pct_of_length_for_relief numeric
#' @param na.rm logical, whether to remove NAs from the given cross section points and any NA comparison points pulled from the dem. Default is TRUE 
#' @importFrom hydroloom rename_geometry
#' @importFrom dplyr left_join mutate any_of select case_when
#'
#' @return sf dataframe of transects with updated geometries 
#' @export
flag_transects_for_change <- function(
    x, 
    crosswalk_id = NULL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = default_dem,
    pct_of_length_for_relief = 0.01,
    na.rm = TRUE
) {
  
  # set geometry column names at beginning
  x    <- hydroloom::rename_geometry(x, "geometry")
  
  # validate input datas
  is_valid_df        <- validate_df(x, 
                                    c(crosswalk_id, "cs_id", 
                                      "cs_lengthm", 
                                      
                                      "initial_length",
                                      "left_distance", "right_distance",
                                      
                                      "cs_measure", 
                                      "valid_banks", "has_relief", "geometry"), 
                                    "x")
  
  # get cross section point elevations``
  new_cs_pts <- cross_section_pts(
    cs             = x,
    crosswalk_id   = crosswalk_id,
    points_per_cs  = points_per_cs,
    min_pts_per_cs = min_pts_per_cs,
    dem            = dem
  ) %>%
    classify_points(
      crosswalk_id             = crosswalk_id,
      pct_of_length_for_relief = pct_of_length_for_relief,
      na.rm                    = na.rm
    ) %>% 
    add_tmp_id(crosswalk_id) %>% 
    fill_missing_cs_attributes()
  
  # compare validity scores between initial validity values and new ones
  cs_validities <- compare_cs_validity(cs_pts1  = x, 
                                       cs_pts2  = new_cs_pts, 
                                       crosswalk_id = crosswalk_id
  )
  
  # identify transects to shorten back to original length and provide a distance to shorten by (extension_distance)
  x <- 
    x %>% 
    add_tmp_id(crosswalk_id) %>% 
    dplyr::left_join(
      cs_validities %>% 
        dplyr::select(dplyr::any_of(crosswalk_id), 
                      cs_id, is_improved),
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::mutate(
      flagged            = (!is_improved) & ((initial_length < cs_lengthm) | (left_distance > 0) | (right_distance > 0)),
      extension_distance = ((cs_lengthm - initial_length) / 2)
    )
  
  return(x)
  
}

#' @title Check and fix cross section points with limited variation in Z values (without removing any flowlines)
#' @description Deprecated. Duplicate process as rectify_cs() but does NOT remove any cross sections, only attempts to extend transects and improve cross sections. This function takes in a set of cross section points (cs_pts), a flowline network (net) and a set of transects lines for that flowline network (transects).
#' This function assumes the cross section points have been classified via "classify_points()" and have "has_relief" and "valid_banks" logical columns.
#' This function will look for cross section points that either have no relief or don't have valid banks, then the transect lines that generated these cross section points
#' are extended and new points are extracted along the newly extended, longer transect line. The newly extracted points are checked for relief AND valid banks and 
#' are removed if they still have no relief or don't have valid banks. Any new points that became valid as a result of the extension process are added to the original dataset 
#' and the rectified set of cross section points will be returned with an "is_extended" logical flag, indicating if the transect line that generated the cross section points was extended.
#' Improved function for rectifying cross section points with flat Z values by extending transect lines and reevaluating the new DEM values.
#' @param cs_pts sf dataframe or dataframe of cross section points from cross_section_pts() followed by classify_points()
#' @param net Hydrographic LINESTRING Network
#' @param transects character, Hydrographic LINESTRING of transects along hydrographic (net) network
#' @param crosswalk_id character, ID column that uniquely identifies and crosswalks features between 'net', 'transects' and 'cs_pts'
#' @param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
#' @param min_pts_per_cs Minimun number of points per cross section required.
#' @param dem the DEM to extract data from
#' @param scale numeric, If a transect line DEM extraction results in all equal Z values,
#'  by what percent of the transect lines length (meters) should the transect line be
#'   extended in both directions to try to capture representative Z values ? Default is 0.5 (50% of the transect length)
#' @param pct_of_length_for_relief numeric, percent of cs_lengthm to use as the threshold depth for classifying whether a cross section has "relief". Default is 0.01 (1% of the cross sections length).
#' @param fix_ids logical, whether to reenumerate the "cs_id" column to 
#' make sure cross sections are number 1 - number of total cross sections on flowline.  Default is FALSE, cs_id will be kept as 
#' they were in the input data and may contain gaps between cs_ids within a flowline (hy_id). 
#' WARNING: Setting fix_ids = TRUE may result in input cross section points (cross_section_pts) having DIFFERENT cs_id values as the input transects (cs) 
#' and the inconsistency can cause problems when trying to cross walk between the datasets in the future.
#' @param verbose logical, whether to print messages or not. Default is TRUE
#' @importFrom dplyr mutate relocate last_col select rename left_join group_by ungroup slice n bind_rows filter
#' @importFrom sf st_drop_geometry
#' @importFrom hydroloom rename_geometry
#' @return sf object of cross section points based on extended transects to try and improve the number of points with "valid_banks" and "has_relief"
#' @noRd
#' @keywords internal
get_improved_cs_pts = function(
    cs_pts         = NULL,   
    net            = NULL,
    transects      = NULL,
    crosswalk_id   = NULL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = default_dem,
    scale          = 0.5,
    pct_of_length_for_relief = 0.01,
    fix_ids        = FALSE,
    verbose        = TRUE
) {
 
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # net <- add_hydrofabric_id(net) 
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  if (!crosswalk_id %in% names(cs_pts)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'cs_pts' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks 'cs_pts' to 'transects' and 'net'")
  }
  
  if (!crosswalk_id %in% names(net)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'net' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks 'net' to 'cs_pts' and 'transects'")
  }
  
  if (!crosswalk_id %in% names(transects)) {
    stop("crosswalk_id '", crosswalk_id, "' is not a column in 'transects' input,\n", 
         "Please provide a valid 'crosswalk_id' that crosswalks 'transects' to 'cs_pts' and 'net'")
  }
  
  # add a "tmp_id" column to easily index transects by hy_id and cs_id 
  transects  <- add_tmp_id(transects, x = crosswalk_id)
  
  # rename geometry column to "geom" 
  cs_pts     <- hydroloom::rename_geometry(cs_pts, "geometry")
  net        <- hydroloom::rename_geometry(net, "geometry")
  transects  <- hydroloom::rename_geometry(transects, "geometry")
  
  # remove any cross sections points that might be missing Z values (i.e. NA Z values)
  cs_pts <- drop_incomplete_cs_pts(cs_pts, crosswalk_id)
  
  if (verbose) { message("Determining points to reevaluate...") }
  
  # add valid_banks and has_relief columns to transects data
  transects <- 
    transects %>% 
    dplyr::left_join(
      dplyr::ungroup(
        dplyr::slice(
          dplyr::group_by(
            dplyr::select(sf::st_drop_geometry(cs_pts), 
                          # hy_id, 
                          dplyr::any_of(crosswalk_id),
                          cs_id, 
                          valid_banks, has_relief),
            # group by the crosswalk_id and cs_id
            dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))
            # hy_id, cs_id
          ), 
          1)
      ),
      by = c(crosswalk_id, "cs_id")
      # by = c("hy_id", "cs_id")
    )
  
  # if there are no transects that need rectification, return the original cs_pts early with a "is_extended" flag
  if (!needs_rectification(transects)) {
    
    cs_pts <- 
      cs_pts %>% 
      dplyr::mutate(
        is_extended = FALSE
      ) %>% 
      dplyr::relocate(geometry, .after = dplyr::last_col())
    
    return(cs_pts)
  }
  
  # 0. Split the data into valid and invalid transects
  # 1. Go through invalid transects
  # 2. Try to EXTEND, 
  # 3. and then UPDATE --> (only IF the extended transect does NOT violate any of the intersection rules)
  # If ALL of the below intersection conditions are TRUE then a given extended transect line will get replace the old transect geometry 
  # Intersection rules: 
  # - Newly extended transect intersects with its flowlines AT MOST 1 time
  # - Newly extended transect does NOT intersect with any of the other NEWLY EXTENDED transect lines
  # - Newly extended transect does NOT intersect with any of the ORIGINAL transect lines
  
  # NOTE: improve_cs_by_extending_invalid_transect_sides() returns the "transects" object with updated attributes for any
  # extensions that were made (geometries, cs_lengthm, "is_extended" flag) and keeps all the rest of the remaining data in place
  extended_transects  <- improve_cs_by_extending_invalid_transect_sides(
    transects_to_check  = transects, 
    net                 = net, 
    crosswalk_id        = crosswalk_id,
    scale               = scale,
    direction           = "both",
    verbose             = verbose
  )
  
  # Remove unextendable transects from extended_geoms 
  extended_transects <- dplyr::filter(extended_transects, is_extended)
  
  # nrow(extended_transects) + nrow(good_to_go_transects) == nrow(transects)
  
  # add cross section points to extended cross sections
  extended_transects <- add_points_per_cs(
    cs             = extended_transects,
    # cs             = trans_to_extend,
    # cs             = dplyr::slice(extended_geoms , 1:100),
    points_per_cs  = points_per_cs,
    min_pts_per_cs = min_pts_per_cs,
    dem            = dem
  )
  
  if (verbose) { message("Extracting new DEM values..")}
  
  # extract DEM values for newly extended cross sections
  extended_pts <- extract_dem_values(
    cs           = extended_transects, 
    crosswalk_id = crosswalk_id, 
    dem          = dem
  )
  
  # remove any cross sections points that might be missing Z values (i.e. NA Z values)
  extended_pts <- drop_incomplete_cs_pts(extended_pts, crosswalk_id)
  
  # Drop the old valid_banks and has_relief columns
  extended_pts <- dplyr::select(extended_pts, 
                                -dplyr::any_of(c("valid_banks", "has_relief"))
                                # -valid_banks, -has_relief
                                )
  
  # add a tmp_id for joining and filtering 
  # extended_pts <- add_tmp_id(extended_pts)
  
  if (verbose) { message("Double checking new extended cross section DEM values for flatness") }
  
  # reclassify the cross sections to look for any improvments in the points bank/relief validity
  reclassified_pts <- classify_points(
    extended_pts, 
    crosswalk_id             = crosswalk_id,
    pct_of_length_for_relief = pct_of_length_for_relief
  )
  
  # add tmp crosswalk_id for convenience
  reclassified_pts <- hydrofabric3D::add_tmp_id(reclassified_pts, x = crosswalk_id)
  
  # Find "validity score" values which just represents a cross sections bank and relief validity as either (0, 1, or 2)
  #  Score 0 = FALSE banks & FALSE relief
  #  Score 1 = Either TRUE banks OR relief
  #  Score 2 = Both TRUE banks & TRUE relief
  # ---> We get this score for the old and the new set of extended cross sections and 
  # then take the points in the new data that showed improvement from the original cross section. 
  # The cross section points that did NOT show improvment remain untouched in the original data
  old_validity_scores <- hydrofabric3D::add_tmp_id(
                                        calc_validity_scores(cs_to_validate = cs_pts, 
                                                            crosswalk_id = crosswalk_id, 
                                                            validity_col_name = "old_validity_score"
                                                            ), 
                                        x = crosswalk_id)
  new_validity_scores <- hydrofabric3D::add_tmp_id(
                                        calc_validity_scores(cs_to_validate = reclassified_pts,   
                                                            crosswalk_id = crosswalk_id, 
                                                            validity_col_name = "new_validity_score"
                                                            ), 
                                        x = crosswalk_id)
  
  # mark as "improved" for any hy_id/cs_ids that increased "validity score" after extending
  check_for_improvement <- dplyr::left_join(
    dplyr::select(
      dplyr::filter(old_validity_scores, tmp_id %in% unique(new_validity_scores$tmp_id)),  
      dplyr::any_of(crosswalk_id), cs_id, old_validity_score
    ), 
    dplyr::select(
      new_validity_scores, 
      dplyr::any_of(crosswalk_id), cs_id, new_validity_score
    ),
    by = c(crosswalk_id, "cs_id")
    # by = c("hy_id", "cs_id")
  ) %>% 
    dplyr::mutate(
      improved = dplyr::case_when(
        new_validity_score > old_validity_score ~ TRUE,
        TRUE                                    ~ FALSE
      )
    ) %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, improved)
  
  # List of unique hy_id/cs_ids (tmp_id) that showed improvement after extension, if valid banks or relief was addded (or both),
  # then the cross section "showed improvement", and the new values will be put into the output cross section dataset
  extended_ids_to_keep <- 
    check_for_improvement %>% 
    dplyr::filter(improved) %>% 
    get_unique_tmp_ids(x = crosswalk_id)
  
  # add a tmp_id for joining and filtering 
  extended_pts <- add_tmp_id(extended_pts, x = crosswalk_id)
  
  # TODO: Left off here to add back and remove old data 03/05/2024
  pts_to_keep <- dplyr::filter(extended_pts, 
                               tmp_id %in% extended_ids_to_keep)
  
  # Reclassify the pts_to_keep so they can be added back to the remaining "good" cross section points from the input
  pts_to_keep             <- classify_points(
    pts_to_keep,
    crosswalk_id = crosswalk_id,
    pct_of_length_for_relief = pct_of_length_for_relief
  )
  
  # add is_extended logical if does not exist
  if (!"is_extended" %in% names(pts_to_keep)) {
    pts_to_keep$is_extended = TRUE
  }
  
  # remove the IDs of newly updated cross section points from the original data, then 
  # bind the new version of these points to the rest of the original data
  final_pts <-
    cs_pts %>%  
    hydrofabric3D::add_tmp_id(x = crosswalk_id) %>% 
    dplyr::filter(
      !tmp_id %in% extended_ids_to_keep
    ) %>% 
    dplyr::mutate(
      is_extended = FALSE
    ) %>% 
    dplyr::bind_rows(
      hydrofabric3D::add_tmp_id(pts_to_keep, x = crosswalk_id)
    ) %>% 
    dplyr::select(-tmp_id) 
  
  # rename geometry column to "geom" 
  final_pts <- hydroloom::rename_geometry(final_pts, "geometry")
 
  # TODO: this should probably be removed and just kept as its own separete function and use outside of this function
  # If TRUE then the cs_ids are renumbered to make sure each hy_id has cross sections
  # that are numbered (1 - number of cross sections) on the hy_id
  if (fix_ids) {
    if (verbose) { message("Renumbering cross section IDs...") }
    final_pts <- renumber_cs_ids(df = final_pts, crosswalk_id = crosswalk_id)
  }
  
  # then move the geometry column to the last column
  final_pts <- move_geometry_to_last(final_pts)
  # final_pts <- dplyr::relocate(final_pts, geom, .after = dplyr::last_col())
  
  return(final_pts)
}

#' @title Extend a set of transects by a percentage based on banks and relief
#' Deprecated 
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
#' @noRd
#' @keywords internal
improve_cs_by_extending_invalid_transect_sides <- function(
    transects_to_check, 
    net, 
    crosswalk_id = NULL,
    scale     = 0.5,
    direction = "both",
    verbose   = TRUE
) {
  
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
  
  # Create an "is_extended" flag to identify which transects were extended and updated 
  transects_to_check$is_extended <- FALSE
  
  # keep track of any transects that having missing values in either valid_banks/has_relief columns, 
  # these get added back to the updated data at the end
  missing_bank_or_relief_data <- 
    transects_to_check %>% 
    dplyr::filter(is.na(valid_banks) | is.na(has_relief))
  
  # TODO: Probably remove this
  count_check <- nrow(dplyr::filter(transects_to_check, valid_banks & has_relief)) + 
    nrow(dplyr::filter(transects_to_check, !valid_banks | !has_relief)) == 
    nrow(transects_to_check) - nrow(missing_bank_or_relief_data)
  
  if(!count_check) {
    warning(paste0(nrow(missing_bank_or_relief_data), " transects have NA values in either 'valid_banks' or 'has_relief' columns"))
  }
  
  # add distances to extend for the left and right side of a transect
  # for any of the the already "valid transects", we just set an extension distance of 0 
  # on both sides and these transects will be KEPT AS IS
  # also set any missing valid_banks or has_relief values to 0
  transects_to_check <- add_attribute_based_extension_distances(transects = transects_to_check, 
                                                                scale = scale, 
                                                                length_col = "cs_lengthm"
  )

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
  
  has_all_uids  <- all(output_uids %in% input_uids)
  
  # throw an error if NOT all hy_id/cs_ids are the same in the input and output data
  if(!has_all_uids) {
    stop("Missing unique hy_id/cs_id from input transects in the output transects")
  }
  
  return(extended_transects)
}

#' @title Makes a summaru dataframe and print out of differences between 2 cross section points dataframes
#' @description
#' Convenience function for printing out the difference between a cross section point dataframe and 
#' the resulting output of putting that dataframe through the rectify_cs() function
#' 
#' @param input_points sf dataframe or dataframe of cross section points
#' @param output_points sf dataframe or dataframe of cross section points, with "is_extended" logical column
#' @param crosswalk_id character, Unique ID column name
#' @param verbose logical, whether to print out summary message/ Default is TRUE
#'
#' @return dataframe
#' @importFrom dplyr select group_by arrange slice ungroup summarize count
#' @importFrom sf st_drop_geometry
#' @noRd
#' @keywords internal
rectify_summary <- function(input_points, 
                            output_points, 
                            crosswalk_id = NULL, 
                            verbose = TRUE) {
  
  # drop geometries
  input_points  <- sf::st_drop_geometry(input_points)  
  output_points <- sf::st_drop_geometry(output_points)  
  
  # change in number of hy_ids
  input_number_of_hyids  <- length(unique(input_points[[crosswalk_id]]))
  output_number_of_hyids <- length(unique(output_points[[crosswalk_id]]))
  number_removed_hyids   <- input_number_of_hyids - output_number_of_hyids
  
  # number of rows 
  number_input_rows      <- nrow(input_points)
  number_output_rows     <- nrow(output_points)
  diff_in_row_number     <- number_input_rows - number_output_rows
  
  change_in_rows_string  <- ifelse(diff_in_row_number >= 0, 
                                   paste0(abs(diff_in_row_number), " rows were removed"), 
                                   paste0(abs(diff_in_row_number), " rows were added")
  )

  # Average points per cross section
  input_pts_per_cs <- 
    input_points %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::arrange(-pt_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarize(avg_pts_per_cs = round(mean(pt_id), 2)) %>% 
    .$avg_pts_per_cs
  
  output_pts_per_cs <- 
    output_points %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::arrange(-pt_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarize(avg_pts_per_cs = round(mean(pt_id), 2)) %>% 
    .$avg_pts_per_cs
  
  # Extensions counts
  output_extended_counts <-  
    output_points %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::count(is_extended)

  is_extended_count      <- output_extended_counts$n[output_extended_counts$is_extended == TRUE]
  is_not_extended_count  <- output_extended_counts$n[output_extended_counts$is_extended == FALSE]
  
  if(verbose) {
    message("------ Rectification summary stats ------")
    message("Change in number of 'hy_ids':")
    message(paste0("  - Start number of 'hy_ids': ", input_number_of_hyids))
    message(paste0("  - End number of 'hy_ids': ", output_number_of_hyids))
    message(paste0("  - Number removed 'hy_ids': ", number_removed_hyids))
    message("Change in number of rows:")
    message(paste0("  - Starting number of rows: ", number_input_rows))
    message(paste0("  - Ending number of rows: ", number_output_rows))
    message(paste0("     > ", change_in_rows_string))
    message("Average points per cross section:")
    message(paste0("  - Starting average points per cross section: ", input_pts_per_cs))
    message(paste0("  - Ending average points per cross section: ", output_pts_per_cs))
    message("Extensions counts:")
    message(paste0("  - Number of extended cross sections: ", is_extended_count, " / ", is_extended_count + is_not_extended_count))
    message(paste0("  - Number of non-extended cross sections: ", is_not_extended_count, " / ", is_extended_count + is_not_extended_count))
    message("-----------------------------------------")
  }
  
  # Create dataframe
  summary_df <- data.frame(
    metric = c("Input number of hy_ids", "Output number of hy_ids", "Number removed hy_ids",
               "Number of input rows", "Number of output rows", "Change in row number",
               "Average input points per cross section", "Average output points per cross section",
               "Count of extended points", "Count of non-extended points"),
    value = c(input_number_of_hyids, output_number_of_hyids, number_removed_hyids,
              number_input_rows, number_output_rows, change_in_rows_string,
              input_pts_per_cs, output_pts_per_cs,
              is_extended_count, is_not_extended_count)
  )
  
  return(summary_df)
}

#' Check if there transects without valid banks or relief
#'
#' @param transects sf linestring with "valid_banks" and "has_relief" logical columns
#'
#' @return logical, TRUE if there are transects without valid banks or relief
#' @importFrom dplyr mutate case_when filter select
#' @importFrom sf st_drop_geometry
#' @noRd
#' @keywords internal
needs_rectification <- function(transects) {
  
  lines_to_inspect_counts <-
    transects %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(
      needs_rectification = dplyr::case_when(
        !valid_banks | !has_relief  ~ TRUE,
        TRUE                        ~ FALSE
      )
    ) %>% 
    dplyr::select(needs_rectification ) %>% 
    table() %>% 
    as.data.frame()  %>% 
    dplyr::mutate(needs_rectification  = as.logical(needs_rectification)) 
  
  has_transects_to_rectify <- ifelse(nrow(dplyr::filter(lines_to_inspect_counts, needs_rectification )) == 0, FALSE, TRUE)
  
  return(has_transects_to_rectify)
  
}

#' Add a "needs_rectification" column to a sf/dataframe
#'
#' @param transects sf linestring with "valid_banks" and "has_relief" logical columns
#'
#' @return logical, TRUE if there are transects without valid banks or relief
#' @importFrom dplyr mutate case_when filter select
#' @importFrom sf st_drop_geometry
#' @noRd
#' @keywords internal
add_needs_rectification <- function(transects) {
  
  transects <-
    transects %>% 
    dplyr::mutate(
      needs_rectification = dplyr::case_when(
        !valid_banks | !has_relief  ~ TRUE,
        TRUE                        ~ FALSE
      )
    )
  
  return(transects)
  
}

#' Calculate percentage of points within a set of cross section points that are near the bottom of the cross section 
#' Adds the following columns: 
#' is_near_bottom: state whether a point is near the bottom of the cross section (within a specified distance threshold of the bottom), 
#' pts_near_bottom: count of points near the bottom of the cross section
#' pct_near_bottom: percent of points near the bottom of the cross section
#' @param cs_pts sf dataframe of cross section points (output of cross_section_pts() function)
#' @param distance_from_bottom numeric, distance threshold (in meters) to determine if a point is near the bottom of the cross section
#' @param look_only_above_bottom logical, whether to look only at points ABOVE the channel bottom as points that can be classified as "near bottom". 
# Default is TRUE, meaning only points that are between Z and Z + distance_from_bottom are classified as "near bottom" 
# If FALSE, then points at Z values BELOW the bottom (Z - distance_from_bottom) AND 
# points at Z values ABOVE the bottom (Z + distance_from_bottom) are classified as 
# "near bottom" if they are within the range BELOW OR ABOVE the bottom.
#' @param total_from_bottom_up logical, whether to use only points ABOVE bottom points as part of total points for calculating percentage of points near bottom. Default is FALSE and ALL points will be used when calculating percentage, even if a point has a Z value BELOW the bottom, but is NOT classified as a bottom point
#' @importFrom dplyr group_by mutate ungroup relocate filter summarize left_join between case_when select all_of last_col
#' @importFrom sf st_drop_geometry
#' @return sf dataframe of cross section points with the added columns described above 
#' @noRd
#' @keywords internal
pct_pts_near_bottom = function(cs_pts, 
                               distance_from_bottom    = 1, 
                               look_only_above_bottom  = TRUE,
                               total_from_bottom_up = FALSE
) {
  
  # Drop geometries to work with tabular data only
  flat_check <- 
    cs_pts  %>% 
    sf::st_drop_geometry()
  
  # classify cross section points and add back point count per cross section column
  flat_check <- 
    flat_check %>% 
    hydrofabric3D::classify_points()  %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      points_per_cs = dplyr::n()
    ) %>% 
    dplyr::ungroup() %>% 
    sf::st_drop_geometry() 
  
  # reorder columns
  flat_check <- dplyr::relocate(flat_check, 
                                hy_id, cs_id, pt_id, Z, class, points_per_cs)
  
  # get the minimum Z value of the bottom points of each cross section and add this as a column to cs_pts
  bottomZ = 
    flat_check  %>% 
    dplyr::group_by(hy_id, cs_id) %>%
    dplyr::filter(class == "bottom") %>%
    dplyr::summarize(
      Z_at_bottom = min(Z)
    )  %>% 
    dplyr::ungroup() 
  
  # join the flat_check dataframe with the dataframe containing the Z values of the bottom depths for each cross section
  bottom_pct =
    flat_check  %>% 
    dplyr::left_join(
      bottomZ,
      by = c("hy_id", "cs_id")
    ) 
  
  # TODO: This code could be shortened and combined with the ELSE clause, just being lazy right now
  if(total_from_bottom_up) {
    # When calculating the percentage, use only points that are GREATER THAN OR EQUAL to the bottom Z as part of percentage calculation.
    bottom_pct <- 
      bottom_pct %>%
      dplyr::group_by(hy_id, cs_id) %>% 
      dplyr::mutate(
        lower_bound    = ifelse(look_only_above_bottom, Z_at_bottom, Z_at_bottom - distance_from_bottom),
        upper_bound    = Z_at_bottom + distance_from_bottom,
        is_near_bottom = dplyr::between(
          Z,
          lower_bound,
          upper_bound
        ),
        ge_bottom   = dplyr::case_when(
          Z     >= Z_at_bottom ~ TRUE,
          TRUE                 ~ FALSE
        ),
        total_valid_pts = sum(ge_bottom),
        pts_near_bottom = sum(is_near_bottom),
        pct_near_bottom = pts_near_bottom/total_valid_pts,
        tmp_id          = paste0(hy_id, "_", cs_id, "_", pt_id)
      )  %>% 
      dplyr::ungroup()  %>% 
      dplyr::select(tmp_id, class, Z_at_bottom, is_near_bottom, pts_near_bottom, pct_near_bottom, lower_bound, upper_bound, total_valid_pts) 
  } else {
    
    # Given the Z value of each point, and the Z value of the bottom points ("Z_at_bottom"),
    #  determine if each point is near the bottom 
    # If the Z value for a given point is between the lower_bound and upper_bound, then the the point is determined to be "is_near_bottom"
    # If look_only_above_bottom is TRUE, then the lower_bound is the Z value at the bottom points (Z_at_bottom), otherwise 
    # If look_only_above_bottom is FALSE, then the lower_bound is the Z value at the bottom points (Z_at_bottom) minus distance_from_bottom (Z_at_bottom - distance_from_bottom)
    bottom_pct <- 
      bottom_pct %>%
      dplyr::group_by(hy_id, cs_id) %>%
      dplyr::mutate(
        lower_bound    = ifelse(look_only_above_bottom, Z_at_bottom, Z_at_bottom - distance_from_bottom),
        upper_bound    = Z_at_bottom + distance_from_bottom,
        is_near_bottom = dplyr::between(
          Z,
          lower_bound,
          upper_bound
        ),
        pts_near_bottom = sum(is_near_bottom),
        pct_near_bottom = pts_near_bottom/points_per_cs,
        tmp_id          = paste0(hy_id, "_", cs_id, "_", pt_id)
      )  %>% 
      dplyr::ungroup()  %>% 
      dplyr::select(
        tmp_id, class, Z_at_bottom,
        is_near_bottom, pts_near_bottom, pct_near_bottom, 
        lower_bound, upper_bound, 
        total_valid_pts = points_per_cs
      ) 
  }
  
  # join bottom points percent table to cs_pts
  cs_pts <- dplyr::left_join(
    dplyr::mutate(
      cs_pts,
      tmp_id = paste0(hy_id, "_", cs_id, "_", pt_id)
    ),
    bottom_pct,
    by = "tmp_id"
  )  %>% 
    dplyr::select(-tmp_id)
  
  # get the sf geometryt column name
  geometry_colname <- names(cs_pts)[sapply(cs_pts, function(col) any( 
    class(col) %in% c("sfc_POINT", "sfc", 
                      "sfc_GEOMETRY", "sfc_MULTIPOINT")))
  ]
  
  # move the geometry column to the end of the dataframe
  cs_pts <- 
    cs_pts %>% 
    dplyr::relocate(dplyr::all_of(geometry_colname), .after = dplyr::last_col())
  
  return(cs_pts)
  
}

#' Get a dataframe of points that should be evaluated due to their proximity (nearness in Z distance) to the bottom
#'
#' @param cs_pts dataframe/sf dataframe of cross section points (requires hy_id, cs_id, and Z values)
#' @param threshold numeric, threshold distance in meters for points to be considered "near the bottom". Default is 1 meter (i.e. check if points are within 1 meter above the bottom)
#' @param pct_threshold numeric, threshold percent of points in the cross section that are within threshold of bottom to 
#' determine whether point should be considered for re evaluation. Default is 0.99 (i.e. 99% of points are near the bottom). Default is 0.99 (i.e. 99&%).
#'
#' @return dataframe with the hy_id, cs_id, pts_near_bottom (count of pts_near_bottom), and pct_near_bottom (% of points in cross section that are near bottom). 
#' An empty dataframe is returned if ZERO points are classified as "near the bottom"
#' @importFrom dplyr mutate filter select group_by slice ungroup
#' @importFrom sf st_drop_geometry
#' @noRd
#' @keywords internal
pts_to_reevaluate <- function(
    cs_pts, 
    threshold = 1, 
    pct_threshold = 0.99
) {
  
  # Determine which points that are within "threshold" meters from the bottom 
  # (only looking at points above threshold, ignoring any points that are BELOW Z)
  # So the "pct_pts_near_bottom()" function adds columns to the "cs_pts" input data that detail which points are "near" the bottom points.
  # "bottom" points are classified via hydrofabric3D::classify_pts()
  near_bottom <-
    cs_pts %>% 
    pct_pts_near_bottom(
      distance_from_bottom   = threshold, 
      look_only_above_bottom = TRUE,
      total_from_bottom_up   = FALSE
    )
  
  # Determine which points should be re evaluated (by extending) because most of the points are all "near the bottom"
  # Filter the "near_bottom" dataframe to only cross sections that 
  # have a percent of all of the cross sections points that are GREATER THAN OR EQUAL to "pct_threshold"
  
  # In simple words, get the cross sections that have, for example, 80% of its points that are "near the bottom" 
  
  # Also filter cross sections that have only a SINGLE point that is NOT near the bottom:
  # -----> So if a cross section has 9/10 of its points near the bottom, 
  #         that means only a single point is NOT near the bottom and thus 
  #         that cross section should be kept for FURTHER EVALUATION
  near_bottom <- 
    near_bottom %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(
      diff_pts = total_valid_pts - pts_near_bottom
    ) %>% 
    dplyr::filter(pct_near_bottom >= pct_threshold | diff_pts == 1) %>%
    dplyr::select(-diff_pts) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() 
  
  return(near_bottom)
  
}

#' Helper function for taking extended cross section points and extending transects to match CS points
#'
#' @param transect_lines sf dataframe
#' @param fixed_cs_pts sf dataframe
#' @param crosswalk_id character
#' @importFrom sf st_drop_geometry 
#' @importFrom hydroloom rename_geometry
#' @importFrom dplyr filter left_join select any_of distinct mutate bind_rows ungroup group_by slice across
#' @return transect lines sf dataframe extended to match CS points
#' @noRd
#' @keywords internal
match_transects_to_extended_cs_pts <- function(transect_lines, 
                                               fixed_cs_pts,
                                               crosswalk_id
                                               ) {
  
  .data <- NULL
  
  fixed_cs_pts      <- hydroloom::rename_geometry(fixed_cs_pts, "geometry")
  transect_lines    <- hydroloom::rename_geometry(transect_lines, "geometry")
  
  # get the counts of each point type to add this data to the transect_lines dataset
  point_type_counts <- get_point_type_counts(classified_pts = fixed_cs_pts, 
                                                            crosswalk_id = crosswalk_id)
  # Check the number of cross sections that were extended
  message("Subsetting cross section points generated after extending transect_lines...")
  
  # extract cross section points that have an "is_extended" value of TRUE
  extended_pts <- 
    fixed_cs_pts %>%
    dplyr::filter(is_extended) %>%
    add_tmp_id(x = crosswalk_id)
  
  # extract transect_lines that have a "crosswalk_id" in the "extended_pts" dataset
  update_transect_lines <- 
    transect_lines %>%
    add_tmp_id(x = crosswalk_id) %>%
    dplyr::filter(tmp_id %in% unique(extended_pts$tmp_id))
  
  update_transect_lines <- 
    update_transect_lines %>% 
    dplyr::left_join(
        extended_pts %>% 
        sf::st_drop_geometry() %>% 
        dplyr::select(dplyr::any_of(crosswalk_id), cs_id, extended_length = cs_lengthm) %>% 
        dplyr::distinct(.data[[crosswalk_id]], cs_id, .keep_all = TRUE),
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::mutate(
      distance_to_extend = (extended_length - cs_lengthm) / 2
    ) %>% 
    dplyr::select(-extended_length)
  
  
  cs_pt_uids    <- unique(add_tmp_id(fixed_cs_pts, x = crosswalk_id)$tmp_id)
  
  # If any transect_lines were extended, update the transect_lines dataset, and overwrite local and S3 transect_lines geopackages
  if (nrow(update_transect_lines) > 0) {
    
    message("Updating ", nrow(update_transect_lines), " transect_lines")
    
    update_transect_lines <- 
      update_transect_lines %>%
      # apply extend_by_percent function to each transect line:
      extend_by_length(
        crosswalk_id  = crosswalk_id,
        length_vector = update_transect_lines$distance_to_extend
      ) %>% 
      dplyr::select(-distance_to_extend)
    
    update_transect_lines <- hydroloom::rename_geometry(update_transect_lines, "geometry")

    # Filter down to ONLY points that were finalized and rectified from rectify_cs_pts()
    # Remove old transect_lines that have "tmp_id" in "extended_pts" (transect_lines that were unchanged and are "good_to_go")
    # and then replace with old transect_lines with the "update_transect_lines"
    out_transect_lines <-
      transect_lines %>%
      hydrofabric3D::add_tmp_id(x = crosswalk_id) %>%
      dplyr::filter(tmp_id %in% cs_pt_uids) %>% 
      dplyr::filter(!tmp_id %in% unique(extended_pts$tmp_id)) %>%
      dplyr::bind_rows(
        dplyr::mutate(update_transect_lines, is_extended = TRUE)
      )
    
  } else {
    # If no transect_lines were extended
    out_transect_lines <- 
      transect_lines %>%
      hydrofabric3D::add_tmp_id(x = crosswalk_id) %>%
      dplyr::filter(tmp_id %in% cs_pt_uids) %>% 
      dplyr::filter(!tmp_id %in% unique(extended_pts$tmp_id))
  }

  # Finalize new transect_lines
  out_transect_lines <- 
    out_transect_lines %>%
    dplyr::left_join(
      point_type_counts, 
      by = c(crosswalk_id, "cs_id")
    ) %>%
    dplyr::left_join(
      dplyr::ungroup(
        dplyr::slice(
          dplyr::group_by(
            dplyr::select(sf::st_drop_geometry(fixed_cs_pts),
                          dplyr::any_of(crosswalk_id), 
                          cs_id, bottom, left_bank, right_bank, valid_banks, has_relief
            ),
            dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))
          ),
          1
        )
      ),
      by = c(crosswalk_id, "cs_id")
    ) %>%
    dplyr::select(
      dplyr::any_of(crosswalk_id),
      cs_source, cs_id, cs_measure, cs_lengthm,
      is_extended,
      left_bank_count, right_bank_count, channel_count, bottom_count,
      bottom, left_bank, right_bank, valid_banks, has_relief,
      geometry
    ) %>% 
    dplyr::mutate(
      is_extended = ifelse(is.na(is_extended), FALSE, is_extended)
    )  
  
  has_same_ids <- has_same_unique_tmp_ids(x = out_transect_lines, 
                                          y = fixed_cs_pts, 
                                          crosswalk_id = crosswalk_id
                                          )
  
  if(!has_same_ids) {
    stop("Mismatch / Missing ", crosswalk_id, "/cs_id combinations in either input 'transect_lines' and input 'fixed_cs_pts'")
  }
  
  return(out_transect_lines)
}
