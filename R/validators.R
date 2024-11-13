
validate_transects_self_intersections <- function(transects) {
  return(
    nrow(transects) == nrow(rm_self_intersections(transects))
  )
}

validate_transects_cs_id_enumeration <- function(transects, crosswalk_id = NULL) {
  
  # reenumerate the cs_ids for each transect based on cs_measure sorting, and make sure all cross sections are correctly numbered
  mismatches <-
    transects %>%
    sf::st_drop_geometry() %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id)))) %>% 
    dplyr::arrange(cs_measure, .by_group = TRUE) %>% 
    dplyr::mutate(
      new_cs_id = 1:dplyr::n()
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(cs_id != new_cs_id)
  
  # FALSE if there are any transects with different cs_ids to the newly created cs_id 
  # Otherwise TRUE
  has_valid_cs_ids <- !(nrow(mismatches) > 0)
  
  return(
    has_valid_cs_ids
  )
  
}

validate_transects_cs_length <- function(transects, crosswalk_id = NULL) {
  
  # re calculate transect geometry length and compare to cs_lengthm column
  wrong_lengths <-
    transects %>%
    dplyr::mutate(
      new_cs_length = as.numeric(sf::st_length(.)) 
    ) %>% 
    dplyr::filter(cs_lengthm != new_cs_length)
  
  # FALSE if there are any transects with different cs_lengthm than the freshly calculated new_cs_length
  has_correct_lengths <- !(nrow(wrong_lengths) > 0)
  
  return(
    has_correct_lengths
  )
  
}

validate_transects_unique_ids <- function(transects, crosswalk_id = NULL) {
  
  duplicate_ids <- 
    transects %>%
    sf::st_drop_geometry() %>% 
    add_tmp_id(x = crosswalk_id) %>% 
    dplyr::select(tmp_id) %>% 
    dplyr::group_by(tmp_id) %>% 
    dplyr::count() %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(n > 1)
  
  # FALSE if there are ANY rows in the duplicate_ids dataframe above 
  # (i.e. a count of greater than 1 for any tmp_id (<crosswalk_id>_cs_id))
  has_unique_ids <- !(nrow(duplicate_ids) > 0)
  
  return(
    has_unique_ids
  )
  
}

validate_transects_cs_measure <- function(transects) {
  
  min_cs_measure <-
    transects %>%
    sf::st_drop_geometry() %>% 
    dplyr::pull(cs_measure) %>% 
    min()
  
  max_cs_measure <-
    transects %>%
    sf::st_drop_geometry() %>% 
    dplyr::pull(cs_measure) %>% 
    max()
  
  # cs_measure should always be:
  # greater than or equal to 0 AND 
  # less than or equal to 100 as its a percentage along a flowline
  has_valid_cs_measure <- min_cs_measure >= 0 & max_cs_measure <= 100
  
  return(
    has_valid_cs_measure
  )
  
}

validate_transects_has_complete_geometries <- function(transects, crosswalk_id = NULL) {
  
  has_empty_geoms <- 
    transects %>% 
    sf::st_is_empty() %>% 
    any()
  
  return(
    !has_empty_geoms
  )
  
}

validate_transects_has_crs <- function(transects) {
  
  missing_crs <- 
    transects %>% 
    sf::st_crs() %>% 
    is.na()
  
  return(
    !missing_crs
  )
  
}

validate_transects <- function(transects, 
                               crosswalk_id = NULL
) {
  
  # # standardize geometry name
  # transects <- hydroloom::rename_geometry(transects, "geometry")
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "cs_source", "cs_measure", "cs_lengthm", "geometry")
  
  # validate dataframe has all correct columns  
  has_all_valid_cols         <- validate_df(
    x = transects, 
    cols = REQUIRED_COLS,
    obj_name = "transects"
  )  
  
  # Validate every flowline (id) has a cs_id of 1:number of transects
  has_valid_cs_ids           <- validate_transects_cs_id_enumeration(transects, crosswalk_id = crosswalk_id)
  
  # Validate there are no self intersections
  has_no_self_intersections  <- validate_transects_self_intersections(transects)
  
  # validate the cs_lengthm column equals the actual transect geometry length
  has_correct_lengths        <- validate_transects_cs_length(transects, crosswalk_id = crosswalk_id)
  
  # validate no duplicate id / cs_id combos
  has_unique_ids             <- validate_transects_unique_ids(transects, crosswalk_id = crosswalk_id)
  
  # validate cs measure is never greater than 100 (i think)
  has_valid_cs_measure       <- validate_transects_cs_measure(transects)
  
  # make sure transects have no empty geometries 
  has_complete_geoemetries   <- validate_transects_has_complete_geometries(transects)
  
  # make sure transects have a CRS
  has_crs                    <- validate_transects_has_crs(transects)
  
  # if everything is TRUE, return true, otherwise return FALSE (or throw an error...?)
  is_validated_transects <- all(
    c(
      has_all_valid_cols,
      has_valid_cs_ids,
      has_no_self_intersections,
      has_correct_lengths,
      has_unique_ids,
      has_valid_cs_measure,
      has_complete_geoemetries,
      has_crs
    )
  )
  
  return(is_validated_transects)
  
}

# all ids in transects are in flowlines
validate_transects_ids_in_flowlines <- function(transects, flowlines, crosswalk_id = NULL) {
  
  # flowlines <- flines
  
  transect_ids <- 
    transects %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id)) %>% 
    dplyr::pull(dplyr::any_of(crosswalk_id)) %>% 
    unique()
  
  flowline_ids <-
    flowlines %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id)) %>% 
    dplyr::pull(dplyr::any_of(crosswalk_id)) %>% 
    unique()
  
  all_transect_ids_in_flowline_ids <- all(transect_ids %in% flowline_ids)
  
  return(
    all_transect_ids_in_flowline_ids
  )
  
}

# make sure no transect crosses more than a single flowline, a single time
validate_transects_flowline_intersections <- function(transects, flowlines) {
  
  # flowlines <- flines
  return (
    nrow(transects) == nrow(hydrofabric3D:::rm_multiflowline_intersections(transects, flowlines)) 
  )
}

# validate 2 SF objects have the same CRS
validate_same_crs <- function(x, y) {
  
  return (
    sf::st_crs(x) == sf::st_crs(y)
  )
  
}

validate_transects_against_flowlines <- function(transects, 
                                                 flowlines,  
                                                 crosswalk_id = NULL
) {
  
  # # standardize geometry name
  # transects <- hydroloom::rename_geometry(transects, "geometry")
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "cs_source", "cs_measure", "cs_lengthm", "geometry")
  
  # validate dataframe has all correct columns  
  has_all_valid_cols         <- hydrofabric3D:::validate_df(
    x = transects, 
    cols = REQUIRED_COLS,
    obj_name = "transects"
  )  
  
  # all ids in transects are in flowlines
  all_transect_ids_in_flowline_ids  <- validate_transects_ids_in_flowlines(transects, flowlines, crosswalk_id = crosswalk_id)
  
  # transects only intersects a single flowline, a single time
  has_valid_flowline_intersects     <- validate_transects_flowline_intersections(transects, flowlines)
  
  # transects and flowlines have the same CRS
  has_same_crs                      <- validate_same_crs(transects, flowlines)
  
  
  # if everything is TRUE, return true, otherwise return FALSE (or throw an error...?)
  is_flowline_validated_transects <- all(
    c(
      has_all_valid_cols,
      all_transect_ids_in_flowline_ids,
      has_valid_flowline_intersects,
      has_same_crs
    )
  )
  
  return(is_flowline_validated_transects)
  
}

validate_cs_pts_cs_id_enumeration <- function(cs_pts, crosswalk_id = NULL) {
  
  # reenumerate the cs_ids for each transect based on cs_measure sorting, and make sure all cross sections are correctly numbered
  mismatches <-
    cs_pts %>%
    # dplyr::slice(1:150) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id)))) %>% 
    dplyr::arrange(cs_id, .by_group = TRUE) %>% 
    dplyr::mutate(
      new_cs_id = 1:dplyr::n()
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(cs_id != new_cs_id)
  
  # FALSE if there are any transects with different cs_ids to the newly created cs_id 
  # Otherwise TRUE
  has_valid_cs_ids <- !(nrow(mismatches) > 0)
  
  return(
    has_valid_cs_ids
  )
  
}

validate_cs_pts_pt_id_enumeration <- function(cs_pts, crosswalk_id = NULL) {
  
  # reenumerate the pt_ids to make sure the pt_ids are valid values of 1:number of points in cross section
  mismatches <-
    cs_pts %>%
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::arrange(pt_id, .by_group = TRUE) %>% 
    dplyr::mutate(
      new_pt_id = 1:dplyr::n()
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(pt_id != new_pt_id)
  
  # FALSE if there are any cs_pts with  pt_ids different from the newly created new_pt_id 
  # Otherwise TRUE
  has_valid_pt_ids <- !(nrow(mismatches) > 0)
  
  return(
    has_valid_pt_ids
  )
  
}

validate_cs_pts_relative_distance <- function(cs_pts, crosswalk_id = NULL) {
  
  # make sure relative distance is greater than or equal to 0
  min_relative_distance <-
    cs_pts %>%
    # dplyr::slice(1:50) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::pull(relative_distance) %>% 
    min()
  
  
  # reenumerate the pt_ids to make sure the pt_ids are valid values of 1:number of points in cross section
  rel_dist_check <-
    cs_pts %>%
    # dplyr::slice(1:50) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, pt_id, relative_distance, cs_lengthm) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::summarise(
      cs_lengthm = max(cs_lengthm),
      # min_rel_dist = min(relative_distance),
      max_rel_dist = max(relative_distance)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # TODO: as long as the lengths are within 1 meter, thats equal
      is_valid_relative_dist = abs(cs_lengthm - max_rel_dist) <= 1
      # approx_equal_lengths = all.equal(cs_lengthm, cs_pts_lengthm, tolerance = 0.01)
    ) 
  
  # dplyr::filter(max_rel_dist > cs_lengthm)
  
  # relative distance is always greater than or equal to 0 and less than the cross sections length
  has_valid_relative_dist_min        <- min_relative_distance >= 0
  has_valid_relative_dist_maximums   <- all(rel_dist_check$is_valid_relative_dist)
  
  return(
    has_valid_relative_dist_min && has_valid_relative_dist_maximums
  )
  
}

validate_cs_pts_point_types <- function(cs_pts) {
  
  # make sure only "left_bank", "right_bank", "channel", and "bottom" values exist in cs_pts point_type column
  valid_point_types  <- c("left_bank", "right_bank", "channel", "bottom")
  
  # unique point types in cs_pts
  unique_point_types <- unique(cs_pts$point_type)
  
  has_only_valid_point_types <- all(unique_point_types %in% valid_point_types)
  
  return(
    has_only_valid_point_types
  )
  
}

validate_cs_pts <- function(
    cs_pts,  
    crosswalk_id = NULL
) {
  # cs_pts <- final_cs_pts
  # # standardize geometry name
  # transects <- hydroloom::rename_geometry(transects, "geometry")
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", 
                     "relative_distance", "cs_lengthm", "X", "Y", "Z", "Z_source",
                     "class", "point_type", "valid_banks", "has_relief"
  )
  
  # validate dataframe has all correct columns  
  has_all_valid_cols         <- hydrofabric3D:::validate_df(
    x = cs_pts, 
    cols = REQUIRED_COLS,
    obj_name = "cs_pts"
  )  
  
  # make sure valid cs_ids
  has_valid_cs_pts_cs_ids       <- validate_cs_pts_cs_id_enumeration(cs_pts, crosswalk_id = crosswalk_id)
  
  # make sure valid pt_ids
  has_valid_cs_pts_pt_ids       <- validate_cs_pts_pt_id_enumeration(cs_pts, crosswalk_id = crosswalk_id)
  
  # check cs_pts have only valid relative_distance values
  has_valid_relative_distances  <- validate_cs_pts_relative_distance(cs_pts, crosswalk_id = crosswalk_id)
  
  has_valid_point_types         <- validate_cs_pts_point_types(cs_pts)
  
  # if everything is TRUE, return true, otherwise return FALSE (or throw an error...?)
  is_validated_cs_pts <- all(
    c(
      has_all_valid_cols,
      has_valid_cs_pts_cs_ids,
      has_valid_cs_pts_pt_ids,
      has_valid_relative_distances,
      has_valid_point_types
    )
  )
  
  return(is_validated_cs_pts)
  
}

# validate all cs_pts id/cs_ids are in the transects
validate_cs_pt_ids_in_transects <- function(cs_pts, transects, crosswalk_id = NULL) {
  
  # flowlines <- flines
  cs_pts_ids <-
    cs_pts %>% 
    sf::st_drop_geometry() %>% 
    hydrofabric3D::get_unique_tmp_ids(x = crosswalk_id)
  
  transect_ids <-
    transects %>% 
    sf::st_drop_geometry() %>% 
    hydrofabric3D::get_unique_tmp_ids(x = crosswalk_id)
  
  
  all_cs_pts_ids_in_transects <- all(cs_pts_ids %in% transect_ids)
  all_transect_ids_in_cs_pts  <- all(transect_ids %in% cs_pts_ids)
  same_number_of_ids          <- length(cs_pts_ids) == length(transect_ids)  
  
  is_valid_cs_pts_ids <- all(
    c(
      all_cs_pts_ids_in_transects,
      all_transect_ids_in_cs_pts,
      same_number_of_ids
    )
  )
  
  return(
    is_valid_cs_pts_ids
  )
  
}

validate_cs_pts_length_against_transects <- function(cs_pts, transects, crosswalk_id = NULL) {
  
  cs_pt_lengths <-
    cs_pts %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, cs_lengthm) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(
      cs_pts_lengthm = cs_lengthm
    )
  
  transect_lengths <-
    transects %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, cs_lengthm) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup()
  
  lengths_check <- 
    dplyr::left_join(
      transect_lengths,
      cs_pt_lengths,
      by = c(crosswalk_id, "cs_id")
    ) %>% 
    dplyr::mutate(
      # TODO: as long as the lengths are within 1 meter, thats equal
      approx_equal_lengths = abs(cs_lengthm - cs_pts_lengthm) <= 1
      # approx_equal_lengths = all.equal(cs_lengthm, cs_pts_lengthm, tolerance = 0.01)
    ) 
  
  all_lengths_are_equal <- all(lengths_check$approx_equal_lengths)
  
  return(
    all_lengths_are_equal
  )
  
}

validate_cs_pts_against_transects <- function(
    cs_pts,  
    transects, 
    crosswalk_id = NULL
) {
  
  # # standardize geometry name
  # transects <- hydroloom::rename_geometry(transects, "geometry")
  
  # make sure all id/cs_id combos are in both transects and cs_pts
  has_valid_cs_pts_ids  <- validate_cs_pt_ids_in_transects(cs_pts, transects, crosswalk_id = crosswalk_id)
  
  # make sure cs_lengthm matches from transects to cs_pts
  has_matching_lengths  <- validate_cs_pts_length_against_transects(cs_pts, transects, crosswalk_id = crosswalk_id)
  
  # if everything is TRUE, return true, otherwise return FALSE (or throw an error...?)
  is_transect_validated_cs_pts <- all(
    c(
      has_valid_cs_pts_ids,
      has_matching_lengths
    )
  )
  
  return(is_transect_validated_cs_pts)
  
}