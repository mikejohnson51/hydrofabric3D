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

# TODO: Finalize version 3 that extends from BOTH directions instead of individually extending LEFT and RIGHT 

#' @title Check and fix cross section points with limited variation in Z values (without removing any flowlines)
#' @description Duplicate process as rectify_cs() but does NOT remove any cross sections, only attempts to extend transects and improve cross sections. This function takes in a set of cross section points (cs_pts), a flowline network (net) and a set of transects lines for that flowline network (transects).
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
#' @importFrom nhdplusTools rename_geometry
#' @return sf object of cross section points based on extended transects to try and improve the number of points with "valid_banks" and "has_relief"
#' @export
get_improved_cs_pts = function(
    cs_pts         = NULL,   
    net            = NULL,
    transects      = NULL,
    crosswalk_id   = NULL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
    scale          = 0.5,
    pct_of_length_for_relief = 0.01,
    fix_ids        = FALSE,
    verbose        = TRUE
) {
  # ----------------------------------------
  
  # library(sf)
  # library(dplyr)
  # library(geos)
  # library(terra)
  # 
  # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # net <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg") %>%
  #   dplyr::rename(hy_id = id)
  # # flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
  # transects <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_transects_06.gpkg")
  # 
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
  # scale          = 0.5
  # pct_of_length_for_relief = 0.01
  # fix_ids        = FALSE
  # crosswalk_id = "hy_id"
  # verbose        = TRUE
  # devtools::load_all()
  # sf::write_sf(cs_pts, "/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # sf::write_sf(flines, "/Users/anguswatters/Desktop/test_improve_flines.gpkg")
  # sf::write_sf(transects, "/Users/anguswatters/Desktop/test_improve_transects.gpkg")
  
  # ----------------------------------------
  
  # make a unique ID if one is not given (NULL 'id')
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
  transects <- hydrofabric3D::add_tmp_id(transects, x = get(crosswalk_id))
  
  # rename geometry column to "geom" 
  cs_pts     <- nhdplusTools::rename_geometry(cs_pts, "geometry")
  net        <- nhdplusTools::rename_geometry(net, "geometry")
  transects  <- nhdplusTools::rename_geometry(transects, "geometry")
  # transects <- nhdplusTools::rename_geometry(transects, "geom")
  
  ### ### ## ## ### ## ### ##  ### ### ## ## ### ## ### ##
  
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
  
  # system.time({
  
  # NOTE: extend_invalid_transect_sides() returns the "transects" object with updated attributes for any
  # extensions that were made (geometries, cs_lengthm, "is_extended" flag) and keeps all the rest of the remaining data in place
  extended_transects  <- extend_invalid_transect_sides(
    transects_to_check  = transects, 
    net                 = net, 
    crosswalk_id        = crosswalk_id,
    scale               = scale,
    direction           = "both",
    verbose             = verbose
  )
  
  # })
  
  # all(hydrofabric3D::add_tmp_id(extended_transects)$tmp_id %in% hydrofabric3D::add_tmp_id(transects)$tmp_id)
  # all(hydrofabric3D::add_tmp_id(extended_transects)$tmp_id %in% hydrofabric3D::add_tmp_id(extended_transects2)$tmp_id)
  
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
  
  # Drop the old valid_banks and has_relief columns
  extended_pts <- dplyr::select(extended_pts, -valid_banks, -has_relief)
  
  # add a tmp_id for joining and filtering 
  # extended_pts <- add_tmp_id(extended_pts)
  
  if (verbose) { message("Double checking new extended cross section DEM values for flatness") }
  
  # reclassify the cross sections to look for any improvments in the points bank/relief validity
  reclassified_pts <- classify_points(
    extended_pts, 
    crosswalk_id             = crosswalk_id,
    pct_of_length_for_relief = pct_of_length_for_relief
  )
  
  # add tmp id for convenience
  reclassified_pts <- hydrofabric3D::add_tmp_id(reclassified_pts, x = get(crosswalk_id))
  
  # Find "validity score" values which just represents a cross sections bank and relief validity as either (0, 1, or 2)
  #  Score 0 = FALSE banks & FALSE relief
  #  Score 1 = Either TRUE banks OR relief
  #  Score 2 = Both TRUE banks & TRUE relief
  # ---> We get this score for the old and the new set of extended cross sections and 
  # then take the points in the new data that showed improvement from the original cross section. 
  # The cross section points that did NOT show improvment remain untouched in the original data
  old_validity_scores <- hydrofabric3D::add_tmp_id(calc_validity_scores(cs_pts, crosswalk_id, "old_validity_score"), x = get(crosswalk_id))
  new_validity_scores <- hydrofabric3D::add_tmp_id(calc_validity_scores(reclassified_pts, crosswalk_id, "new_validity_score"), x = get(crosswalk_id))
  
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
    get_unique_tmp_ids(x = get(crosswalk_id))
  
  # ids_to_add_to_good_set <- 
  #   check_for_improvement %>% 
  #   dplyr::filter(!improved) %>% 
  #   get_unique_tmp_ids()
  
  # add a tmp_id for joining and filtering 
  extended_pts <- add_tmp_id(extended_pts, x = get(crosswalk_id))
  
  # TODO: Left off here to add back and remove old data 03/05/2024
  pts_to_keep <- dplyr::filter(extended_pts, 
                               tmp_id %in% extended_ids_to_keep)
  # pts_to_keep <- dplyr::filter(extended_pts2, !tmp_id %in% ids_to_add_to_good_set)
  # pts_to_move_to_good_set <- dplyr::filter(extended_pts2, tmp_id %in% ids_to_add_to_good_set)
  
  # Reclassify the pts_to_keep so they can be added back to the remaining "good" cross section points from the input
  pts_to_keep             <- classify_points(
    pts_to_keep,
    crosswalk_id = crosswalk_id,
    pct_of_length_for_relief = pct_of_length_for_relief
  )
  # pts_to_keep             <- hydrofabric3D::classify_points(pts_to_keep, pct_of_length_for_relief = pct_of_length_for_relief)
  
  # pts_to_keep %>% 
  #   dplyr::filter(is_extended)
  
  # add is_extended logical if does not exist
  if (!"is_extended" %in% names(pts_to_keep)) {
    pts_to_keep$is_extended = TRUE
  }
  
  # remove the IDs of newly updated cross section points from the original data, then 
  # bind the new version of these points to the rest of the original data
  final_pts <-
    cs_pts %>%  
    hydrofabric3D::add_tmp_id(x = get(crosswalk_id)) %>% 
    dplyr::filter(
      !tmp_id %in% extended_ids_to_keep
    ) %>% 
    dplyr::mutate(
      is_extended = FALSE
    ) %>% 
    dplyr::bind_rows(
      hydrofabric3D::add_tmp_id(pts_to_keep, x = get(crosswalk_id))
    ) %>% 
    dplyr::select(-tmp_id) 
  
  # start_ids <- unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id)
  # end_ids <- unique(final_pts$tmp_id)
  # 
  # length(unique(final_pts$tmp_id))
  # length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  # length(unique(final_pts$tmp_id)) == length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  # length(unique(hydrofabric3D::add_tmp_id(final_pts)$tmp_id)) == length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  
  # rename geometry column to "geom" 
  final_pts <- nhdplusTools::rename_geometry(final_pts, "geometry")
  
  # TODO: this should probably be removed and just kept as its own separete function and use outside of this function
  # If TRUE then the cs_ids are renumbered to make sure each hy_id has cross sections
  # that are numbered (1 - number of cross sections) on the hy_id
  if (fix_ids) {
    if (verbose) { message("Renumbering cross section IDs...") }
    final_pts <- renumber_cs_ids2(df = final_pts, crosswalk_id = crosswalk_id)
  }
  
  # final_pts ==  hydrofabric3D:::renumber_cs_ids(final_pts)
  
  # then move the geometry column to the last column
  final_pts <- move_geometry_to_last(final_pts)
  # final_pts <- dplyr::relocate(final_pts, geom, .after = dplyr::last_col())
  
  return(final_pts)
}

#' @title Check and fix cross section points with limited variation in Z values (without removing any flowlines)
#' @description Duplicate process as rectify_cs() but does NOT remove any cross sections, only attempts to extend transects and improve cross sections. This function takes in a set of cross section points (cs_pts), a flowline network (net) and a set of transects lines for that flowline network (transects).
#' This function assumes the cross section points have been classified via "classify_points()" and have "has_relief" and "valid_banks" logical columns.
#' This function will look for cross section points that either have no relief or don't have valid banks, then the transect lines that generated these cross section points
#' are extended and new points are extracted along the newly extended, longer transect line. The newly extracted points are checked for relief AND valid banks and 
#' are removed if they still have no relief or don't have valid banks. Any new points that became valid as a result of the extension process are added to the original dataset 
#' and the rectified set of cross section points will be returned with an "is_extended" logical flag, indicating if the transect line that generated the cross section points was extended.
#' Improved function for rectifying cross section points with flat Z values by extending transect lines and reevaluating the new DEM values.
#' @param cs_pts sf dataframe or dataframe of cross section points from cross_section_pts() followed by classify_points()
#' @param net Hydrographic LINESTRING Network
#' @param transects character, Hydrographic LINESTRING of transects along hydrographic (net) network
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
#' @importFrom nhdplusTools rename_geometry
#' @return sf object of cross section points based on extended transects to try and improve the number of points with "valid_banks" and "has_relief"
#' @export
improve_invalid_cs3 = function(
    cs_pts         = NULL,   
    net            = NULL,
    transects      = NULL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
    scale          = 0.5,
    pct_of_length_for_relief = 0.01,
    fix_ids        = FALSE,
    verbose        = TRUE
) {
  # ----------------------------------------
  
  # library(sf)
  # library(dplyr)
  # library(geos)
  # library(terra)
  # 
  # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # net <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg") %>%
  #   dplyr::rename(hy_id = id)
  # # flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
  # transects <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_transects_06.gpkg")
  # 
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
  # scale          = 0.5
  # pct_of_length_for_relief = 0.01
  # fix_ids        = FALSE
  # verbose        = TRUE
  
  # sf::write_sf(cs_pts, "/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # sf::write_sf(flines, "/Users/anguswatters/Desktop/test_improve_flines.gpkg")
  # sf::write_sf(transects, "/Users/anguswatters/Desktop/test_improve_transects.gpkg")
  
  # ----------------------------------------
  
  # add a "tmp_id" column to easily index transects by hy_id and cs_id 
  transects <- hydrofabric3D::add_tmp_id(transects)
  
  # set geometry coluimn name as beginning 
  transects <- nhdplusTools::rename_geometry(transects, "geometry") 
  
  ### ### ## ## ### ## ### ##  ### ### ## ## ### ## ### ##
  
  if (verbose) { message("Determining points to reevaluate...") }
  
  # add valid_banks and has_relief columns to transects data
  transects <- 
    transects %>% 
    dplyr::left_join(
      dplyr::ungroup(
        dplyr::slice(
          dplyr::group_by(
            dplyr::select(sf::st_drop_geometry(cs_pts), hy_id, cs_id, valid_banks, has_relief),
            hy_id, cs_id), 
          1)
      ),
      by = c("hy_id", "cs_id")
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
  
  # flag_transects <- transects %>% 
  #   dplyr::mutate(
  #     needs_extension = dplyr::case_when(
  #       tmp_id %in% unique(hydrofabric3D::add_tmp_id(pts_to_inspect)$tmp_id) ~ TRUE,
  #       TRUE                                   ~ FALSE),
  #     length_to_extend = dplyr::case_when(
  #       needs_extension ~ (cs_lengthm * scale) / 2,
  #       TRUE            ~ 0)) 
  
  # 0. Split the data into valid and invalid transects
  # 1. Go through invalid transects
  # 2. Try to EXTEND, 
  # 3. and then UPDATE --> (only IF the extended transect does NOT violate any of the intersection rules)
  # If ALL of the below intersection conditions are TRUE then a given extended transect line will get replace the old transect geometry 
  # Intersection rules: 
  # - Newly extended transect intersects with its flowlines AT MOST 1 time
  # - Newly extended transect does NOT intersect with any of the other NEWLY EXTENDED transect lines
  # - Newly extended transect does NOT intersect with any of the ORIGINAL transect lines
  
  # system.time({
  
  # NOTE: extend_invalid_transects() returns the "transects" object with updated attributes for any
  # extensions that were made (geometries, cs_lengthm, "is_extended" flag) and keeps all the rest of the remaining data in place
  extended_geoms <- extend_invalid_transects3(
    transects_to_check  = transects, 
    net                 = net, 
    crosswalk_id        = "hy_id",
    scale               = scale,
    verbose             = verbose
  )
  
  # })
  
  # system.time({
  #   
  #   # NOTE: extend_invalid_transects() returns the "transects" object with updated attributes for any
  #   # extensions that were made (geometries, cs_lengthm, "is_extended" flag) and keeps all the rest of the remaining data in place
  #   extended_geoms2 <- extend_invalid_transects(
  #     transects_to_check  = transects, 
  #     net                 = dplyr::rename(net, id = hy_id), 
  #     scale               = scale,
  #     verbose             = verbose
  #   )
  #   
  # })
  # 
  # all(hydrofabric3D::add_tmp_id(extended_geoms)$tmp_id %in% hydrofabric3D::add_tmp_id(extended_geoms2)$tmp_id)
  # extended_geoms %>% 
  # dplyr::filter(is_extended)
  # good_to_go_transects <- dplyr::filter(extended_geoms, !is_extended)
  
  # Remove unextendable transects from extended_geoms 
  extended_transects <- dplyr::filter(extended_geoms, is_extended)
  
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
  extended_pts <- extract_dem_values(cs = extended_transects, dem = dem)
  
  # Drop the old valid_banks and has_relief columns
  extended_pts <- dplyr::select(extended_pts, -valid_banks, -has_relief)
  
  # add a tmp_id for joining and filtering 
  # extended_pts <- add_tmp_id(extended_pts)
  
  if (verbose) { message("Double checking new extended cross section DEM values for flatness") }
  
  # reclassify the cross sections to look for any improvments in the points bank/relief validity
  reclassified_pts <- hydrofabric3D::classify_points(
    extended_pts, 
    pct_of_length_for_relief = pct_of_length_for_relief
  )
  
  # add tmp id for convenience
  reclassified_pts <- hydrofabric3D::add_tmp_id(reclassified_pts)
  
  # Find "validity score" values which just represents a cross sections bank and relief validity as either (0, 1, or 2)
  #  Score 0 = FALSE banks & FALSE relief
  #  Score 1 = Either TRUE banks OR relief
  #  Score 2 = Both TRUE banks & TRUE relief
  # ---> We get this score for the old and the new set of extended cross sections and 
  # then take the points in the new data that showed improvement from the original cross section. 
  # The cross section points that did NOT show improvment remain untouched in the original data
  old_validity_scores <- hydrofabric3D::add_tmp_id(calc_validity_scores(cs_pts, "hy_id", "old_validity_score"))
  new_validity_scores <- hydrofabric3D::add_tmp_id(calc_validity_scores(reclassified_pts, "hy_id", "new_validity_score"))
  
  # mark as "improved" for any hy_id/cs_ids that increased "validity score" after extending
  check_for_improvement <- dplyr::left_join(
    dplyr::select(dplyr::filter(old_validity_scores, 
                                tmp_id %in% unique(new_validity_scores$tmp_id)
    ),  
    hy_id, cs_id, old_validity_score
    ), 
    dplyr::select(new_validity_scores, hy_id, cs_id, new_validity_score),
    by = c("hy_id", "cs_id")
  ) %>% 
    dplyr::mutate(
      improved = dplyr::case_when(
        new_validity_score > old_validity_score ~ TRUE,
        TRUE                                    ~ FALSE
      )
    ) %>% 
    dplyr::select(hy_id, cs_id, improved)
  
  # List of unique hy_id/cs_ids (tmp_id) that showed improvement after extension, if valid banks or relief was addded (or both),
  # then the cross section "showed improvement", and the new values will be put into the output cross section dataset
  extended_ids_to_keep <- 
    check_for_improvement %>% 
    dplyr::filter(improved) %>% 
    get_unique_tmp_ids()
  
  # ids_to_add_to_good_set <- 
  #   check_for_improvement %>% 
  #   dplyr::filter(!improved) %>% 
  #   get_unique_tmp_ids()
  
  # add a tmp_id for joining and filtering 
  extended_pts <- add_tmp_id(extended_pts)
  
  # TODO: Left off here to add back and remove old data 03/05/2024
  pts_to_keep <- dplyr::filter(extended_pts, 
                               tmp_id %in% extended_ids_to_keep)
  # pts_to_keep <- dplyr::filter(extended_pts2, !tmp_id %in% ids_to_add_to_good_set)
  # pts_to_move_to_good_set <- dplyr::filter(extended_pts2, tmp_id %in% ids_to_add_to_good_set)
  
  # Reclassify the pts_to_keep so they can be added back to the remaining "good" cross section points from the input
  pts_to_keep             <- hydrofabric3D::classify_points(pts_to_keep,
                                                            pct_of_length_for_relief = pct_of_length_for_relief)
  
  # pts_to_keep %>% 
  #   dplyr::filter(is_extended)
  
  # add is_extended logical if does not exist
  if (!"is_extended" %in% names(pts_to_keep)) {
    pts_to_keep$is_extended = TRUE
  }
  
  # remove the IDs of newly updated cross section points from the original data, then 
  # bind the new version of these points to the rest of the original data
  final_pts <-
    cs_pts %>%  
    hydrofabric3D::add_tmp_id() %>% 
    dplyr::filter(
      !tmp_id %in% extended_ids_to_keep
    ) %>% 
    dplyr::mutate(
      is_extended = FALSE
    ) %>% 
    dplyr::bind_rows(
      hydrofabric3D::add_tmp_id(pts_to_keep)
    ) %>% 
    dplyr::select(-tmp_id) 
  
  # start_ids <- unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id)
  # end_ids <- unique(final_pts$tmp_id)
  # 
  # length(unique(final_pts$tmp_id))
  # length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  # length(unique(final_pts$tmp_id)) == length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  # length(unique(hydrofabric3D::add_tmp_id(final_pts)$tmp_id)) == length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  
  # rename geometry column to "geom" 
  final_pts <- nhdplusTools::rename_geometry(final_pts, "geometry")
  
  # TODO: this should probably be removed and just kept as its own separete function and use outside of this function
  # If TRUE then the cs_ids are renumbered to make sure each hy_id has cross sections
  # that are numbered (1 - number of cross sections) on the hy_id
  if (fix_ids) {
    if (verbose) { message("Renumbering cross section IDs...") }
    final_pts <- renumber_cs_ids(final_pts)
  }
  
  # final_pts ==  hydrofabric3D:::renumber_cs_ids(final_pts)
  
  # then move the geometry column to the last column
  final_pts <- move_geometry_to_last(final_pts)
  # final_pts <- dplyr::relocate(final_pts, geom, .after = dplyr::last_col())
  
  return(final_pts)
}

#' @title Check and fix cross section points with limited variation in Z values (without removing any flowlines)
#' @description Duplicate process as rectify_cs() but does NOT remove any cross sections, only attempts to extend transects and improve cross sections. This function takes in a set of cross section points (cs_pts), a flowline network (net) and a set of transects lines for that flowline network (transects).
#' This function assumes the cross section points have been classified via "classify_points()" and have "has_relief" and "valid_banks" logical columns.
#' This function will look for cross section points that either have no relief or don't have valid banks, then the transect lines that generated these cross section points
#' are extended and new points are extracted along the newly extended, longer transect line. The newly extracted points are checked for relief AND valid banks and 
#' are removed if they still have no relief or don't have valid banks. Any new points that became valid as a result of the extension process are added to the original dataset 
#' and the rectified set of cross section points will be returned with an "is_extended" logical flag, indicating if the transect line that generated the cross section points was extended.
#' Improved function for rectifying cross section points with flat Z values by extending transect lines and reevaluating the new DEM values.
#' @param cs_pts sf dataframe or dataframe of cross section points from cross_section_pts() followed by classify_points()
#' @param net Hydrographic LINESTRING Network
#' @param transects character, Hydrographic LINESTRING of transects along hydrographic (net) network
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
#' @importFrom nhdplusTools rename_geometry
#' @return sf object of cross section points based on extended transects to try and improve the number of points with "valid_banks" and "has_relief"
#' @export
improve_invalid_cs2 = function(
    cs_pts         = NULL,   
    net            = NULL,
    transects      = NULL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
    scale          = 0.5,
    pct_of_length_for_relief = 0.01,
    fix_ids        = FALSE,
    verbose        = TRUE
) {
   # ----------------------------------------
  
  # library(sf)
  # library(dplyr)
  # library(geos)
  # library(terra)
  # 
  # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # net <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg") %>% 
  #   dplyr::rename(hy_id = id)
  # # flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
  # transects <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_transects_06.gpkg")
  # 
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
  # scale          = 0.5
  # pct_of_length_for_relief = 0.01
  # fix_ids        = FALSE
  # verbose        = TRUE

  # sf::write_sf(cs_pts, "/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # sf::write_sf(flines, "/Users/anguswatters/Desktop/test_improve_flines.gpkg")
  # sf::write_sf(transects, "/Users/anguswatters/Desktop/test_improve_transects.gpkg")
  
  # ----------------------------------------
  
  # add a "tmp_id" column to easily index transects by hy_id and cs_id 
  transects <- hydrofabric3D::add_tmp_id(transects)
  
  ### ### ## ## ### ## ### ##  ### ### ## ## ### ## ### ##
  
  if (verbose) { message("Determining points to reevaluate...") }

  # add valid_banks and has_relief columns to transects data
  transects <- 
    transects %>% 
    dplyr::left_join(
      dplyr::ungroup(
        dplyr::slice(
          dplyr::group_by(
            dplyr::select(sf::st_drop_geometry(cs_pts), hy_id, cs_id, valid_banks, has_relief),
            hy_id, cs_id), 
          1)
      ),
      by = c("hy_id", "cs_id")
    )
  
  # if there are no transects that need rectification, return the original cs_pts early with a "is_extended" flag
  if (!needs_rectification(transects)) {
    
    cs_pts <- 
      cs_pts %>% 
      dplyr::mutate(
        is_extended = FALSE
      ) %>% 
      dplyr::relocate(geom, .after = dplyr::last_col())
    
    return(cs_pts)
  }
  
  # flag_transects <- transects %>% 
  #   dplyr::mutate(
  #     needs_extension = dplyr::case_when(
  #       tmp_id %in% unique(hydrofabric3D::add_tmp_id(pts_to_inspect)$tmp_id) ~ TRUE,
  #       TRUE                                   ~ FALSE),
  #     length_to_extend = dplyr::case_when(
  #       needs_extension ~ (cs_lengthm * scale) / 2,
  #       TRUE            ~ 0)) 
  
  # 0. Split the data into valid and invalid transects
  # 1. Go through invalid transects
  # 2. Try to EXTEND, 
  # 3. and then UPDATE --> (only IF the extended transect does NOT violate any of the intersection rules)
  # If ALL of the below intersection conditions are TRUE then a given extended transect line will get replace the old transect geometry 
  # Intersection rules: 
  # - Newly extended transect intersects with its flowlines AT MOST 1 time
  # - Newly extended transect does NOT intersect with any of the other NEWLY EXTENDED transect lines
  # - Newly extended transect does NOT intersect with any of the ORIGINAL transect lines
  
  # system.time({
    
  # NOTE: extend_invalid_transects() returns the "transects" object with updated attributes for any
  # extensions that were made (geometries, cs_lengthm, "is_extended" flag) and keeps all the rest of the remaining data in place
  extended_geoms <- extend_invalid_transects3(
    transects_to_check  = transects, 
    net                 = net, 
    crosswalk_id        = "hy_id",
    scale               = scale,
    verbose             = verbose
  )
  
  # })
  
  # system.time({
  #   
  #   # NOTE: extend_invalid_transects() returns the "transects" object with updated attributes for any
  #   # extensions that were made (geometries, cs_lengthm, "is_extended" flag) and keeps all the rest of the remaining data in place
  #   extended_geoms2 <- extend_invalid_transects(
  #     transects_to_check  = transects, 
  #     net                 = dplyr::rename(net, id = hy_id), 
  #     scale               = scale,
  #     verbose             = verbose
  #   )
  #   
  # })
  # 
  # all(hydrofabric3D::add_tmp_id(extended_geoms)$tmp_id %in% hydrofabric3D::add_tmp_id(extended_geoms2)$tmp_id)
  # extended_geoms %>% 
    # dplyr::filter(is_extended)
  # good_to_go_transects <- dplyr::filter(extended_geoms, !is_extended)
  
  # Remove unextendable transects from extended_geoms 
  extended_transects <- dplyr::filter(extended_geoms, is_extended)
  
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
  extended_pts <- extract_dem_values(cs = extended_transects, dem = dem)
  
  # Drop the old valid_banks and has_relief columns
  extended_pts <- dplyr::select(extended_pts, -valid_banks, -has_relief)
  
  # add a tmp_id for joining and filtering 
  # extended_pts <- add_tmp_id(extended_pts)
  
  if (verbose) { message("Double checking new extended cross section DEM values for flatness") }
  
  # reclassify the cross sections to look for any improvments in the points bank/relief validity
  reclassified_pts <- hydrofabric3D::classify_points(
    extended_pts, 
    pct_of_length_for_relief = pct_of_length_for_relief
  )
  
  # add tmp id for convenience
  reclassified_pts <- hydrofabric3D::add_tmp_id(reclassified_pts)

  # Find "validity score" values which just represents a cross sections bank and relief validity as either (0, 1, or 2)
  #  Score 0 = FALSE banks & FALSE relief
  #  Score 1 = Either TRUE banks OR relief
  #  Score 2 = Both TRUE banks & TRUE relief
  # ---> We get this score for the old and the new set of extended cross sections and 
  # then take the points in the new data that showed improvement from the original cross section. 
  # The cross section points that did NOT show improvment remain untouched in the original data
  old_validity_scores <- hydrofabric3D::add_tmp_id(calc_validity_scores(cs_pts, "hy_id", "old_validity_score"))
  new_validity_scores <- hydrofabric3D::add_tmp_id(calc_validity_scores(reclassified_pts, "hy_id", "new_validity_score"))
  
  # mark as "improved" for any hy_id/cs_ids that increased "validity score" after extending
  check_for_improvement <- dplyr::left_join(
                            dplyr::select(dplyr::filter(old_validity_scores, 
                                                        tmp_id %in% unique(new_validity_scores$tmp_id)
                                                        ),  
                                            hy_id, cs_id, old_validity_score
                                          ), 
                            dplyr::select(new_validity_scores, hy_id, cs_id, new_validity_score),
                            by = c("hy_id", "cs_id")
                          ) %>% 
    dplyr::mutate(
      improved = dplyr::case_when(
        new_validity_score > old_validity_score ~ TRUE,
        TRUE                                    ~ FALSE
        )
      ) %>% 
    dplyr::select(hy_id, cs_id, improved)
  
  # List of unique hy_id/cs_ids (tmp_id) that showed improvement after extension, if valid banks or relief was addded (or both),
  # then the cross section "showed improvement", and the new values will be put into the output cross section dataset
  extended_ids_to_keep <- 
    check_for_improvement %>% 
    dplyr::filter(improved) %>% 
    get_unique_tmp_ids()
  
  # ids_to_add_to_good_set <- 
  #   check_for_improvement %>% 
  #   dplyr::filter(!improved) %>% 
  #   get_unique_tmp_ids()
  
  # add a tmp_id for joining and filtering 
  extended_pts <- add_tmp_id(extended_pts)
  
  # TODO: Left off here to add back and remove old data 03/05/2024
  pts_to_keep <- dplyr::filter(extended_pts, 
                               tmp_id %in% extended_ids_to_keep)
  # pts_to_keep <- dplyr::filter(extended_pts2, !tmp_id %in% ids_to_add_to_good_set)
  # pts_to_move_to_good_set <- dplyr::filter(extended_pts2, tmp_id %in% ids_to_add_to_good_set)
  
  # Reclassify the pts_to_keep so they can be added back to the remaining "good" cross section points from the input
  pts_to_keep             <- hydrofabric3D::classify_points(pts_to_keep,
                                                            pct_of_length_for_relief = pct_of_length_for_relief)
  
  # pts_to_keep %>% 
  #   dplyr::filter(is_extended)
  
  # add is_extended logical if does not exist
  if (!"is_extended" %in% names(pts_to_keep)) {
    pts_to_keep$is_extended = TRUE
  }
  
  # remove the IDs of newly updated cross section points from the original data, then 
  # bind the new version of these points to the rest of the original data
  final_pts <-
    cs_pts %>%  
    hydrofabric3D::add_tmp_id() %>% 
    dplyr::filter(
      !tmp_id %in% extended_ids_to_keep
    ) %>% 
    dplyr::mutate(
      is_extended = FALSE
    ) %>% 
    dplyr::bind_rows(
      hydrofabric3D::add_tmp_id(pts_to_keep)
    ) %>% 
    dplyr::select(-tmp_id) 
  
  # start_ids <- unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id)
  # end_ids <- unique(final_pts$tmp_id)
  # 
  # length(unique(final_pts$tmp_id))
  # length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  # length(unique(final_pts$tmp_id)) == length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  # length(unique(hydrofabric3D::add_tmp_id(final_pts)$tmp_id)) == length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  
  # rename geometry column to "geom" 
  final_pts <- nhdplusTools::rename_geometry(final_pts, "geom")
  
  # TODO: this should probably be removed and just kept as its own separete function and use outside of this function
  # If TRUE then the cs_ids are renumbered to make sure each hy_id has cross sections
  # that are numbered (1 - number of cross sections) on the hy_id
  if (fix_ids) {
    if (verbose) { message("Renumbering cross section IDs...") }
    final_pts <- renumber_cs_ids(final_pts)
  }
  
  # final_pts ==  hydrofabric3D:::renumber_cs_ids(final_pts)
  
  # then move the geometry column to the last column
  final_pts <- move_geometry_to_last(final_pts)
  # final_pts <- dplyr::relocate(final_pts, geom, .after = dplyr::last_col())
  
  return(final_pts)
}

#' @title Check and fix cross section points with limited variation in Z values (without removing any flowlines)
#' @description Duplicate process as rectify_cs() but does NOT remove any cross sections, only attempts to extend transects and improve cross sections. This function takes in a set of cross section points (cs_pts), a flowline network (net) and a set of transects lines for that flowline network (transects).
#' This function assumes the cross section points have been classified via "classify_points()" and have "has_relief" and "valid_banks" logical columns.
#' This function will look for cross section points that either have no relief or don't have valid banks, then the transect lines that generated these cross section points
#' are extended and new points are extracted along the newly extended, longer transect line. The newly extracted points are checked for relief AND valid banks and 
#' are removed if they still have no relief or don't have valid banks. Any new points that became valid as a result of the extension process are added to the original dataset 
#' and the rectified set of cross section points will be returned with an "is_extended" logical flag, indicating if the transect line that generated the cross section points was extended.
#' Improved function for rectifying cross section points with flat Z values by extending transect lines and reevaluating the new DEM values.
#' @param cs_pts sf dataframe or dataframe of cross section points from cross_section_pts() followed by classify_points()
#' @param net Hydrographic LINESTRING Network
#' @param transects character, Hydrographic LINESTRING of transects along hydrographic (net) network
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
#' @importFrom nhdplusTools rename_geometry
#' @return sf object of cross section points based on extended transects to try and improve the number of points with "valid_banks" and "has_relief"
#' @export
improve_invalid_cs = function(
    cs_pts         = NULL,   
    net            = NULL,
    transects      = NULL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
    scale          = 0.5,
    pct_of_length_for_relief = 0.01,
    fix_ids        = FALSE,
    verbose        = TRUE
) {
  # ----------------------------------------
  
  # library(sf)
  # library(dplyr)
  # library(geos)
  # library(terra)
  # 
  # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # net <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg") %>% 
  #   dplyr::rename(hy_id = id)
  # # flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
  # transects <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_transects_06.gpkg")
  # 
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
  # scale          = 0.5
  # pct_of_length_for_relief = 0.01
  # fix_ids        = FALSE
  # verbose        = TRUE
  
  # sf::write_sf(cs_pts, "/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
  # sf::write_sf(flines, "/Users/anguswatters/Desktop/test_improve_flines.gpkg")
  # sf::write_sf(transects, "/Users/anguswatters/Desktop/test_improve_transects.gpkg")
  
  # ----------------------------------------
  
  # add a "tmp_id" column to easily index transects by hy_id and cs_id 
  transects <- hydrofabric3D::add_tmp_id(transects)
  
  ### ### ## ## ### ## ### ##  ### ### ## ## ### ## ### ##
  
  if (verbose) { message("Determining points to reevaluate...") }
  
  # add valid_banks and has_relief columns to transects data
  transects <- 
    transects %>% 
    dplyr::left_join(
      dplyr::ungroup(
        dplyr::slice(
          dplyr::group_by(
            dplyr::select(sf::st_drop_geometry(cs_pts), hy_id, cs_id, valid_banks, has_relief),
            hy_id, cs_id), 
          1)
      ),
      by = c("hy_id", "cs_id")
    )
  
  # if there are no transects that need rectification, return the original cs_pts early with a "is_extended" flag
  if (!needs_rectification(transects)) {
    
    cs_pts <- 
      cs_pts %>% 
      dplyr::mutate(
        is_extended = FALSE
      ) %>% 
      dplyr::relocate(geom, .after = dplyr::last_col())
    
    return(cs_pts)
  }
  
  # flag_transects <- transects %>% 
  #   dplyr::mutate(
  #     needs_extension = dplyr::case_when(
  #       tmp_id %in% unique(hydrofabric3D::add_tmp_id(pts_to_inspect)$tmp_id) ~ TRUE,
  #       TRUE                                   ~ FALSE),
  #     length_to_extend = dplyr::case_when(
  #       needs_extension ~ (cs_lengthm * scale) / 2,
  #       TRUE            ~ 0)) 
  
  # 0. Split the data into valid and invalid transects
  # 1. Go through invalid transects
  # 2. Try to EXTEND, 
  # 3. and then UPDATE --> (only IF the extended transect does NOT violate any of the intersection rules)
  # If ALL of the below intersection conditions are TRUE then a given extended transect line will get replace the old transect geometry 
  # Intersection rules: 
  # - Newly extended transect intersects with its flowlines AT MOST 1 time
  # - Newly extended transect does NOT intersect with any of the other NEWLY EXTENDED transect lines
  # - Newly extended transect does NOT intersect with any of the ORIGINAL transect lines
  
  
  # NOTE: extend_invalid_transects() returns the "transects" object with updated attributes for any
  # extensions that were made (geometries, cs_lengthm, "is_extended" flag) and keeps all the rest of the remaining data in place
  extended_geoms <- extend_invalid_transects2(
    transects_to_check  = transects, 
    net                 = net, 
    scale               = scale,
    verbose             = verbose
  )
  
  # good_to_go_transects <- dplyr::filter(extended_geoms, !is_extended)
  
  # Remove unextendable transects from extended_geoms 
  extended_transects <- dplyr::filter(extended_geoms, is_extended)
  
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
  extended_pts <- extract_dem_values(cs = extended_transects, dem = dem)
  
  # Drop the old valid_banks and has_relief columns
  extended_pts <- dplyr::select(extended_pts, -valid_banks, -has_relief)
  
  # add a tmp_id for joining and filtering 
  # extended_pts <- add_tmp_id(extended_pts)
  
  if (verbose) { message("Double checking new extended cross section DEM values for flatness") }
  
  # reclassify the cross sections to look for any improvments in the points bank/relief validity
  reclassified_pts <- hydrofabric3D::classify_points(
    extended_pts, 
    pct_of_length_for_relief = pct_of_length_for_relief
  )
  
  # add tmp id for convenience
  reclassified_pts <- hydrofabric3D::add_tmp_id(reclassified_pts)
  
  # Find "validity score" values which just represents a cross sections bank and relief validity as either (0, 1, or 2)
  #  Score 0 = FALSE banks & FALSE relief
  #  Score 1 = Either TRUE banks OR relief
  #  Score 2 = Both TRUE banks & TRUE relief
  # ---> We get this score for the old and the new set of extended cross sections and 
  # then take the points in the new data that showed improvement from the original cross section. 
  # The cross section points that did NOT show improvment remain untouched in the original data
  old_validity_scores <- hydrofabric3D::add_tmp_id(calc_validity_scores(cs_pts, "hy_id", "old_validity_score"))
  new_validity_scores <- hydrofabric3D::add_tmp_id(calc_validity_scores(reclassified_pts, "hy_id", "new_validity_score"))
  
  # mark as "improved" for any hy_id/cs_ids that increased "validity score" after extending
  check_for_improvement <- dplyr::left_join(
    dplyr::select(dplyr::filter(old_validity_scores, 
                                tmp_id %in% unique(new_validity_scores$tmp_id)),  
                  hy_id, cs_id, old_validity_score), 
    dplyr::select(new_validity_scores, hy_id, cs_id, new_validity_score),
    by = c("hy_id", "cs_id")
  ) %>% 
    dplyr::mutate(
      improved = dplyr::case_when(
        new_validity_score > old_validity_score ~ TRUE,
        TRUE                                    ~ FALSE
      )
    ) %>% 
    dplyr::select(hy_id, cs_id, improved)
  
  # List of unique hy_id/cs_ids (tmp_id) that showed improvement after extension, if valid banks or relief was addded (or both),
  # then the cross section "showed improvement", and the new values will be put into the output cross section dataset
  extended_ids_to_keep <- 
    check_for_improvement %>% 
    dplyr::filter(improved) %>% 
    get_unique_tmp_ids()
  
  # ids_to_add_to_good_set <- 
  #   check_for_improvement %>% 
  #   dplyr::filter(!improved) %>% 
  #   get_unique_tmp_ids()
  
  # add a tmp_id for joining and filtering 
  extended_pts <- add_tmp_id(extended_pts)
  
  # TODO: Left off here to add back and remove old data 03/05/2024
  pts_to_keep <- dplyr::filter(extended_pts, tmp_id %in% extended_ids_to_keep)
  # pts_to_keep <- dplyr::filter(extended_pts2, !tmp_id %in% ids_to_add_to_good_set)
  # pts_to_move_to_good_set <- dplyr::filter(extended_pts2, tmp_id %in% ids_to_add_to_good_set)
  
  # Reclassify the pts_to_keep so they can be added back to the remaining "good" cross section points from the input
  pts_to_keep             <- hydrofabric3D::classify_points(pts_to_keep,
                                                            pct_of_length_for_relief = pct_of_length_for_relief)
  
  # add is_extended logical if does not exist
  if (!"is_extended" %in% names(pts_to_keep)) {
    pts_to_keep$is_extended = TRUE
  }
  
  # remove the IDs of newly updated cross section points from the original data, then 
  # bind the new version of these points to the rest of the original data
  final_pts <-
    cs_pts %>%  
    hydrofabric3D::add_tmp_id() %>% 
    dplyr::filter(
      !tmp_id %in% extended_ids_to_keep
    ) %>% 
    dplyr::mutate(
      is_extended = FALSE
    ) %>% 
    dplyr::bind_rows(
      hydrofabric3D::add_tmp_id(pts_to_keep)
    ) %>% 
    dplyr::select(-tmp_id) 
  
  # start_ids <- unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id)
  # end_ids <- unique(final_pts$tmp_id)
  # 
  # length(unique(final_pts$tmp_id))
  # length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  # length(unique(final_pts$tmp_id)) == length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  # length(unique(hydrofabric3D::add_tmp_id(final_pts)$tmp_id)) == length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
  
  # rename geometry column to "geom" 
  final_pts <- nhdplusTools::rename_geometry(final_pts, "geom")
  
  # TODO: this should probably be removed and just kept as its own separete function and use outside of this function
  # If TRUE then the cs_ids are renumbered to make sure each hy_id has cross sections
  # that are numbered (1 - number of cross sections) on the hy_id
  if (fix_ids) {
    if (verbose) { message("Renumbering cross section IDs...") }
    final_pts <- renumber_cs_ids(final_pts)
  }
  
  # final_pts ==  hydrofabric3D:::renumber_cs_ids(final_pts)
  
  # then move the geometry column to the last column
  final_pts <- move_geometry_to_last(final_pts)
  # final_pts <- dplyr::relocate(final_pts, geom, .after = dplyr::last_col())
  
  return(final_pts)
}

#' @title Check and fix cross section points with limited variation in Z values (version 2 latest)
#' @description 
#' This function takes in a set of cross section points (cs_pts), a flowline network (net) and a set of transects lines for that flowline network (transects).
#' This function assumes the cross section points have been classified via "classify_points()" and have "has_relief" and "valid_banks" logical columns.
#' This function will look for cross section points that either have no relief or don't have valid banks, then the transect lines that generated these cross section points
#' are extended and new points are extracted along the newly extended, longer transect line. The newly extracted points are checked for relief AND valid banks and 
#' are removed if they still have no relief or don't have valid banks. Any new points that became valid as a result of the extension process are added to the original dataset 
#' and the rectified set of cross section points will be returned with an "is_extended" logical flag, indicating if the transect line that generated the cross section points was extended.
#' Improved function for rectifying cross section points with flat Z values by extending transect lines and reevaluating the new DEM values.
#' @param cs_pts sf dataframe or dataframe of cross section points from cross_section_pts() followed by classify_points()
#' @param net Hydrographic LINESTRING Network
#' @param transects character, Hydrographic LINESTRING of transects along hydrographic (net) network
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
#' WARNING: Setting fix_ids = TRUE may result in input cross section points (cs_pts) having DIFFERENT cs_id values as the input transects (cs) 
#' and the inconsistency can cause problems when trying to cross walk between the datasets in the future.
#' @param verbose logical, whether to print messages or not. Default is TRUE
#' @importFrom dplyr mutate relocate last_col select rename left_join group_by ungroup slice n bind_rows filter
#' @importFrom sf st_drop_geometry
#' @importFrom nhdplusTools rename_geometry
#' @return sf object of cs_pts with only cross sections points that have relief and have valid banks, other points that don't meet this condition are removed
#' @export
rectify_cs = function(
    cs_pts         = NULL,   
    net            = NULL,
    transects      = NULL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
    scale          = 0.5,
    pct_of_length_for_relief = 0.01,
    fix_ids        = FALSE,
    verbose        = TRUE
) {
  
  ###  ###  ###  ### ###  ###  ###  ###
  ###  ###  ###  ### ###  ###  ###  ###
  # cs_pts = classified_pts
  # net = flines
  # transects = transects
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
  # scale          = 0.5
  # pct_of_length_for_relief = 0.01
  # fix_ids        = FALSE
  # verbose        = TRUE
  ###  ###  ###  ### ###  ###  ###  ###
  ###  ###  ###  ### ###  ###  ###  ###
  
  # # starting column names
  # start_cols <- names(cs_pts)
  
  # add a "tmp_id" column to easily index transects by hy_id and cs_id 
  transects <- hydrofabric3D::add_tmp_id(transects)
  
  ### ### ## ## ### ## ### ##  ### ### ## ## ### ## ### ##

  if (verbose) { message("Determining points to reevaluate...") }
  # # Check if any cross sections are "flat" within a threshold (All Z values are the same or the difference is within the threshold)
  # pts_to_inspect <- pts_to_reevaluate(cs_pts        = cs_pts, 
  #                              threshold     = threshold,
  #                              pct_threshold = pct_threshold)
  
  # filter down to cross sections that DON'T have valid banks OR DON'T have any relief
  pts_to_inspect <-
    cs_pts %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(!valid_banks | !has_relief)
  
  # # Check if any cross sections are "flat" within a threshold (All Z values are the same or the difference is within the threshold)
  # pts_to_inspect <- pts_to_reevaluate(cs_pts = cs_pts, threshold = threshold, pct_threshold = pct_threshold)
  
  # if there are no flatlines, return the cs_pts object
  if (nrow(pts_to_inspect) == 0) {
    
    cs_pts <- 
      cs_pts %>% 
      dplyr::mutate(
        is_extended = FALSE
      ) %>% 
      dplyr::relocate(geom, .after = dplyr::last_col())
    
    return(cs_pts)
  }
  
  # subset transects (transects) to the flat cross sections in pts_to_inspect
  trans_to_extend <- 
    transects %>% 
    dplyr::filter(tmp_id %in% unique(hydrofabric3D::add_tmp_id(pts_to_inspect)$tmp_id)) %>%
    dplyr::select(-tmp_id) 
  
  
  # 1. Loop through geometries that might need to be extended, 
  # 2. Try to EXTEND, 
  # 3. and then UPDATE --> (only IF the extended transect does NOT violate any of the intersection rules)
  # If ALL of the below intersection conditions are TRUE then a given extended transect line will get replace the old transect geometry 
  # Intersection rules: 
  # - Newly extended transect intersects with its flowlines AT MOST 1 time
  # - Newly extended transect does NOT intersect with any of the other NEWLY EXTENDED transect lines
  # - Newly extended transect does NOT intersect with any of the ORIGINAL transect lines
  # extend_transects() returns the "trans_to_extend" object with updated attributes for any extensions that were made (geometries, cs_lengthm, "is_extended" flag) 
  extended_geoms <- extend_transects(
    transects_to_extend = trans_to_extend,
    transects           = transects, 
    net                 = net, 
    scale               = scale,
    verbose             = verbose
  )
  
  # Store unextendable transects for filtering out later on 
  # (these are transects that were flat AND could NOT be extended without violating an intersection rule)
  unextendable   <- dplyr::filter(extended_geoms, !is_extended)
  
  # Remove unextendable transects from extended_geoms 
  extended_geoms <- dplyr::filter(extended_geoms, is_extended)
  
  # system.time({
    
  # add cross section points to extended cross sections
  extended_geoms <- add_points_per_cs(
    cs             = extended_geoms,
    # cs             = trans_to_extend,
    # cs             = dplyr::slice(extended_geoms , 1:100),
    points_per_cs  = points_per_cs,
    min_pts_per_cs = min_pts_per_cs,
    dem            = dem
  )
  
  # })
  
  if (verbose) { message("Extracting new DEM values..")}
  # system.time({
  
  # extract DEM values for newly extended cross sections
  extended_pts <- extract_dem_values(cs = extended_geoms, dem = dem)
  
  # })
  
  # add a tmp_id for joining and filtering 
  extended_pts <- add_tmp_id(extended_pts)
  
  if (verbose) { message("Double checking new extended cross section DEM values for flatness") }
  
  classify_pts_again <- hydrofabric3D::classify_points(
    extended_pts, 
    pct_of_length_for_relief = pct_of_length_for_relief
  )
  
  # add tmp id for convenience
  classify_pts_again <- hydrofabric3D::add_tmp_id(classify_pts_again)

  # List of unique hy_id/cs_ids (tmp_id) that are STILL bad after attempting to extend and 
  # re-extract new cross section points from the extended transect line 
  # ---> ("Bad" = No relief OR not valid banks)
  still_bad_ids <- 
    classify_pts_again %>% 
    dplyr::filter(!has_relief | !valid_banks) %>% 
    get_unique_tmp_ids()
  
  # TODO: Left off here to add back and remove old data 03/05/2024
  pts_to_keep <- dplyr::filter(extended_pts, !tmp_id %in% still_bad_ids)
  pts_to_drop <- dplyr::filter(extended_pts, tmp_id %in% still_bad_ids)
  # pts_to_keep <- dplyr::filter(extended_pts, !tmp_id %in% unique(dplyr::filter(classify_pts_again, 
                                                              # !has_relief | !valid_banks)$tmp_id))
  # pts_to_drop <- dplyr::filter(extended_pts, tmp_id %in% unique(dplyr::filter(classify_pts_again, 
                                                          # !has_relief | !valid_banks)$tmp_id))
  
  # classify the pts_to_keep so they can be added back to the remaining "good" cross section points from the input
  pts_to_keep <- hydrofabric3D::classify_points(pts_to_keep, pct_of_length_for_relief = pct_of_length_for_relief)
  
  # add is_extended logical if does not exist
  if (!"is_extended" %in% names(pts_to_keep)) {
    pts_to_keep$is_extended = TRUE
  }

  # get list of unique tmp_ids for the "unextendable" dataframe, and for the "keep" and "drop" dataframes
  unextendable_ids <- get_unique_tmp_ids(unextendable, x = hy_id, y = cs_id)
  drop_ids         <- get_unique_tmp_ids(pts_to_drop)
  keep_ids         <- get_unique_tmp_ids(pts_to_keep)
  
  # filter out cross section points that have "same Z" values (remove flat Z values)
  final_pts <-
    cs_pts %>%  
    hydrofabric3D::add_tmp_id() %>% 
    dplyr::filter(
      !tmp_id %in% unextendable_ids
      # !tmp_id %in% unique(hydrofabric3D::add_tmp_id(unextendable)$tmp_id)
      # !tmp_id %in% unique(pts_to_drop$tmp_id)
    ) %>% 
    dplyr::filter(
      !tmp_id %in% drop_ids
      # !tmp_id %in% unique(pts_to_drop$tmp_id)
    ) 
  
  # remove the old versions of the "pts_to_keep" cross section points and 
  # replace them with the updated cross section points with the extended "cs_lengthm" and "Z" values
  final_pts <-
    final_pts %>%
    dplyr::filter(
      !tmp_id %in% keep_ids
      # !tmp_id %in% unique(pts_to_keep$tmp_id)
      # !tmp_id %in% unique(extended_pts$tmp_id)
    ) %>% 
    dplyr::mutate(
      is_extended = FALSE
    ) %>% 
    dplyr::bind_rows(
      # pts_to_keep
      hydrofabric3D::add_tmp_id(pts_to_keep)
    ) %>% 
    dplyr::select(-tmp_id) 

  # rectify_summary(cs_pts, final_pts)

  # rename geometry column to "geom" 
  final_pts <- nhdplusTools::rename_geometry(final_pts, "geom")
  
  # TODO: this should probably be removed and just kept as its own separete function and use outside of this function
  # If TRUE then the cs_ids are renumbered to make sure each hy_id has cross sections
  # that are numbered (1 - number of cross sections) on the hy_id
  if (fix_ids) {
    if (verbose) { message("Renumbering cross section IDs...") }
    final_pts <- renumber_cs_ids(final_pts)
  }
  
  # then move the geometry column to the last column
  final_pts <- move_geometry_to_last(final_pts)
  # final_pts <- dplyr::relocate(final_pts, geom, .after = dplyr::last_col())
  
  return(final_pts)
}
#' @title Fix IDs in a dataframe
#'
#' @description 
#' This function renumbers cross section IDs in a dataframe to ensure each hy_id has cross sections
#' numbered from 1 to the total number of cross sections on the hy_id.
#'
#' @param df A dataframe containing hy_id and cs_id columns.
#' @return The input dataframe with renumbered cs_id values.
#' @importFrom dplyr select group_by slice ungroup mutate n left_join rename relocate
#' @importFrom sf st_drop_geometry
renumber_cs_ids <- function(df) {
  
  if (!"hy_id" %in% colnames(df) || !"cs_id" %in% colnames(df)) {
    stop("The dataframe must contain 'hy_id' and 'cs_id' columns.")
  }
  
  if (length(unique(df$hy_id)) == 0 || length(unique(df$cs_id)) == 0) {
    stop("The dataframe must have non-empty 'hy_id' and 'cs_id' columns.")
  }
  
  if (any(is.na(df$hy_id)) || any(is.na(df$cs_id))) {
    stop("The 'hy_id' and 'cs_id' columns cannot have NA values.")
  }
  
  # make a dataframe that has a new_cs_id column that has 
  # the cs_id renumbered to fill in any missing IDs,
  # so each hy_id has cs_ids that go from 1 - number of cross sections on hy_id
  # The dataframe below will be used to join the "new_cs_id" with 
  # the original "cs_ids" in the final_pts output data
  renumbered_ids <- 
    df %>%
    sf::st_drop_geometry() %>%
    dplyr::select(hy_id, cs_id, pt_id, cs_measure) %>%
    dplyr::group_by(hy_id, cs_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(hy_id) %>%
    dplyr::mutate(
      new_cs_id = 1:dplyr::n(),
      tmp_id = paste0(hy_id, "_", cs_id)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(new_cs_id, tmp_id)
  
  # Join the new cs_ids back with the final output data to replace the old cs_ids
  df <- dplyr::left_join(
    dplyr::mutate(
      df,
      tmp_id = paste0(hy_id, "_", cs_id)
    ),
    renumbered_ids,
    by = "tmp_id"
  ) %>%
    dplyr::select(-cs_id, -tmp_id) %>%
    dplyr::rename("cs_id" = "new_cs_id") %>%
    dplyr::relocate(hy_id, cs_id)
  
  return(df)
}

#' @title Fix IDs in a dataframe
#'
#' @description 
#' This function renumbers cross section IDs in a dataframe to ensure each crosswalk_id has cross sections
#' numbered from 1 to the total number of cross sections on the crosswalk_id.
#'
#' @param df A dataframe containing crosswalk_id and cs_id columns.
#' @param df crosswalk_id character, name of primary ID column
#' @return The input dataframe with renumbered cs_id values.
#' @importFrom dplyr select group_by slice ungroup mutate n left_join rename relocate
#' @importFrom sf st_drop_geometry
renumber_cs_ids2 <- function(df, crosswalk_id = NULL) {
  
  # set to the default unique crosswalk ID if NULL is given 
  if(is.null(crosswalk_id)) {
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  if (!crosswalk_id %in% names(df)) {
     stop("'crosswalk_id' ", crosswalk_id, " is not a column in input dataframe.")
  } 
  
  if (!"cs_id" %in% names(df)) {
     stop("'cs_id' is not a column in input dataframe. Input dataframe must have a 'cs_id' column to uniquely identfy each cross section within each 'crosswalk_id'")
  } 
  
  if (length(unique(df[[crosswalk_id]])) == 0) {
    stop("'crosswalk_id' ", crosswalk_id, " contains only empty values")
  }
  
  if (length(unique(df$cs_id)) == 0) {
    stop("'cs_id' contains only empty values")
  }
  
  if (any(is.na(df[[crosswalk_id]]))) {
    stop("'crosswalk_id' ", crosswalk_id, " column contains NA values")
  }
  
  if (any(is.na(df$cs_id))) {
    stop("'cs_id' column contains NA values")
  }
  
  # make a dataframe that has a new_cs_id column that has 
  # the cs_id renumbered to fill in any missing IDs,
  # so each hy_id has cs_ids that go from 1 - number of cross sections on hy_id
  # The dataframe below will be used to join the "new_cs_id" with 
  # the original "cs_ids" in the final_pts output data
  renumbered_ids <- 
    df %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      # hy_id, 
      dplyr::any_of(crosswalk_id),
      cs_id, pt_id, cs_measure
      ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::group_by(hy_id, cs_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id)))) %>% 
    # dplyr::group_by(hy_id) %>%
    dplyr::mutate(
      new_cs_id = 1:dplyr::n()
      # tmp_id = paste0(hy_id, "_", cs_id)
    ) %>%
    add_tmp_id(x = get(crosswalk_id)) %>% 
    dplyr::ungroup() %>%
    dplyr::select(new_cs_id, tmp_id)
  
  # Join the new cs_ids back with the final output data to replace the old cs_ids
  df <- dplyr::left_join(
    add_tmp_id(df, x = get(crosswalk_id)),
    # dplyr::mutate(df,tmp_id = paste0(hy_id, "_", cs_id)),
    renumbered_ids,
    by = "tmp_id"
  ) %>%
    dplyr::select(-cs_id, -tmp_id) %>%
    dplyr::rename("cs_id" = "new_cs_id") %>%
    dplyr::relocate(dplyr::any_of(crosswalk_id), cs_id)
    # dplyr::relocate(hy_id, cs_id)
  
  return(df)
}


#' @title Makes a summaru dataframe and print out of differences between 2 cross section points dataframes
#' @description
#' Convenience function for printing out the difference between a cross section point dataframe and 
#' the resulting output of putting that dataframe through the rectify_cs() function
#' 
#' @param input_points sf dataframe or dataframe of cross section points
#' @param output_points sf dataframe or dataframe of cross section points, with "is_extended" logical column
#' @param verbose logical, whether to print out summary message/ Default is TRUE
#'
#' @return dataframe
#' @importFrom dplyr select group_by arrange slice ungroup summarize count
#' @importFrom sf st_drop_geometry
#' @export
rectify_summary <- function(input_points, output_points, verbose = TRUE) {
  
  # drop geometries
  input_points  <- sf::st_drop_geometry(input_points)  
  output_points <- sf::st_drop_geometry(output_points)  
  
  # change in number of hy_ids
  input_number_of_hyids  <- length(unique(input_points$hy_id))
  output_number_of_hyids <- length(unique(output_points$hy_id))
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
    dplyr::select(hy_id, cs_id, pt_id) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::arrange(-pt_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarize(avg_pts_per_cs = round(mean(pt_id), 2)) %>% 
    .$avg_pts_per_cs
  
  output_pts_per_cs <- 
    output_points %>% 
    dplyr::select(hy_id, cs_id, pt_id) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::arrange(-pt_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarize(avg_pts_per_cs = round(mean(pt_id), 2)) %>% 
    .$avg_pts_per_cs
  
  # Extensions counts
  output_extended_counts <-  
    output_points %>%
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::count(is_extended)
  # output_extended_counts <- 
  #   output_points %>%
  #   dplyr::count(is_extended) 

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
#' @export
pct_pts_near_bottom = function(cs_pts, 
                               distance_from_bottom    = 1, 
                               look_only_above_bottom  = TRUE,
                               total_from_bottom_up = FALSE
) {
  
  #
  # cs_pts = cs_pts
  # distance_from_bottom = 1
  # look_only_above_bottom = TRUE
  # look_only_above_bottom = FALSE
  # total_from_bottom_up = FALSE
  #
  
  # Drop geometries to work with tabular data only
  flat_check <- 
    cs_pts  %>% 
    sf::st_drop_geometry()
  
  # classify cross section points and add back point count per cross section column
  flat_check <- 
    flat_check %>% 
    # dplyr::rename(cs_widths = cs_lengthm) %>%
    hydrofabric3D::classify_points()  %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      points_per_cs = dplyr::n()
    ) %>% 
    dplyr::ungroup() %>% 
    sf::st_drop_geometry() 
  # dplyr::relocate(hy_id, cs_id, pt_id, Z, class, points_per_cs)
  
  # # if there is no "class" column, classify the points using classify_points()
  # if (!"class" %in% colnames(cs_pts)) { }
  
  # reorder columns
  flat_check <- dplyr::relocate(flat_check, 
                                hy_id, cs_id, pt_id, Z, class, points_per_cs)
  
  # get the minimum Z value of the bottom points of each cross section and add this as a column to cs_pts
  bottomZ = 
    flat_check  %>% 
    # sf::st_drop_geometry() %>%
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
      # sf::st_drop_geometry() %>%
      # dplyr::filter(hy_id == "wb-2399072", cs_id == 3)  %>% 
      dplyr::group_by(hy_id, cs_id) %>%
      dplyr::mutate(
        lower_bound    = ifelse(look_only_above_bottom, Z_at_bottom, Z_at_bottom - distance_from_bottom),
        upper_bound    = Z_at_bottom + distance_from_bottom,
        is_near_bottom = dplyr::between(
          Z,
          lower_bound,
          upper_bound
        ),
        # pts_near_bottom = sum(dplyr::between(Z, Z_at_bottom - distance_from_bottom, Z_at_bottom + distance_from_bottom)),
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
    # dplyr::relocate(hy_id, cs_id, pt_id, Z, class, Z_at_bottom, is_near_bottom, pts_near_bottom, pct_near_bottom)
    # dplyr::relocate(geometry_colname, .after = dplyr::last_col())
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
#' @export
pts_to_reevaluate <- function(
    cs_pts, 
    threshold = 1, 
    pct_threshold = 0.99
) {
  
  #
  # cs_pts = cs_pts
  # threshold = 1
  # pct_threshold = 0.99
  #
  
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
    # to_check %>% 
    dplyr::mutate(
      diff_pts = total_valid_pts - pts_near_bottom
    ) %>% 
    dplyr::filter(pct_near_bottom >= pct_threshold | diff_pts == 1) %>%
    dplyr::select(-diff_pts) %>% 
    # dplyr::filter(pct_near_bottom >= pct_threshold) %>%
    # dplyr::relocate(pts_near_bottom, total_valid_pts, pct_near_bottom) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() 
  # dplyr::select(-is_near_bottom, -Z_at_bottom, -pts_near_bottom, -pct_near_bottom, -lower_bound, -upper_bound)
  # dplyr::select(hy_id, cs_id, Z_at_bottom, pts_near_bottom, pct_near_bottom, lower_bound, upper_bound)
  
  return(near_bottom)
  
}

#' Check and fix cross section points with limited variation in Z values (version 2 latest)
#' This function takes in a set of cross section points (cs_pts), a flowline network (net) and a set of transects lines for that flowline network (cs).
#' The function that looks at the cross section points and identifies cross sections that are "flat" 
#' (have a percent of all points in the cross section within a threshold distance from the bottom of the cross section).
#' The transect lines that generated the "flat" cross section points are then extended and new points are extracted
#' along this new longer transect line. The newly extracted points are checked for "flatness" and are removed if they are still "flat", otherwise the original dataset 
#' of points is updated with the new set of point derived from an extended transect line.
#' Improved function for rectifying cross section points with flat Z values by extending transect lines and reevaluating the new DEM values.
#' @param cs_pts Output from extract_dem_values_first
#' @param net Hydrographic LINESTRING Network
#' @param cs character, Hydrographic LINESTRING Network file path
#' @param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
#' @param min_pts_per_cs Minimun number of points per cross section required.
#' @param dem the DEM to extract data from
#' @param scale numeric, If a transect line DEM extraction results in all equal Z values,
#'  by what percent of the transect lines length (meters) should the transect line be
#'   extended in both directions to try to capture representative Z values ? Default is 0.5 (50% of the transect length)
#' @param threshold numeric, threshold Z value (meters) that determines if a cross section is flat. 
#' A threshold = 0 means if all Z values are the same, then the cross section is considered flat. 
#' A threshold value of 1 means that any cross section with Z values all within 1 meter of eachother, is considered flat. Default is 0.
#' @param pct_threshold numeric, threshold percent of points in the cross section that are within threshold of bottom to 
#' determine whether point should be considered for re evaluation. Default is 0.99 (i.e. 99% of points are near the bottom)
#' @param fix_ids logical, whether to reenumerate the "cs_id" column to 
#' make sure cross sections are number 1 - number of total cross sections on flowline.  Default is FALSE, cs_id will be kept as 
#' they were in the input data and may contain gaps between cs_ids within a flowline (hy_id). 
#' WARNING: Setting fix_ids = TRUE may result in input cross section points (cs_pts) having DIFFERENT cs_id values as the input transects (cs) 
#' and the inconsistency can cause problems when trying to cross walk between the datasets in the future.
#' @importFrom dplyr mutate relocate last_col select rename left_join group_by ungroup slice n bind_rows filter
#' @importFrom sf st_drop_geometry
#' @importFrom nhdplusTools rename_geometry
#' @return sf object of cs_pts with "flat" cross sections removed/updated with longer transects to capture more Z data
#' @export
rectify_flat_cs = function(
    cs_pts         = NULL,   
    net            = NULL,
    cs             = NULL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
    scale          = 0.5,
    threshold      = 0,
    pct_threshold  = 0.99,
    fix_ids        = FALSE
) {
  
  # net            = flines2
  # cs             = transects2
  # cs_pts         = cs_pts2
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = DEM_URL
  # scale          = 0.5
  # threshold      = 1
  # pct_threshold  = 0.99
  
  # add a "tmp_id" column to easily index transects by hy_id and cs_id 
  cs <- hydrofabric3D::add_tmp_id(cs)
  # cs <- dplyr::mutate(cs,
  #                     tmp_id = paste0(hy_id, "_", cs_id)
  # ) 
  
  ### ### ## ## ### ## ### ##  ### ### ## ## ### ## ### ##
  message("Determining points to reevaluate...")
  # logger::log_info("Determining points to reevaluate...")
  
  # Check if any cross sections are "flat" within a threshold (All Z values are the same or the difference is within the threshold)
  flat_cs <- pts_to_reevaluate(cs_pts        = cs_pts, 
                               threshold     = threshold,
                               pct_threshold = pct_threshold
  )
  
  # if there are no flatlines, return the cs_pts object
  if (nrow(flat_cs) == 0) {
    
    cs_pts <- 
      cs_pts %>% 
      dplyr::mutate(
        is_extended = FALSE
      ) %>% 
      dplyr::relocate(geom, .after = dplyr::last_col())
    
    return(cs_pts)
  }
  
  # subset transects (cs) to the flat cross sections in flat_cs
  to_extend <- 
    cs %>% 
    # dplyr::mutate(# tmp_id = paste0(hy_id, "_", cs_id)is_extended = FALSE) %>%
    dplyr::filter(tmp_id %in% unique(dplyr::mutate(flat_cs, # Filter the cross sections ("cs") for any cross sections that were decided to be flat/needing reevaluation
                                                   tmp_id = paste0(hy_id, "_", cs_id))$tmp_id) 
    ) %>%
    dplyr::select(-tmp_id) 
  
  # 1. Loop through geometries that might need to be extended, 
  # 2. Try to EXTEND, 
  # 3. and then UPDATE --> (only IF the extended transect does NOT violate any of the intersection rules)
  # If ALL of the below intersection conditions are TRUE then a given extended transect line will get replace the old transect geometry 
  # Intersection rules: 
  # - Newly extended transect intersects with its flowlines AT MOST 1 time
  # - Newly extended transect does NOT intersect with any of the other NEWLY EXTENDED transect lines
  # - Newly extended transect does NOT intersect with any of the ORIGINAL transect lines
  # extend_transects() returns the "to_extend" object with updated attributes for any extensions that were made (geometries, cs_lengthm, "is_extended" flag) 
  extended_geoms <- extend_transects(
    transects_to_extend = to_extend,
    transects           = cs, 
    net                 = net, 
    scale               = scale
  )
  
  # TODO: 
  # # Probably can just drop any "is_extended" == FALSE because 
  # # these were cross sections that yield FLAT points
  # # AND they CAN'T be extended according to extend_transects()
  # hopeless <- dplyr::filter(extended_geoms, !is_extended)
  
  # Store unextendable transects for filtering out later on 
  # (these are transects that were flat AND could NOT be extended without violating an intersection rule)
  unextendable <- dplyr::filter(extended_geoms, !is_extended)
  
  # Remove unextendable transects from extended_geoms 
  extended_geoms <- dplyr::filter(extended_geoms, is_extended)
  
  message("Attempted extensions: ", nrow(to_extend))
  message("- FAILED extensions: ", nrow(unextendable))
  message("- SUCCESSFUL extensions: ", nrow(extended_geoms))
  message("Adding points per cross section...")
  
  # add cross section points to extended cross sections
  extended_geoms <- add_points_per_cs(
    cs             = extended_geoms,
    # cs             = to_extend,
    # cs             = dplyr::slice(extended_geoms , 1:100),
    points_per_cs  = points_per_cs,
    min_pts_per_cs = min_pts_per_cs,
    dem            = dem
  )
  
  
  message("Extracting new DEM values..")
  
  # extract DEM values for newly extended cross sections
  extended_pts <- extract_dem_values(cs = extended_geoms, dem = dem)
  
  # add a tmp_id for joining and filtering 
  extended_pts <- add_tmp_id(extended_pts)
  # extended_pts <- dplyr::mutate(
  #                     extended_pts, 
  #                     tmp_id = paste0(hy_id, "_", cs_id)
  #                   ) 
  
  message("Double checking new extended cross section DEM values for flatness")
  
  # Check the new extended_pts cross section points for any "flat" set of points
  second_flat_check <- pts_to_reevaluate(
    cs_pts        = extended_pts, 
    threshold     = threshold,
    pct_threshold = pct_threshold
  )
  
  
  # add a tmp_id column to second_flat_check to filter out any set of cross section points 
  # that are STILL flat after extending the transect lines
  second_flat_check <- add_tmp_id(second_flat_check)
  # second_flat_check <- dplyr::mutate(
  #                         second_flat_check, 
  #                         tmp_id = paste0(hy_id, "_", cs_id)
  #                       ) 
  
  # take the below points, and put them back into "cs_pts" object
  # then go back to the input "transects" ("cs") object and update the transect geometries based on the extensions done above^^
  # then resave the input transects dataset back to its original location....
  
  # separate newly extended cross sections with new Z values into groups (those that show "good" DEM values after extension are kept) 
  to_keep <- dplyr::filter(extended_pts, !tmp_id %in% unique(second_flat_check$tmp_id))
  to_drop <- dplyr::filter(extended_pts, tmp_id %in% unique(second_flat_check$tmp_id))
  
  message("Count of extended cross sections POINTS to KEEP: ", nrow(to_keep))
  message("Count of extended cross sections POINTS to DROP: ", nrow(to_drop))
  
  # filter out cross section points that have "same Z" values (remove flat Z values)
  final_pts <-
    cs_pts %>%  
    # dplyr::mutate(
    #   tmp_id = paste0(hy_id, "_", cs_id)
    # ) %>% 
    add_tmp_id() %>% 
    dplyr::filter(
      !tmp_id %in% unique(
        dplyr::mutate(
          unextendable,
          tmp_id = paste0(hy_id, "_", cs_id)
        )$tmp_id)
      # !tmp_id %in% unique(to_drop$tmp_id)
    ) %>% 
    dplyr::filter(
      !tmp_id %in% unique(to_drop$tmp_id)
    ) 
  
  # remove the old versions of the "to_keep" cross section points and 
  # replace them with the updated cross section points with the extended "cs_lengthm" and "Z" values
  final_pts <-
    final_pts %>%
    dplyr::filter(
      !tmp_id %in% unique(to_keep$tmp_id)
      # !tmp_id %in% unique(extended_pts$tmp_id)
    ) %>% 
    dplyr::mutate(
      is_extended = FALSE
    ) %>% 
    dplyr::bind_rows(
      to_keep
    ) %>% 
    dplyr::select(-tmp_id) 
  
  # rename geometry column to "geom" 
  final_pts <- nhdplusTools::rename_geometry(final_pts, "geom")
  
  # If TRUE then the cs_ids are renumbered to make sure each hy_id has cross sections
  # that are numbered (1 - number of cross sections) on the hy_id
  if (fix_ids) {
    
    message("Renumbering cross section IDs...")
    
    # make a dataframe that has a new_cs_id column that has 
    # the cs_id renumbered to fill in any missing IDs,
    # so each hy_id has cs_ids that go from 1 - number of cross sections on hy_id
    # The dataframe below will be used to join the "new_cs_id" with 
    # the original "cs_ids" in the final_pts output data
    renumbered_ids <-
      final_pts %>% 
      sf::st_drop_geometry() %>% 
      # dplyr::filter(hy_id %in% c("wb-2402800", "wb-2398282", "wb-2400351")) %>%
      dplyr::select(hy_id, cs_id, pt_id, cs_measure) %>% 
      dplyr::group_by(hy_id, cs_id) %>% 
      dplyr::slice(1) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(hy_id) %>% 
      dplyr::mutate(
        new_cs_id = 1:dplyr::n(),
        tmp_id    = paste0(hy_id, "_", cs_id)
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(new_cs_id, tmp_id)
    
    # Join the new cs_ids back with the final output data to replace the old cs_ids
    final_pts <- dplyr::left_join(
      dplyr::mutate(
        final_pts,
        tmp_id = paste0(hy_id, "_", cs_id)
      ),
      renumbered_ids,
      by = "tmp_id"
    ) %>% 
      dplyr::select(-cs_id, -tmp_id) %>% 
      dplyr::rename("cs_id" = "new_cs_id") %>% 
      dplyr::relocate(hy_id, cs_id)
  }
  
  # move geom column to the last column
  final_pts <- dplyr::relocate(final_pts, geom, .after = dplyr::last_col())
  
  message("TOTAL # of transects EVALUATED > ",  nrow(to_extend))
  message("# of transects that are INVALID after extension > ",  nrow(unextendable))
  message("# of transects KEPT after extension > ",  length(unique(to_keep$tmp_id)))
  message("# of transects REMOVED after extension >", length(unique(to_drop$tmp_id)))
  message("INVALID + KEPT + REMOVED = ", 
          nrow(unextendable), " + ", length(unique(to_keep$tmp_id)), " + ", length(unique(to_drop$tmp_id)), 
          " = ",
          nrow(unextendable) +  length(unique(to_keep$tmp_id)) +  length(unique(to_drop$tmp_id))
  )
  message("Start # of cross section points > ",  length(unique(dplyr::mutate(cs_pts, tmp_id = paste0(hy_id, '_', cs_id, '_',pt_id))$tmp_id)))
  message("End # of cross section points > ",   length(unique(dplyr::mutate(final_pts, tmp_id = paste0(hy_id, '_', cs_id, '_',pt_id))$tmp_id)))
  message("INPUT # of unique hy_id/cs_id cross section points > ",  length(unique(dplyr::mutate(cs_pts, tmp_id = paste0(hy_id, '_', cs_id))$tmp_id)))
  message("OUTPUT # of unique hy_id/cs_id cross section points > ",  length(unique(dplyr::mutate(final_pts, tmp_id = paste0(hy_id, '_', cs_id))$tmp_id)))
  
  # final_pts$is_extended %>% table()
  
  return(final_pts)
}


#Check for flat cross sections and try to update these values by extending the original cross sections and reextracting DEM values
#(Deprecated version 1)
#@param cs_pts Output from extract_dem_values_first
#@param net Hydrographic LINESTRING Network
#@param cs character, Hydrographic LINESTRING Network file path
#@param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
#@param min_pts_per_cs Minimun number of points per cross section required.
#@param dem the DEM to extract data from
#@param scale numeric, If a transect line DEM extraction results in all equal Z values,
# by what percent of the transect lines length (meters) should the transect line be
#  extended in both directions to try to capture representative Z values ? Default is 0.5 (50% of the transect length)
#@param threshold numeric, threshold Z value (meters) that determines if a cross section is flat. 
#A threshold = 0 means if all Z values are the same, then the cross section is considered flat. 
#A threshold value of 1 means that any cross section with Z values all within 1 meter of eachother, is considered flat. Default is 0.
#@importFrom dplyr mutate relocate last_col group_by ungroup n select everything relocate last_col bind_rows filter
#@importFrom sf st_intersection st_is st_intersects
#@importFrom nhdplusTools rename_geometry
#@return sf object of cs_pts with "flat" cross sections removed/updated with longer transects to capture more Z data
# rectify_flat_cs_v1 = function(
#     cs_pts         = NULL, 
#     net            = NULL,
#     cs             = NULL,
#     points_per_cs  = NULL,
#     min_pts_per_cs = 10,
#     dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
#     scale          = 0.5,
#     threshold      = 0
# ) {
#   
#   ### ### ## ## ### ## ### ##
#   ## ### ### ### ### #### ##
#   
#   # add a "tmp_id" column to easily index transects by hy_id and cs_id 
#   cs <- dplyr::mutate(cs,
#                       tmp_id = paste0(hy_id, "_", cs_id)
#   ) 
#   
#   # Check if any cross sections are "flat" within a threshold (All Z values are the same or the difference is within the threshold)
#   flat_cs <- check_z_values(pts = cs_pts, threshold = threshold)
#   
#   # if there are no flatlines, return the cs_pts object
#   if (nrow(flat_cs) == 0) {
#     
#     cs_pts <- 
#       cs_pts %>% 
#       dplyr::mutate(
#         is_extended = FALSE
#       ) %>% 
#       dplyr::relocate(geom, .after = dplyr::last_col())
#     
#     return(cs_pts)
#   }
#   
#   # subset transects (cs) to the flat cross sections in flat_cs
#   to_extend <- 
#     cs %>% 
#     # dplyr::mutate(
#     #   # tmp_id = paste0(hy_id, "_", cs_id)
#     #   is_extended = FALSE
#     # ) %>%
#     dplyr::filter(tmp_id %in% unique(
#       dplyr::mutate(flat_cs,
#                     tmp_id = paste0(hy_id, "_", cs_id))$tmp_id
#     )) %>%
#     dplyr::select(-tmp_id) 
#   # dplyr::relocate(geom, .after = dplyr::last_col())
#   
#   # loop through geometries that might need to be extended, try to extend, and then update 
#   # the 'to_extend' values IF the extended transectr does NOT violate any intersection rules
#   for(i in 1:nrow(to_extend)) {
#     # message("i: ", i)
#     # extend transect out by "scale" % of lines length
#     extended_tran <- extend_by_percent(
#       x          = to_extend[i, ],
#       pct        = scale, 
#       length_col = "cs_lengthm"
#     )
#     
#     # filter down to the rest of the transects on the given "hy_id", EXCLUDING SELF
#     neighbor_transects <- dplyr::filter(cs, 
#                                         hy_id == to_extend[i, ]$hy_id,
#                                         cs_id != to_extend[i, ]$cs_id
#     )
#     
#     # # filter down to ALL OF THE OTHER TRANSECTS (EXCEPT SELF) 
#     # neighbor_transects <- dplyr::filter(cs, tmp_id != to_extend[i, ]$tmp_id)
#     
#     # Make sure that newly extended line only interesects its origin flowline at MOST 1 time
#     # AND that the newly extended transect does NOT intersect with any previously computed transect lines
#     
#     fline_intersect <- sf::st_intersection(
#       extended_tran,
#       net[net$id == to_extend[i, ]$hy_id, ]
#       # dplyr::filter(net, id == to_extend[i, ]$hy_id)
#     )
#     
#     if(nrow(fline_intersect) > 0) {
#       
#       # Check that newly extended cross section only interesects its origin flowline at MOST 1 time (This value will be a "MULTIPOINT" if it intersects more than once)
#       if (
#         sf::st_is(
#           fline_intersect, "POINT"
#         ) &&
#         # Check that extended transect doesn't intersect with any of the NEWLY EXTENDED cross sections
#         !any(sf::st_intersects(
#           extended_tran,
#           to_extend[-i, ],
#           sparse = FALSE
#         )) &&
#         # Check that extended transect doesn't intersect with any of the original cross sections on this "hy_id"
#         !any(sf::st_intersects(
#           extended_tran,
#           neighbor_transects,
#           sparse = FALSE
#         ))
#       ) {
#         
#         # # set is_extended to TRUE
#         # extended_tran$is_extended <- TRUE
#         
#         # replace old transect with extended geometry and updated lengths, etc.
#         to_extend[i, ] <- extended_tran
#         
#       }
#     }
#     # message("=========")
#   }
#   
#   # # extend linestring geometries by a percent of linestring length
#   # extended <- extend_by_percent(x = to_extend, pct = scale, length_col = "cs_lengthm")
#   
#   # add cross section points to extended cross sections
#   extended <- add_points_per_cs(
#     cs             = to_extend,
#     points_per_cs  = points_per_cs,
#     min_pts_per_cs = min_pts_per_cs,
#     dem            = dem
#   )
#   
#   # extract DEM values for newly extended cross sections
#   extended_pts <- extract_dem_values(cs = extended, dem = dem)
#   
#   # take the below points, and put them back into "cs_pts" object
#   # then go back to the input "transects" ("cs") object and update the transect geometries based on the extensions done above^^
#   # then resave the input transects dataset back to its original location....
#   extended_pts <- 
#     extended_pts %>% 
#     dplyr::group_by(hy_id, cs_id) %>% 
#     dplyr::mutate(
#       is_same_Z = max(Z) - min(Z) <= threshold
#     ) %>% 
#     dplyr::ungroup() %>%    
#     dplyr::mutate(
#       tmp_id = paste0(hy_id, "_", cs_id)
#     )
#   
#   # separate newly extended cross sections with new Z values into groups (those that show "good" DEM values after extension are kept) 
#   to_keep <- dplyr::filter(extended_pts, !is_same_Z)
#   to_drop <- dplyr::filter(extended_pts, is_same_Z)
#   
#   # filter out cross section points that have "same Z" values (remove flat Z values)
#   final_pts <-
#     cs_pts %>%  
#     dplyr::mutate(
#       tmp_id = paste0(hy_id, "_", cs_id)
#     ) %>% 
#     dplyr::filter(
#       !tmp_id %in% unique(to_drop$tmp_id)
#     ) 
#   
#   # remove the old versions of the "to_keep" cross section points and 
#   # replace them with the updated cross section points with the extended "cs_lengthm" and "Z" values
#   final_pts <-
#     final_pts %>%
#     dplyr::filter(
#       !tmp_id %in% unique(to_keep$tmp_id)
#     ) %>% 
#     dplyr::mutate(
#       is_extended = FALSE
#     ) %>% 
#     dplyr::bind_rows(
#       dplyr::select(
#         dplyr::mutate(
#           to_keep,
#           is_extended = TRUE
#         ), 
#         -is_same_Z)
#     ) %>% 
#     dplyr::select(-tmp_id) 
#   
#   # rename geometry column to "geom" 
#   final_pts <- nhdplusTools::rename_geometry(final_pts, "geom")
#   
#   # move geom column to the last column
#   final_pts <- dplyr::relocate(final_pts, geom, .after = dplyr::last_col())
#   
#   # final_pts$is_extended %>% table()
#   
#   return(final_pts)
# }

#Check for any Z values that are all equal or within a given threshold value
#@param pts sf points dataframe
#@param threshold numeric, default is 1 meter
#@importFrom dplyr select group_by mutate filter slice ungroup
#@importFrom sf st_drop_geometry st_line_sample st_cast
#@return dataframe with hy_id, cs_id, Z, and is_same_Z value columns
# check_z_values <- function(pts, threshold = 1) {
#   
#   # check for any flat cross sections (All Z values are equal within a given cross section)
#   flat_pts <-
#     pts %>% 
#     sf::st_drop_geometry() %>% 
#     dplyr::select(hy_id, cs_id, Z) %>%
#     # dplyr::filter(hy_id != "wb-2959") %>%
#     # dplyr::filter(!hy_id %in% c("wb-2959", "wb-2960", "wb-4131", "wb-4364", "wb-4365", "wb-4770")) %>%
#     dplyr::group_by(hy_id, cs_id) %>% 
#     dplyr::mutate(
#       is_same_Z = max(Z) - min(Z) <= threshold
#       # is_same_Z = as.integer(dplyr::n_distinct(Z) == 1)
#     ) %>%
#     dplyr::filter(is_same_Z) %>%
#     # dplyr::filter(is_same_Z == 1) %>%
#     dplyr::slice(1) %>%
#     dplyr::ungroup() 
#   
#   return(flat_pts)
# }


#Calculate percentage of points within a set of cross section points that are near the bottom of the cross section 
#Adds the following columns: 
#is_near_bottom: state whether a point is near the bottom of the cross section (within a specified distance threshold of the bottom), 
#pts_near_bottom: count of points near the bottom of the cross section
#pct_near_bottom: percent of points near the bottom of the cross section
#@param cs_pts sf dataframe of cross section points (output of cross_section_pts() function)
#@param distance_from_bottom numeric, distance threshold (in meters) to determine if a point is near the bottom of the cross section
#@param look_only_above_bottom logical, whether to look only at points ABOVE the channel bottom as points that can be classified as "near bottom". 
# Default is TRUE, meaning only points that are between Z and Z + distance_from_bottom are classified as "near bottom" 
# If FALSE, then points at Z values BELOW the bottom (Z - distance_from_bottom) AND 
# points at Z values ABOVE the bottom (Z + distance_from_bottom) are classified as 
# "near bottom" if they are within the range BELOW OR ABOVE the bottom.
#@param total_from_bottom_up logical, whether to use only points ABOVE bottom points as part of total points for calculating percentage of points near bottom. Default is FALSE and ALL points will be used when calculating percentage, even if a point has a Z value BELOW the bottom, but is NOT classified as a bottom point
#@importFrom dplyr group_by mutate ungroup relocate filter summarize left_join between case_when select all_of last_col
#@importFrom sf st_drop_geometry
#@return sf dataframe of cross section points with the added columns described above 
#@export
# pct_pts_near_bottom = function(cs_pts, 
#                                distance_from_bottom    = 1, 
#                                look_only_above_bottom  = TRUE,
#                                total_from_bottom_up = FALSE
# ) {
#   
#   #
#   # cs_pts = cs_pts
#   # distance_from_bottom = 1
#   # look_only_above_bottom = TRUE
#   # look_only_above_bottom = FALSE
#   # total_from_bottom_up = FALSE
#   #
#   
#   # Drop geometries to work with tabular data only
#   flat_check <- 
#     cs_pts  %>% 
#     sf::st_drop_geometry()
#   
#   # classify cross section points and add back point count per cross section column
#   flat_check <- 
#     flat_check %>% 
#     # dplyr::rename(cs_widths = cs_lengthm) %>%
#     hydrofabric3D::classify_points()  %>% 
#     dplyr::group_by(hy_id, cs_id) %>% 
#     dplyr::mutate(
#       points_per_cs = dplyr::n()
#     ) %>% 
#     dplyr::ungroup() %>% 
#     sf::st_drop_geometry() 
#   # dplyr::relocate(hy_id, cs_id, pt_id, Z, class, points_per_cs)
#   
#   # # if there is no "class" column, classify the points using classify_points()
#   # if (!"class" %in% colnames(cs_pts)) { }
#   
#   # reorder columns
#   flat_check <- dplyr::relocate(flat_check, 
#                                 hy_id, cs_id, pt_id, Z, class, points_per_cs)
#   
#   # get the minimum Z value of the bottom points of each cross section and add this as a column to cs_pts
#   bottomZ = 
#     flat_check  %>% 
#     # sf::st_drop_geometry() %>%
#     dplyr::group_by(hy_id, cs_id) %>%
#     dplyr::filter(class == "bottom") %>%
#     dplyr::summarize(
#       Z_at_bottom = min(Z)
#     )  %>% 
#     dplyr::ungroup() 
#   
#   # join the flat_check dataframe with the dataframe containing the Z values of the bottom depths for each cross section
#   bottom_pct =
#     flat_check  %>% 
#     dplyr::left_join(
#       bottomZ,
#       by = c("hy_id", "cs_id")
#     ) 
#   
#   # TODO: This code could be shortened and combined with the ELSE clause, just being lazy right now
#   if(total_from_bottom_up) {
#     # When calculating the percentage, use only points that are GREATER THAN OR EQUAL to the bottom Z as part of percentage calculation.
#     bottom_pct <- 
#       bottom_pct %>%
#       dplyr::group_by(hy_id, cs_id) %>% 
#       dplyr::mutate(
#         lower_bound    = ifelse(look_only_above_bottom, Z_at_bottom, Z_at_bottom - distance_from_bottom),
#         upper_bound    = Z_at_bottom + distance_from_bottom,
#         is_near_bottom = dplyr::between(
#           Z,
#           lower_bound,
#           upper_bound
#         ),
#         ge_bottom   = dplyr::case_when(
#           Z     >= Z_at_bottom ~ TRUE,
#           TRUE                 ~ FALSE
#         ),
#         total_valid_pts = sum(ge_bottom),
#         pts_near_bottom = sum(is_near_bottom),
#         pct_near_bottom = pts_near_bottom/total_valid_pts,
#         tmp_id          = paste0(hy_id, "_", cs_id, "_", pt_id)
#       )  %>% 
#       dplyr::ungroup()  %>% 
#       dplyr::select(tmp_id, class, Z_at_bottom, is_near_bottom, pts_near_bottom, pct_near_bottom, lower_bound, upper_bound, total_valid_pts) 
#   } else {
#     
#     # Given the Z value of each point, and the Z value of the bottom points ("Z_at_bottom"),
#     #  determine if each point is near the bottom 
#     # If the Z value for a given point is between the lower_bound and upper_bound, then the the point is determined to be "is_near_bottom"
#     # If look_only_above_bottom is TRUE, then the lower_bound is the Z value at the bottom points (Z_at_bottom), otherwise 
#     # If look_only_above_bottom is FALSE, then the lower_bound is the Z value at the bottom points (Z_at_bottom) minus distance_from_bottom (Z_at_bottom - distance_from_bottom)
#     bottom_pct <- 
#       bottom_pct %>%
#       # sf::st_drop_geometry() %>%
#       # dplyr::filter(hy_id == "wb-2399072", cs_id == 3)  %>% 
#       dplyr::group_by(hy_id, cs_id) %>%
#       dplyr::mutate(
#         lower_bound    = ifelse(look_only_above_bottom, Z_at_bottom, Z_at_bottom - distance_from_bottom),
#         upper_bound    = Z_at_bottom + distance_from_bottom,
#         is_near_bottom = dplyr::between(
#           Z,
#           lower_bound,
#           upper_bound
#         ),
#         # pts_near_bottom = sum(dplyr::between(Z, Z_at_bottom - distance_from_bottom, Z_at_bottom + distance_from_bottom)),
#         pts_near_bottom = sum(is_near_bottom),
#         pct_near_bottom = pts_near_bottom/points_per_cs,
#         tmp_id          = paste0(hy_id, "_", cs_id, "_", pt_id)
#       )  %>% 
#       dplyr::ungroup()  %>% 
#       dplyr::select(
#         tmp_id, class, Z_at_bottom,
#         is_near_bottom, pts_near_bottom, pct_near_bottom, 
#         lower_bound, upper_bound, 
#         total_valid_pts = points_per_cs
#       ) 
#   }
#   
#   # join bottom points percent table to cs_pts
#   cs_pts <- dplyr::left_join(
#     dplyr::mutate(
#       cs_pts,
#       tmp_id = paste0(hy_id, "_", cs_id, "_", pt_id)
#     ),
#     bottom_pct,
#     by = "tmp_id"
#   )  %>% 
#     dplyr::select(-tmp_id)
#   
#   # get the sf geometryt column name
#   geometry_colname <- names(cs_pts)[sapply(cs_pts, function(col) any( 
#     class(col) %in% c("sfc_POINT", "sfc", 
#                       "sfc_GEOMETRY", "sfc_MULTIPOINT")))
#   ]
#   
#   # move the geometry column to the end of the dataframe
#   cs_pts <- 
#     cs_pts %>% 
#     # dplyr::relocate(hy_id, cs_id, pt_id, Z, class, Z_at_bottom, is_near_bottom, pts_near_bottom, pct_near_bottom)
#     # dplyr::relocate(geometry_colname, .after = dplyr::last_col())
#     dplyr::relocate(dplyr::all_of(geometry_colname), .after = dplyr::last_col())
#   
#   return(cs_pts)
#   
# }

#Get a dataframe of points that should be evaluated due to their proximity (nearness in Z distance) to the bottom
#@param cs_pts dataframe/sf dataframe of cross section points (requires hy_id, cs_id, and Z values)
#@param threshold numeric, threshold distance in meters for points to be considered "near the bottom". Default is 1 meter (i.e. check if points are within 1 meter above the bottom)
#@param pct_threshold numeric, threshold percent of points in the cross section that are within threshold of bottom to 
#determine whether point should be considered for re evaluation. Default is 0.99 (i.e. 99% of points are near the bottom). Default is 0.99 (i.e. 99&%).
#@return dataframe with the hy_id, cs_id, pts_near_bottom (count of pts_near_bottom), and pct_near_bottom (% of points in cross section that are near bottom). 
#An empty dataframe is returned if ZERO points are classified as "near the bottom"
#@importFrom dplyr mutate filter select group_by slice ungroup
#@importFrom sf st_drop_geometry
#@export
# pts_to_reevaluate <- function(
#     cs_pts, 
#     threshold = 1, 
#     pct_threshold = 0.99
# ) {
#   
#   #
#   # cs_pts = cs_pts
#   # threshold = 1
#   # pct_threshold = 0.99
#   #
#   
#   # Determine which points that are within "threshold" meters from the bottom 
#   # (only looking at points above threshold, ignoring any points that are BELOW Z)
#   # So the "pct_pts_near_bottom()" function adds columns to the "cs_pts" input data that detail which points are "near" the bottom points.
#   # "bottom" points are classified via hydrofabric3D::classify_pts()
#   near_bottom <-
#     cs_pts %>% 
#     pct_pts_near_bottom(
#       distance_from_bottom   = threshold, 
#       look_only_above_bottom = TRUE,
#       total_from_bottom_up   = FALSE
#     )
#   
#   # Determine which points should be re evaluated (by extending) because most of the points are all "near the bottom"
#   # Filter the "near_bottom" dataframe to only cross sections that 
#   # have a percent of all of the cross sections points that are GREATER THAN OR EQUAL to "pct_threshold"
#   
#   # In simple words, get the cross sections that have, for example, 80% of its points that are "near the bottom" 
#   
#   # Also filter cross sections that have only a SINGLE point that is NOT near the bottom:
#   # -----> So if a cross section has 9/10 of its points near the bottom, 
#   #         that means only a single point is NOT near the bottom and thus 
#   #         that cross section should be kept for FURTHER EVALUATION
#   near_bottom <- 
#     near_bottom %>% 
#     sf::st_drop_geometry() %>% 
#     # to_check %>% 
#     dplyr::mutate(
#       diff_pts = total_valid_pts - pts_near_bottom
#     ) %>% 
#     dplyr::filter(pct_near_bottom >= pct_threshold | diff_pts == 1) %>%
#     dplyr::select(-diff_pts) %>% 
#     # dplyr::filter(pct_near_bottom >= pct_threshold) %>%
#     # dplyr::relocate(pts_near_bottom, total_valid_pts, pct_near_bottom) %>% 
#     dplyr::group_by(hy_id, cs_id) %>% 
#     dplyr::slice(1) %>% 
#     dplyr::ungroup() 
#   # dplyr::select(-is_near_bottom, -Z_at_bottom, -pts_near_bottom, -pct_near_bottom, -lower_bound, -upper_bound)
#   # dplyr::select(hy_id, cs_id, Z_at_bottom, pts_near_bottom, pct_near_bottom, lower_bound, upper_bound)
#   
#   return(near_bottom)
#   
# }