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
# system.time({
#   
# improved_cs <- get_improved_cs_pts(
#                     cs_pts         = cs_pts,    # cross section points generated from hydrofabric3D::cross_section_pts()
#                     net            = net,    # original flowline network
#                     # net            = flines,    # original flowline network
#                     transects      = transects, # original transect lines
#                     points_per_cs  = NULL, 
#                     min_pts_per_cs = 10, # number of points per cross sections
#                     dem            =  "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt", # DEM to extract points from
#                     scale          = 0.5, # How far to extend transects if the points need to be rechecked
#                     pct_of_length_for_relief = 0.01, # percent of cross sections length to be needed in relief calculation to consider cross section to "have relief"
#                     fix_ids = FALSE,
#                     crosswalk_id   = "hy_id",
#                     verbose = TRUE
#                     )
# 
# })

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
  cs_pts    <- nhdplusTools::rename_geometry(cs_pts, "geometry")
  net       <- nhdplusTools::rename_geometry(net, "geometry")
  transects <- nhdplusTools::rename_geometry(transects, "geometry")
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
