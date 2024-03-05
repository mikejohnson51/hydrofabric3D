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
    "make_geoms_to_cut_plot"
  )
)

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