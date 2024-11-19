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
    "new_cs_lengthm", 
    "crosswalk_id", "extend_invalid_transects2",
    "anchors", "deriv_type", "edge", "extension_distance", 
    "left_is_extended", "right_is_extended", "to_node", "verbose", 
    "toindid", "indid", "toid", "is", "internal_is_braided2"
  )
)

#' Calculate sinuosity between cross sections on flowlines by
#' 
#' @param flowlines sf linestring geometry of flowlines with a unique "hy_id" column
#' @param transects sf linestring dataframe with a "hy_id" unique identifier that maps to a linestring geometry in 'flowlines'. Must contain a "cs_id" column to uniquely identify cross sections in each hy_id and the order on the given hy_id.
#'  Also must include a "cs_measure" column, which indicates the percent downstream the cross section linestring is along the linestring in 'flowlines'.
#' @param crosswalk_id character, name of the unique identifier column in 'flowlines' and 'transects' 
#' @param add logical, whether to add the sinuosity values to the original cross section dataset (TRUE). Default is TRUE. If FALSE, a dataframe with hy_id, cs_id, cs_measure, and sinuosity columns is returned
#'
#' @noRd
#' @keywords internal
#' @return sf dataframe containing the transects dataset with an added sinuosity column (add = TRUE), or a dataframe with with hy_id, cs_id, cs_measure, and sinuosity columns with values for each linestring in 'transects' 
#' @importFrom dplyr select group_by ungroup relocate mutate bind_rows lead filter left_join
#' @importFrom sf st_geometry st_distance st_length st_centroid st_drop_geometry
#' @importFrom nhdplusTools get_node
get_cs_sinuosity <- function(
    flowlines, 
    transects, 
    crosswalk_id = "hydrofabric_id",
    add = TRUE
) {
  
  # flowlines          = net
  # transects = transects
  # crosswalk_id   = "hydrofabric_id"
  # add            = TRUE
  
  # convert cross section linestrings into points at the centroid of each cross section
  pts <- 
    transects %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, cs_measure, ds_distance, geometry) %>% 
    # dplyr::select(hy_id, cs_id, cs_measure, ds_distance, geometry) %>% 
    sf::st_centroid()
  
  # # plot(pts$geometry)
  # plot(start$geometry, col = "green", add= T)
  # plot(end$geometry, col = "red", add= T)
  # plot(dplyr::slice(cs_lines, 3)$geometry, col = "red", add= T)
  # plot(cs_lines$geometry, col = "red", add= T)
  # plot(flowlines$geometry, add= T)
  
  # calculate line lengths
  flowlines <- 
    flowlines %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), geometry) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
    # dplyr::select(hy_id, geometry) %>% 
    # dplyr::group_by(hy_id) %>% 
    dplyr::mutate(
      ds_distance = as.numeric(sf::st_length(geometry)),
      cs_id       = "end",
      cs_measure  = 100
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::relocate(dplyr::any_of(crosswalk_id), cs_id, cs_measure, ds_distance, geometry)
  # dplyr::relocate(hy_id, cs_id, cs_measure, ds_distance, geometry)
  
  # replace linestring geometries with the point endpoint geometry for each hy_id linestring in 'flowlines'
  # this is needed so that the final cross section point on the linestring has a final point that 
  # it can use to calculate sinuosity between
  sf::st_geometry(flowlines) <- sf::st_geometry(nhdplusTools::get_node(flowlines, "end"))
  
  # bind the cross section points together with the extra point geometries at the end of each linestring 
  pts <- dplyr::bind_rows(
    dplyr::mutate(pts, 
                  cs_id = as.character(cs_id)), 
    flowlines
  ) 
  
  # calculate euclidean distance between each point and the next point on the linestring 
  # and calculate the along channel distance and then calcualate sinuosity as along_channel / euclid_dist
  pts <- 
    pts %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>%
    # dplyr::group_by(hy_id) %>%
    dplyr::mutate(
      euclid_dist   = as.numeric(sf::st_distance(geometry,        
                                                 dplyr::lead(geometry),
                                                 by_element = TRUE)),
      along_channel = dplyr::lead(ds_distance) - ds_distance, 
      sinuosity     = along_channel / euclid_dist
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, cs_measure, ds_distance, along_channel, 
                  euclid_dist, sinuosity, geometry) %>% 
    # dplyr::select(hy_id, cs_id, cs_measure, ds_distance, along_channel, 
    #               euclid_dist, sinuosity, geometry) %>% 
    dplyr::filter(cs_id != "end") %>% 
    dplyr::mutate(cs_id = as.integer(cs_id)) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, cs_measure, sinuosity)
  # dplyr::select(hy_id, cs_id, cs_measure, sinuosity)
  
  
  # if add is TRUE, then add the sinuosity column back to the original data
  if (add) {
    transects <- dplyr::left_join(
      transects,
      dplyr::select(pts, -cs_measure), 
      by = c(crosswalk_id, "cs_id")
      # by = c("hy_id", "cs_id")
    )
    
    return(transects)
    
  }
  
  return(pts)
  
}