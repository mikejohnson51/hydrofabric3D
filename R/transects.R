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
sf::sf_use_s2(FALSE)

#' Generate a Perpendicular Linestring of a Given Width
#' @param edge geos_geometry LINESTRING
#' @param width Length of Perpendicular LINESTRING
#' @return GEOS object
#' @importFrom geos geos_interpolate_normalized geos_point_end geos_x geos_y geos_length
#' @importFrom wk wk_transform wk_affine_compose wk_affine_translate wk_affine_scale wk_affine_rotate wk_set_crs wk_crs
#' @export
cut_transect <- function(edge, width){
  
  if(!inherits(edge, "geos_geometry")) {
    stop("'edge' must be of type 'geos_geometry'")
  }
  
  if(geos::geos_is_empty(edge)) {
    stop("'edge' is an empty geos_geometry")
  }
  
  midpoint <- geos::geos_interpolate_normalized(edge, 0.5)
  ep       <- geos::geos_point_end(edge)
  
  normal_edge <- wk::wk_transform(
                    edge, 
                    wk::wk_affine_compose(
                      wk::wk_affine_translate(dx = -geos::geos_x(midpoint), dy = -geos::geos_y(midpoint)),
                      wk::wk_affine_scale(1 / geos::geos_length(edge), 1 / geos::geos_length(edge)),
                      wk::wk_affine_rotate(90)
                      )
                    )
  
  return(
    wk::wk_set_crs(
      wk::wk_transform(
        normal_edge,
        wk::wk_affine_compose(
          wk::wk_affine_scale(width, width),
          wk::wk_affine_translate(geos::geos_x(ep), geos::geos_y(ep))
          )
        ), 
      wk::wk_crs(edge)
    )
  )
}

#' Generate Multiple cross section along a linestring
#' @param line sf linestring or geos_geometry, original line element
#' @param bf_width Bankfull Width (length of cross section)
#' @param n number of cross sections
#' @return sf dataframe with 'n' evenly spaced transect lines with cs_measures for each cross section geometry
#' @importFrom geos geos_empty geos_type geos_intersection geos_intersects geos_is_empty geos_length as_geos_geometry
#' @importFrom vctrs vec_c
#' @importFrom wk wk_vertices wk_linestring
#' @importFrom sf st_as_sf
#' @importFrom methods is
#' @export
get_transects <- function(line, bf_width, n) {
  
  # if NOT a geos_geometry class, coerce
  if(!inherits(line, "geos_geometry")) {
    
    is_sf <- inherits(line, "sf")
    
    is_linestring <- (
      methods::is(line, "LINESTRING") || 
      methods::is(line, "MULTILINESTRING") || 
      methods::is(line, "sf_LINESTRING") || 
      methods::is(line, "sf_MULTILINESTRING") || 
      methods::is(line, "sfc_LINESTRING") ||
      methods::is(line, "sfc_MULTILINESTRING")
    )
    
    if (is_sf || is_linestring) {
      # convert to geos geometry
      line <- geos::as_geos_geometry(line)  
    
    } else if (!is_linestring) {
      stop("Invalid 'line' value, must be geos_geometry LINESTRING or MULTILINESTRING, sf LINESTRING / MULTILINESTRING, or LINESTRING / MULTILINESTRING")
    } 
    
    
  }
  
  if (!is.numeric(bf_width)) {
    stop("Invalid 'bf_width', bf_width must be a numeric")
  }
  
  # vertices of line
  vertices <- wk::wk_vertices(line)
  
  # create evenly spaced linestring geometries along line of interest
  edges <- geos::as_geos_geometry(
    wk::wk_linestring(
      vertices[c(1, 
                 rep(
                    seq_along(vertices)[-c(1, length(vertices))], each = 2
                  ), 
                 length(vertices))],
      feature_id = rep(seq_len(length(vertices) - 1), each = 2)
    )
  )
  
  # # get the cumulative length of edges along flowline
  edge_lengths <- cumsum(
    geos::geos_length(edges)
  )
  
  # total length of linestring
  total_length <- edge_lengths[length(edge_lengths)]
  
  # # the below check should be TRUE 
  # total_length == geos::geos_length(line)
  
  # keep all lines except first and last edges
  edges <- edges[-c(1, length(edges))]
  
  # # keep all edge lengths except first and last edge lengths
  edge_lengths <- edge_lengths[-c(1, length(edge_lengths))]
  
  # create a sequence of edges along 'line'
  if (!is.null(n)) {
    if (n == 1) {
      # get a single edge at the midpoint
      edges <- edges[as.integer(ceiling(length(edges)/ 2))]
      
      # get the edge length for the single midpoint edge
      edge_lengths <- edge_lengths[as.integer(ceiling(length(edge_lengths)/ 2))]
      
    } else {
      # extract edges at intervals of 'n' 
      edges <- edges[as.integer(
        seq.int(1, length(edges), length.out = min(n, length(edges)))
      )
      ]
      # extract edge lengths at intervals of 'n' (same interval/indices of above edges indexing)
      edge_lengths <- edge_lengths[as.integer(
        seq.int(1, length(edge_lengths), length.out = min(n, length(edge_lengths)))
      )
      ]
    }
  }
  
  # make sure bf_width is the same length as the number of edges
  if (length(bf_width) != length(edges)) {
    bf_width <- rep(bf_width[1], length(edges))
  }
  
  transects <- geos::geos_empty()
  measures  <- vctrs::vec_c()
  
  for(i in 1:length(edges)){
    
    # message("TRANSECT: ", i)
    tran = cut_transect(edges[i], bf_width[i])
    
    # # # measure of edge
    meas <- edge_lengths[i]
    
    # # If a MULTIPOINT, then it crosses more the once
    if(geos::geos_type(geos::geos_intersection(tran, line)) == "point") {
      # message("intersect IS point ")
      # Ensure that there are no intersections with previously computed cross sections
      if (!any(geos::geos_intersects(tran, transects))) {
        # message("----> KEEPING TRANSECT: ", i)
        transects <- vctrs::vec_c(transects, tran)
        measures  <- vctrs::vec_c(measures, meas)
      }
    }
  }
  
  # index for only valid transects
  is_valid <- !geos::geos_is_empty(transects)
  
  # is_valid[-1]
  # extract only edge lengths of remaining transect lines only valid edge lengths
  measures <- measures[is_valid[-1]]
  # edge_lengths <- edge_lengths[is_valid[-1]]
  
  # # calculate cs_measure value
  edge_measure <- (measures/total_length) * 100
  # edge_lengths <- (edge_lengths/total_length) * 100
  
  # drop empty geos geometries
  transects <- transects[is_valid]
  
  transects <- sf::st_as_sf(transects)
  transects$ds_distance <- measures
  transects$cs_measure  <- edge_measure
  # transects$cs_measure <- edge_lengths
  
  return(transects)
  
}

#' Calculate sinuosity between cross sections on flowlines by
#' 
#' @param lines sf linestring geometry of flowlines with a unique "hy_id" column
#' @param cross_sections sf linestring dataframe with a "hy_id" unique identifier that maps to a linestring geometry in 'lines'. Must contain a "cs_id" column to uniquely identify cross sections in each hy_id and the order on the given hy_id.
#'  Also must include a "cs_measure" column, which indicates the percent downstream the cross section linestring is along the linestring in 'lines'.
#' @param add logical, whether to add the sinuosity values to the original cross section dataset (TRUE). Default is TRUE. If FALSE, a dataframe with hy_id, cs_id, cs_measure, and sinuosity columns is returned
#'
#' @noRd
#' @keywords internal
#' @return sf dataframe containing the cross_sections dataset with an added sinuosity column (add = TRUE), or a dataframe with with hy_id, cs_id, cs_measure, and sinuosity columns with values for each linestring in 'cross_sections' 
#' @importFrom dplyr select group_by ungroup relocate mutate bind_rows lead filter left_join
#' @importFrom sf st_geometry st_distance st_length st_centroid st_drop_geometry
#' @importFrom nhdplusTools get_node
get_cs_sinuosity2 <- function(
    lines, 
    cross_sections, 
    add = TRUE
) {
  
  # convert cross section linestrings into points at the centroid of each cross section
  pts <- 
    cross_sections %>% 
    dplyr::select(hy_id, cs_id, cs_measure, ds_distance, geometry) %>% 
    sf::st_centroid()
  
  # # plot(pts$geometry)
  # plot(start$geometry, col = "green", add= T)
  # plot(end$geometry, col = "red", add= T)
  # plot(dplyr::slice(cs_lines, 3)$geometry, col = "red", add= T)
  # plot(cs_lines$geometry, col = "red", add= T)
  # plot(lines$geometry, add= T)
  
  # calculate line lengths
  lines <- 
    lines %>% 
    dplyr::select(hy_id, geometry) %>% 
    dplyr::group_by(hy_id) %>% 
    dplyr::mutate(
      ds_distance = as.numeric(sf::st_length(geometry)),
      cs_id       = "end",
      cs_measure  = 100
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::relocate(hy_id, cs_id, cs_measure, ds_distance, geometry)
  
  # replace linestring geometries with the point endpoint geometry for each hy_id linestring in 'lines'
  # this is needed so that the final cross section point on the linestring has a final point that 
  # it can use to calculate sinuosity between
  sf::st_geometry(lines) <- sf::st_geometry(nhdplusTools::get_node(lines, "end"))
  
  # bind the cross section points together with the extra point geometries at the end of each linestring 
  pts <- dplyr::bind_rows(
    dplyr::mutate(pts, 
                  cs_id = as.character(cs_id)), 
    lines
  ) 
  
  # calculate euclidean distance between each point and the next point on the linestring 
  # and calculate the along channel distance and then calcualate sinuosity as along_channel / euclid_dist
  pts <- 
    pts %>% 
    dplyr::group_by(hy_id) %>%
    dplyr::mutate(
      euclid_dist   = as.numeric(sf::st_distance(geometry,        
                                                 dplyr::lead(geometry),
                                                 by_element = TRUE)),
      along_channel = dplyr::lead(ds_distance) - ds_distance, 
      sinuosity     = along_channel / euclid_dist
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(hy_id, cs_id, cs_measure, ds_distance, along_channel, euclid_dist, sinuosity, geometry) %>% 
    dplyr::filter(cs_id != "end") %>% 
    dplyr::mutate(cs_id = as.integer(cs_id)) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(hy_id, cs_id, cs_measure, sinuosity)
  
  # if add is TRUE, then add the sinuosity column back to the original data
  if (add) {
    cross_sections <- dplyr::left_join(
      cross_sections,
      dplyr::select(pts, -cs_measure), 
      by = c("hy_id", "cs_id")
    )
    
    return(cross_sections)
    
  }
  
  return(pts)
  
}
#' Calculate sinuosity between cross sections on flowlines by
#' 
#' @param lines sf linestring geometry of flowlines with a unique "hy_id" column
#' @param cross_sections sf linestring dataframe with a "hy_id" unique identifier that maps to a linestring geometry in 'lines'. Must contain a "cs_id" column to uniquely identify cross sections in each hy_id and the order on the given hy_id.
#'  Also must include a "cs_measure" column, which indicates the percent downstream the cross section linestring is along the linestring in 'lines'.
#' @param crosswalk_id character, name of the unique identifier column in 'lines' and 'cross_sections' 
#' @param add logical, whether to add the sinuosity values to the original cross section dataset (TRUE). Default is TRUE. If FALSE, a dataframe with hy_id, cs_id, cs_measure, and sinuosity columns is returned
#'
#' @noRd
#' @keywords internal
#' @return sf dataframe containing the cross_sections dataset with an added sinuosity column (add = TRUE), or a dataframe with with hy_id, cs_id, cs_measure, and sinuosity columns with values for each linestring in 'cross_sections' 
#' @importFrom dplyr select group_by ungroup relocate mutate bind_rows lead filter left_join
#' @importFrom sf st_geometry st_distance st_length st_centroid st_drop_geometry
#' @importFrom nhdplusTools get_node
get_cs_sinuosity <- function(
    lines, 
    cross_sections, 
    crosswalk_id = "hydrofabric_id",
    add = TRUE
) {
  
  # lines          = net
  # cross_sections = transects
  # crosswalk_id   = "hydrofabric_id"
  # add            = TRUE
  
  # convert cross section linestrings into points at the centroid of each cross section
  pts <- 
    cross_sections %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), cs_id, cs_measure, ds_distance, geometry) %>% 
    # dplyr::select(hy_id, cs_id, cs_measure, ds_distance, geometry) %>% 
    sf::st_centroid()

  # # plot(pts$geometry)
  # plot(start$geometry, col = "green", add= T)
  # plot(end$geometry, col = "red", add= T)
  # plot(dplyr::slice(cs_lines, 3)$geometry, col = "red", add= T)
  # plot(cs_lines$geometry, col = "red", add= T)
  # plot(lines$geometry, add= T)
  
  # calculate line lengths
  lines <- 
    lines %>% 
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

  # replace linestring geometries with the point endpoint geometry for each hy_id linestring in 'lines'
  # this is needed so that the final cross section point on the linestring has a final point that 
  # it can use to calculate sinuosity between
  sf::st_geometry(lines) <- sf::st_geometry(nhdplusTools::get_node(lines, "end"))
  
  # bind the cross section points together with the extra point geometries at the end of each linestring 
  pts <- dplyr::bind_rows(
    dplyr::mutate(pts, 
                  cs_id = as.character(cs_id)), 
    lines
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
    cross_sections <- dplyr::left_join(
      cross_sections,
      dplyr::select(pts, -cs_measure), 
      by = c(crosswalk_id, "cs_id")
      # by = c("hy_id", "cs_id")
    )
    
    return(cross_sections)
    
  }
  
  return(pts)
  
}

# # Generate Cross Sections Across Hydrographic Network
# #
# # @param net Hydrographic LINESTRING Network
# # @param crosswalk_id Unique Identifier in net
# # @param cs_widths numeric, Bankfull Widths (length of cross sections for each net element)
# # @param num numeric, Number of transects per Net element
# # @param smooth logical, whether to smooth linestring geometries or not. Default is TRUE.
# # @param densify numeric, how many times more points should be added to linestrings. Default is 2.
# # @param rm_self_intersect logical, whether to remove self intersecting transect linestrings
# # @param fix_braids logical, whether to fix braided transect lines or not. If TRUE, linestrings that are part of a braided network are augmented. Default is FALSE.
# # @param terminal_id character, column name containing a unique identifier, delineating separate networks in the 'net' dataset. Default is NULL which will best effort determine the connected components in the network to try and create a 'component_id' column in 'net' 
# # @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
# # lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
# # Default is NULL, which will attempt to fix all the braid transects in the data
# # @param version integer, version number of braid algorithm to use, either 1 or 2. Default is 2.
# # @param braid_method The method to determine the geometries to cut. Options are "comid", "component", or "neighbor". Default is "comid"
# # @param precision int, distance in meters. Only applicable when fix_braids = TRUE. This is the number of meters to approximate final cross section linestring length. Increasing this value will decrease runtime of cross section extension algorithm. Value you must be greater than 0. Default is 1
# # @param add logical indicating whether to add original 'net' data to the outputted transect lines. Default is FALSE.
# #
# # @return sf object
# # @importFrom dplyr group_by mutate ungroup n left_join all_of rename
# # @importFrom sf st_crs st_transform st_intersects st_length st_drop_geometry st_as_sf
# # @importFrom smoothr smooth densify
# # @importFrom geos as_geos_geometry
# # @importFrom wk wk_vertices wk_linestring
# # @export
# cut_cross_sections2 <- function(
#     net, 
#     crosswalk_id                = NULL,
#     cs_widths         = 100, 
#     num               = 10,
#     smooth            = TRUE,
#     densify           = 2,
#     rm_self_intersect = TRUE,
#     fix_braids        = FALSE,
#     terminal_id       = NULL,
#     braid_threshold   = NULL,
#     version           = 2,
#     braid_method      = "comid",
#     precision         = 1,
#     add               = FALSE
# ) {
#   
#   # validate all inputs are valid, throws an error if they are not
#   validate_cut_cross_section_inputs(net = net, 
#                                     crosswalk_id = crosswalk_id, 
#                                     cs_widths = cs_widths, 
#                                     num = num, 
#                                     smooth = smooth, 
#                                     densify = densify, 
#                                     rm_self_intersect = rm_self_intersect, 
#                                     fix_braids = fix_braids,
#                                     terminal_id = terminal_id,
#                                     braid_threshold = braid_threshold,
#                                     version = version, 
#                                     braid_method = braid_method, 
#                                     precision = precision,
#                                     add = add 
#   )
#   
#   # keep track of the CRS of the input to retransform return 
#   start_crs <- sf::st_crs(net, parameters = T)$epsg
#   
#   # check if net CRS is 5070, if not, transform it to 5070
#   if(start_crs != 5070) {
#     # message("Transforming CRS to EPSG: 5070")
#     net <- sf::st_transform(net, 5070) 
#   }
#   
#   # Densify network flowlines, adds more points to each linestring
#   if(!is.null(densify)){ 
#     message("Densifying")
#     net <- smoothr::densify(net, densify) 
#   }
#   
#   # smooth out flowlines
#   if(smooth){ 
#     message("Smoothing")
#     # net = smoothr::smooth(net, "ksmooth")
#     net <- smoothr::smooth(net, "spline")
#   }
#   
#   # list to store transect outputs
#   transects <- list()
#   
#   # if there is a missing number of cross section widths given relative to the number of rows in net, fill in the missing values
#   if (length(cs_widths) != nrow(net)) {
#     cs_widths = rep(cs_widths[1], nrow(net))
#   }
#   
#   if (length(num) != nrow(net)) {
#     num = pmax(3, rep(num[1], nrow(net)))
#   }
#   
#   message("Cutting")
#   
#   # iterate through each linestring in "net" and generate transect lines along each line 
#   for (j in 1:nrow(net)) {
#     # logger::log_info("{j} / {nrow(net)}")
#     # cut transect lines at each 'edge' generated along our line of interest
#     trans <- get_transects(
#       line     = geos::as_geos_geometry(net$geometry[j]),
#       bf_width = cs_widths[j],
#       n        = num[j]
#     )
#     
#     # if 0 transects can be formed, skip the iteration
#     if(nrow(trans) == 0) {
#       # logger::log_info("---> SKIPPING ITERATION {j}")
#       next
#     }
#     
#     # assign hy_id from net
#     trans$hy_id <- net[[crosswalk_id]][j]
#     trans$cs_widths <- cs_widths[j]
#     
#     # insert 'trans' sf dataframe into list
#     transects[[j]] <- trans
#     
#     # # cut transect lines at each 'edge' generated along our line of interest
#     # transects[[j]] <- get_transects(
#     #                       line     = geos::as_geos_geometry(net$geometry[j]),
#     #                       bf_width = cs_widths[j],
#     #                       n        = num[j]
#     #                     )
#   }
#   
#   # # get length of each dataframe to assign "hy_id" back with cross sections
#   # ids_length <- sapply(transects, nrow)
#   # # # ids_length <- lengths(transects)
#   
#   # crs_list <- lapply(transects, function(i) { is.na(sf::st_crs(i)$epsg) } )
#   
#   # bind list of sf dataframes of transects back together
#   transects <- dplyr::bind_rows(transects)
#   # transects <- sf::st_as_sf(Reduce(c, transects))]
#   
#   if(nrow(transects) == 0){
#     return(NULL)
#   }
#   
#   message("Formating")
#   
#   # # add crosswalk_id column if provided as an input
#   # if (!is.null(crosswalk_id)) {
#   #   transects$hy_id = rep(net[[crosswalk_id]], times = ids_length)
#   # } else {
#   #   transects$hy_id = rep(1:nrow(net), times = ids_length)
#   # }
#   # 
#   # # add back cross sections width column
#   # transects$cs_widths = rep(cs_widths, times = ids_length)
#   
#   # remove self intersecting transects or not
#   if(rm_self_intersect){
#     transects <- 
#       transects[lengths(sf::st_intersects(transects)) == 1, ] %>% 
#       dplyr::group_by(hy_id) %>% 
#       dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
#       dplyr::ungroup() %>% 
#       dplyr::mutate(lengthm = as.numeric(sf::st_length(.)))
#   } else {
#     transects <- 
#       transects %>% 
#       dplyr::group_by(hy_id) %>% 
#       dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
#       dplyr::ungroup() %>% 
#       dplyr::mutate(lengthm = as.numeric(sf::st_length(.)))
#   }
#   
#   # if original columns of data should be added to transects dataset
#   if(add) {
#     transects <-
#       dplyr::left_join(
#         transects,
#         sf::st_drop_geometry(net),
#         by = c("hy_id" = crosswalk_id)
#         # by = c("hy_id" = "comid")
#       )
#   }
#   
#   # if fix_braids is set to TRUE, then fix the braided transect lines
#   if(fix_braids) {
#     
#     # message(paste0("Applying fixes to braided transects using:\n",
#     #          "- Braid detection version: ", version, "\n",
#     #          "- Braid grouping method: ", braid_method
#     #          ))
#     
#     transects <- fix_braid_transects(
#       net             = net,
#       transect_lines  = transects,
#       terminal_id     = terminal_id,
#       braid_threshold = braid_threshold,
#       version         = version,
#       method          = braid_method,
#       precision       = precision,
#       rm_intersects   = rm_self_intersect
#     )
#   }
#   
#   # remove any transect lines that intersect with any flowlines more than 1 time
#   transects <- transects[lengths(sf::st_intersects(transects, net)) == 1, ]
#   
#   # rename "crosswalk_id" column to hy_id if "hy_id" is not already present
#   if(!"hy_id" %in% names(net)) {
#     net <- dplyr::rename(net, hy_id = dplyr::all_of(crosswalk_id))
#   }
#   
#   # calculate sinuosity and add it as a column to the cross sections
#   transects <- get_cs_sinuosity2(
#     lines          = net, 
#     cross_sections = transects, 
#     add            = TRUE
#   )
#   
#   # transform CRS back to input CRS
#   if(start_crs != 5070) {
#     # message("Transforming CRS back to EPSG: ", start_crs)
#     transects <- sf::st_transform(transects, start_crs)
#   }
#   
#   # rename the cs_widths column to cs_lengthm
#   transects <- dplyr::rename(transects, "cs_lengthm" = cs_widths)
#   
#   # select all relevent columns and set output columns order
#   transects <-
#     transects %>%
#     dplyr::select(
#       dplyr::any_of(c("hy_id",
#                       "cs_id",
#                       "cs_lengthm", 
#                       # "cs_widths", 
#                       "cs_measure",
#                       "ds_distance",
#                       "lengthm",
#                       "sinuosity",
#                       "geometry"
#       ))
#     )
#   
#   return(transects)
#   
# }

#' Generate Cross Sections Across Hydrographic Network
#'
#' @param net Hydrographic LINESTRING Network
#' @param crosswalk_id Unique Identifier in net
#' @param cs_widths numeric, Bankfull Widths (length of cross sections for each net element)
#' @param num numeric, Number of transects per Net element
#' @param smooth logical, whether to smooth linestring geometries or not. Default is TRUE.
#' @param densify numeric, how many times more points should be added to linestrings. Default is 2.
#' @param rm_self_intersect logical, whether to remove self intersecting transect linestrings
#' @param fix_braids logical, whether to fix braided transect lines or not. If TRUE, linestrings that are part of a braided network are augmented. Default is FALSE.
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#' @param braid_method The method to determine the geometries to cut. Options are "comid", "component", or "neighbor". Default is "comid"
#' @param precision int, distance in meters. Only applicable when fix_braids = TRUE. This is the number of meters to approximate final cross section linestring length. Increasing this value will decrease runtime of cross section extension algorithm. Value you must be greater than 0. Default is 1
#' @param add logical indicating whether to add original 'net' data to the outputted transect lines. Default is FALSE.
#'
#' @return sf object
#' @importFrom dplyr group_by mutate ungroup n left_join all_of rename
#' @importFrom sf st_crs st_transform st_intersects st_length st_drop_geometry st_as_sf
#' @importFrom smoothr smooth densify
#' @importFrom geos as_geos_geometry
#' @importFrom hydroloom rename_geometry
#' @importFrom wk wk_vertices wk_linestring
#' @export
cut_cross_sections <- function(
    net, 
    crosswalk_id      = NULL,
    cs_widths         = 100, 
    num               = 10,
    smooth            = TRUE,
    densify           = 2,
    rm_self_intersect = TRUE,
    fix_braids        = FALSE,
    braid_threshold   = NULL,
    braid_method      = "comid",
    precision         = 1,
    add               = FALSE
) {
  
  # library(dplyr)
  # library(sf)
  # net = flowline
  # num = 20
  # crosswalk_id                = NULL
  # cs_widths         = 100 
  # smooth            = TRUE
  # densify           = 2
  # rm_self_intersect = TRUE
  # fix_braids        = FALSE
  # braid_threshold   = NULL
  # braid_method      = "comid"
  # precision         = 1
  # add               = FALSE
  
  # 
  # net    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # # crosswalk_id = "comid"
  # # 
  # # # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # # # crosswalk_id = "crosswalk_id"
  # # # verbose = TRUE
  # # # nested      = TRUE
  # # # verbose     = TRUE
  # # net <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>%
  # #   dplyr::select(-crosswalk_id)
  # crosswalk_id = "comid"
  # cs_widths = 100
  # num = 5
  # densify=2
  # smooth = TRUE
  # rm_self_intersect = TRUE
  # 
  # fix_braids        = FALSE
  # terminal_id       = NULL
  # braid_threshold   = NULL
  # version           = 2
  # braid_method      = "comid"
  # precision         = 1
  # add               = FALSE

  # net %>% 
    # add_hydrofabric_id() %>%  
    # dplyr::relocate(hydrofabric_id)
  # flowline <- 
    # flowlines[10, ] %>% 
    # dplyr::select(geometry = geom)
  
  suppressWarnings({
    
    # validate all inputs are valid, throws an error if they are not
    validate_cut_cross_section_inputs(net = net, 
                                      crosswalk_id = crosswalk_id, 
                                      cs_widths = cs_widths, 
                                      num = num, 
                                      smooth = smooth, 
                                      densify = densify, 
                                      rm_self_intersect = rm_self_intersect, 
                                      fix_braids = fix_braids,
                                      braid_threshold = braid_threshold,
                                      braid_method = braid_method, 
                                      precision = precision,
                                      add = add 
    )
    
    # make a unique ID if one is not given (NULL 'crosswalk_id')
    if (is.null(crosswalk_id)) {
      net <- add_hydrofabric_id(net) 
      crosswalk_id  <- 'hydrofabric_id'
    }
    
    # standardize geometry name
    net <- hydroloom::rename_geometry(net, "geometry")
  
    REQUIRED_COLS <- c(crosswalk_id, "geometry")
  
    # validate input dataframe has correct columns  
    is_valid <- validate_df(net, REQUIRED_COLS, "net")  
  
    # -----------------------------
    # TODO: Testing out removing forced CRS change
    # -----------------------------
    
    # # keep track of the CRS of the input to retransform return 
    # start_crs <- sf::st_crs(net, parameters = T)$epsg
    # 
    # # check if net CRS is 5070, if not, transform it to 5070
    # if(start_crs != 5070) {
    #   # message("Transforming CRS to EPSG: 5070")
    #   net <- sf::st_transform(net, 5070) 
    # }
    # -----------------------------
    
    # Densify network flowlines, adds more points to each linestring
    if(!is.null(densify)){ 
      message("Densifying")
      net <- smoothr::densify(net, densify) 
    }
    
    # smooth out flowlines
    if(smooth){ 
      message("Smoothing")
      # net = smoothr::smooth(net, "ksmooth")
      net <- smoothr::smooth(net, "spline")
    }
    
    # list to store transect outputs
    transects <- list()
    
    # if there is a missing number of cross section widths given relative to the number of rows in net, fill in the missing values
    if (length(cs_widths) != nrow(net)) {
      cs_widths = rep(cs_widths[1], nrow(net))
    }
    
    if (length(num) != nrow(net)) {
      num = pmax(3, rep(num[1], nrow(net)))
    }
    
    message("Cutting")
    
    # iterate through each linestring in "net" and generate transect lines along each line 
    for (j in 1:nrow(net)) {
      # j = 1
      
      # cut transect lines at each 'edge' generated along our line of interest
      trans <- get_transects(
        line     = geos::as_geos_geometry(net$geometry[j]),
        bf_width = cs_widths[j],
        n        = num[j]
      )
      
      # if 0 transects can be formed, skip the iteration
      if(nrow(trans) == 0) {
        # logger::log_info("---> SKIPPING ITERATION {j}")
        next
      }
      
      # assign hy_id from net
      trans[[crosswalk_id]]     <- net[[crosswalk_id]][j]
      trans$cs_widths <- cs_widths[j]
      
      # insert 'trans' sf dataframe into list
      transects[[j]] <- trans
      
      # # cut transect lines at each 'edge' generated along our line of interest
      # transects[[j]] <- get_transects(
      #                       line     = geos::as_geos_geometry(net$geometry[j]),
      #                       bf_width = cs_widths[j],
      #                       n        = num[j]
      #                     )
    }
    
    # # get length of each dataframe to assign "hy_id" back with cross sections
    # ids_length <- sapply(transects, nrow)
    # # # ids_length <- lengths(transects)
    
    # crs_list <- lapply(transects, function(i) { is.na(sf::st_crs(i)$epsg) } )
    
    # bind list of sf dataframes of transects back together
    transects <- dplyr::bind_rows(transects)
    # transects <- sf::st_as_sf(Reduce(c, transects))]
    
    # mapview::mapview(transects, color = "red") + net 
    # plot(net$geometry, add = F)
    # plot(transects$geometry, col = "red", add = T)
    
    if(nrow(transects) == 0){
      return(NULL)
    }
    
    message("Formatting")
    
    # # add crosswalk_id column if provided as an input
    # if (!is.null(crosswalk_id)) {
    #   transects$hy_id = rep(net[[crosswalk_id]], times = ids_length)
    # } else {
    #   transects$hy_id = rep(1:nrow(net), times = ids_length)
    # }
    # 
    # # add back cross sections width column
    # transects$cs_widths = rep(cs_widths, times = ids_length)
    
    # remove self intersecting transects or not
    if (rm_self_intersect) {
      transects <- 
        rm_self_intersections(transects) %>% 
        # transects[lengths(sf::st_intersects(transects)) == 1, ] %>% 
        dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
        # dplyr::group_by(hy_id) %>% 
        dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(lengthm = as.numeric(sf::st_length(.)))
    } else {
      transects <- 
        transects %>% 
        dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
        # dplyr::group_by(hy_id) %>% 
        dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(lengthm = as.numeric(sf::st_length(.)))
    }
    
    # if original columns of data should be added to transects dataset
    if(add) {
      transects <-
        dplyr::left_join(
          transects,
          sf::st_drop_geometry(net),
          by = crosswalk_id
          # by = c("hy_id" = crosswalk_id)
        )
    }
    
    # if fix_braids is set to TRUE, then fix the braided transect lines
    if(fix_braids) {
      
      # message(paste0("Applying fixes to braided transects using:\n",
      #          "- Braid detection version: ", version, "\n",
      #          "- Braid grouping method: ", braid_method
      #          ))
      transects <- fix_braided_transects(
        net             = net,
        transect_lines  = transects,
        crosswalk_id    = crosswalk_id,
        braid_threshold = braid_threshold,
        method          = braid_method,
        precision       = precision,
        rm_intersects   = rm_self_intersect
      )
      
      # #   # if one of the transect lines interesects MORE than 1 line in net AND it also has a braid_id == "no_braid", then remove it from output
      #   transect_lines <- transect_lines[!(lengths(sf::st_intersects(transect_lines, net)) > 1 & transect_lines$braid_id == "no_braid"), ]
      #   transect_lines <- transect_lines[!(lengths(sf::st_intersects(transect_lines)) > 1 & transect_lines$braid_id == "no_braid"), ]
    } else {
      
      # remove any transect lines that intersect with any flowlines more than 1 time 
      # NOTE: IF we DID NOT do braid fixing, which could cause a transect to purposefully interesect multiple flowlines
      transects <- rm_multiflowline_intersections(transects = transects, flowlines = net)
      # transects <- transects[lengths(sf::st_intersects(transects, net)) == 1, ]
    }
    
    # # remove any transect lines that intersect with any flowlines more than 1 time
    # transects <- transects[lengths(sf::st_intersects(transects, net)) == 1, ]
     
    # TODO: removed this forcing to "hy_id", part of migration of ALL code 
    # TODO: to rely on an specified "crosswalk_id" and/or "crosswalk_id" 
    # # rename "crosswalk_id" column to hy_id if "hy_id" is not already present
    # if (!"hy_id" %in% names(net)) {
    #   net <- dplyr::rename(net, hy_id = dplyr::all_of(crosswalk_id))
    # }
    
    # get_cs_sinuosity2(
    #   lines          = net, 
    #   cross_sections = transects, 
    #   crosswalk_id   = "hydrofabric_id",
    #   add            = TRUE
    # )
    
    # calculate sinuosity and add it as a column to the cross sections
    transects <- get_cs_sinuosity(
      lines          = net, 
      cross_sections = transects, 
      crosswalk_id   = crosswalk_id,
      add            = TRUE
    )
    
    # -----------------------------
    # TODO: Testing out removing forced CRS change
    # -----------------------------
  
    # # transform CRS back to input CRS
    # if(start_crs != 5070) {
    #   # message("Transforming CRS back to EPSG: ", start_crs)
    #   transects <- sf::st_transform(transects, start_crs)
    # }
  
    # -----------------------------
    
    # rename the cs_widths column to cs_lengthm
    transects <- dplyr::rename(transects, "cs_lengthm" = cs_widths)
    
    # select all relevent columns and set output columns order
    transects <-
      transects %>%
      dplyr::select(
        dplyr::any_of(
          c(
            crosswalk_id,
            # "hy_id",
            "cs_id",
            "cs_lengthm", 
            # "cs_widths", 
            "cs_measure",
            "ds_distance",
            "lengthm",
            "sinuosity",
            "geometry"
            )
          )
      )
    
    return(transects)
  
    })  
}



#' Remove transect lines that intersect with more than one flowline
#'
#' @param transects sf linestring dataframe of transect lines
#' @param flowlines sf linestring dataframe of flowlines
#' @noRd
#' @keywords internal
#' @importFrom sf st_intersects 
#' @return sf linestring dataframe
rm_multiflowline_intersections <- function(transects, flowlines) {
  
  transects <- transects[lengths(sf::st_intersects(transects, flowlines)) == 1, ]
  
  return(transects)
  
}

#' Remove linestrings that intersect with any other linestring 
#'
#' @param x sf linestring dataframe 
#' @noRd
#' @keywords internal
#' @importFrom sf st_intersects 
#' @return sf linestring dataframe
rm_self_intersections <- function(x) {
  
  x <- x[lengths(sf::st_intersects(x)) == 1, ]
  # x <- x[!(lengths(sf::st_intersects(x)) > 1), ]
  
  return(x)
  
}


# reindex_cs_id  <- function(transects, crosswalk_id = NULL) {
#   
#   
# }




#' Remove transect lines that cross their given flowline more than once
#' Internal function used in get_transects2(), a slightly faster method over get_transects() but yields slightly different outputs. 
#' More efficient checking of all the transect lines on a given linestring, by removing some of the extraneous calls to geos_intersection() and geos_intersects()
#' @param transects geos_linestring of transects that cross perpendicularly to 'line'
#' @param line geos_linestring that is cut by 'transects'
#' 
#' @noRd
#' @keywords internal
#' @return geos_linestring
drop_multicrossings <- function(transects, line) {
  
  # check for multipoint intersections with the primary line segement, 
  # remove transects that cut through more than once ("multipoints")
  # index and keep only the transects that have a "point" intersections, and NOT the "multipoint" intersections
  transects <- transects[geos::geos_type(
    geos::geos_intersection(
      transects,
      line
    )
  ) == "point"]
  
  return(transects)
  
}

#' Check transect lines for double crosses and intersections with other transects in the set of transects
#' DEPRECATED
#' @param transects geos_linestring
#' @param line geos_linestring
#' 
#' @noRd
#' @keywords internal
#' @return geos_geometry list of non intersecting geometries within the list of geometries
#' @importFrom geos geos_type geos_intersection geos_empty geos_intersects geos_is_empty
#' @importFrom vctrs vec_c
check_intersects <- function(transects, line) {
  
  # reverse the order of the transects, to keep starting from the end of the vector
  transects <- rev(transects)
  
  # check for multipoint intersections with the primary line segement, 
  # remove transects that cut through more than once ("multipoints")
  # index and keep only the transects that have a "point" intersections, and NOT the "multipoint" intersections
  transects <- transects[geos::geos_type(
    geos::geos_intersection(
      transects,
      line
    )
  ) == "point"]
  
  # empty geometry
  to_keep <- geos::geos_empty()
  
  # while there is more than one geometry left in the 'transects_rev' geos geometry:
  #  - each iteration we take the transect at the end of the list which is also the most upstream line,
  #  - we stash this line because we are going to add it to our 'to_keep' geometry vector during each iteration
  #  - We then do the intersection between our current transect (most upstream transect left in the geos vector)
  #     and the rest of the geometries in the vector. We apply a "!" to the results of geos_intersects() so that we are getting a boolean 
  #     that is TRUE for ALL the geometries downstream that do NOT intersect with our current most upstream transect,
  # - we then set transects_rev equal to all the transects that are NOT intersecting our current most upstream transect (i.e. dropping the intersections we have so far)
  # - we then add our "stash" geometry (most upstream transect we stashed at the beginning of each iteration), to the "to_keep" geometry vector
  while (length(transects) > 0) {
    #   
    # message("length(xx_rev): ", length(transects))
    # message("length(to_keep): ", length(to_keep))
    
    # stash current most upstream transect to keep at end of iteration
    stash <- transects[length(transects)]
    
    # compare current most upstream transect (i.e. geometry that is at the end of our "transects_rev" vector) 
    # to the other downstream transects, and then drop any geometries that intersect with our current transect at the end of the list
    # this step DROPS THE INTERSECTING DOWNSTREAM TRANSECTS, removing them from future (unneccessary) checks 
    transects <- transects[-length(transects)][
      !geos::geos_intersects(
        transects[length(transects)],
        transects[-length(transects)]
      )
    ]
    
    # add "stash" to the 'to_keep' vector
    to_keep <-  vctrs::vec_c(to_keep, stash)
    
    # message("=============")
    
  }
  
  # drop empty geometries in the vector
  to_keep <- to_keep[!geos::geos_is_empty(to_keep)]
  
  # # reverse 'to_keep' back to original ordering
  # to_keep <- rev(to_keep)
  
  return(to_keep)
  
}

#' Selectively removes intersecting transect lines
#' Attempts to remove transects intersecting other transects by first removing transects that interesect the most other transects, then re checking intersection condition,  and doing this until there are no multi intersections
#' this gives the benefit of removing a transect line that intersects many other transects, potentially leaving those other transects with no extraneous intersections ONCE the MULTI intersecting transect is removed
#' @param x sf dataframe of linestrings
#' @importFrom sf st_intersects
#'
#' @return sf dataframe
rm_multi_intersects <- function(x) {
  
  # x <- tmp_trans
  # x
  
  while (any(lengths(sf::st_intersects(x)) > 1)) {
    
    intersect_counts <- lengths(sf::st_intersects(x))
    max_crossings    <- which(intersect_counts == max(intersect_counts))
    x                <- x[-max_crossings, ]
    
    # message("# intersects > 1 intersect_counts: ",       sum(intersect_counts > 1))
    # message("Removing ", length(max_crossings), " from x")
    # message(nrow(x), " rows in x remain...\n")
    # mapview::mapview(x, color = "red") + 
    # mapview::mapview(tmp_trans, color = "green") 
    
  }
  
  return(x)
  
}

# #Generate Cross Sections Across Hydrographic Network
# #@param net Hydrographic LINESTRING Network
# #@param crosswalk_id Unique Identifier in net
# #@param cs_widths Bankfull Widths (length of cross sections for each net element)
# #@param num Number of transects per Net element
# #@param smooth logical, whether to smooth linestring geometries or not. Default is TRUE.
# #@param densify numeric, how many times more points should be added to linestrings. Default is 2.
# #@param rm_self_intersect logical, whether to remove self intersecting transect linestrings
# #@param fix_braids logical, whether to fix braided transect lines or not. If TRUE (default), linestrings that are part of a braided network are augmented
# #@param terminal_id character, column name containing a unique identifier, delineating separate networks in the 'net' dataset. Default is NULL which will best effort determine the connected components in the network to try and create a 'component_id' column in 'net' 
# #@param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
# #lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
# #Default is NULL, which will attempt to fix all the braid transects in the data
# #@param version integer, version number of braid algorithm to use, either 1 or 2. Default is 2.
# #@param braid_method The method to determine the geometries to cut. Options are "comid", "component", or "neighbor". Default is "comid"
# #@param precision int, distance in meters. Only applicable when fix_braids = TRUE. This is the number of meters to approximate final cross section linestring length. Increasing this value will decrease runtime of cross section extension algorithm. Value you must be greater than 0. Default is 1
# #@param add logical indicating whether to add original 'net' data to the outputted transect lines. Default is FALSE.
# #@return sf object
# #@importFrom dplyr group_by mutate ungroup n left_join
# #@importFrom sf st_crs st_transform st_intersects st_length st_drop_geometry st_as_sf
# #@importFrom smoothr smooth densify
# #@importFrom geos as_geos_geometry
# #@importFrom wk wk_vertices wk_linestring
# cut_cross_sections2 <- function(
    #     net, 
#     crosswalk_id                = NULL,
#     cs_widths         = 100, 
#     num               = 10,
#     smooth            = TRUE,
#     densify           = 2,
#     rm_self_intersect = TRUE,
#     fix_braids        = FALSE,
#     terminal_id       = NULL,
#     braid_threshold   = NULL,
#     version           = 2,
#     braid_method      = "comid",
#     precision         = 1,
#     add               = FALSE
# ) {
#   
#   # keep track of the CRS of the input to retransform return 
#   start_crs <- sf::st_crs(net, parameters = T)$epsg
#   
#   # check if net CRS is 5070, if not, transform it to 5070
#   if(start_crs != 5070) {
#     # message("Transforming CRS to EPSG: 5070")
#     net <- sf::st_transform(net, 5070) 
#   }
#   
#   # smooth out flowlines
#   if(smooth){ 
#     message("Smoothing")
#     # net = smoothr::smooth(net, "ksmooth")
#     net = smoothr::smooth(net, "spline")
#   }
#   
#   # Densify network flowlines, adds more points to each linestring
#   if(!is.null(densify)){ 
#     message("Densifying")
#     net = smoothr::densify(net, 2) 
#   }
#   
#   # list to store transect outputs
#   ll <- list()
#   
#   # list to store cs_measure values
#   measure_list <- list()
#   
#   # if there is a missing number of cross section widths given relative to the number of rows in net, fill in the missing values
#   if (length(cs_widths) != nrow(net)) {
#     cs_widths = rep(cs_widths[1], nrow(net))
#   }
#   
#   if (length(num) != nrow(net)) {
#     num = pmax(3, rep(num[1], nrow(net)))
#   }
#   
#   message("Cutting")
#   
#   # iterate through each linestring in "net" and generate transect lines along each line 
#   for (j in 1:nrow(net)) {
#     # message("==== j: ", j, " =====")
#     
#     # convert sf line to geos_geometry
#     line <- geos::as_geos_geometry(net[j,])
# 
#     # vertices of line
#     vertices <- wk::wk_vertices(line)
#     
#     # create evenly spaced linestring geometries along line of interest
#     edges <- geos::as_geos_geometry(
#       wk::wk_linestring(
#         vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#         feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#       )
#     )
#     
#     # keep all lines except first and last edges
#     edges <- edges[-c(1, length(edges))]
#     
#     # create a sequence of edges along 'line'
#     if (!is.null(num)) {
#       if (num[j] == 1) {
#         edges <- edges[as.integer(ceiling(length(edges)/ 2))]
#         
#       } else {
#         edges <- edges[as.integer(
#                         seq.int(1, length(edges), length.out = min(num[j], length(edges)))
#                         )
#                       ]
#       }
#     }
#     
#     # cut transect lines at each 'edge' generated along our line of interest
#     ll[[j]] <- get_transects1(edges, line, cs_widths[j])
#     # ll[[j]] = get_transects2(edges, line, cs_widths[j])
#     
#   }
#   
#   # geos::geos_intersects_matrix(tlines, line)
#   ids_length <-  lengths(ll)
#   ll <- sf::st_as_sf(Reduce(c, ll))
#   
#   if(nrow(ll) == 0){
#     return(NULL)
#   }
#   
#   message("Formating")
#   
#   # add crosswalk_id column if provided as an input
#   if(!is.null(crosswalk_id)){
#     ll$hy_id = rep(net[[crosswalk_id]], times = ids_length)
#   } else {
#     ll$hy_id = rep(1:nrow(net), times = ids_length)
#   }
#   
#   # add back cross sections width column
#   ll$cs_widths = rep(cs_widths, times = ids_length)
#   
#   # remove self intersecting transects or not
#   if(rm_self_intersect){
#     ll <- 
#       ll[lengths(sf::st_intersects(ll)) == 1, ] %>% 
#       dplyr::group_by(hy_id) %>% 
#       dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
#       dplyr::ungroup() %>% 
#       dplyr::mutate(lengthm = as.numeric(sf::st_length(.)))
#   } else {
#     ll <- 
#       ll %>% 
#       dplyr::group_by(hy_id) %>% 
#       dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
#       dplyr::ungroup() %>% 
#       dplyr::mutate(lengthm = as.numeric(sf::st_length(.)))
#   }
#   
#   # if original columns of data should be added to transects dataset
#   if(add) {
#     ll <-
#       dplyr::left_join(
#         ll,
#         sf::st_drop_geometry(net),
#         by = c("hy_id" = crosswalk_id)
#         # by = c("hy_id" = "comid")
#       )
#   }
#   
#   # if fix_braids is set to TRUE, then fix the braided transect lines
#   if(fix_braids) {
# 
#     # message(
#     #   paste0("Applying fixes to braided transects using:\n",
#     #          "- Braid detection version: ", version, "\n",
#     #          "- Braid grouping method: ", braid_method
#     #          )
#     #   )
#     
#     ll <- fix_braid_transects(
#       net             = net,
#       transect_lines  = ll,
#       terminal_id     = terminal_id,
#       braid_threshold = braid_threshold,
#       version         = version,
#       method          = braid_method,
#       precision       = precision,
#       rm_intersects   = rm_self_intersect
#     )
#     
#   }
#   
#   # transform CRS back to input CRS
#   if(start_crs != 5070) {
#     # message("Transforming CRS back to EPSG: ", start_crs)
#     ll <- sf::st_transform(ll, start_crs)
#   }
#   
#   return(ll)
#   
# }

#Generate Cross Sections Across Hydrographic Network
#@param net Hydrographic LINESTRING Network
#@param crosswalk_id  Uniuqe Identifier in net
#@param cs_widths Bankfull Widths (length of cross sections for each net element)
#@param num Number of transects per Net element
#@return sf object
#@export
# cut_cross_sections1 = function(net, crosswalk_id = NULL,
#                               cs_widths = 100,
#                               num = 10,
#                               smooth = TRUE,
#                               densify = 2,
#                               rm_self_intersect = TRUE
#                               ){
# 
# 
#   if(smooth){
#     message("Smoothing")
#     net = smoothr::smooth(net, "spline")
#   }
# 
#   if(!is.null(densify)){
#     message("Densifying")
#     net = smoothr::densify(net, densify)
#   }
# 
#   ll = list()
# 
#   if(length(cs_widths) != nrow(net)){
#     cs_widths = rep(cs_widths[1], nrow(net))
#   }
# 
#   if(length(num) != nrow(net)){
#     num = pmax(3, rep(num[1], nrow(net)))
#   }
#   # fin
#   message("Cutting")
#   # nrow(net)
#   # j = 2
#   for(j in 1:nrow(net)){
# 
#     line <- as_geos_geometry(net[j,])
# 
#     vertices <- wk_vertices(line)
# 
#     edges <- as_geos_geometry(
#       wk_linestring(
#         vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#         feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#       )
#     )
# 
#     edges = edges[-c(1, length(edges))]
# 
#     if(!is.null(num)){
#       if(num[j] == 1){
#         edges = edges[as.integer(ceiling(length(edges)/ 2))]
#       } else {
#         edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
#       }
#     }
# 
#     ll[[j]] = get_transects(edges, line, cs_widths[j])
# 
#   }
# 
# 
#   ids_length = lengths(ll)
#   ll = st_as_sf(Reduce(c,ll))
# 
#   if(nrow(ll) == 0){
#     return(NULL)
#   }
# 
#   # plot(ll$geometry)
#   # plot(perp, add = T)
#   # perp_sf <- sf::st_as_sf(perp)
#   # perp2_sf <- sf::st_as_sf(perp2)
#   #
#   # mapview::mapview(ll, color = "red") +
#   #   # mapview::mapview(perp_sf, color = "blue") +
#   #   mapview::mapview(perp2_sf, color = "green")
# 
#   message("Formating")
# 
#   if(!is.null(crosswalk_id)){
#     ll$hy_id = rep(net[[crosswalk_id]], times = ids_length)
#   } else {
#     ll$hy_id = rep(1:nrow(net), times = ids_length)
#   }
# 
#   ll$cs_widths = rep(cs_widths, times = ids_length)
# 
#   if(rm_self_intersect){
#     ll[lengths(st_intersects(ll)) == 1, ] %>%
#       group_by(hy_id) %>%
#       mutate(cs_id = 1:n()) %>%
#       ungroup() %>%
#       mutate(lengthm = as.numeric(st_length(.)))
#   } else {
#     ll %>%
#       group_by(hy_id) %>%
#       mutate(cs_id = 1:n()) %>%
#       ungroup() %>%
#       mutate(lengthm = as.numeric(st_length(.)))
#   }
# 
# }


# cut_cross_sections2 = function(net, 
#                                    crosswalk_id = NULL,
#                                    cs_widths = 100, 
#                                    num = 10,
#                                    smooth = TRUE,
#                                    densify = 2,
#                                    rm_self_intersect = TRUE,
#                                    fix_braids = TRUE,
#                                    braid_threshold = NULL,
#                                    add = FALSE
#                                    ){
#   
#   # net       = net3
#   # crosswalk_id        = "comid"
#   # cs_widths = pmax(50, net3$bf_width * 7)
#   # num       = 10
#   # add       = TRUE
#   # smooth = TRUE
#   # densify = 2
#   # rm_self_intersect = TRUE
#   # add = TRUE
# 
#   # keep track of the CRS of the input to retransform return 
#   start_crs <- sf::st_crs(net, parameters = T)$epsg
#   
#   # check if net CRS is 5070, if not, transform it to 5070
#   if(start_crs != 5070) {
#   # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
#     message("Transforming CRS to EPSG: 5070")
#     net <- sf::st_transform(net, 5070) 
#   }
#   
#   start_time <- Sys.time()
#   
#   if(smooth){ 
#     message("Smoothing")
#     net = smoothr::smooth(net, "ksmooth")
#     # net = smoothr::smooth(net, "spline") 
#   }
#   
#   end_time <- Sys.time()
#   
#   smooth_time = end_time - start_time
#   
#   message("Time to smooth linestrings:\n- ", 
#           round(smooth_time, 1), " ",  units(smooth_time)
#           )
#   
#   start_time <- Sys.time()
#   
#   if(!is.null(densify)){ 
#     message("Densifying")
#     net = smoothr::densify(net, densify) 
#   }
#   
#   end_time <- Sys.time()
#   
#   dense_time = end_time - start_time
#   
#   message("Time to densify linestrings:\n- ", 
#           round(dense_time, 1), " ",  units(dense_time)
#           )
#   
#   
#   ll = list()
#   
#   if(length(cs_widths) != nrow(net)){
#     cs_widths = rep(cs_widths[1], nrow(net))
#   }
#   
#   if(length(num) != nrow(net)){
#     num = pmax(3, rep(num[1], nrow(net)))
#   }
#   
#   message("Cutting")
#   
#   start_time <- Sys.time()
# 
#   for(j in 1:nrow(net)){
#     # message("==== JJJJ: ", j, " =====")
#     line <- as_geos_geometry(net[j,])
#     
#     vertices <- wk_vertices(line)
#     
#     edges <- as_geos_geometry(
#       wk_linestring(
#         vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#         feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#       )
#     )
#     
#     edges = edges[-c(1, length(edges))]
#     
#     if(!is.null(num)){
#       if(num[j] == 1){
#         edges = edges[as.integer(ceiling(length(edges)/ 2))]
#       } else {
#         edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
#       }
#     }
#     # tmp <- get_transects(edges, line, cs_widths[j])
#     # if(is.null(tmp)){
#     #   break
#     # }
#     # ll[[j]] = tmp
#     ll[[j]] = get_transects(edges, line, cs_widths[j])
#   }
#   
#   end_time <- Sys.time()
# 
#   transect_time = end_time - start_time
#   # round(task2_time, 1)
#   # 40/60
#   
#   message("Time to create all transects:\n- ", 
#           round(transect_time, 1), " ",  units(transect_time)
#           )
#   
#   ids_length = lengths(ll)
#   ll = st_as_sf(Reduce(c,ll))
#   
#   if(nrow(ll) == 0){
#     return(NULL)
#   }
#   
#   message("Formating")
#   
#   # add crosswalk_id column if provided as an input
#   if(!is.null(crosswalk_id)){
#     ll$hy_id = rep(net[[crosswalk_id]], times = ids_length)
#   } else {
#     ll$hy_id = rep(1:nrow(net), times = ids_length)
#   }
#   
#   # add back cross sections width column
#   ll$cs_widths = rep(cs_widths, times = ids_length)
# 
#   # remove self intersecting transects or not
#   if(rm_self_intersect){
#     ll <- 
#       ll[lengths(st_intersects(ll)) == 1, ] %>% 
#       group_by(hy_id) %>% 
#       mutate(cs_id = 1:n()) %>% 
#       ungroup() %>% 
#       mutate(lengthm = as.numeric(st_length(.)))
#   } else {
#     ll <- 
#       ll %>% 
#       group_by(hy_id) %>% 
#       mutate(cs_id = 1:n()) %>% 
#       ungroup() %>% 
#       mutate(lengthm = as.numeric(st_length(.)))
#   }
# 
#   # if original columns of data should be added to transects dataset
#   if(add) {
#    ll <-
#      dplyr::left_join(
#                   ll,
#                   sf::st_drop_geometry(net),
#                   by = c("hy_id" = crosswalk_id)
#                   # by = c("hy_id" = "comid")
#                   )
#   }
#   
#   if(fix_braids) {
#     
#     # # fix the braided transects
#     # ll <- fix_braid_transects(
#     #           net             = net, 
#     #           transect_lines  = ll,
#     #           braid_threshold = braid_threshold
#     #         )
#     
#     start_time <- Sys.time()
#     
#     # fix the braided transects
#     ll <- fix_braid_transects(
#                 net             = net,
#                 transect_lines  = ll,
#                 braid_threshold = braid_threshold
#               )
#     
#     end_time <- Sys.time()
#     
#     braid_time = end_time - start_time
#     # round(task2_time, 1)
#     # 
#     # 40/60
#     
#     message("Time to fix braid transects:\n- ", 
#             round(braid_time, 1), " ",  units(braid_time)
#     )
#     
#   }
#   
#   # transform CRS back to input CRS
#   if(start_crs != 5070) {
#     message("Transforming CRS back to EPSG: ", start_crs)
#     ll <- sf::st_transform(ll, start_crs)
#   }
#   
#   return(ll)
#   
# }
# 
# cut_cross_sections3 = function(net, 
#                                crosswalk_id                = NULL,
#                                cs_widths         = 100, 
#                                num               = 10,
#                                smooth            = TRUE,
#                                densify           = 2,
#                                rm_self_intersect = TRUE,
#                                fix_braids        = TRUE,
#                                terminal_id       = NULL,
#                                braid_threshold   = NULL,
#                                add               = FALSE,
#                                use_original      = FALSE
#                                ){
#   
#   # net       = ref_net
#   # crosswalk_id        = "comid"
#   # cs_widths = pmax(50, ref_net$bf_width * 7)
#   # num       = 5
#   # fix_braids = TRUE
#   # add       = TRUE
#   # use_original = T
#   # smooth = TRUE
#   # densify = 2
#   # rm_self_intersect = TRUE
#   # use_original = FALSE
#   
#   # keep track of the CRS of the input to retransform return 
#   start_crs <- sf::st_crs(net, parameters = T)$epsg
#   
#   # check if net CRS is 5070, if not, transform it to 5070
#   if(start_crs != 5070) {
#     # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
#     message("Transforming CRS to EPSG: 5070")
#     net <- sf::st_transform(net, 5070) 
#   }
#   
#   
#   start_time <- Sys.time()
#   
#   # smooth out flowlines
#   if(smooth){ 
#     message("Smoothing")
#     # net = smoothr::smooth(net, "ksmooth")
#     net = smoothr::smooth(net, "spline")
#   }
#   
#   end_time <- Sys.time()
#   
#   smooth_time = end_time - start_time
#   
#   message("Time to smooth linestrings:\n- ", 
#           round(smooth_time, 1), " ",  units(smooth_time)
#   )
#   
#   start_time <- Sys.time()
#   
#   # Densify network flowlines, adds more points to each linestring
#   if(!is.null(densify)){ 
#     message("Densifying")
#     net = smoothr::densify(net, 2) 
#   }
#   
#   end_time <- Sys.time()
#   
#   dense_time = end_time - start_time
#   
#   message("Time to densify linestrings:\n- ", 
#           round(dense_time, 1), " ",  units(dense_time)
#   )
#   
#   # list to store transect outputs
#   ll = list()
#   
#   # if there is a missing number of cross section widths given relative to the number of rows in net, fill in the missing values
#   if(length(cs_widths) != nrow(net)){
#     cs_widths = rep(cs_widths[1], nrow(net))
#   }
#   
#   if(length(num) != nrow(net)){
#     num = pmax(3, rep(num[1], nrow(net)))
#   }
#   
#   message("Cutting")
# 
#   # system.time({
#   start_time <- Sys.time()
#   
#    # iterate through each linestring in "net" and generate transect lines along each line 
#   for(j in 1:nrow(net)){
#   # for(j in 1:30){
#     # message("==== JJJJ: ", j, " =====")
#     
#     # convert sf line to geos_geometry
#     line <- geos::as_geos_geometry(net[j,])
#     
#     # vertices of line
#     vertices <- wk::wk_vertices(line)
#     
#     # create evenly spaced linestring geometries along line of interest
#     edges <- geos::as_geos_geometry(
#       wk::wk_linestring(
#         vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#         feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#       )
#     )
#     
#     # keep all lines except first and last edges
#     edges = edges[-c(1, length(edges))]
#     
#     # create a sequence of edges along 'line'
#     if(!is.null(num)){
#       if(num[j] == 1){
#         edges = edges[as.integer(ceiling(length(edges)/ 2))]
#       } else {
#         edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
#       }
#     }
# 
#     # # # generate transect lines for 'line'
#     # tlines <- get_transects2(edges, line, cs_widths[j])
#     # if(is.null(tlines)) {
#     #   message("STOPPING BC MULTIPOINT ", j)
#     #   break
#     # }
#     
#     if(!use_original) {
#       
#       # cut transect lines at each 'edge' generated along our line of interest
#       ll[[j]] = get_transects2(edges, line, cs_widths[j])
#     
#       } else {
#       
#       # cut transect lines at each 'edge' generated along our line of interest
#       ll[[j]] = get_transects(edges, line, cs_widths[j])
#     
#       }
#     # cut transect lines at each 'edge' generated along our line of interest
#     # ll[[j]] = get_transects2(edges, line, cs_widths[j])
#     # ll[[j]] = tlines
#     # ll[[j]] = get_transects(edges, line, cs_widths[j])
#     # ll[[j]] = check_intersects(get_transects2(edges, line, cs_widths[j]), line)
#   }
#   # })
#   
#   end_time <- Sys.time()
#   
#   transect_time = end_time - start_time
#   # round(task2_time, 1)
#   # 40/60
#   
#   message("Time to create all transects:\n- ", 
#           round(transect_time, 1), " ",  units(transect_time)
#   )
#   
#   # geos::geos_intersects_matrix(tlines, line)
#   ids_length = lengths(ll)
#   ll = st_as_sf(Reduce(c,ll))
#   
#   if(nrow(ll) == 0){
#     return(NULL)
#   }
#   
#   message("Formating")
#   
#   # add crosswalk_id column if provided as an input
#   if(!is.null(crosswalk_id)){
#     ll$hy_id = rep(net[[crosswalk_id]], times = ids_length)
#   } else {
#     ll$hy_id = rep(1:nrow(net), times = ids_length)
#   }
#   
#   # add back cross sections width column
#   ll$cs_widths = rep(cs_widths, times = ids_length)
#   
#   # remove self intersecting transects or not
#   if(rm_self_intersect){
#     ll <- 
#       ll[lengths(st_intersects(ll)) == 1, ] %>% 
#       group_by(hy_id) %>% 
#       mutate(cs_id = 1:n()) %>% 
#       ungroup() %>% 
#       mutate(lengthm = as.numeric(st_length(.)))
#   } else {
#     ll <- 
#       ll %>% 
#       group_by(hy_id) %>% 
#       mutate(cs_id = 1:n()) %>% 
#       ungroup() %>% 
#       mutate(lengthm = as.numeric(st_length(.)))
#   }
#   
#   # if original columns of data should be added to transects dataset
#   if(add) {
#     ll <-
#       dplyr::left_join(
#         ll,
#         sf::st_drop_geometry(net),
#         by = c("hy_id" = crosswalk_id)
#         # by = c("hy_id" = "comid")
#       )
#   }
#   
#   # if fix_braids is set to TRUE, then fix the braided transect lines
#   if(fix_braids) {
#     
#     # # fix the braided transects
#     # ll <- fix_braid_transects(
#     #           net             = net, 
#     #           transect_lines  = ll,
#     #           braid_threshold = braid_threshold
#     #         )
#     
#     start_time <- Sys.time()
#     
#     # fix the braided transects
#     ll <- fix_braid_transects(
#                     net             = net,
#                     transect_lines  = ll,
#                     terminal_id     = terminal_id,
#                     braid_threshold = braid_threshold
#                   )
#     
#     # # fix the braided transects
#     # ll <- fix_braid_transects(
#     #   net             = net,
#     #   transect_lines  = ll,
#     #   braid_threshold = braid_threshold
#     # )
#     
#     
#     end_time <- Sys.time()
#     
#     braid_time = end_time - start_time
#     # round(task2_time, 1)
#     # 
#     # 40/60
#     
#     message("Time to fix braid transects:\n- ", 
#             round(braid_time, 1), " ",  units(braid_time)
#     )
#     
#   }
#   
#   # transform CRS back to input CRS
#   if(start_crs != 5070) {
#     message("Transforming CRS back to EPSG: ", start_crs)
#     ll <- sf::st_transform(ll, start_crs)
#   }
#   
#   return(ll)
#   
# }
