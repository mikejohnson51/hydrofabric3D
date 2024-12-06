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
    "crosswalk_id",  
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
  
  # get the cumulative length of edges along flowline
  edge_lengths <- cumsum(
    geos::geos_length(edges)
  )
  
  # total length of linestring
  total_length <- edge_lengths[length(edge_lengths)]
  
  # keep all lines except first and last edges
  edges <- edges[-c(1, length(edges))]
  
  # keep all edge lengths except first and last edge lengths
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
    
    tran = cut_transect(edges[i], bf_width[i])
    
    # measure of edge
    meas <- edge_lengths[i]
    
    # If a MULTIPOINT, then it crosses more the once
    if (geos::geos_type(geos::geos_intersection(tran, line)) == "point") {
      # Ensure that there are no intersections with previously computed cross sections
      if (!any(geos::geos_intersects(tran, transects))) {
        transects <- vctrs::vec_c(transects, tran)
        measures  <- vctrs::vec_c(measures, meas)
      }
    }
  }
  
  # index for only valid transects
  is_valid <- !geos::geos_is_empty(transects)
  
  # extract only edge lengths of remaining transect lines only valid edge lengths
  measures <- measures[is_valid[-1]]
  
  # # calculate cs_measure value
  edge_measure <- (measures/total_length) * 100
  
  # drop empty geos geometries
  transects <- transects[is_valid]
  
  transects <- sf::st_as_sf(transects)
  transects$ds_distance <- measures
  transects$cs_measure  <- edge_measure
  
  return(transects)
  
}

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
#' @param braid_method The method to determine the geometries to cut. Options are "crosswalk_id", "component", or "neighbor". Default is "crosswalk_id"
#' @param precision int, distance in meters. Only applicable when fix_braids = TRUE. This is the number of meters to approximate final cross section linestring length. Increasing this value will decrease runtime of cross section extension algorithm. Value you must be greater than 0. Default is 1
#' @param add logical indicating whether to add original 'net' data to the outputted transect lines. Default is FALSE.
#' @param verbose logical, whether to output messages or not. Default is TRUE, and messages will be given 
#' 
#' @return sf object of transect linestrings 
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
    braid_method      = "crosswalk_id",
    precision         = 1,
    add               = FALSE,
    verbose           = TRUE
) {
  
  suppressWarnings({
    # -------------------------------------------
    # Input validation
    # -------------------------------------------
    
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
                                      add = add,
                                      verbose = verbose
    )
    
    # make a unique ID if one is not given (NULL 'crosswalk_id')
    if (is.null(crosswalk_id)) {
      net           <- add_hydrofabric_id(net) 
      crosswalk_id  <- 'hydrofabric_id'
    }
    
    # standardize geometry name
    net <- hydroloom::rename_geometry(net, "geometry")
  
    REQUIRED_COLS <- c(crosswalk_id, "geometry")
  
    # validate input dataframe has correct columns  
    is_valid <- validate_df(net, REQUIRED_COLS, "net")  
  
    # -------------------------------------------------------
    # Preserve initial_order of flowlines 
    #   - Add an initial ordering column to
    #     ensure transects are returned 
    #     in the same order as the provided flowlines
    # -------------------------------------------------------
    
    # Add an initial ordering column to ensure transects are returned in the same order as the provided flowlines
    net <- 
      net %>% 
      add_initial_order()
    
    # Densify flowlines (adds more points to each linestring)
    # smooth out flowlines
    # ensure that each flowline has a minimum of 4 points to guarentee cross sectiosn can be cut
    net <- prep_flowlines(flowlines = net, 
                   densify = densify, 
                   smooth = smooth, 
                   verbose = verbose
                   )
    
    # list to store transect outputs
    transects <- list()
    
    # if there is a missing number of cross section widths given relative to the number of rows in net, fill in the missing values
    if (length(cs_widths) != nrow(net)) {
      cs_widths = rep(cs_widths[1], nrow(net))
    }
    
    if (length(num) != nrow(net)) {
      num = pmax(3, rep(num[1], nrow(net)))
    }
    
    if(verbose) { message("Cutting transects") }
    
    # iterate through each linestring in "net" and generate transect lines along each line 
    for (j in 1:nrow(net)) {
      
      # cut transect lines at each 'edge' generated along our line of interest
      trans <- get_transects(
        line     = geos::as_geos_geometry(net$geometry[j]),
        bf_width = cs_widths[j],
        n        = num[j]
      )
      
      # if 0 transects can be formed, skip the iteration
      if(nrow(trans) == 0) {
        next
      }
      
      # assign hy_id from net
      trans[[crosswalk_id]]   <- net[[crosswalk_id]][j]
      trans$cs_widths         <- cs_widths[j]
      
      # insert 'trans' sf dataframe into list
      transects[[j]] <- trans
      
    }
    
    # bind list of sf dataframes of transects back together
    transects <- dplyr::bind_rows(transects)
    
    if(nrow(transects) == 0){
      return(NULL)
    }
    
    if(verbose) { message("Formatting transects") }
    
    # -------------------------------------------
    # Removing self intersections
    # -------------------------------------------
    
    # remove self intersecting transects or not
    if (rm_self_intersect) {
     
      transects <- 
        transects %>% 
        rm_multi_intersects()
    } 
    
    # -------------------------------------------
    # Add transect attribute columns
    # - cs_id
    # - lengthm
    # - initial_order (temporary)
    # -------------------------------------------
    
    # add cs_id and lengthm columns
    transects <- 
      transects %>% 
      add_cs_id_sequence(crosswalk_id = crosswalk_id) %>% 
      add_length_col(length_col = "lengthm")
    
    # add the initial_order column to the transects
    transects <-
      transects %>% 
      dplyr::left_join(
        net %>% 
          sf::st_drop_geometry() %>% 
          dplyr::select(dplyr::any_of(crosswalk_id), initial_order),
        by = crosswalk_id
      )
    
    
    # if original columns of data should be added to transects dataset
    if(add) {
      transects <-
        dplyr::left_join(
          transects,
          net %>% 
            sf::st_drop_geometry() %>% 
            dplyr::select(-initial_order),
          by = crosswalk_id
        )
    }
    
    # if fix_braids is set to TRUE, then fix the braided transect lines
    if(fix_braids) {
      
      if(verbose) { message("Fixing braided transects") }
      
      transects <- fix_braided_transects(
        network         = net,
        transect_lines  = transects,
        crosswalk_id    = crosswalk_id,
        braid_threshold = braid_threshold,
        method          = braid_method,
        precision       = precision,
        rm_intersects   = rm_self_intersect
      )
    } 
    
    # if transects were NOT updated to try and fix_braids, then we want to remove transects that intersect with multiple flowlines
    # braided transects by definition, may cross over multiple flowlines
    if (!fix_braids) {
      
      # NOTE: IF we DID NOT do braid fixing, which could cause a transect to purposefully intersect multiple flowlines
      transects <- rm_multiflowline_intersections(transects = transects, flowlines = net)
      
    }
    

    # regenerate the cs_id after multiflowline intersections are removed
    transects <- 
      transects %>% 
      add_cs_id_sequence(crosswalk_id) %>% 
      add_length_col(length_col = "lengthm")
    
    # -----------------------------
    # Sinuosity calculation 
    # -----------------------------
    
    # calculate sinuosity and add it as a column to the cross sections
    transects <- get_cs_sinuosity(
      flowlines      = net, 
      transects      = transects, 
      crosswalk_id   = crosswalk_id,
      add            = TRUE
    )
    
    # -----------------------------
    # Final reordering 
    # -----------------------------
    
      transects <- 
        transects %>% 
        dplyr::arrange(initial_order, cs_id) 
    
    # select all relevent columns and set output columns order
    transects <-
      transects %>%
      dplyr::rename("cs_lengthm" = cs_widths) %>% 
      dplyr::select(
        dplyr::any_of(
          c(
            crosswalk_id,
            "cs_id",
            "cs_lengthm", 
            "cs_measure",
            "ds_distance",
            "sinuosity",
            "geometry"
            )
          )
      )
    
    return(transects)
  
    })
}

#' Prepare flowlines have a more dense and/or smoother surface for cutting transects
#'
#' @param flowlines sf dataframe of flowline linestrings
#' @param densify numeric, if NULL, no densification happens. Default is NULL
#' @param smooth logical, whether to smooth linestrings 
#' @param verbose logical
#' @importFrom smoothr densify smooth 
#' @return sf dataframe
#' @export
prep_flowlines <- function(flowlines, 
                           densify = NULL, 
                           smooth = FALSE, 
                           verbose = TRUE
) {
  
  # Densify network flowlines, adds more points to each linestring
  if(!is.null(densify)){ 
    if(verbose) { message("Densifying") }
    flowlines <- smoothr::densify(flowlines, densify) 
  }
  
  # smooth out flowlines
  if(smooth){ 
    if(verbose) { message("Smoothing") }
    flowlines <- smoothr::smooth(flowlines, "spline")
  }
  
  flowlines <- force_min_npts_per_flowlines(flowlines)
  
  return(flowlines)
  
}

#' Require each linestring to have a minimum number of points
#'
#' @param lines sf linestring dataframe
#' @param min_npts numeric
#'
#' @importFrom geos geos_num_coordinates
#' @importFrom hydroloom rename_geometry
#' @importFrom sf st_line_sample st_cast st_geometry 
#' @noRd
#' @keywords internal
#' @return sf linestring dataframe with added points to lines with less than min_npts
set_min_num_pts_per_line <- function(lines, min_npts = 4) {
  
  lines     <- hydroloom::rename_geometry(lines, "geometry")
  is_valid  <- validate_df(lines, c("geometry"), "lines")
  
  if(!is.numeric(min_npts)) {
    stop("Invalid type for 'min_npts' argument, must be numeric")
  }
  
  node_counts      <- geos::geos_num_coordinates(lines)
  idxs_to_densify  <- node_counts < min_npts
  
  no_lines_need_added_pts <- !any(idxs_to_densify)
  
  if(no_lines_need_added_pts) {
    return(lines)
  }
  
  # pull out the lines to densify, then sample the min_npts for each line and cast back to LINESTRING
  dense_lines <- lines[idxs_to_densify, ]
  
  dense_lines <- 
    dense_lines %>% 
    sf::st_line_sample(min_npts) %>% 
    sf::st_cast("LINESTRING")
  
  sf::st_geometry(lines[idxs_to_densify, ]) <- sf::st_geometry(dense_lines)
  
  return(lines)
  
}

#' Require each flowline linestring to have at minimum 4 points per linestring 
#'
#' @param lines sf linestring dataframe
#'
#' @importFrom geos geos_num_coordinates
#' @importFrom hydroloom rename_geometry
#' @importFrom smoothr densify
#' @importFrom sf st_geometry 
#' @noRd
#' @keywords internal
#' @return sf linestring dataframe with added points to lines with less than 4 points 
force_min_npts_per_flowlines <- function(lines) {
  
  lines     <- hydroloom::rename_geometry(lines, "geometry")
  is_valid  <- validate_df(lines, c("geometry"), "lines")
  
  node_counts      <- geos::geos_num_coordinates(lines)
  idxs_to_densify  <- node_counts < 4
  
  no_lines_need_added_pts <- !any(idxs_to_densify)
  
  if(no_lines_need_added_pts) {
    return(lines)
  }
  
  # pull out the lines to densify, then sample the min_npts for each line and cast back to LINESTRING
  dense_lines <- lines[idxs_to_densify, ]
  
  dense_lines <- 
    dense_lines %>% 
    smoothr::densify(n = 3) 
  
  sf::st_geometry(lines[idxs_to_densify, ]) <- sf::st_geometry(dense_lines)
  
  return(lines)
  
}

#' Remove transect lines that intersect with more than one flowline
#'
#' @param transects sf linestring dataframe of transect lines
#' @param flowlines sf linestring dataframe of flowlines
#' 
#' @importFrom sf st_intersects 
#' @return sf linestring dataframe
#' @export
rm_multiflowline_intersections <- function(transects, flowlines) {
  
  transects <- transects[lengths(sf::st_intersects(transects, flowlines)) == 1, ]
  
  return(transects)
  
}

#' Selectively removes intersecting transect lines
#' Attempts to remove transects intersecting other transects by first removing transects that interesect the most other transects, then re checking intersection condition,  and doing this until there are no multi intersections
#' this gives the benefit of removing a transect line that intersects many other transects, potentially leaving those other transects with no extraneous intersections ONCE the MULTI intersecting transect is removed
#' @param x sf dataframe of linestrings
#' @importFrom sf st_intersects
#'
#' @return sf dataframe
#' @export
rm_multi_intersects <- function(x) {
  
  while (any(lengths(sf::st_intersects(x)) > 1)) {
    
    intersect_counts <- lengths(sf::st_intersects(x))
    max_crossings    <- which(intersect_counts == max(intersect_counts))
    x                <- x[-max_crossings, ]
  }
  
  return(x)
  
}

#' Remove linestrings that intersect with any other linestring 
#'
#' @param x sf linestring dataframe 
#' @noRd
#' @keywords internal
#' @importFrom sf st_intersects 
#' @return sf linestring dataframe
#' @export
rm_self_intersections <- function(x) {
  
  x <- x[lengths(sf::st_intersects(x)) == 1, ]
  
  return(x)
  
}

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
    
  }
  
  # drop empty geometries in the vector
  to_keep <- to_keep[!geos::geos_is_empty(to_keep)]
  
  return(to_keep)
  
}
