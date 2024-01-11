#' Generate a Perpendicular Linestring of a Given Width
#' @param edge LINESRTING
#' @param width Length of Perpendicular LINESTRING
#' @return GEOS object
#' @importFrom geos geos_interpolate_normalized geos_point_end geos_x geos_y geos_length
#' @importFrom wk wk_transform wk_affine_compose wk_affine_translate wk_affine_scale wk_affine_rotate wk_set_crs wk_crs
#' @export
cut_transect = function(edge, width){
  
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
#' @export
get_transects <- function(line, bf_width, n) {
  
  # if NOT a geos_geometry class, coerce
  if(!inherits(line, "geos_geometry")) {
    # convert to geos geometry
    line <- geos::as_geos_geometry(line)
  }
  
  # vertices of line
  vertices <- wk::wk_vertices(line)
  
  # create evenly spaced linestring geometries along line of interest
  edges <- geos::as_geos_geometry(
                  wk::wk_linestring(
                    vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
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
    
    # If a MULTIPOINT, then it crosses more the once
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
  # return(list(
  #         transects = transects,
  #         cs_measure = measures
  #         # cs_measure = edge_lengths
  #         )
  #       )
  
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
get_cs_sinuosity <- function(
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

#' Generate Cross Sections Across Hydrographic Network
#'
#' @param net Hydrographic LINESTRING Network
#' @param id Unique Identifier in net
#' @param cs_widths Bankfull Widths (length of cross sections for each net element)
#' @param num Number of transects per Net element
#' @param smooth logical, whether to smooth linestring geometries or not. Default is TRUE.
#' @param densify numeric, how many times more points should be added to linestrings. Default is 2.
#' @param rm_self_intersect logical, whether to remove self intersecting transect linestrings
#' @param fix_braids logical, whether to fix braided transect lines or not. If TRUE, linestrings that are part of a braided network are augmented. Default is FALSE.
#' @param terminal_id character, column name containing a unique identifier, delineating separate networks in the 'net' dataset. Default is NULL which will best effort determine the connected components in the network to try and create a 'component_id' column in 'net' 
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#' @param version integer, version number of braid algorithm to use, either 1 or 2. Default is 2.
#' @param braid_method The method to determine the geometries to cut. Options are "comid", "component", or "neighbor". Default is "comid"
#' @param precision int, distance in meters. Only applicable when fix_braids = TRUE. This is the number of meters to approximate final cross section linestring length. Increasing this value will decrease runtime of cross section extension algorithm. Value you must be greater than 0. Default is 1
#' @param add logical indicating whether to add original 'net' data to the outputted transect lines. Default is FALSE.
#'
#' @return sf object
#' @importFrom dplyr group_by mutate ungroup n left_join all_of
#' @importFrom sf st_crs st_transform st_intersects st_length st_drop_geometry st_as_sf
#' @importFrom smoothr smooth densify
#' @importFrom geos as_geos_geometry
#' @importFrom wk wk_vertices wk_linestring
#' @export
cut_cross_sections <- function(
    net, 
    id                = NULL,
    cs_widths         = 100, 
    num               = 10,
    smooth            = TRUE,
    densify           = 2,
    rm_self_intersect = TRUE,
    fix_braids        = FALSE,
    terminal_id       = NULL,
    braid_threshold   = NULL,
    version           = 2,
    braid_method      = "comid",
    precision         = 1,
    add               = FALSE
) {
  
  # keep track of the CRS of the input to retransform return 
  start_crs <- sf::st_crs(net, parameters = T)$epsg
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs != 5070) {
    # message("Transforming CRS to EPSG: 5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # smooth out flowlines
  if(smooth){ 
    message("Smoothing")
    # net = smoothr::smooth(net, "ksmooth")
    net = smoothr::smooth(net, "spline")
  }
  
  # Densify network flowlines, adds more points to each linestring
  if(!is.null(densify)){ 
    message("Densifying")
    net = smoothr::densify(net, densify) 
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
    # logger::log_info("{j} / {nrow(net)}")
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
    trans$hy_id <- net[[id]][j]
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
  
  if(nrow(transects) == 0){
    return(NULL)
  }
  
  message("Formating")
  
  # # add id column if provided as an input
  # if (!is.null(id)) {
  #   transects$hy_id = rep(net[[id]], times = ids_length)
  # } else {
  #   transects$hy_id = rep(1:nrow(net), times = ids_length)
  # }
  # 
  # # add back cross sections width column
  # transects$cs_widths = rep(cs_widths, times = ids_length)

  # remove self intersecting transects or not
  if(rm_self_intersect){
    transects <- 
      transects[lengths(sf::st_intersects(transects)) == 1, ] %>% 
      dplyr::group_by(hy_id) %>% 
      dplyr::mutate(cs_id = 1:dplyr::n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(lengthm = as.numeric(sf::st_length(.)))
  } else {
    transects <- 
      transects %>% 
      dplyr::group_by(hy_id) %>% 
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
        by = c("hy_id" = id)
        # by = c("hy_id" = "comid")
      )
  }
  
  # if fix_braids is set to TRUE, then fix the braided transect lines
  if(fix_braids) {
    
    # message(paste0("Applying fixes to braided transects using:\n",
    #          "- Braid detection version: ", version, "\n",
    #          "- Braid grouping method: ", braid_method
    #          ))
    
    transects <- fix_braid_transects(
      net             = net,
      transect_lines  = transects,
      terminal_id     = terminal_id,
      braid_threshold = braid_threshold,
      version         = version,
      method          = braid_method,
      precision       = precision,
      rm_intersects   = rm_self_intersect
    )
  }
  
  # remove any transect lines that intersect with any flowlines more than 1 time
  transects <- transects[lengths(sf::st_intersects(transects, net)) == 1, ]
  
  # rename "id" column to hy_id if "hy_id" is not already present
  if(!"hy_id" %in% names(net)) {
    net <- dplyr::rename(net, hy_id = dplyr::all_of(id))
  }

  # calculate sinuosity and add it as a column to the cross sections
  transects <- get_cs_sinuosity(
            lines          = net, 
            cross_sections = transects, 
            add            = TRUE
            )
  
  # transform CRS back to input CRS
  if(start_crs != 5070) {
    # message("Transforming CRS back to EPSG: ", start_crs)
    transects <- sf::st_transform(transects, start_crs)
  }
  
  return(transects)
  
}

#' Get Points across transects with elevation values
#' @param cs character, Hydrographic LINESTRING Network file path
#' @param points_per_cs the desired number of points per CS. If NULL, then approximately 1 per grid cell resultion of DEM is selected.
#' @param min_pts_per_cs Minimum number of points per cross section required.
#' @param dem the DEM to extract data from
#' @return sf object cross section points along the 'cs' linestring geometries
#' @importFrom dplyr mutate group_by ungroup n select everything relocate last_col bind_rows filter
#' @importFrom terra linearUnits res rast extract project vect crs 
#' @importFrom sf st_line_sample st_set_geometry st_cast
#' @export
cross_section_pts = function(
    cs             = NULL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
){
  
  ### ### ## ## ### ## ### ##
  
  # cs             = tmp_trans
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = DEM_URL
  # scale          = 5

  ## ### ### ### ### #### ##

  # check if a cross section is given, and return NULL if missing
  if (is.null(cs)) {
    return(NULL)
  }
  
  # check if a file path or not
  if(is.character(cs)) {
    # Read in file
    cs <- sf::read_sf(cs)
  }
  
  # add points per cross sections 
  cs <- add_points_per_cs(
    cs             = cs,
    points_per_cs  = points_per_cs,
    min_pts_per_cs = min_pts_per_cs,
    dem            = dem
  )
  
  
  # Extract DEM "Z" values for each point along cross section linestrings
  cs_pts <- extract_dem_values(cs = cs, dem = dem)
  
  return(cs_pts)
  
}

#' Check for flat cross sections and try to update these values by extending the original cross sections and reextracting DEM values
#' @param net Hydrographic LINESTRING Network
#' @param cs character, Hydrographic LINESTRING Network file path
#' @param cs_pts Output from extract_dem_values_first
#' @param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
#' @param min_pts_per_cs Minimun number of points per cross section required.
#' @param dem the DEM to extract data from
#' @param scale numeric, If a transect line DEM extraction results in all equal Z values,
#'  by what percent of the transect lines length (meters) should the transect line be
#'   extended in both directions to try to capture representative Z values ? Default is 0.5 (50% of the transect length)
#' @param threshold numeric, threshold Z value (meters) that determines if a cross section is flat. 
#' A threshold = 0 means if all Z values are the same, then the cross section is considered flat. 
#' A threshold value of 1 means that any cross section with Z values all within 1 meter of eachother, is considered flat. Default is 0.
#' @importFrom dplyr mutate relocate last_col group_by ungroup n select everything relocate last_col bind_rows filter
#' @importFrom sf st_intersection st_is st_intersects
#' @importFrom nhdplusTools rename_geometry
#' @return sf object of cs_pts with "flat" cross sections removed/updated with longer transects to capture more Z data
#' @export
rectify_flat_cs = function(
    net            = NULL,
    cs             = NULL,
    cs_pts         = NULL, 
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
    scale          = 0.5,
    threshold      = 0
    ) {
  
  ### ### ## ## ### ## ### ##
  ## ### ### ### ### #### ##
  
  # add a "tmp_id" column to easily index transects by hy_id and cs_id 
  cs <- dplyr::mutate(cs,
                      tmp_id = paste0(hy_id, "_", cs_id)
                      ) 
  
  # Check if any cross sections are "flat" within a threshold (All Z values are the same or the difference is within the threshold)
  flat_cs <- check_z_values(pts = cs_pts, threshold = threshold)
  
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
    # dplyr::mutate(
    #   # tmp_id = paste0(hy_id, "_", cs_id)
    #   is_extended = FALSE
    # ) %>%
    dplyr::filter(tmp_id %in% unique(
      dplyr::mutate(flat_cs,
                    tmp_id = paste0(hy_id, "_", cs_id))$tmp_id
    )) %>%
    dplyr::select(-tmp_id) 
    # dplyr::relocate(geom, .after = dplyr::last_col())

  # loop through geometries that might need to be extended, try to extend, and then update 
  # the 'to_extend' values IF the extended transectr does NOT violate any intersection rules
  for(i in 1:nrow(to_extend)) {
    # message("i: ", i)
    # extend transect out by "scale" % of lines length
    extended_tran <- extend_by_percent(
                        x          = to_extend[i, ],
                        pct        = scale, 
                        length_col = "cs_lengthm"
                        )
    
    # filter down to the rest of the transects on the given "hy_id", EXCLUDING SELF
    neighbor_transects <- dplyr::filter(cs, 
                  hy_id == to_extend[i, ]$hy_id,
                  cs_id != to_extend[i, ]$cs_id
                  )
    
    # # filter down to ALL OF THE OTHER TRANSECTS (EXCEPT SELF) 
    # neighbor_transects <- dplyr::filter(cs, tmp_id != to_extend[i, ]$tmp_id)
    
    # plot(extended_tran$geom, col = "red", add = F)
    # plot(net[net$id == to_extend[i, ]$hy_id, ]$geom, add = T)
    # mapview::mapview(net, color = "dodgerblue") +  
    # mapview::mapview(to_extend, color = "red") + 
    #   mapview::mapview(extended_tran, color = "green")
    
    # Make sure that newly extended line only interesects its origin flowline at MOST 1 time
    # AND that the newly extended transect does NOT intersect with any previously computed transect lines
    # mapview::mapview(       extended_tran) +
    #                         net[net$id == to_extend[i, ]$hy_id, ]
    
    fline_intersect <- sf::st_intersection(
        extended_tran,
        net[net$id == to_extend[i, ]$hy_id, ]
        # dplyr::filter(net, id == to_extend[i, ]$hy_id)
      )

    if(nrow(fline_intersect) > 0) {
      
      # Check that newly extended cross section only interesects its origin flowline at MOST 1 time (This value will be a "MULTIPOINT" if it intersects more than once)
      if (
        sf::st_is(
          fline_intersect, "POINT"
          ) &&
          # Check that extended transect doesn't intersect with any of the NEWLY EXTENDED cross sections
          !any(sf::st_intersects(
            extended_tran,
            to_extend[-i, ],
            sparse = FALSE
          )) &&
          # Check that extended transect doesn't intersect with any of the original cross sections on this "hy_id"
          !any(sf::st_intersects(
            extended_tran,
            neighbor_transects,
            sparse = FALSE
          ))
          ) {
        
        # # set is_extended to TRUE
        # extended_tran$is_extended <- TRUE
        
        # replace old transect with extended geometry and updated lengths, etc.
        to_extend[i, ] <- extended_tran
      
      }
    }
    # message("=========")
  }
  
  # # extend linestring geometries by a percent of linestring length
  # extended <- extend_by_percent(x = to_extend, pct = scale, length_col = "cs_lengthm")
  
  # add cross section points to extended cross sections
  extended <- add_points_per_cs(
    cs             = to_extend,
    points_per_cs  = points_per_cs,
    min_pts_per_cs = min_pts_per_cs,
    dem            = dem
  )
  
  # extract DEM values for newly extended cross sections
  extended_pts <- extract_dem_values(cs = extended, dem = dem)
 
  # take the below points, and put them back into "cs_pts" object
  # then go back to the input "transects" ("cs") object and update the transect geometries based on the extensions done above^^
  # then resave the input transects dataset back to its original location....
  extended_pts <- 
    extended_pts %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      is_same_Z = max(Z) - min(Z) <= threshold
    ) %>% 
    dplyr::ungroup() %>%    
    dplyr::mutate(
      tmp_id = paste0(hy_id, "_", cs_id)
    )
  
  # separate newly extended cross sections with new Z values into groups (those that show "good" DEM values after extension are kept) 
  to_keep <- dplyr::filter(extended_pts, !is_same_Z)
  to_drop <- dplyr::filter(extended_pts, is_same_Z)
  
  # filter out cross section points that have "same Z" values (remove flat Z values)
  final_pts <-
    cs_pts %>%  
    dplyr::mutate(
      tmp_id = paste0(hy_id, "_", cs_id)
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
    ) %>% 
    dplyr::mutate(
      is_extended = FALSE
    ) %>% 
    dplyr::bind_rows(
      dplyr::select(
        dplyr::mutate(
          to_keep,
          is_extended = TRUE
        ), 
        -is_same_Z)
    ) %>% 
    dplyr::select(-tmp_id) 
  
  # rename geometry column to "geom" 
  final_pts <- nhdplusTools::rename_geometry(final_pts, "geom")
  
  # move geom column to the last column
  final_pts <- dplyr::relocate(final_pts, geom, .after = dplyr::last_col())
  
  # final_pts$is_extended %>% table()
  
  return(final_pts)
}

#' Given a set of linestrings, extract DEM values at points along the linestring
#'
#' @param cs cross section sf object
#' @param dem SpatRaster DEM or character pointing to remote DEM resource
#' @importFrom dplyr mutate group_by n ungroup select everything
#' @importFrom sf st_set_geometry st_line_sample st_cast
#' @importFrom terra extract project vect crs rast
#' @return sf dataframe with Z values extracted from DEM
extract_dem_values <- function(cs, dem) {
  
  extract_pt_val <- function(rast, pts) {
    terra::extract(
      rast,
      terra::project(terra::vect(pts), terra::crs(rast))
    )[, 2]
  }
  
  suppressWarnings({
    cs_pts <- 
      sf::st_set_geometry(cs, sf::st_line_sample(cs, cs$points_per_cs)) %>% 
      sf::st_cast("POINT") %>%
      dplyr::mutate(Z = extract_pt_val(terra::rast(dem), .)) %>% 
      dplyr::group_by(hy_id, cs_id) %>% 
      dplyr::mutate(
        pt_id = 1:dplyr::n(),
        relative_distance = seq(from = 0, to = cs_lengthm[1], length.out = dplyr::n())
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(hy_id, cs_id, pt_id, Z, cs_lengthm, relative_distance, dplyr::everything())
  })
    
  return(cs_pts)
  
}

#' Check for any Z values that are all equal or within a given threshold value
#'
#' @param pts sf points dataframe
#' @param threshold numeric, default is 1 meter
#' @importFrom dplyr select group_by mutate filter slice ungroup
#' @importFrom sf st_drop_geometry st_line_sample st_cast
#' @return dataframe with hy_id, cs_id, Z, and is_same_Z value columns
check_z_values <- function(pts, threshold = 1) {
  
  # check for any flat cross sections (All Z values are equal within a given cross section)
  flat_pts <-
    pts %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(hy_id, cs_id, Z) %>%
    # dplyr::filter(hy_id != "wb-2959") %>%
    # dplyr::filter(!hy_id %in% c("wb-2959", "wb-2960", "wb-4131", "wb-4364", "wb-4365", "wb-4770")) %>%
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      is_same_Z = max(Z) - min(Z) <= threshold
      # is_same_Z = as.integer(dplyr::n_distinct(Z) == 1)
    ) %>%
    dplyr::filter(is_same_Z) %>%
    # dplyr::filter(is_same_Z == 1) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() 
  
  return(flat_pts)
}

#' Extend an sf linestring dataframe by a percent of the lines length
#'
#' @param x linestring sf dataframe
#' @param pct numeric, percent of line to extend linestring by in both directions
#' @param length_col character, name of the column in "x" that has the length of the linestring (meters)
#'
#' @return sf dataframe with extended linestring geometries
extend_by_percent <- function(
    x, 
    pct        = 0.5, 
    length_col = NULL
) {

  # x = to_extend
  # pct = scale
  # length_col = "cs_lengthm"
  # length_col = NULL
  
  # rename the geometry to "geom"
  x <- nhdplusTools::rename_geometry(x, "geom")
  
  # length_col is NULL then set it to "cs_lengthm"
  if(is.null(length_col)) {
    length_col = "cs_lengthm"
  }
  
  #  if the length_col string is not a column in the x,
  # then create a column based on the length of the linestring using "length_col" as name of column 
  if (!length_col %in% names(x)) {
    
    # add a "length_col" column of the length of each linestring in meters
    x[length_col] <- as.numeric(sf::st_length(sf::st_geometry(x)))
    # x <- dplyr::mutate(x,  length_col = as.numeric(sf::st_length(.)))
  }
  
  # extend linestrings by pct * length of line
  extended_df <-
    x %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      extended_geom = geos_extend_line(
                          geom, 
                          distance = (pct)*(!!dplyr::sym(length_col)),
                          dir      = "both"
                        ) 
    ) %>% 
    dplyr::ungroup()
  
  # drop original geometry column
  extended_df <-  sf::st_drop_geometry(extended_df)
  
  # set the extended geometry as the new geometry
  extended_df$extended_geom <- sf::st_geometry(sf::st_as_sf(extended_df$extended_geom))
  
  # make extended_df an sf object
  extended_df <- sf::st_as_sf(
                        extended_df, 
                        crs = sf::st_crs(x)
                      )
  
  # rename "extended_geom" col to "geom"
  extended_df <- dplyr::rename(extended_df, "geom" = "extended_geom")
  
  # recalculate length of linestring and update length_col value
  extended_df[[length_col]] <- as.numeric(sf::st_length(extended_df$geom))
  
  # extended_df$extended_geom %>%
  #   plot()
  # plot(x$geom, add = T, col = "red")
  # plot(out_geom$geometry)
  # plot(x$geom[1], add = T, col = "red")
  
  return(extended_df)
  
}

# 
#' Add a points per cross section column to an sf dataframe of linestrings given a DEM and min points value
#' 
#' This function calculates and adds a column called 'points_per_cs' to an sf dataframe
#' representing cross-sections (linestrings) based on a provided DEM and a minimum points
#' value per cross section.
#'
#' @param cs An sf dataframe representing cross-sections (linestrings).
#' @param points_per_cs numeric, number of points per cross section. Default is NULL
#' @param min_pts_per_cs An optional minimum points value per cross section. If not provided, 
#' @param dem A SpatRaster object representing the Digital Elevation Model (DEM) or a character string referencing a remote resource.
#' the function calculates it based on the length of cross-sections and the resolution of the DEM.
#'
#' @return An updated sf dataframe with the 'points_per_cs' column added.
add_points_per_cs <- function(cs,
                              points_per_cs  = NULL,
                              min_pts_per_cs = 10,
                              dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
                              ) {
  
  # If NULL value is given to points_per_cs argument, calculate points_per_cs values
  # - IF DEM has a longitude/latitude CRS (terra::linearUnits == 0):
  # -- then divide the cross section length by 111139 and divide that resulting value by the minimum resolution value from the DEM (then round the result up)
  # - ELSE:
  # -- just divide the cross section length by the minimum resolution value from the DEM (then round the result up)
  if (is.null(points_per_cs)) {
    if (terra::linearUnits(terra::rast(dem)) == 0) {
      points_per_cs = ceiling(
        (cs$cs_lengthm / 111139) / min(terra::res(terra::rast(dem)))
      )
    } else {
      points_per_cs = ceiling(
        (cs$cs_lengthm) / min(terra::res(terra::rast(dem)))
      )
    }
    
  }
  # else {
  #   points_per_cs = min_pts_per_cs
  # }
  
  # Take the max between the given minimum points per cross section and the derived points per cross section
  cs$points_per_cs = pmax(min_pts_per_cs, points_per_cs)
  
  return(cs)
}

#' Classify Cross Section Points 
#' @param cs_pts CS points
#' @return sf object
#' @export
classify_points = function(cs_pts){
  
  . <-  L <-  L1 <-  L2  <-  R  <-  R1 <-  R2  <- Z  <-  Z2 <-  anchor <-  b1  <- b2  <- cs_lengthm  <- count_left <- 
    count_right  <-  cs_id <-  hy_id <-  in_channel_pts  <- lengthm <-  low_pt  <- max_bottom  <- mean_dist <-  mid_bottom  <- min_bottom  <- pt_id <- relative_distance <-  third <- NULL

  dplyr::filter(cs_pts) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
          third = ceiling(n() / 3),
           mean_dist = mean(diff(relative_distance)),
           in_channel_pts = ceiling(cs_lengthm[1] / mean_dist),
           b1 = ceiling(in_channel_pts / 2),
           b2 = in_channel_pts - b1,
           low_pt  = min(Z[third[1]:(2*third[1] - 1)]),
           class = ifelse(Z <= low_pt & between(pt_id, third[1], (2*third[1] - 1)), 
                          "bottom", 
                          "bank"),
           Z2 = c(Z[1], zoo::rollmean(Z, 3), Z[n()]),
           Z = ifelse(class == "bottom", Z, Z2),
           min_bottom = which(class == "bottom")[1],
           mid_bottom = which(class == "bottom")[ceiling(length(which(class == "bottom"))/2)],
           max_bottom = which(class == "bottom")[length(which(class == "bottom"))],
           L1 = pmax(1, mid_bottom - b1),
           L2 = pmax(1, mid_bottom - b2),
           R1 = pmin(mid_bottom + b2, n()),
           R2 = pmin(mid_bottom + b1, n()),
           anchor = ifelse(Z[R2] < Z[L1], 2, 1),
           L = pmax(third, ifelse(anchor == 1, L1, L2)),
           R = pmin(2*third[1], ifelse(anchor == 1, R1, R2)),
           count_left = min_bottom - L,
           count_right = R - max_bottom,
           L = ifelse(count_left == 0, L - count_right, L),
           R = ifelse(count_right == 0, R + count_left, R),
           class = ifelse(between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
           class = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
           class = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(point_type = class) %>% 
    dplyr::select(hy_id, cs_id, pt_id, Z, relative_distance, cs_lengthm, class, point_type)
  
}

#' Get Points across transects with elevation values
#' @param cs character, Hydrographic LINESTRING Network file path
#' @param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
#' @param min_pts_per_cs Minimun number of points per cross section required.
#' @param dem the DEM to extract data from
#' @param scale numeric, If a transect line DEM extraction results in all equal Z values,
#'  by what percent of the transect lines length (meters) should the transect line be
#'   extended in both directions to try to capture representative Z values ? Default is 0.5 (50% of the transect length)
#' @return sf object
#' @importFrom dplyr mutate group_by ungroup n select everything relocate last_col bind_rows filter
#' @importFrom terra linearUnits res rast extract project vect crs 
#' @importFrom sf st_line_sample st_set_geometry st_cast
#' @export
cross_section_pts_v3 = function(
    cs             = NULL,
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
    scale          = 0.5
){
  
  ### ### ## ## ### ## ### ##
  ## ### ### ### ### #### ##
  
  # cs             = tmp_trans
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = DEM_URL
  # scale          = 5
  
  ### ### ## ## ### ## ### ##
  ## ### ### ### ### #### ##
  
  ### ### ## ## ### ## ### ##
  ## ### ### ### ### #### ##
  # #### function is still WORK IN PROGRESS
  
  # cs             = tmp_trans[lengths(sf::st_intersects(tmp_trans, tmp)) == 1, ]
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = DEM_URL
  # scale = 0.5
  # library(dplyr)
  # library(sf)
  # library(terra)
  # base_dir <- "/Users/anguswatters/Desktop/lynker-spatial/"
  # out_dir <- "/Users/anguswatters/Desktop/rand_tester/"
  # transects_dir <- glue::glue("{base_dir}/01_transects/")
  # transect_files <- list.files(transects_dir, full.names = T)
  # 
  # test_file <- transect_files[1]
  # 
  # test_data <- sf::read_sf(test_file)
  # aoi_bb <-
  #   test_data %>%
  #   dplyr::filter(hy_id == "wb-2959") %>%
  #   sf::st_buffer(2500) %>%
  #   sf::st_bbox() %>%
  #   sf::st_as_sfc() %>%
  #   sf::st_as_sf()
  # final_test_data <-
  #   test_data %>%
  #   sf::st_filter(aoi_bb)
  # cs_path <- glue::glue("{out_dir}test_transects_01.gpkg")
  # sf::write_sf(final_test_data, cs_path)
  # mapview::mapview(aoi_bb) + dplyr::filter(test_data, hy_id == "wb-2959") + final_test_data
  # cs = cs_path
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
  # scale          = 0.5
  
  ### ### ## ## ### ## ### ##
  ## ### ### ### ### #### ##
  
  # check if a cross section is given, and return NULL if missing
  if (is.null(cs)) {
    return(NULL)
  }
  
  # check if a file path or not
  if(is.character(cs)) {
    # Read in file
    cs <- sf::read_sf(cs)
  }
  
  # add points per cross sections 
  cs <- add_points_per_cs(
    cs             = cs,
    points_per_cs  = points_per_cs,
    min_pts_per_cs = min_pts_per_cs,
    dem            = dem
  )
  
  
  # Extract DEM "Z" values for each point along cross section linestrings
  cs_pts <- extract_dem_values(cs = cs, dem = dem)
  
  # check for any flat cross sections (All Z values are equal within a given cross section)
  # flat_cs <- check_z_values(pts = cs_pts, threshold = 0)
  flat_cs <- check_z_values(pts = cs_pts, threshold = 0.5)
  
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
    dplyr::mutate(
      tmp_id = paste0(hy_id, "_", cs_id)
    ) %>% 
    dplyr::filter(tmp_id %in% unique(
      dplyr::mutate(flat_cs,
                    tmp_id = paste0(hy_id, "_", cs_id))$tmp_id
    )
    ) %>% 
    dplyr::select(-tmp_id)
  
  # dplyr::mutate(extend_by = scale * cs_lengthm)
  # extend linestring geometries by a percent of linestring length
  extended <- extend_by_percent(x = to_extend, pct = scale, length_col = "cs_lengthm")
  
  # mapview::mapview(cs, color = "dodgerblue") +  
  # mapview::mapview(extended, color = "red") +  
  #   mapview::mapview(to_extend, color = "green")
  # 
  # add cross section points to extended cross sections
  extended <- add_points_per_cs(
    cs             = extended,
    points_per_cs  = points_per_cs,
    min_pts_per_cs = min_pts_per_cs,
    dem            = dem
  )
  
  # extended <- add_points_per_cs(cs = extended, dem = dem,  points_per_cs = NULL, min_pts_per_cs = 10)
  
  # extract DEM values for newly extended cross sections
  extended_pts <- extract_dem_values(cs = extended, dem = dem)
  
  # take the below points, and put them back into "cs_pts" object
  # then go back to the input "transects" ("cs") object and update the transect geometries based on the extensions done above^^
  # then resave the input transects dataset back to its original location....
  extended_pts <- 
    extended_pts %>% 
    # sf::st_drop_geometry() %>% 
    # dplyr::select(hy_id, cs_id, Z) %>%
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      is_same_Z = max(Z) - min(Z) <= 0
      # is_same_Z = dplyr::n_distinct(Z) == 1,
    ) %>% 
    dplyr::ungroup() %>%    
    dplyr::mutate(
      tmp_id = paste0(hy_id, "_", cs_id)
    )
  
  # separate newly extended cross sections with new Z values into groups (those that show "good" DEM values after extension are kept) 
  to_keep <- dplyr::filter(extended_pts, !is_same_Z)
  to_drop <- dplyr::filter(extended_pts, is_same_Z)
  
  # filter out cross section points that have "same Z" values (remove flat Z values)
  final_pts <-
    cs_pts %>%  
    dplyr::mutate(
      tmp_id = paste0(hy_id, "_", cs_id)
    ) %>% 
    dplyr::filter(
      !tmp_id %in% unique(to_drop$tmp_id)
      # !tmp_id %in% unique(paste0(to_drop$hy_id, "_", to_drop$cs_id))
    ) 
  
  # remove the old versions of the "to_keep" cross section points and 
  # replace them with the updated cross section points with the extended "cs_lengthm" and "Z" values
  final_pts <-
    final_pts %>%
    dplyr::filter(
      !tmp_id %in% unique(to_keep$tmp_id)
    ) %>% 
    dplyr::mutate(
      is_extended = FALSE
    ) %>% 
    dplyr::bind_rows(
      dplyr::select(
        dplyr::mutate(
          to_keep,
          is_extended = TRUE
        ), 
        -is_same_Z)
    ) %>% 
    dplyr::select(-tmp_id) %>% 
    dplyr::relocate(geom, .after = dplyr::last_col())
  
  return(final_pts)
  
  # tmp %>% 
  #   ggplot2::ggplot() +
  #   ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z,color = is_same_Z)) +
  #   ggplot2::facet_wrap(~cs_id)
  
}

#' Get Points across transects with elevation values
#' @param cs Hydrographic LINESTRING Network
#' @param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
#' @param min_pts_per_cs Minimun number of points per cross section required.
#' @param dem the DEM to extract data from
#' @return sf object
#' @importFrom dplyr mutate group_by ungroup n select everything
#' @importFrom terra linearUnits res rast extract project vect crs 
#' @importFrom sf st_line_sample st_set_geometry st_cast
#' @export
cross_section_pts_v2 = function(cs,
                             points_per_cs = NULL,
                             min_pts_per_cs = 10,
                             dem = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"){
  
  # check if a cross section is given, and return NULL if missing
  if (is.null(cs)) {
    return(NULL)
    }
  
  # IF NULL value is given to points_per_cs argument, calculate points_per_cs values
  # - IF DEM has a longitude/latitude CRS (terra::linearUnits == 0):
    # -- then divide the cross section length by 111139 and divide that resulting value by the minimum resolution value from the DEM (then round the result up)
  # - ELSE: 
    # -- just divide the cross section length by the minimum resolution value from the DEM (then round the result up)
  if (is.null(points_per_cs)) {
    if (terra::linearUnits(terra::rast(dem)) == 0) {
      points_per_cs = ceiling(
                        (cs$lengthm / 111139) / min(terra::res(terra::rast(dem)))
                        )
    } else {
      points_per_cs = ceiling(
                        (cs$lengthm) / min(terra::res(terra::rast(dem)))
                        )
    }
  }
  
  # take the max between the given minimum points per cross section and the derived points per cross section
  cs$points_per_cs = pmax(min_pts_per_cs, points_per_cs)
  
  # function to extract Z/elevation values at a point from DEM
  extract_pt_val = function(rast, pts){ 
    terra::extract(rast, 
                   terra::project(terra::vect(pts), 
                                  terra::crs(rast))
                   )[, 2] 
    }
    
  suppressWarnings({
    
    return(
      sf::st_set_geometry(cs, sf::st_line_sample(cs, cs$points_per_cs)) %>% 
        sf::st_cast("POINT") %>%
        dplyr::mutate(Z = extract_pt_val(terra::rast(dem), .)) %>% 
        dplyr::group_by(hy_id, cs_id) %>% 
        dplyr::mutate(
          pt_id             = 1:dplyr::n(),
          relative_distance = seq(from = 0, to = lengthm[1], length.out = dplyr::n())
          ) %>% 
        dplyr::ungroup() %>% 
        dplyr::select(hy_id, cs_id, pt_id, Z, lengthm, relative_distance, dplyr::everything())
      )
    
  })
  
}

# #Get Points across transects with elevation values
# #@param cs Hydrographic LINESTRING Network
# #@param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
# #@param min_pts_per_cs Minimun number of points per cross section required.
# #@param dem the DEM to extract data from
# #@return sf object
# #@export
# cross_section_pts = function(cs,
#                              points_per_cs = NULL,
#                              min_pts_per_cs = 10,
#                              dem = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"){
# 
#   if(is.null(cs)){ return(NULL) }
#   
#   if(is.null(points_per_cs)){
#     if(linearUnits(rast(dem)) == 0){
#       points_per_cs = ceiling((cs$lengthm / 111139) / min(res(rast(dem))))
#     } else {
#       points_per_cs = ceiling((cs$lengthm) / min(res(rast(dem))))
#     }
#   }
#   
#   cs$points_per_cs = pmax(min_pts_per_cs, points_per_cs)
#     
#   extract_pt_val = function(rast, pts){ extract(rast, project(vect(pts), crs(rast)))[, 2] }
# 
#   suppressWarnings({
#     st_set_geometry(cs, st_line_sample(cs, cs$points_per_cs)) %>% 
#       st_cast("POINT") %>%
#       mutate(Z   = extract_pt_val(rast(dem), .)) %>% 
#       group_by(hy_id, cs_id) %>% 
#       mutate(pt_id = 1:n(),
#              relative_distance = seq(from = 0, to = lengthm[1], length.out = n())) %>% 
#       ungroup() %>% 
#       select(hy_id, cs_id, pt_id, Z, lengthm, relative_distance, everything())
#   })
#   
# }

# #Generate Multiple cross section along a linestring
# #@param edges data.frame of LINESTRINGs (pieces of line)
# #@param line original line element
# #@param bf_width Bankfull Width (length of cross section)
# #@noRd
# #@keywords internal
# #@return GEOS object
# #@importFrom geos geos_empty geos_type geos_intersection geos_intersects geos_is_empty
# #@importFrom vctrs vec_c
# get_transects1 <-  function(edges, line, bf_width){
#   
#   if(length(bf_width) != length(edges)){
#     bf_width = rep(bf_width[1], length(edges))
#   }
#   
#   transects <- geos::geos_empty()
#   
#   for(i in 1:length(edges)){
#     
#     # message("TRANSECT: ", i)
#     tran = cut_transect(edges[i], bf_width[i])
#     
#     
#     # If a MULTIPOINT, then it crosses more the once
#     if(geos::geos_type(geos::geos_intersection(tran, line)) == "point") {
#       # message("intersect IS point ")
#       # Ensure that there are no intersections with previously computed cross sections
#       if (!any(geos::geos_intersects(tran, transects))) {
#         # message("----> KEEPING TRANSECT: ", i)
#         transects <-  vctrs::vec_c(transects, tran)
#       } 
#       
#     } 
#   }
#   
#   transects[!geos::geos_is_empty(transects)]
#   
# }
# 
# #Generate Multiple cross section along a linestring
# #Internal function, version 2 of get_transects(), may provide a slight runtime improvement. 
# #Lessens the number of total intersection calculations done in total. WIP.
# #@param edges data.frame of LINESTRINGs (pieces of line)
# #@param line original line element
# #@param bf_width Bankfull Width (length of cross section)
# #
# #@noRd
# #@keywords internal
# #@return GEOS object
# #@importFrom geos geos_empty geos_intersects geos_is_empty
# #@importFrom vctrs vec_c
# get_transects2 = function(edges, line, bf_width) {
#   
#   # validate "bf_wdith" input
#   if(length(bf_width) != length(edges)){
#     bf_width = rep(bf_width[1], length(edges))
#   }
#   
#   # initialize empty geos geometry
#   transects <- geos::geos_empty()
#   
#   # iterate through edges and create a transect line at each edge on the 'line', and keep only transect lines that do NOT intersect w/ previously computed transect lines
#   for(i in 1:length(edges)){
#     
#     # message("TRANSECT: ", i)
#     tran = cut_transect(edges[i], bf_width[i])
#     
#     # Ensure that there are no intersections with previously computed cross sections
#     if (!any(geos::geos_intersects(tran, transects))) {
#       transects <-  vctrs::vec_c(transects, tran)
#     }
#   }
#   
#   # Drop transects that cross 'line' more than once (geos_intersection appears as a MULTIPOINT)
#   transects <- drop_multicrossings(transects, line)
#   
#   return(transects[!geos::geos_is_empty(transects)])
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

# #Generate Cross Sections Across Hydrographic Network
# #@param net Hydrographic LINESTRING Network
# #@param id Unique Identifier in net
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
#     id                = NULL,
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
#   # add id column if provided as an input
#   if(!is.null(id)){
#     ll$hy_id = rep(net[[id]], times = ids_length)
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
#         by = c("hy_id" = id)
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
#@param id  Uniuqe Identifier in net
#@param cs_widths Bankfull Widths (length of cross sections for each net element)
#@param num Number of transects per Net element
#@return sf object
#@export
# cut_cross_sections1 = function(net, id = NULL,
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
#   if(!is.null(id)){
#     ll$hy_id = rep(net[[id]], times = ids_length)
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
#                                    id = NULL,
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
#   # id        = "comid"
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
#   # add id column if provided as an input
#   if(!is.null(id)){
#     ll$hy_id = rep(net[[id]], times = ids_length)
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
#                   by = c("hy_id" = id)
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
#                                id                = NULL,
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
#   # id        = "comid"
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
#   # add id column if provided as an input
#   if(!is.null(id)){
#     ll$hy_id = rep(net[[id]], times = ids_length)
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
#         by = c("hy_id" = id)
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
