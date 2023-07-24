#' Generate a Perpendicular Linestring of a Given Width
#' @param edge LINESRTING
#' @param width Length of Perpendicular LINESTRING
#' @return GEOS object
#' @export

cut_transect = function(edge, width){
  # edge = edges[i]
  # width = bf_width[i]
  
  midpoint <- geos_interpolate_normalized(edge, 0.5)
  ep       <- geos_point_end(edge)
  
  normal_edge <- wk_transform(edge, 
                               wk_affine_compose(
                                 wk_affine_translate(dx = -geos_x(midpoint), dy = -geos_y(midpoint)),
                                 wk_affine_scale(1 / geos_length(edge), 1 / geos_length(edge)),
                                 wk_affine_rotate(90)))
  
  wk_set_crs(wk_transform(
    normal_edge,
    wk_affine_compose(
      wk_affine_scale(width, width),
      wk_affine_translate(geos_x(ep), geos_y(ep))
    )
  ), wk_crs(edge))
}


#' Generate Multiple cross section along a linestring
#' @param edges data.frame of LINESTRINGs (pieces of line)
#' @param line original line element
#' @param bf_width Bankfull Width (length of cross section)
#' @return GEOS object
#' @export

get_transects = function(edges, line, bf_width){
  # get_transects(edges, line, cs_widths[j])
  # edges <- edges[[1]]
  # line <- line[[1]]
  # bf_width
  
  # bf_width = cs_widths[j]
  if(length(bf_width) != length(edges)){
    bf_width = rep(bf_width[1], length(edges))
  }
  
  transects <- geos_empty()
  
  for(i in 1:length(edges)){
    
    tran = cut_transect(edges[i], bf_width[i])
    
    # If a MULTIPOINT, then it crosses more the once
    if(geos_type(geos_intersection(tran, line)) == "point") {
      # Ensure that there are no intersections with previously computed cross sections
      if (!any(geos_intersects(tran, transects))) {
        transects <-  vec_c(transects, tran)
      }
    }
  }

  transects[!geos_is_empty(transects)]
  
}

#' Generate Cross Sections Across Hydrographic Network
#' @param net Hydrographic LINESTRING Network
#' @param id  Uniuqe Identifier in net
#' @param cs_widths Bankfull Widths (length of cross sections for each net element)
#' @param num Number of transects per Net element
#' @return sf object
#' @export

cut_cross_sections = function(net, id = NULL, cs_widths = 100, num = 10,
                              smooth = TRUE, densify = 2,
                              rm_self_intersect = TRUE){
  
  
  if(smooth){ 
    message("Smoothing")
    net = smoothr::smooth(net, "spline") 
  }
  
  if(!is.null(densify)){ 
    message("Densifying")
    net = smoothr::densify(net, densify) 
  }
  
  ll = list()
  
  if(length(cs_widths) != nrow(net)){
    cs_widths = rep(cs_widths[1], nrow(net))
  }
  
  if(length(num) != nrow(net)){
    num = pmax(3, rep(num[1], nrow(net)))
  }
  # fin
  message("Cutting")
  # nrow(net)
  # j = 2
  for(j in 1:nrow(net)){
    
    line <- as_geos_geometry(net[j,])
    
    vertices <- wk_vertices(line)
    
    edges <- as_geos_geometry(
      wk_linestring(
        vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
        feature_id = rep(seq_len(length(vertices) - 1), each = 2)
      )
    )
    
    edges = edges[-c(1, length(edges))]
    
    if(!is.null(num)){
      if(num[j] == 1){
        edges = edges[as.integer(ceiling(length(edges)/ 2))]
      } else {
        edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
      }
    } 
    
    ll[[j]] = get_transects(edges, line, cs_widths[j])
    
  }
  
  
  ids_length = lengths(ll)
  ll = st_as_sf(Reduce(c,ll))
  
  if(nrow(ll) == 0){
    return(NULL)
  }
  
  # plot(ll$geometry)
  # plot(perp, add = T)
  # perp_sf <- sf::st_as_sf(perp)
  # perp2_sf <- sf::st_as_sf(perp2)
  # 
  # mapview::mapview(ll, color = "red") +   
  #   # mapview::mapview(perp_sf, color = "blue") +
  #   mapview::mapview(perp2_sf, color = "green")
  
  message("Formating")
  
  if(!is.null(id)){
    ll$hy_id = rep(net[[id]], times = ids_length)
  } else {
    ll$hy_id = rep(1:nrow(net), times = ids_length)
  }
  
  ll$cs_widths = rep(cs_widths, times = ids_length)
  
  if(rm_self_intersect){
    ll[lengths(st_intersects(ll)) == 1, ] %>% 
      group_by(hy_id) %>% 
      mutate(cs_id = 1:n()) %>% 
      ungroup() %>% 
      mutate(lengthm = as.numeric(st_length(.)))
  } else {
    ll %>% 
      group_by(hy_id) %>% 
      mutate(cs_id = 1:n()) %>% 
      ungroup() %>% 
      mutate(lengthm = as.numeric(st_length(.)))
  }
 
}

cut_cross_sections2 = function(net, 
                                   id = NULL,
                                   cs_widths = 100, 
                                   num = 10,
                                   smooth = TRUE,
                                   densify = 2,
                                   rm_self_intersect = TRUE,
                                   fix_braids = TRUE,
                                   add = FALSE
                                   ){
  # net       = net3
  # id        = "comid"
  # cs_widths = pmax(50, net3$bf_width * 7)
  # num       = 5
  # add       = TRUE
  # smooth = TRUE
  # densify = 2
  # rm_self_intersect = TRUE
  # add = TRUE
  
  # keep track of the CRS of the input to retransform return 
  start_crs <- sf::st_crs(net, parameters = T)$epsg
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs != 5070) {
  # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG:5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  if(smooth){ 
    message("Smoothing")
    net = smoothr::smooth(net, "ksmooth")
    # net = smoothr::smooth(net, "spline") 
  }
  
  if(!is.null(densify)){ 
    message("Densifying")
    net = smoothr::densify(net, densify) 
  }
  
  ll = list()
  
  if(length(cs_widths) != nrow(net)){
    cs_widths = rep(cs_widths[1], nrow(net))
  }
  
  if(length(num) != nrow(net)){
    num = pmax(3, rep(num[1], nrow(net)))
  }
  
  message("Cutting")
  for(j in 1:nrow(net)){
    
    line <- as_geos_geometry(net[j,])
    
    vertices <- wk_vertices(line)
    
    edges <- as_geos_geometry(
      wk_linestring(
        vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
        feature_id = rep(seq_len(length(vertices) - 1), each = 2)
      )
    )
    
    edges = edges[-c(1, length(edges))]
    
    if(!is.null(num)){
      if(num[j] == 1){
        edges = edges[as.integer(ceiling(length(edges)/ 2))]
      } else {
        edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
      }
    }
    
    ll[[j]] = get_transects(edges, line, cs_widths[j])
  }
  
  ids_length = lengths(ll)
  ll = st_as_sf(Reduce(c,ll))
  
  if(nrow(ll) == 0){
    return(NULL)
  }
  
  message("Formating")
  
  # add id column if provided as an input
  if(!is.null(id)){
    ll$hy_id = rep(net[[id]], times = ids_length)
  } else {
    ll$hy_id = rep(1:nrow(net), times = ids_length)
  }
  
  # add back cross sections width column
  ll$cs_widths = rep(cs_widths, times = ids_length)

  # remove self intersecting transects or not
  if(rm_self_intersect){
    ll <- 
      ll[lengths(st_intersects(ll)) == 1, ] %>% 
      group_by(hy_id) %>% 
      mutate(cs_id = 1:n()) %>% 
      ungroup() %>% 
      mutate(lengthm = as.numeric(st_length(.)))
  } else {
    ll <- 
      ll %>% 
      group_by(hy_id) %>% 
      mutate(cs_id = 1:n()) %>% 
      ungroup() %>% 
      mutate(lengthm = as.numeric(st_length(.)))
  }

  # if original columns of data should be added to transects dataset
  if(add) {
   ll <-
     dplyr::left_join(
                  ll,
                  sf::st_drop_geometry(net),
                  by = c("hy_id" = id)
                  # by = c("hy_id" = "comid")
                  )
  }
  
  if(fix_braids) {
    
    # fix the braided transects
    ll <- fix_braid_transects(net, ll)
    
  }
  
  # transform CRS back to input CRS
  if(start_crs != 5070) {
    message("Transforming CRS back to EPSG:", start_crs)
    ll <- sf::st_transform(ll, start_crs)
  }
  
  return(ll)
  
}

#' Get Points across transects with elevation values
#' @param cs Hydrographic LINESTRING Network
#' @param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
#' @param min_pts_per_cs Minimun number of points per cross section required.
#' @param dem the DEM to extract data from
#' @return sf object
#' @export

cross_section_pts = function(cs,
                             points_per_cs = NULL,
                             min_pts_per_cs = 10,
                             dem = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"){
  
  if(is.null(cs)){ return(NULL) }
  
  if(is.null(points_per_cs)){
    if(linearUnits(rast(dem)) == 0){
      points_per_cs = ceiling((cs$lengthm / 111139) / min(res(rast(dem))))
    } else {
      points_per_cs = ceiling((cs$lengthm) / min(res(rast(dem))))
    }
  }
  
  cs$points_per_cs = pmax(min_pts_per_cs, points_per_cs)
  
  extract_pt_val = function(rast, pts){ extract(rast, project(vect(pts), crs(rast)))[, 2] }
  
  suppressWarnings({
    st_set_geometry(cs, st_line_sample(cs, cs$points_per_cs)) %>% 
      st_cast("POINT") %>%
      mutate(Z   = extract_pt_val(rast(dem), .)) %>% 
      group_by(hy_id, cs_id) %>% 
      mutate(pt_id = 1:n(),
             relative_distance = seq(from = 0, to = lengthm[1], length.out = n())) %>% 
      ungroup() %>% 
      select(hy_id, cs_id, pt_id, Z, lengthm, relative_distance, everything())
  })
  
}

#' Get Points across transects with elevation values
#' @param cs Hydrographic LINESTRING Network
#' @param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
#' @param min_pts_per_cs Minimun number of points per cross section required.
#' @param dem the DEM to extract data from
#' @return sf object
#' @export

cross_section_pts = function(cs,
                             points_per_cs = NULL,
                             min_pts_per_cs = 10,
                             dem = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"){

  if(is.null(cs)){ return(NULL) }
  
  if(is.null(points_per_cs)){
    if(linearUnits(rast(dem)) == 0){
      points_per_cs = ceiling((cs$lengthm / 111139) / min(res(rast(dem))))
    } else {
      points_per_cs = ceiling((cs$lengthm) / min(res(rast(dem))))
    }
  }
  
  cs$points_per_cs = pmax(min_pts_per_cs, points_per_cs)
    
  extract_pt_val = function(rast, pts){ extract(rast, project(vect(pts), crs(rast)))[, 2] }

  suppressWarnings({
    st_set_geometry(cs, st_line_sample(cs, cs$points_per_cs)) %>% 
      st_cast("POINT") %>%
      mutate(Z   = extract_pt_val(rast(dem), .)) %>% 
      group_by(hy_id, cs_id) %>% 
      mutate(pt_id = 1:n(),
             relative_distance = seq(from = 0, to = lengthm[1], length.out = n())) %>% 
      ungroup() %>% 
      select(hy_id, cs_id, pt_id, Z, lengthm, relative_distance, everything())
  })
  
}

#' Classify Cross Section Points 
#' @param cs_pts CS points
#' @return sf object
#' @export

classify_points = function(cs_pts){
  
  . <-  L <-  L1 <-  L2  <-  R  <-  R1 <-  R2  <- Z  <-  Z2 <-  anchor <-  b1  <- b2  <- cs_widths  <- count_left <- 
    count_right  <-  cs_id <-  hy_id <-  in_channel_pts  <- lengthm <-  low_pt  <- max_bottom  <- mean_dist <-  mid_bottom  <- min_bottom  <- pt_id <- relative_distance <-  third <-  NULL
  
  filter(cs_pts) %>% 
    group_by(hy_id, cs_id) %>% 
    mutate(third = ceiling(n() / 3),
           mean_dist = mean(diff(relative_distance)),
           in_channel_pts = ceiling(cs_widths[1] / mean_dist),
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
    ungroup() %>% 
    select(hy_id, cs_id, pt_id, Z, relative_distance, cs_widths, class)
  
}

#############################################################################
################################# SCRAPS ####################################
#############################################################################

#' 
#' #' Check geos_linestrings for any intersections other than itself
#' #'
#' #' @param linestrings geos_linestring geometry
#' #'
#' #' @return logical, Return TRUE if any geos_linestring intersects with ANY other geos_linestrings other than itself, otherwise return FALSE
#' #' @export
#' #'
#' #' @examples
#' non_self_intersects <- function(linestrings) {
#'   
#'   # Check if linestring intersects with any linestring other than itself
#'   any_intersects <- sapply(seq_along(linestrings), function(i) {
#'     
#'     # !any(
#'     any(
#'       sapply(linestrings[-i], function(other_lines) {
#'         
#'         # check intersection with other linestrings other than self
#'         geos_intersects(linestrings[i], other_lines)
#'         
#'       })
#'     )
#'   })
#'   
#'   # all(any_intersects)
#'   # !all(any_intersects)
#'   
#'   # Return TRUE if linestring intersects with ANY other linestring other than itself, otherwise return FALSE
#'   return(all(any_intersects))
#' }
#' 
#' #' Remove any geos_linestrings that intersects with any other linestring other than itself
#' #'
#' #' @param linestrings geos_linestring geometry
#' #'
#' #' @return geos_linestring with any non self intersections removed
#' #' @export
#' #'
#' #' @examples
#' rm_intersects <- function(linestrings) {
#'   
#'   # Check if linestring intersects with any linestring other than itself
#'   any_intersects <- sapply(seq_along(linestrings), function(i) {
#'     
#'     # !any(
#'     any(
#'       sapply(linestrings[-i], function(other_lines) {
#'         
#'         # check intersection with other linestrings other than self
#'         geos_intersects(linestrings[i], other_lines)
#'         
#'       })
#'     )
#'   })
#'   
#'   # Map(function(x, flag) if (flag) c(x, new_transect) else x, linestrings, any_intersects)
#'   
#'   # Remove any linestrings that intersect with other linestrings 
#'   return(linestrings[!any_intersects])
#' }
#' 
#' is_single_intersection <- function(cross, linestring) {
#'   
#'   single_pt <- 
#'     Reduce(c, 
#'            mapply(function(transect_list, line_elem) {
#'              geos_type(geos_intersection(transect_list, line_elem)) == "point"
#'            }, 
#'            cross, linestring,
#'            SIMPLIFY = FALSE
#'            ) 
#'     )
#'   
#'   return(single_pt)
#'   
#' }
#' 
#' #' Remove geos_linestrings that intersect with another geos_linestring more than once
#' #'
#' #' @param transects list of geos_linestrings that need to be checked for multiple intersections with `linestring` argument. The list should be the same length as `linestring` argument, if a geos_linestring is given, the geos_linestring will be put into a list and its length will be recycled to match the length of the `linestring` list
#' #' @param linestring list of geos_linestrings to check again `transects` list of geos_linestrings
#' #'
#' #' @return list of geos_linestrings with any multi intersecting transects removed. The returned list will be the same length as the `transects` list provided
#' #' @export
#' #'
#' #' @examples
#' rm_multi_intersects2 <- function(transects, linestring) {
#'   # transects <- transects2
#'   # linestring <- line
#'   
#'   
#'   keep_lines <-  unique(
#'     Reduce(
#'       c, 
#'       lapply(1:length(linestring), function(i) {
#'         int <- geos_intersection(transects, linestring[i])
#'         which(!geos::geos_is_empty(int) & geos::geos_type(int) == "point")
#'       })
#'     )
#'   )
#'   
#'   return(transects[keep_lines])
#'   
#' }
#' 
#' #' Remove geos_linestrings that intersect with another geos_linestring more than once
#' #'
#' #' @param transects list of geos_linestrings that need to be checked for multiple intersections with `linestring` argument. The list should be the same length as `linestring` argument, if a geos_linestring is given, the geos_linestring will be put into a list and its length will be recycled to match the length of the `linestring` list
#' #' @param linestring list of geos_linestrings to check again `transects` list of geos_linestrings
#' #'
#' #' @return list of geos_linestrings with any multi intersecting transects removed. The returned list will be the same length as the `transects` list provided
#' #' @export
#' #'
#' #' @examples
#' rm_multi_intersects <- function(transects, linestring, num) {
#'   # transects <- transects2
#'   # linestring <- line
#'   
#'   # if a geos_geometry is given, convert to a list
#'   if (methods::is(transects, "geos_geometry")) {
#'     
#'     transects <- list(transects)
#'     
#'   }
#'   
#'   # if length of the new transcript list doesn't equal the number of main linestrings, repeat the vector match the linestring vector
#'   if (length(transects) != length(linestring)) {
#'     
#'     transects2 <- rep(transects, length(linestring))
#'     
#'   }
#'   
#'   rmlines <- lapply(1:length(linestring), function(i) {
#'     # i = 4
#'     message("i: ", i)
#'     int <- geos_intersection(transects, linestring[i])
#'     # int
#'     
#'     # geos::geos_intersects_any(transects, geos::geos_strtree(linestring, 20))
#'     # tmp <- geos::geos_intersects_matrix(transects, geos::geos_strtree(linestring, 20))
#'     # multi_ints_sf <- sf::st_as_sf(transects[lapply(tmp,length)>1])
#'     
#'     
#'     which(!geos::geos_is_empty(int) & geos::geos_type(int) == "point")
#'     # geos::geos_intersects_any()
#'     # transects[!geos::geos_is_empty(int) & geos::geos_type(int) == "point"] %>% 
#'     #   sf::st_as_sf()
#'     # 
#'   }) %>% 
#'     Reduce(c, .) %>% 
#'     unique()
#'   
#'   new_trans <- transects[rmlines] %>% sf::st_as_sf()
#'   orig_trans <- transects %>% sf::st_as_sf()
#'   
#'   unique(Reduce(c, rmlines)) %>% 
#'     rmlines %>% 
#'     dplyr::bind_rows()
#'   
#'   int <- geos_intersection(transects, linestring[4])
#'   transects[!geos::geos_is_empty(int) & geos::geos_type(int) == "point"]
#'   line_sf <- sf::st_as_sf(linestring)
#'   num
#'   mapview::mapview(line_sf) + tmpsf
#'   
#'   
#'   mapview::mapview(orig_trans, color= "red") +
#'     line_sf + 
#'     mapview::mapview(multi_ints_sf, color= "blue") + 
#'     mapview::mapview(new_trans, color= "green")
#'   grp <- seq(1, length(transects), num)
#'   i = 4
#'   grp[i]
#'   grp[i +1]
#'   
#'   transects[grp[i]:grp[i+1]]
#'   
#'   geos_type(geos_intersection(transects, linestring))
#'   
#'   int <- geos_intersection(transects, linestring[4])
#'   transects[!geos::geos_is_empty(int) & geos::geos_type(int) == "point"]
#'   
#'   tmp_sf <- sf::st_as_sf(transects[!geos::geos_is_empty(int) & geos::geos_type(int) == "point"])
#'   
#'   geos_type(geos_intersection(transects,   linestring[4])) != "point"
#'   int <- geos_intersection(transects, linestring[4])
#'   
#'   int
#'   
#'   !geos::geos_is_empty(int) & geos::geos_type(int) != "point"
#'   
#'   tmp_sf
#'   geos_intersection(transects,   linestring[4]) 
#'   
#'   mapview::mapview(line_sf[4, ])
#'   mapview::mapview(trans1, color= "red") + trans2 + mapview::mapview( line_sf[4, ], color= "green") + line_sf + 
#'     mapview::mapview(tmp_sf, color= "black") +
#'     mapview::mapview(multi_ints_sf, color= "orange")
#'   geos_intersection(linestring[1], transects)
#'   
#'   linestring[1]
#'   # check if linestring only intersects a single time with main linestring
#'   single_pts <- 
#'     Reduce(c,
#'            mapply(function(transect_list, line_elem) {
#'              geos_type(geos_intersection(transect_list, line_elem)) == "point"
#'            }, 
#'            transects, linestring,
#'            SIMPLIFY = FALSE
#'            ) 
#'     )
#'   !single_pts
#'   transects[!single_pts]
#'   transects[single_pts]
#'   trans1 <- transects %>% sf::st_as_sf()
#'   trans2 <- transects[!single_pts] %>% sf::st_as_sf()
#'   trans2
#'   line_sf
#'   mapview::mapview(trans1, color= "red") + trans2 + line_sf
#'   
#'   # Using single_pts to index transects geometries
#'   trans <- lapply(seq_along(single_pts), function(i) {
#'     transects[[i]][single_pts[[i]]]
#'   })
#'   
#'   # return all transects list elements with lengths greater than 0
#'   trans <- trans[lapply(trans,length)>0]
#'   
#'   return(trans)
#'   
#'   # # return(single_pt)
#'   # 
#'   # # Apply intersection logic to each element in transects and line
#'   # is_point <- mapply(function(transect_list, line_elem) {
#'   #   lapply(transect_list, function(transect) {
#'   #     geos_type(geos_intersection(transect, line_elem)) == "point"
#'   #   })
#'   # }, 
#'   # transects, line, 
#'   # SIMPLIFY = FALSE)
#'   # 
#'   # # Reduce list down to single vector for each element in transects
#'   # # Collapse inner list of intersection points
#'   # is_point <- lapply(is_point, function(lst) {
#'   #   Reduce(c, lst)
#'   # })
#'   # 
#'   # # Using is_point to index transects
#'   # trans <- lapply(seq_along(is_point), function(i) {
#'   #   transects[[i]][is_point[[i]]]
#'   # })
#'   # 
#'   # # Return the indexed transects
#'   # return(trans)
#' }
#' 
#' get_transects2 = function(edges, line, bf_width, num){
#'   
#'   line <- result$line
#'   edges <- result$cross_sections
#'   bf_width <- result$cs_widths
#'   # result$num
#'   num = num[1]
#'   
#'   
#'   edges <- Reduce(c, edges)
#'   
#'   if(length(bf_width) != length(edges)){
#'     bf_width = rep(bf_width[1], length(edges))
#'   }
#'   
#'   # transects <- geos::geos_empty()
#'   
#'   # vectorized midpoints
#'   # midpoint <- lapply(edges, function(edge) geos_interpolate_normalized(edge, 0.5))
#'   midpoint <- Reduce(
#'     c,
#'     lapply(edges, function(edge) geos_interpolate_normalized(edge, 0.5))
#'   )
#'   
#'   # vectorized end points
#'   # ep <- lapply(edges, geos_point_end)
#'   # ep <- lapply(edges, geos_point_end)
#'   ep <- Reduce(
#'     c,
#'     lapply(edges, geos_point_end)
#'   )
#'   
#'   # edges <- Reduce(c, edges)
#'   
#'   # plot(edges[[1]][[2]])
#'   # plot(midpoint[[1]][[2]], col = "red", add = T)
#'   # plot(ep[[1]][[2]], col = "black", add = T)
#'   
#'   # perp <- wk_transform(edges[[1]][[2]],
#'   #              wk_affine_compose(
#'   #                wk_affine_translate(dx = -geos_x(midpoint[[1]][[2]]), dy = -geos_y(midpoint[[1]][[2]])),
#'   #                wk_affine_scale(1 / geos_length(edges[[1]][[2]]), 1 / geos_length(edges[[1]][[2]])),
#'   #                wk_affine_rotate(90)
#'   #              )
#'   # )
#'   # perp2 <- wk_set_crs(
#'   #   wk_transform(
#'   #     perp,
#'   #     wk_affine_compose(
#'   #       wk_affine_scale(cs_widths[1], cs_widths[1]),
#'   #       # wk_affine_translate(geos_x(ep[[1]][[2]]), geos_y(ep[[1]][[2]]))
#'   #       wk_affine_translate(geos_x(midpoint[[1]][[2]]), geos_y(midpoint[[1]][[2]]))
#'   #     )
#'   #   ),
#'   #   wk_crs(midpoint[[1]][[2]])
#'   #   # wk_crs(ep[[1]][[2]])
#'   # )
#'   # plot(edges[[1]][[2]])
#'   # plot(midpoint[[1]][[2]], col = "red", add = T)
#'   # plot(ep[[1]][[2]], col = "black", add = T)
#'   # plot(perp2, col = "green", add = T)
#'   
#'   # Reduce(c, midpoint) %>% length()
#'   # Reduce(c, edges) %>% length()
#'   
#'   normal_edge <- Reduce(
#'     c, 
#'     mapply(function(edge, mid) {
#'       wk_transform(edge,
#'                    wk_affine_compose(
#'                      wk_affine_translate(dx = -geos_x(mid), dy = -geos_y(mid)),
#'                      wk_affine_scale(1 / geos_length(edge), 1 / geos_length(edge)),
#'                      wk_affine_rotate(90)
#'                    )
#'       )
#'     }, 
#'     edges, midpoint, SIMPLIFY = FALSE
#'     )
#'   )
#'   
#'   # create transect lines
#'   transects <- mapply(function(edge, mid, width) {
#'     wk_set_crs(
#'       wk_transform(
#'         edge,
#'         wk_affine_compose(
#'           wk_affine_scale(width, width),
#'           wk_affine_translate(geos_x(mid), geos_y(mid))
#'         )
#'       ),
#'       wk_crs(mid)
#'     )
#'   },
#'   normal_edge, midpoint, bf_width, SIMPLIFY = FALSE
#'   )
#'   
#'   transects  <- Reduce(c, transects)
#'   # transects2 <- transects
#'   # remove transects that interesect with lines more than once
#'   transects2 <- rm_multi_intersects(transects, line)
#'   transects2 <- rm_multi_intersects2(transects, line)
#'   # remove any transects that intersect with other transects other than itselfs
#'   transects3 <- rm_intersects(Reduce(c, transects2))
#'   
#'   plot(line[1:4])
#'   # plot(transects2[[1]], add = T)
#'   plot(  Reduce(c, transects2)[1:32], add = T)
#'   Reduce(c, transects2)[1:16]
#'   8*2
#'   
#'   line_sf <- sf::st_as_sf(line)
#'   tran_sf <- sf::st_as_sf(Reduce(c, transects2)[1:40])
#'   tran3_sf <- sf::st_as_sf(transects3[1:38])
#'   
#'   mapview(line_sf) + mapview::mapview(tran_sf, color = "red") + tran3_sf
#'   # reducing list of geos_linestrings into a single geos_linestring geometry
#'   transects <- Reduce(c, transects)
#'   
#'   # remove any transects that intersect with other transects other than itselfs
#'   transects <- rm_intersects(transects)
#'   
#'   # return all non empty geos_geometries
#'   return(transects[!geos_is_empty(transects)])
#'   
#'   normal_edge2 %>% Reduce(c, .)
#'   
#'   normal_edge <- mapply(function(edge_list, mid_list) {
#'     mapply(function(edge, mid) {
#'       wk_transform(edge,
#'                    wk_affine_compose(
#'                      wk_affine_translate(dx = -geos_x(mid), dy = -geos_y(mid)),
#'                      wk_affine_scale(1 / geos_length(edge), 1 / geos_length(edge)),
#'                      wk_affine_rotate(90)
#'                    )
#'       )
#'     }, 
#'     edge_list, mid_list, SIMPLIFY = FALSE)
#'   }, 
#'   edges, midpoint, SIMPLIFY = FALSE
#'   )
#'   
#'   midpoint
#'   normal_edge2 <- Reduce(c, normal_edge)
#'   transects[[1]]
#'   
#'   
#'   
#'   # create transect lines
#'   transects <- mapply(function(norm_edge_list, mid_list) {
#'     mapply(function(edge, mid, width) {
#'       wk_set_crs(
#'         wk_transform(
#'           edge,
#'           wk_affine_compose(
#'             wk_affine_scale(width, width),
#'             wk_affine_translate(geos_x(mid), geos_y(mid))
#'           )
#'         ),
#'         wk_crs(mid)
#'       )
#'     },
#'     norm_edge_list, mid_list, bf_width, SIMPLIFY = FALSE
#'     )
#'   },
#'   normal_edge, midpoint, SIMPLIFY = FALSE
#'   )
#'   
#'   
#'   # # collapse inner list of transect lines into single geos_geometry object
#'   transects <- lapply(transects, function(lst) {
#'     Reduce(c, lst)
#'   })
#'   
#'   transects  <- Reduce(c, transects)
#'   # remove transects that interesect with lines more than once
#'   transects <- rm_multi_intersects(transects, line)
#'   
#'   # reducing list of geos_linestrings into a single geos_linestring geometry
#'   transects <- Reduce(c, transects)
#'   
#'   # remove any transects that intersect with other transects other than itselfs
#'   transects <- rm_intersects(transects)
#'   
#'   # return all non empty geos_geometries
#'   return(transects[!geos_is_empty(transects)])
#'   
#'   # transects <- rm_intersects(
#'   #               Reduce(
#'   #                 c,
#'   #                 rm_multi_intersects(transects, line)
#'   #                 )
#'   #               )
#'   # 
#'   # return(transects[!geos_is_empty(transects)])
#'   
#'   
#'   # # NEW VERSION OF CODE:
#'   # # Apply intersection logic to each element in transects and line
#'   # is_point <- mapply(function(transect_list, line_elem) {
#'   #   lapply(transect_list, function(transect) {
#'   #     geos_type(geos_intersection(transect, line_elem)) == "point"
#'   #   })
#'   # }, 
#'   # transects, line, 
#'   # SIMPLIFY = FALSE
#'   # )
#'   # 
#'   # # reduce list down to single vector for each element in transects
#'   # # collapse inner list of intersection points
#'   # is_point <- lapply(is_point, function(lst) {
#'   #   Reduce(c, lst)
#'   # })
#'   # is_point <- Reduce(c, Reduce(c,is_point))
#'   # is_point <- Reduce(c,is_point)
#'   
#'   
#'   # # ---- METHOD USED IN OLD VERSION OF CODE TO VALIDATE TRANSECTS ----
#'   # # MAKE SURE NO MULTIPOINT intersections and to remove non self intersections from final transects vector
#'   # # If a MULTIPOINT, then it crosses more the once
#'   # 
#'   # !non_self_intersects(Reduce(c, transects))
#'   # 
#'   # if(all(is_point)) {
#'   #   # Insert logic for "Ensure that there are no intersections with previously computed cross sections"
#'   #   if(!non_self_intersects(Reduce(c, transects))) {
#'   #   
#'   #   }
#'   # }
#'   # fin <- mapply(function(norm_edge_list, ep_list) {
#'   #         mapply(function(edge, end, width) {
#'   #           wk_set_crs(
#'   #             wk_transform(
#'   #               edge,
#'   #               wk_affine_compose(
#'   #                 wk_affine_scale(width, width),
#'   #                 wk_affine_translate(geos_x(end), geos_y(end))
#'   #               )
#'   #             ),
#'   #             wk_crs(end)
#'   #           )
#'   #         },
#'   #         norm_edge_list, ep_list, bf_width, SIMPLIFY = FALSE
#'   #         )
#'   #       },
#'   #       normal_edge, ep, SIMPLIFY = FALSE
#'   #       )
#'   
#'   # fin_shp <- sf::st_as_sf(
#'   #                 Reduce(c, 
#'   #                        Reduce(c,fin)
#'   #                        )
#'   #                 ) 
#'   
#'   
#' }
#' 
#' cut_cross_sections3 = function(net, 
#'                                id = NULL, 
#'                                cs_widths = 100,
#'                                num = 10,
#'                                smooth = TRUE,
#'                                densify = 2,
#'                                rm_self_intersect = TRUE,
#'                                parallels = NULL
#' ){
#'   
#'   library(fastmap)
#'   library(pbapply)
#'   library(terra)
#'   library(sf)
#'   library(dplyr)
#'   library(terrainSliceR)
#'   library(mapview)
#'   library(smoothr)
#'   library(wk)
#'   library(geos)
#'   library(vctrs)
#'   library(AOI)
#'   
#'   aoi <- AOI::aoi_get("Sterling, Colorado")
#'   
#'   aoi <-
#'     aoi %>% 
#'     sf::st_transform(5070) %>% 
#'     # sf::st_buffer(15000) %>%
#'     sf::st_buffer(12000) %>%
#'     # sf::st_transform(4326) %>% 
#'     sf::st_bbox() %>% 
#'     sf::st_as_sfc() %>% 
#'     sf::st_sf()
#'   
#'   net <- nhdplusTools::get_nhdplus(aoi) 
#'   mapview::mapview(aoi) + net
#'   # names(net)
#'   net <- 
#'     new_hope_flowline %>% 
#'     nhdplusTools::get_tocomid(add = T) %>% 
#'     dplyr::select(comid, tocomid, hydroseq, divergence, rtndiv, 
#'                   levelpathi, streamleve, streamorde,
#'                   totdasqkm, divdasqkm, lengthkm, geometry = geom
#'     ) %>% 
#'     dplyr::mutate(
#'       dist_m   = lengthkm*1000,
#'       bf_width = exp(0.700 + 0.365* log(totdasqkm))
#'     ) %>% 
#'     # dplyr::select(-lengthkm) %>% 
#'     dplyr::relocate(geometry,  .after = last_col())
#'   net <- 
#'     net %>% 
#'     dplyr::select(comid, hydroseq, divergence, rtndiv, 
#'                   levelpathi, streamleve, streamorde,
#'                   totdasqkm, divdasqkm, lengthkm, geometry
#'     ) %>% 
#'     dplyr::mutate(
#'       dist_m   = lengthkm*1000,
#'       bf_width = exp(0.700 + 0.365* log(totdasqkm))
#'     ) %>% 
#'     dplyr::select(-lengthkm) %>% 
#'     dplyr::relocate(geometry,  .after = last_col())
#'   
#'   # terrainSliceR::linestring
#'   # sf::st_crs(terrainSliceR::linestring)
#'   id = "comid"
#'   cs_widths = 100
#'   num = 4
#'   smooth = TRUE
#'   densify = 2
#'   rm_self_intersect = TRUE
#'   #   # dplyr::filter(comid %in% net$comid)
#'   # net <-
#'   #   terrainSliceR::linestring %>%
#'   #   dplyr::mutate(
#'   #     bf_width = exp(0.700 + 0.365* log(totdasqkm))
#'   #   ) %>% 
#'   #   dplyr::slice(1:10)
#'   # 
#'   # bb <- net %>% 
#'   #   sf::st_bbox() %>% 
#'   #   sf::st_as_sfc() %>% 
#'   #   sf::st_sf()
#'   # 
#'   # net <- nhdplusTools::get_nhdplus(bb) %>% 
#'   #   dplyr::filter(comid %in% net$comid)
#'   # 
#'   # net <- 
#'   #   net %>% 
#'   #   dplyr::select(comid, streamleve, streamorde, hydroseq, totdasqkm, divdasqkm, lengthkm, divergence, geometry) %>% 
#'   #   dplyr::mutate(
#'   #     dist_m   = lengthkm*1000,
#'   #     bf_width = exp(0.700 + 0.365* log(totdasqkm))
#'   #     ) %>% 
#'   #   dplyr::select(-lengthkm) %>% 
#'   #   dplyr::relocate(geometry,  .after = last_col())
#'   # 
#'   # net %>% names()
#'   # %>% 
#'   # dplyr::filter(nhdplus_comid %in% c(24599575, 101))
#'   # id = "comid"
#'   # cs_widths = 100
#'   # num = 4
#'   # smooth = TRUE
#'   # densify = 2
#'   # rm_self_intersect = TRUE
#'   # system.time({
#'   #   cs = net %>% 
#'   #     cut_cross_sections(id = "comid", 
#'   #                        bf_widths = pmax(50, net$bf_width * 7),
#'   #                        num = 10) %>% 
#'   #     cross_section_pts(dem = '/Volumes/Transcend/ngen/DEM-products/dem.vrt') %>% 
#'   #     classify_points()
#'   # })
#'   # system.time({
#'   #   cs = net %>% 
#'   #     cut_cross_sections(id = "comid", 
#'   #                        # bf_widths = pmax(50, net$bf_width * 7),
#'   #                        num = 10)
#'   # })
#'   # # 
#'   # net <-
#'   #   terrainSliceR::linestring %>%
#'   #   dplyr::mutate(
#'   #     bf_width = exp(0.700 + 0.365* log(totdasqkm))
#'   #   ) %>%
#'   # dplyr::filter(nhdplus_comid %in% c(24599575, 101))
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   # spline smooth network flowlines
#'   if(smooth){ 
#'     message("Smoothing")
#'     net = smoothr::smooth(net, "spline") 
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("Smoothing time: ", round(time_block1, 3))
#'   
#'   # mapview(net, color = "red") + net2
#'   # plot(net$geometry)
#'   # plot(net2$geometry, col = "red", add = TRUE)
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   # Code block 1
#'   
#'   # add more points but still keep original vertices
#'   if(!is.null(densify)){
#'     net = smoothr::densify(net, densify) 
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("Densifying time: ", round(time_block1, 3))
#'   
#'   
#'   # mapview::npts(net)
#'   # mapview(net, color = "red") + net2
#'   
#'   # make cs_widths vector match the length of the number of network elements
#'   if(length(cs_widths) != nrow(net)) {
#'     cs_widths = rep(cs_widths[1], nrow(net))
#'   }
#'   
#'   # match number of cross sections 'nums' to number rows in net
#'   if(length(num) != nrow(net)) {
#'     num = pmax(3, rep(num[1], nrow(net)))
#'   }
#'   
#'   net$num = num
#'   net$cs_widths = cs_widths
#'   
#'   # net
#'   
#'   line <- as_geos_geometry(net[j,])
#'   
#'   vertices <- wk_vertices(line)
#'   
#'   edges <- as_geos_geometry(
#'     wk_linestring(
#'       vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#'       feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#'     )
#'   )
#'   
#'   edges = edges[-c(1, length(edges))]
#'   
#'   if(!is.null(num)){
#'     if(num[j] == 1){
#'       edges = edges[as.integer(ceiling(length(edges)/ 2))]
#'     } else {
#'       edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
#'     }
#'   }
#'   
#'   ll[[j]] = get_transects(edges, line, cs_widths[j])
#'   
#'   system.time(
#'     result <- 
#'       net %>%
#'       mutate(
#'         line = geos::as_geos_geometry(geometry)
#'       ) %>%
#'       dplyr::mutate(
#'         vertices = lapply(line, wk::wk_vertices),
#'         edges = lapply(vertices, function(v) {
#'           geos::as_geos_geometry(
#'             wk::wk_linestring(
#'               v[c(1, rep(seq_along(v)[-c(1, length(v))], each = 2), length(v))],
#'               feature_id = rep(seq_len(length(v) - 1), each = 2)
#'             )
#'           )
#'         }),
#'         edges = lapply(edges, function(e) {
#'           e[-c(1, length(e))]
#'         }),
#'         cross_sections = mapply(function(e, n) {
#'           # if (length(e) > 1) {
#'           if (n == 1) {
#'             e[as.integer(ceiling(length(e) / 2))]
#'           } else {
#'             e[as.integer(seq.int(1, length(e), length.out = min(n, length(e))))]
#'           }
#'           # } else {
#'           #   e
#'           # }
#'         }, edges, num, SIMPLIFY = FALSE
#'         ),
#'         # transects = get_transects2(cross_sections, line, cs_widths)
#'         # edges = lapply(edges, function(e) {
#'         #   e[-c(1, length(e))]
#'         # }),
#'         # transects = mapply(function(e, l, w) {
#'         #   get_transects2(e, l, w)
#'         # }, cross_sections, line, cs_widths, SIMPLIFY = FALSE
#'         # )
#'         transects = mapply(function(e, l, w) {
#'           terrainSliceR::get_transects(e, l, w)
#'         }, cross_sections, line, cs_widths, SIMPLIFY = FALSE
#'         )
#'       )
#'   )
#'   
#'   
#'   j = 2
#'   result$transects[1]
#'   
#'   result2 <- 
#'     result %>% 
#'     dplyr::group_by(comid) %>%
#'     dplyr::mutate(
#'       transects = sf::st_as_sfc(geos::geos_make_collection(Reduce(c, transects)))
#'     ) %>% 
#'     dplyr::ungroup() 
#'   
#'   mapview::mapview(net, color = "blue") +  mapview::mapview(ll2, color = "red") +  mapview::mapview(result2$transects, color = "green")
#'   
#'   result2$transects2 %>% class()
#'   result2$transects3[2]
#'   sf::st_as_sf(result2$transects2[2]) %>% .$geometry %>% plot()
#'   result2$transects2[1:3]
#'   sf::st_as_sf(result2$transects2[2])
#'   
#'   
#'   plot(result2$transects2[1])
#'   result2$transects2
#'   sf::st_as_sf(Reduce(c, result$transects))
#'   result$cross_sections[1]
#'   
#'   result$transects[[j]] %>% sf::st_as_sf()
#'   plot(result$line[j])
#'   # plot(result$transects[[1]], col = "red", add= F)
#'   plot(result$cross_sections[[j]], col = "red", add= T)
#'   # plot(result$line[1], add= T)
#'   plot(result$transects[[j]], col = "red", add= T)
#'   
#'   
#'   result$transects
#'   transects2 <- get_transects2(result$cross_sections, result$line, result$cs_widths)
#'   
#'   # get_transects2(result$cross_sections, result$line, result$cs_widths)
#'   
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   # get number of IDs per net element 
#'   ids_length <- lengths(ll)
#'   
#'   # Reduce list of cut cross sections into c() and then sf object
#'   ll <- sf::st_as_sf(Reduce(c,ll))
#'   
#'   # if 0 rows returned by get_transects, return NULL
#'   if(nrow(ll) == 0){
#'     return(NULL)
#'   }
#'   
#'   message("Formating")
#'   
#'   # Create IDs
#'   if(!is.null(id)){
#'     
#'     # using original ID value, create number or replicas to match cross sections in sf dataframe
#'     ll$hy_id = rep(net[[id]], times = ids_length)
#'     
#'   } else {
#'     
#'     # if no ID value was provided, just use the element number as ID
#'     ll$hy_id = rep(1:nrow(net), times = ids_length)
#'     
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("Formatting time: ", round(time_block1, 3))
#'   
#'   # add cross section widths column
#'   ll$cs_widths = rep(cs_widths, times = ids_length)
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   if(rm_self_intersect){
#'     
#'     ll <- 
#'       ll[lengths(sf::st_intersects(ll)) == 1, ] %>% 
#'       group_by(hy_id) %>% 
#'       mutate(cs_id = 1:n()) %>% 
#'       ungroup() %>% 
#'       mutate(lengthm = as.numeric(st_length(.)))
#'     
#'   } else {
#'     ll <- 
#'       ll %>% 
#'       group_by(hy_id) %>% 
#'       mutate(cs_id = 1:n()) %>% 
#'       ungroup() %>% 
#'       mutate(lengthm = as.numeric(st_length(.)))
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("End time: ", round(time_block1, 3))
#'   
#'   result$edges[[1]] %>% 
#'     sf::st_as_sf() %>% 
#'     .$geometry %>% 
#'     plot()
#'   
#'   result$cross_sections[[1]] %>% 
#'     sf::st_as_sf() %>% 
#'     .$geometry %>% 
#'     plot(col = 'red', add = F)
#'   
#'   result$vertices[[1]] %>% 
#'     sf::st_as_sf() %>% 
#'     .$geometry %>% 
#'     plot(add = T)
#'   
#'   result$edges
#'   result$line
#'   result$cross_sections
#'   result$cs_widths
#'   
#'   
#'   
#'   result$line
#'   result$transects[[1]] %>% plot()
#'   
#'   result$cross_sections[[1]] %>% 
#'     plot()
#'   plot(vertices, add = T)
#'   message("Cutting")
#'   parallels = 3
#'   # make parallel cluster
#'   cl <- get_parallels(parallels)
#'   
#'   # Split dataframe rows into a list with corresponding numeric values
#'   net_lst <- mapply(function(row, n, cs) unname(list(row, n, cs)),
#'                     split(net, seq_len(nrow(net))),
#'                     num,
#'                     cs_widths,
#'                     SIMPLIFY = FALSE,
#'                     USE.NAMES = FALSE
#'   )
#'   
#'   # TODO: replace lapply with future.apply
#'   # loop over each net element and create edges and perpindicular lines
#'   # Start timing
#'   start_time <- Sys.time()
#'   cl = NULL
#'   system.time(
#'     ll <-  pbapply::pblapply(
#'       cl  = cl,
#'       X   = net_lst,
#'       FUN = function(j) {
#'         # ll <- lapply(seq_len(nrow(net)), function(j) {
#'         
#'         # create geos geometry of line
#'         line <- geos::as_geos_geometry(j[[1]])
#'         # line <- geos::as_geos_geometry(net[j,])
#'         
#'         # extract vertices from line
#'         vertices <- wk::wk_vertices(line)
#'         
#'         # duplicate all points except first and last points and make edges between each point, create a feature id
#'         edges <- geos::as_geos_geometry(
#'           wk::wk_linestring(
#'             vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#'             feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#'           )
#'         )
#'         
#'         # remove the first and last edge 
#'         edges = edges[-c(1, length(edges))]
#'         
#'         # extract the cross section edges
#'         # if (!is.null(num)) {
#'         #   if (num[j] == 1) {
#'         if (!is.null(j[[2]])) {
#'           if (j[[2]] == 1) {
#'             # if number of cross sections is 1, take the mid point as the cross section
#'             edges = edges[as.integer(ceiling(length(edges) / 2))]
#'           } else {
#'             # sequence along the number of edges and get every nth element to make n cross sections
#'             edges = edges[as.integer(seq.int(1, length(edges), length.out = min(j[[2]], length(edges))))]
#'             # edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
#'           }
#'         } 
#'         
#'         # create perpiniduclar transect lines
#'         terrainSliceR::get_transects(edges, line, j[[3]])
#'         # get_transects(edges, line, cs_widths[j])
#'         
#'       })
#'   )
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("Lapply time: ", round(time_block1, 3))
#'   
#'   # stop cluster if needed
#'   if(!is.null(cl)) {
#'     
#'     # stop parallel cluster     
#'     parallel::stopCluster(cl)
#'     
#'   }
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   # get number of IDs per net element 
#'   ids_length <- lengths(ll)
#'   
#'   # Reduce list of cut cross sections into c() and then sf object
#'   ll <- sf::st_as_sf(Reduce(c,ll))
#'   
#'   # if 0 rows returned by get_transects, return NULL
#'   if(nrow(ll) == 0){
#'     return(NULL)
#'   }
#'   
#'   message("Formating")
#'   
#'   # Create IDs
#'   if(!is.null(id)){
#'     
#'     # using original ID value, create number or replicas to match cross sections in sf dataframe
#'     ll$hy_id = rep(net[[id]], times = ids_length)
#'     
#'   } else {
#'     
#'     # if no ID value was provided, just use the element number as ID
#'     ll$hy_id = rep(1:nrow(net), times = ids_length)
#'     
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("Formatting time: ", round(time_block1, 3))
#'   
#'   # add cross section widths column
#'   ll$cs_widths = rep(cs_widths, times = ids_length)
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   if(rm_self_intersect){
#'     
#'     ll <- 
#'       ll[lengths(sf::st_intersects(ll)) == 1, ] %>% 
#'       group_by(hy_id) %>% 
#'       mutate(cs_id = 1:n()) %>% 
#'       ungroup() %>% 
#'       mutate(lengthm = as.numeric(st_length(.)))
#'     
#'   } else {
#'     ll <- 
#'       ll %>% 
#'       group_by(hy_id) %>% 
#'       mutate(cs_id = 1:n()) %>% 
#'       ungroup() %>% 
#'       mutate(lengthm = as.numeric(st_length(.)))
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("End time: ", round(time_block1, 3))
#'   
#'   return(ll)
#'   
#' }

#' #' Generate Cross Sections Across Hydrographic Network
#' #' @param net Hydrographic LINESTRING Network
#' #' @param id  Unique Identifier in net
#' #' @param cs_widths Bankfull Widths (length of cross sections for each net element)
#' #' @param num Number of transects per Net element
#' #' @param parallels Number of parallel clusters to make, Default is NULL which will run function sequentially
#' #' @return sf object
#' #' @export
#' 
#' cut_cross_sections3 = function(net, 
#'                                id = NULL, 
#'                                cs_widths = 100,
#'                                num = 10,
#'                                smooth = TRUE,
#'                                densify = 2,
#'                                rm_self_intersect = TRUE,
#'                                parallels = NULL
#' ){
#'   
#'   library(fastmap)
#'   library(pbapply)
#'   library(terra)
#'   library(sf)
#'   library(dplyr)
#'   library(terrainSliceR)
#'   library(mapview)
#'   library(smoothr)
#'   library(wk)
#'   library(geos)
#'   library(vctrs)
#'   library(AOI)
#'   
#'   aoi <- AOI::aoi_get("Sterling, Colorado")
#'   
#'   aoi <-
#'     aoi %>% 
#'     sf::st_transform(5070) %>% 
#'     # sf::st_buffer(15000) %>%
#'     sf::st_buffer(12000) %>%
#'     # sf::st_transform(4326) %>% 
#'     sf::st_bbox() %>% 
#'     sf::st_as_sfc() %>% 
#'     sf::st_sf()
#'   
#'   net <- nhdplusTools::get_nhdplus(aoi) 
#'   mapview::mapview(aoi) + net
#'   # names(net)
#'   net <- 
#'     new_hope_flowline %>% 
#'     nhdplusTools::get_tocomid(add = T) %>% 
#'     dplyr::select(comid, tocomid, hydroseq, divergence, rtndiv, 
#'                   levelpathi, streamleve, streamorde,
#'                   totdasqkm, divdasqkm, lengthkm, geometry = geom
#'     ) %>% 
#'     dplyr::mutate(
#'       dist_m   = lengthkm*1000,
#'       bf_width = exp(0.700 + 0.365* log(totdasqkm))
#'     ) %>% 
#'     # dplyr::select(-lengthkm) %>% 
#'     dplyr::relocate(geometry,  .after = last_col())
#'   net <- 
#'     net %>% 
#'     dplyr::select(comid, hydroseq, divergence, rtndiv, 
#'                   levelpathi, streamleve, streamorde,
#'                   totdasqkm, divdasqkm, lengthkm, geometry
#'     ) %>% 
#'     dplyr::mutate(
#'       dist_m   = lengthkm*1000,
#'       bf_width = exp(0.700 + 0.365* log(totdasqkm))
#'     ) %>% 
#'     dplyr::select(-lengthkm) %>% 
#'     dplyr::relocate(geometry,  .after = last_col())
#'   
#'   # terrainSliceR::linestring
#'   # sf::st_crs(terrainSliceR::linestring)
#'   id = "comid"
#'   cs_widths = 100
#'   num = 4
#'   smooth = TRUE
#'   densify = 2
#'   rm_self_intersect = TRUE
#'   #   # dplyr::filter(comid %in% net$comid)
#'   # net <-
#'   #   terrainSliceR::linestring %>%
#'   #   dplyr::mutate(
#'   #     bf_width = exp(0.700 + 0.365* log(totdasqkm))
#'   #   ) %>% 
#'   #   dplyr::slice(1:10)
#'   # 
#'   # bb <- net %>% 
#'   #   sf::st_bbox() %>% 
#'   #   sf::st_as_sfc() %>% 
#'   #   sf::st_sf()
#'   # 
#'   # net <- nhdplusTools::get_nhdplus(bb) %>% 
#'   #   dplyr::filter(comid %in% net$comid)
#'   # 
#'   # net <- 
#'   #   net %>% 
#'   #   dplyr::select(comid, streamleve, streamorde, hydroseq, totdasqkm, divdasqkm, lengthkm, divergence, geometry) %>% 
#'   #   dplyr::mutate(
#'   #     dist_m   = lengthkm*1000,
#'   #     bf_width = exp(0.700 + 0.365* log(totdasqkm))
#'   #     ) %>% 
#'   #   dplyr::select(-lengthkm) %>% 
#'   #   dplyr::relocate(geometry,  .after = last_col())
#'   # 
#'   # net %>% names()
#'   # %>% 
#'   # dplyr::filter(nhdplus_comid %in% c(24599575, 101))
#'   # id = "comid"
#'   # cs_widths = 100
#'   # num = 4
#'   # smooth = TRUE
#'   # densify = 2
#'   # rm_self_intersect = TRUE
#'   # system.time({
#'   #   cs = net %>% 
#'   #     cut_cross_sections(id = "comid", 
#'   #                        bf_widths = pmax(50, net$bf_width * 7),
#'   #                        num = 10) %>% 
#'   #     cross_section_pts(dem = '/Volumes/Transcend/ngen/DEM-products/dem.vrt') %>% 
#'   #     classify_points()
#'   # })
#'   # system.time({
#'   #   cs = net %>% 
#'   #     cut_cross_sections(id = "comid", 
#'   #                        # bf_widths = pmax(50, net$bf_width * 7),
#'   #                        num = 10)
#'   # })
#'   # # 
#'   # net <-
#'   #   terrainSliceR::linestring %>%
#'   #   dplyr::mutate(
#'   #     bf_width = exp(0.700 + 0.365* log(totdasqkm))
#'   #   ) %>%
#'   # dplyr::filter(nhdplus_comid %in% c(24599575, 101))
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   # spline smooth network flowlines
#'   if(smooth){ 
#'     message("Smoothing")
#'     net = smoothr::smooth(net, "spline") 
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("Smoothing time: ", round(time_block1, 3))
#'   
#'   # mapview(net, color = "red") + net2
#'   # plot(net$geometry)
#'   # plot(net2$geometry, col = "red", add = TRUE)
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   # Code block 1
#'   
#'   # add more points but still keep original vertices
#'   if(!is.null(densify)){
#'     net = smoothr::densify(net, densify) 
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("Densifying time: ", round(time_block1, 3))
#'   
#'   
#'   # mapview::npts(net)
#'   # mapview(net, color = "red") + net2
#'   
#'   # make cs_widths vector match the length of the number of network elements
#'   if(length(cs_widths) != nrow(net)) {
#'     cs_widths = rep(cs_widths[1], nrow(net))
#'   }
#'   
#'   # match number of cross sections 'nums' to number rows in net
#'   if(length(num) != nrow(net)) {
#'     num = pmax(3, rep(num[1], nrow(net)))
#'   }
#'   
#'   net$num = num
#'   net$cs_widths = cs_widths
#'   
#'   # net
#'   
#'   line <- as_geos_geometry(net[j,])
#'   
#'   vertices <- wk_vertices(line)
#'   
#'   edges <- as_geos_geometry(
#'     wk_linestring(
#'       vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#'       feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#'     )
#'   )
#'   
#'   edges = edges[-c(1, length(edges))]
#'   
#'   if(!is.null(num)){
#'     if(num[j] == 1){
#'       edges = edges[as.integer(ceiling(length(edges)/ 2))]
#'     } else {
#'       edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
#'     }
#'   }
#'   
#'   ll[[j]] = get_transects(edges, line, cs_widths[j])
#'   
#'   system.time(
#'     result <- 
#'       net %>%
#'       mutate(
#'         line = geos::as_geos_geometry(geometry)
#'       ) %>%
#'       dplyr::mutate(
#'         vertices = lapply(line, wk::wk_vertices),
#'         edges = lapply(vertices, function(v) {
#'           geos::as_geos_geometry(
#'             wk::wk_linestring(
#'               v[c(1, rep(seq_along(v)[-c(1, length(v))], each = 2), length(v))],
#'               feature_id = rep(seq_len(length(v) - 1), each = 2)
#'             )
#'           )
#'         }),
#'         edges = lapply(edges, function(e) {
#'           e[-c(1, length(e))]
#'         }),
#'         cross_sections = mapply(function(e, n) {
#'           # if (length(e) > 1) {
#'           if (n == 1) {
#'             e[as.integer(ceiling(length(e) / 2))]
#'           } else {
#'             e[as.integer(seq.int(1, length(e), length.out = min(n, length(e))))]
#'           }
#'           # } else {
#'           #   e
#'           # }
#'         }, edges, num, SIMPLIFY = FALSE
#'         ),
#'         # transects = get_transects2(cross_sections, line, cs_widths)
#'         # edges = lapply(edges, function(e) {
#'         #   e[-c(1, length(e))]
#'         # }),
#'         # transects = mapply(function(e, l, w) {
#'         #   get_transects2(e, l, w)
#'         # }, cross_sections, line, cs_widths, SIMPLIFY = FALSE
#'         # )
#'         transects = mapply(function(e, l, w) {
#'           terrainSliceR::get_transects(e, l, w)
#'         }, cross_sections, line, cs_widths, SIMPLIFY = FALSE
#'         )
#'       )
#'   )
#'   
#'   
#'   j = 2
#'   result$transects[1]
#'   
#'   result2 <- 
#'     result %>% 
#'     dplyr::group_by(comid) %>%
#'     dplyr::mutate(
#'       transects = sf::st_as_sfc(geos::geos_make_collection(Reduce(c, transects)))
#'     ) %>% 
#'     dplyr::ungroup() 
#'   
#'   mapview::mapview(net, color = "blue") +  mapview::mapview(ll2, color = "red") +  mapview::mapview(result2$transects, color = "green")
#'   
#'   result2$transects2 %>% class()
#'   result2$transects3[2]
#'   sf::st_as_sf(result2$transects2[2]) %>% .$geometry %>% plot()
#'   result2$transects2[1:3]
#'   sf::st_as_sf(result2$transects2[2])
#'   
#'   
#'   plot(result2$transects2[1])
#'   result2$transects2
#'   sf::st_as_sf(Reduce(c, result$transects))
#'   result$cross_sections[1]
#'   
#'   result$transects[[j]] %>% sf::st_as_sf()
#'   plot(result$line[j])
#'   # plot(result$transects[[1]], col = "red", add= F)
#'   plot(result$cross_sections[[j]], col = "red", add= T)
#'   # plot(result$line[1], add= T)
#'   plot(result$transects[[j]], col = "red", add= T)
#'   
#'   
#'   result$transects
#'   transects2 <- get_transects2(result$cross_sections, result$line, result$cs_widths)
#'   
#'   # get_transects2(result$cross_sections, result$line, result$cs_widths)
#'   
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   # get number of IDs per net element 
#'   ids_length <- lengths(ll)
#'   
#'   # Reduce list of cut cross sections into c() and then sf object
#'   ll <- sf::st_as_sf(Reduce(c,ll))
#'   
#'   # if 0 rows returned by get_transects, return NULL
#'   if(nrow(ll) == 0){
#'     return(NULL)
#'   }
#'   
#'   message("Formating")
#'   
#'   # Create IDs
#'   if(!is.null(id)){
#'     
#'     # using original ID value, create number or replicas to match cross sections in sf dataframe
#'     ll$hy_id = rep(net[[id]], times = ids_length)
#'     
#'   } else {
#'     
#'     # if no ID value was provided, just use the element number as ID
#'     ll$hy_id = rep(1:nrow(net), times = ids_length)
#'     
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("Formatting time: ", round(time_block1, 3))
#'   
#'   # add cross section widths column
#'   ll$cs_widths = rep(cs_widths, times = ids_length)
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   if(rm_self_intersect){
#'     
#'     ll <- 
#'       ll[lengths(sf::st_intersects(ll)) == 1, ] %>% 
#'       group_by(hy_id) %>% 
#'       mutate(cs_id = 1:n()) %>% 
#'       ungroup() %>% 
#'       mutate(lengthm = as.numeric(st_length(.)))
#'     
#'   } else {
#'     ll <- 
#'       ll %>% 
#'       group_by(hy_id) %>% 
#'       mutate(cs_id = 1:n()) %>% 
#'       ungroup() %>% 
#'       mutate(lengthm = as.numeric(st_length(.)))
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("End time: ", round(time_block1, 3))
#'   
#'   result$edges[[1]] %>% 
#'     sf::st_as_sf() %>% 
#'     .$geometry %>% 
#'     plot()
#'   
#'   result$cross_sections[[1]] %>% 
#'     sf::st_as_sf() %>% 
#'     .$geometry %>% 
#'     plot(col = 'red', add = F)
#'   
#'   result$vertices[[1]] %>% 
#'     sf::st_as_sf() %>% 
#'     .$geometry %>% 
#'     plot(add = T)
#'   
#'   result$edges
#'   result$line
#'   result$cross_sections
#'   result$cs_widths
#'   
#'   
#'   
#'   result$line
#'   result$transects[[1]] %>% plot()
#'   
#'   result$cross_sections[[1]] %>% 
#'     plot()
#'   plot(vertices, add = T)
#'   message("Cutting")
#'   parallels = 3
#'   # make parallel cluster
#'   cl <- get_parallels(parallels)
#'   
#'   # Split dataframe rows into a list with corresponding numeric values
#'   net_lst <- mapply(function(row, n, cs) unname(list(row, n, cs)),
#'                     split(net, seq_len(nrow(net))),
#'                     num,
#'                     cs_widths,
#'                     SIMPLIFY = FALSE,
#'                     USE.NAMES = FALSE
#'   )
#'   
#'   # TODO: replace lapply with future.apply
#'   # loop over each net element and create edges and perpindicular lines
#'   # Start timing
#'   start_time <- Sys.time()
#'   cl = NULL
#'   system.time(
#'     ll <-  pbapply::pblapply(
#'       cl  = cl,
#'       X   = net_lst,
#'       FUN = function(j) {
#'         # ll <- lapply(seq_len(nrow(net)), function(j) {
#'         
#'         # create geos geometry of line
#'         line <- geos::as_geos_geometry(j[[1]])
#'         # line <- geos::as_geos_geometry(net[j,])
#'         
#'         # extract vertices from line
#'         vertices <- wk::wk_vertices(line)
#'         
#'         # duplicate all points except first and last points and make edges between each point, create a feature id
#'         edges <- geos::as_geos_geometry(
#'           wk::wk_linestring(
#'             vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#'             feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#'           )
#'         )
#'         
#'         # remove the first and last edge 
#'         edges = edges[-c(1, length(edges))]
#'         
#'         # extract the cross section edges
#'         # if (!is.null(num)) {
#'         #   if (num[j] == 1) {
#'         if (!is.null(j[[2]])) {
#'           if (j[[2]] == 1) {
#'             # if number of cross sections is 1, take the mid point as the cross section
#'             edges = edges[as.integer(ceiling(length(edges) / 2))]
#'           } else {
#'             # sequence along the number of edges and get every nth element to make n cross sections
#'             edges = edges[as.integer(seq.int(1, length(edges), length.out = min(j[[2]], length(edges))))]
#'             # edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
#'           }
#'         } 
#'         
#'         # create perpiniduclar transect lines
#'         terrainSliceR::get_transects(edges, line, j[[3]])
#'         # get_transects(edges, line, cs_widths[j])
#'         
#'       })
#'   )
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("Lapply time: ", round(time_block1, 3))
#'   
#'   # stop cluster if needed
#'   if(!is.null(cl)) {
#'     
#'     # stop parallel cluster     
#'     parallel::stopCluster(cl)
#'     
#'   }
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   # get number of IDs per net element 
#'   ids_length <- lengths(ll)
#'   
#'   # Reduce list of cut cross sections into c() and then sf object
#'   ll <- sf::st_as_sf(Reduce(c,ll))
#'   
#'   # if 0 rows returned by get_transects, return NULL
#'   if(nrow(ll) == 0){
#'     return(NULL)
#'   }
#'   
#'   message("Formating")
#'   
#'   # Create IDs
#'   if(!is.null(id)){
#'     
#'     # using original ID value, create number or replicas to match cross sections in sf dataframe
#'     ll$hy_id = rep(net[[id]], times = ids_length)
#'     
#'   } else {
#'     
#'     # if no ID value was provided, just use the element number as ID
#'     ll$hy_id = rep(1:nrow(net), times = ids_length)
#'     
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("Formatting time: ", round(time_block1, 3))
#'   
#'   # add cross section widths column
#'   ll$cs_widths = rep(cs_widths, times = ids_length)
#'   
#'   # Start timing
#'   start_time <- Sys.time()
#'   
#'   if(rm_self_intersect){
#'     
#'     ll <- 
#'       ll[lengths(sf::st_intersects(ll)) == 1, ] %>% 
#'       group_by(hy_id) %>% 
#'       mutate(cs_id = 1:n()) %>% 
#'       ungroup() %>% 
#'       mutate(lengthm = as.numeric(st_length(.)))
#'     
#'   } else {
#'     ll <- 
#'       ll %>% 
#'       group_by(hy_id) %>% 
#'       mutate(cs_id = 1:n()) %>% 
#'       ungroup() %>% 
#'       mutate(lengthm = as.numeric(st_length(.)))
#'   }
#'   
#'   # Measure execution time of code block 1
#'   time_block1 <- Sys.time() - start_time
#'   message("End time: ", round(time_block1, 3))
#'   
#'   return(ll)
#'   
#' }

# 
# cut_cross_sections2 = function(net, id = NULL, cs_widths = 100, num = 10,
#                                smooth = TRUE, densify = 2,
#                                rm_self_intersect = TRUE){
#   library(pbapply)
#   library(terra)
#   library(sf)
#   library(dplyr)
#   library(terrainSliceR)
#   library(mapview)
#   library(smoothr)
#   library(wk)
#   library(geos)
#   library(vctrs)
#   library(AOI)
#   source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
#   
#   
#   # aoi <- AOI::aoi_get("Sterling, Colorado")
#   aoi   <- new_hope_flowline
#   aoi <-
#     aoi %>% 
#     sf::st_transform(5070) %>% 
#     # sf::st_buffer(15000) %>%
#     # sf::st_buffer(12000) %>%
#     # sf::st_transform(4326) %>% 
#     sf::st_bbox() %>% 
#     sf::st_as_sfc() %>% 
#     sf::st_sf()
#   
#   net <- nhdplusTools::get_nhdplus(aoi)
#   
#   
#   # mapview::mapview(aoi) + net
#   names(net)
#   # tonode, fromnode,
#   net <- 
#     net %>% 
#     dplyr::select(comid, fromnode, tonode, hydroseq, dnhydroseq, dnminorhyd, dndraincou, 
#                   divergence, rtndiv, levelpathi, streamleve, streamorde,
#                   totdasqkm, divdasqkm, lengthkm, geometry
#     ) %>% 
#     dplyr::mutate(
#       dist_m   = lengthkm*1000,
#       bf_width = exp(0.700 + 0.365* log(totdasqkm))
#     ) %>% 
#     # dplyr::select(-lengthkm) %>% 
#     dplyr::relocate(geometry,  .after = last_col())
#   
#   net <- 
#     new_hope_flowline %>% 
#     nhdplusTools::get_tocomid(add = T) %>% 
#     dplyr::select(comid, tocomid, fromnode, tonode, hydroseq, divergence, rtndiv, 
#                   hydroseq, dnhydroseq, dnminorhyd, dndraincou, 
#                   levelpathi, streamleve, streamorde,
#                   totdasqkm, divdasqkm, lengthkm, geometry = geom
#     ) %>% 
#     dplyr::mutate(
#       dist_m   = lengthkm*1000,
#       bf_width = exp(0.700 + 0.365* log(totdasqkm))
#     ) %>% 
#     # dplyr::select(-lengthkm) %>% 
#     dplyr::relocate(geometry,  .after = last_col())
#   
#   # terrainSliceR::linestring
#   # sf::st_crs(terrainSliceR::linestring)
#   id = "comid"
#   cs_widths = 130
#   num = 4
#   smooth = TRUE
#   densify = 2
#   rm_self_intersect = TRUE
#   
#   if(smooth){ 
#     message("Smoothing")
#     net = smoothr::smooth(net, "ksmooth") 
#   }
#   
#   if(!is.null(densify)){ 
#     message("Densifying")
#     net = smoothr::densify(net, densify) 
#   }
#   
#   # create empty list to add to
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
#   # loop through net flowlines, for each flowline make 'num' edges, and make perpindicular transect at the end of each edge
#   for(j in 1:nrow(net)){
#     
#     # create geos geometry of line
#     line <- as_geos_geometry(net[j,])
#     
#     # extract vertices from line
#     vertices <- wk_vertices(line)
#     
#     # duplicate all points except first and last points and make edges between each point, create a feature id
#     edges <- as_geos_geometry(
#       wk_linestring(
#         vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#         feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#       )
#     )
#     
#     # remove the first and last edge 
#     edges = edges[-c(1, length(edges))]
#     
#     # extract the cross section edges
#     if(!is.null(num)){
#       if(num[j] == 1){
#         # if number of cross sections is 1, take the mid point as the cross section
#         edges = edges[as.integer(ceiling(length(edges)/ 2))]
#       } else {
#         # sequence along the number of edges and get every nth element to make n cross sections
#         edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
#       }
#     }
#     # create perpiniduclar transect lines w/ get_transects
#     ll[[j]] = get_transects(edges, line, cs_widths[j])
#   }
#   
#   # get number of IDs per net element 
#   ids_length = lengths(ll)
#   
#   # Reduce list of cut cross sections into a single vector and then sf object
#   ll = st_as_sf(Reduce(c,ll))
#   
#   # tmpy <- result2$transects[lengths(sf::st_intersects(result2$transects)) == 1]
#   # # mapview::mapview(ll, color = "red") + 
#   #   mapview::mapview(result2$transects, color = "green") +
#   #     net +  
#   #   mapview::mapview(tmpy, color = "hotpink") +
#   #   mapview::mapview(ll2, color = "yellow")
#   
#   # if 0 rows returned by get_transects, return NULL
#   if(nrow(ll) == 0){
#     return(NULL)
#   }
#   
#   message("Formating")
#   
#   # add ID column if avaliable, otherwise create ID from row number of each transect group
#   if(!is.null(id)){
#     
#     # using original ID value, create number or replicas to match cross sections in sf dataframe
#     ll$hy_id = rep(net[[id]], times = ids_length)
#     
#   } else {
#     
#     # if no ID value was provided, just use the element number as ID
#     ll$hy_id = rep(1:nrow(net), times = ids_length)
#     
#   }
#   
#   # add divergence column if available in original data, otherwise give 0 value to all flowlines
#   if("divergence" %in% names(net)) {
#     
#     ll$divergence = rep(net[["divergence"]], times = ids_length)
#     
#   } else {
#     
#     ll$divergence = seq(0, 0, length.out = nrow(ll))
#     
#   }
#   
#   # add divergence column if available in original data, otherwise give 0 value to all flowlines
#   if("rtndiv" %in% names(net)) {
#     
#     ll$rtndiv = rep(net[["rtndiv"]], times = ids_length)
#     
#   } else {
#     
#     ll$rtndiv = seq(0, 0, length.out = nrow(ll))
#     
#   }
#   
#   # add divergence column if available in original data, otherwise give 0 value to all flowlines
#   if("hydroseq" %in% names(net)) {
#     
#     ll$hydroseq = rep(net[["hydroseq"]], times = ids_length)
#     
#   } 
#   
#   # add divergence column if available in original data, otherwise give 0 value to all flowlines
#   if("dnhydroseq" %in% names(net)) {
#     
#     ll$dnhydroseq = rep(net[["dnhydroseq"]], times = ids_length)
#     
#   } 
#   
#   # add divergence column if available in original data, otherwise give 0 value to all flowlines
#   if("dnminorhyd" %in% names(net)) {
#     
#     ll$dnminorhyd = rep(net[["dnminorhyd"]], times = ids_length)
#     
#   } 
#   
#   # add divergence column if available in original data, otherwise give 0 value to all flowlines
#   if("dndraincou" %in% names(net)) {
#     
#     ll$dndraincou = rep(net[["dndraincou"]], times = ids_length)
#     
#   } 
#   
#   # add divergence column if available in original data, otherwise give 0 value to all flowlines
#   if("fromnode" %in% names(net)) {
#     
#     ll$fromnode = rep(net[["fromnode"]], times = ids_length)
#     
#   } 
#   
#   # add divergence column if available in original data, otherwise give 0 value to all flowlines
#   if("tonode" %in% names(net)) {
#     
#     ll$tonode = rep(net[["tonode"]], times = ids_length)
#     
#   } 
#   
#   # add divergence column if available in original data, otherwise give 0 value to all flowlines
#   if("lengthkm" %in% names(net)) {
#     
#     ll$lengthkm = rep(net[["lengthkm"]], times = ids_length)
#     
#   } 
#   
#   # add cross sections widths as column
#   ll$cs_widths = rep(cs_widths, times = ids_length)
#   
#   ll
#   divs %>% 
#     nhdplusTools::get_sorted(split = T) 
#   
#   # source(system.file("extdata", "sample_flines.R", package = "nhdplusTools"))
#   # 
#   # dend <- get_tocomid(sample_flines)
#   # 
#   # nondend <- get_tocomid(sample_flines, return_dendritic = T)
#   # dend %>% 
#   #   dplyr::filter(comid %in% nondend$comid)
#   # 
#   # extras <- 
#   #   nondend %>% 
#   #   dplyr::group_by(comid) %>% 
#   #   dplyr::mutate(
#   #     grp = 1:n(),
#   #     comid = as.character(comid),
#   #     divergence = as.character(divergence)
#   #   ) %>% 
#   #   dplyr::relocate(comid, hydroseq, grp, divergence, rtndiv, dndraincou)
#   #   dplyr::filter(grp != 1)
#   # 
#   # nondend %>% 
#   #   dplyr::filter(comid %in% dend$comid)
#   # mapview::mapview(dend) + mapview::mapview(nondend, color = "red") + mapview::mapview(extras, color = "green")
#   
#   added_div <- get_tocomid(net,
#                            return_dendritic = FALSE,
#                            remove_coastal = FALSE)
#   added_div <- added_div[added_div$tocomid %in%
#                            divs$comid[divs$divergence == 2],]
#   divs <- 
#     net %>% 
#     # nhdplusTools::get_tocomid(return_dendritic = T, remove_coastal = F) %>%
#     # nhdplusTools::get_sorted(split = T) %>% 
#     nhdplusTools::get_sorted(split = F) %>% 
#     dplyr::select(-fromnode, -tonode) %>% 
#     dplyr::mutate(
#       braid_start = dplyr::case_when(
#         dndraincou > 1 & divergence == 0 ~ TRUE,
#         TRUE                             ~ FALSE
#       ),
#       braid_end = dplyr::case_when(
#         rtndiv == 1 ~ TRUE,
#         TRUE        ~ FALSE
#       )
#     )
#   divs %>% 
#     dplyr::group_by(streamleve)
#   
#   lvls <- as.character(sort(unique(divs$streamorde), decreasing = T))
#   
#   
#   for (i in 1:length(lvls)) {
#     print(i)
#     # i = 1
#     lvl_plot <-
#       divs %>% 
#       dplyr::mutate(
#         streamleve = as.character(streamleve),
#         streamorde = as.character(streamorde)
#       ) %>% 
#       ggplot2::ggplot() +
#       # ggplot2::geom_sf(ggplot2::aes(color = streamleve), lwd = 1) +
#       # gghighlight::gghighlight(streamleve == lvls[i]) +
#       ggplot2::geom_sf(ggplot2::aes(color = streamorde), lwd = 1) +
#       gghighlight::gghighlight(streamorde == lvls[i]) +
#       ggplot2::labs(title = paste0("STREAM LEVEL: ", lvls[i])) +
#       ggplot2::theme_bw() +
#       ggplot2::theme(
#         plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5)
#       )
#     
#     ggplot2::ggsave(
#       lvl_plot,
#       filename = paste0("stream_lvl_gif_", i, ".png"),
#       width = 12,
#       height = 10
#     )
#     
#   }
#   library(gifski)
#   
#   # list.files(".", pattern = "stream_lvl")
#   png_files <-  list.files(".", pattern = "stream_lvl")
#   gifski::gifski(png_files, width = 1800, height = 1100, delay = 1.5)
#   
#   
#   divs %>% 
#     dplyr::mutate(
#       streamleve = as.character(streamleve)
#     ) %>% 
#     ggplot2::ggplot() +
#     ggplot2::geom_sf(ggplot2::aes(color = streamleve))
#   
#   added_div <- get_tocomid(net,
#                            return_dendritic = FALSE,
#                            remove_coastal = FALSE)
#   added_div <- added_div[added_div$tocomid %in%
#                            divs$comid[divs$divergence == 2],]
#   y<- make_node_topology(divs, add_div = added_div)
#   #  
#   #  yy <- 
#   #    y %>% 
#   #    dplyr::group_by(tonode) %>% 
#   #    dplyr::mutate(fromnode_list = as.character(list(as.character(fromnode)))) %>% 
#   #    # dplyr::slice(1) %>% 
#   #    dplyr::select(comid, tonode, fromnode = fromnode_list, hydroseq, divergence, rtndiv)
#   #  mapview::mapview(yy) +   mapview::mapview(y, color = "red") 
#   #  x <- dplyr::select(get_tocomid(
#   #    dplyr::select(new_hope_flowline, COMID, FromNode, ToNode, Divergence, FTYPE,
#   #                  AreaSqKM, LENGTHKM, GNIS_ID)
#   #  ), -tonode, -fromnode)
#   #  div <- get_tocomid(dplyr::select(new_hope_flowline, COMID, FromNode, ToNode),
#   #                     return_dendritic = FALSE,
#   #                     remove_coastal = FALSE)
#   #  div <- div[div$tocomid %in%
#   #               new_hope_flowline$COMID[new_hope_flowline$Divergence == 2],]
#   #  
#   # y<- make_node_topology(x, add_div = div)
#   #  divs %>% 
#   #    slice_tail(divs, n = 100) %>% 
#   #    .$geometry %>% plot()
#   
#   
#   start_braid <- 
#     divs %>% 
#     dplyr::filter(braid_start == TRUE) 
#   
#   braid_ids <- 
#     divs %>% 
#     dplyr::filter(braid_start == TRUE)  %>% 
#     dplyr::select(comid, hydroseq, dnhydroseq, dnminorhyd, braid_start, braid_end) %>% 
#     dplyr::mutate(
#       new_id = as.character(1:n())
#     ) %>% 
#     sf::st_drop_geometry() %>%
#     dplyr::select(dnhydroseq, dnminorhyd, new_id) %>% 
#     tidyr::pivot_longer(
#       cols = c(dnhydroseq, dnminorhyd),
#       names_to = "name",
#       values_to = "hydroseq"
#     ) %>% 
#     dplyr::mutate(
#       divergence = ifelse(name == "dnminorhyd", 2, 1)
#     ) %>% 
#     dplyr::select(-name)
#   
#   braids <- 
#     divs %>% 
#     dplyr::filter(
#       hydroseq %in% braid_ids$hydroseq
#     )
#   
#   
#   net
#   
#   
#   mapview::mapview(braid_ids, burst = T) +   mapview::mapview(braids, color = "red")
#   
#   
#   # x <- dplyr::select(get_tocomid(net, remove_coastal = F)
#   #   dplyr::select(
#   #     net,
#   #     # dplyr::filter(new_hope_flowline, COMID %in% coms),
#   #     COMID, FromNode, ToNode, 
#   #     Hydroseq, Divergence,RtnDiv,DnDrainCou, FTYPE, DnHydroseq, DnMinorHyd, 
#   #     AreaSqKM, LENGTHKM, GNIS_ID)
#   # ), -tonode, -fromnode)
#   
#   unique(net$divergence)
#   
#   main_divs <- divs %>% 
#     dplyr::filter(divergence == 1)
#   
#   min_divs <- divs %>% 
#     dplyr::filter(divergence == 2)
#   
#   no_divs <- divs %>% 
#     dplyr::filter(divergence == 0)
#   
#   ret_divs <- divs %>% 
#     dplyr::filter(rtndiv == 1)
#   
#   dndrain <- divs %>% 
#     dplyr::filter(dndraincou == 2)
#   
#   dndrain_no_div <-
#     divs %>% 
#     dplyr::filter(divergence == 0)
#   
#   start_braid <- 
#     divs %>% 
#     dplyr::filter(braid_start == TRUE)
#   
#   end_braid <- 
#     divs %>% 
#     dplyr::filter(braid_end == TRUE)
#   
#   # tmpy <- result2$transects[lengths(sf::st_intersects(result2$transects)) == 1]
#   mapview::mapview(net) +
#     mapview::mapview(no_divs, color = "dodgerblue") +
#     # mapview::mapview(ret_divs, color = "white") +
#     
#     mapview::mapview(ret_divs, color = "hotpink") +
#     mapview::mapview(dndrain, color = "yellow") +
#     mapview::mapview(dndrain_no_div, color = "hotpink") +
#     mapview::mapview(start_braid, color = "white") + 
#     mapview::mapview(end_braid, color = "white") + 
#     # mapview::mapview(braids, burst = T) +
#     mapview::mapview(braids, color = "red") +
#     # mapview::mapview(ret_divs, color = "yellow") +
#     # mapview::mapview(dndrain, color = "yellow") +
#     mapview::mapview(main_divs, color = "green") +
#     # mapview::mapview(min_divs, color = "yellow") +
#     mapview::mapview(min_divs, color = "red") +
#     mapview::mapview(ll, color = "white") +
#     mapview::mapview(ll2, color = "white") 
#   # mapview::mapview(tmpy, color = "hotpink") +
#   # mapview::mapview(ll2, color = "yellow")
#   
#   # net_fl <- 
#   outlets <-  
#     net %>% 
#     dplyr::filter(dndraincou == 2 | rtndiv == 1) %>% 
#     .$comid
#   
#   # outlets <- net_fl$comid
#   
#   outlets
#   net_fl
#   
#   # Add toCOMID
#   net_fl <- nhdplusTools::get_tocomid(net, remove_coastal = FALSE, add = TRUE)
#   
#   outs <-
#     net %>% 
#     dplyr::filter(comid %in% outlets)
#   
#   fl <- dplyr::select(net_fl, ID = comid, toID = tocomid, lengthkm)
#   
#   paths <- nhdplusTools::get_path_members(outlets, fl)
#   
#   paths$path
#   
#   tmp1 <- fl[paths$path[[1]],]
#   tmp2 <- fl[paths$path[[2]],]
#   tmp3 <- fl[paths$path[[3]],]
#   
#   mapview::mapview(net_fl) +
#     mapview::mapview(outs, color = "red") +
#     mapview::mapview(tmp1, color = "green") +
#     mapview::mapview(tmp2, color = "green") + 
#     mapview::mapview(tmp3, color = "green") +
#     mapview::mapview(tmp11, color = "green") + 
#     mapview::mapview(tmp12, color = "green") 
#   
#   
#   fl[lengths(paths$path) > 1, ]
#   
#   tmp1 <- fl[paths$path[[1]],]
#   tmp2 <- fl[paths$path[[2]],]
#   tmp3 <- fl[paths$path[[3]],]
#   tmp11 <- fl[paths$path[[11]],]
#   tmp12 <- fl[paths$path[[12]],]
#   
#   
#   paths
#   mapview::mapview(fline) +   mapview::mapview(outs, color = "red") + 
#     mapview::mapview(tmp1, color = "green") +
#     mapview::mapview(tmp2, color = "green") + 
#     mapview::mapview(tmp3, color = "green") +
#     mapview::mapview(tmp11, color = "green") + 
#     mapview::mapview(tmp12, color = "green")
#   # nhdplusTools::get
#   source(system.file("extdata/new_hope_data.R", package = "nhdplusTools"))
#   mapview::mapview(x)
#   library(nhdplusTools)
#   new_hope_flowline %>% names()
#   coms <- c(8894192, 8893842, 8893844, 8893850)
#   x <- dplyr::select(get_tocomid(
#     dplyr::select(
#       new_hope_flowline,
#       # dplyr::filter(new_hope_flowline, COMID %in% coms),
#       COMID, FromNode, ToNode, 
#       Hydroseq, Divergence,RtnDiv,DnDrainCou, FTYPE, DnHydroseq, DnMinorHyd, 
#       AreaSqKM, LENGTHKM, GNIS_ID)
#   ), -tonode, -fromnode)
#   
#   head(y <- make_node_topology(x))
#   ysort <- nhdplusTools::get_sorted(x)
#   
#   ysort <- 
#     ysort %>% 
#     dplyr::mutate(
#       braid_start = dplyr::case_when(
#         dndraincou > 1 & divergence == 0 ~ TRUE,
#         TRUE                             ~ FALSE
#       ),
#       braid_end = dplyr::case_when(
#         rtndiv == 1 ~ TRUE,
#         TRUE        ~ FALSE
#       )
#     )
#   
#   start_braid <- 
#     ysort %>% 
#     dplyr::filter(braid_start == TRUE)
#   
#   end_braid <- 
#     ysort %>% 
#     dplyr::filter(braid_end == TRUE)
#   
#   
#   
#   div1 <- ysort %>% 
#     dplyr::filter(divergence == 1)
#   
#   div2 <- ysort %>% 
#     dplyr::filter(divergence == 2)
#   
#   retdiv <- ysort %>% 
#     dplyr::filter(rtndiv == 1)
#   # 250016991
#   # 250016571
#   
#   drain_count <- ysort %>% 
#     dplyr::filter(dndraincou == 2, divergence == 0)
#   
#   mapview::mapview(ysort) +  
#     mapview::mapview(start_braid, color = "white") + 
#     mapview::mapview(end_braid, color = "white") + 
#     mapview::mapview(div1, color = "green") + 
#     mapview::mapview(div2, color = "red") +
#     mapview::mapview(retdiv, color = "cyan") + 
#     mapview::mapview(drain_count, color = "yellow") 
#   
#   library(igraph)
#   library(tidygraph)
#   
#   # igraph::make_graph()
#   igraph::make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = TRUE)
#   rstat_nodes <- data.frame(name = c("Hadley", "David", "Romain", "Julia"))
#   rstat_edges <- data.frame(from = c(1, 1, 1, 2, 3, 3, 4, 4, 4),
#                             to = c(2, 3, 4, 1, 1, 2, 1, 2, 3))
#   tbl_graph(nodes = rstat_nodes, edges = rstat_edges) %>% plot()
#   nodes <- sf::st_drop_geometry(dplyr::select(ysort, comid, from = fromnode, to = tonode))    
#   edges <- sf::st_drop_geometry(dplyr::select(ysort,from = fromnode, to = tonode))
#   nodes
#   edges
#   tidygraph::as_tbl_graph(nodes = nodes, edges = edges)
#   
#   y %>% 
#     dplyr::filter(tonode == 704)
#   
#   # just the divergences which have unique fromids in x but don't in new hope.
#   div <- get_tocomid(dplyr::select(new_hope_flowline, COMID, FromNode, ToNode),
#                      return_dendritic = FALSE,
#                      remove_coastal = FALSE)
#   div <- div[div$tocomid %in%
#                new_hope_flowline$COMID[new_hope_flowline$Divergence == 2],]
#   
#   yy <- make_node_topology(x, div)
#   yy
#   ysort <- nhdplusTools::get_sorted(y)
#   
#   mapview::mapview(y) +  
#     mapview::mapview(yy, color = "red") +  
#     mapview::mapview(div1, color = "green") + 
#     mapview::mapview(div2, color = "red") +
#     mapview::mapview(retdiv, color = "cyan") + 
#     mapview::mapview(drain_count, color = "yellow") 
#   
#   # Define the adjacency list
#   adj_list <- list(
#     "1" = c("2"),            # Node 1 connects to Node 2
#     "2" = c("3", "4"),       # Node 2 connects to Node 3 and Node 4 (splitting into braid)
#     "3" = c("5"),            # Node 3 connects to Node 5
#     "4" = c("6"),            # Node 4 connects to Node 6
#     "5" = c("7"),            # Node 5 connects to Node 7
#     "6" = c("7"),            # Node 6 connects to Node 7
#     "7" = c("8"),            # Node 7 connects to Node 8 (coming back together)
#     "8" = character(0)       # Node 8 is the end node (no outgoing connections)
#   )
#   
#   source(system.file("extdata", "walker_data.R", package = "nhdplusTools"))
#   fline <- walker_flowline
#   # mapview::mapview(fline)
#   # outlets <- c(5329303, 5329357, 5329317, 5329365, 5329435, 5329817)
#   outlets <- c(5329303,  5329317, 5329817)
#   
#   # Add toCOMID
#   fline <- nhdplusTools::get_tocomid(fline, add = TRUE)
#   outs <-
#     fline %>% 
#     dplyr::filter(comid %in% outlets)
#   
#   fl <- dplyr::select(fline, ID = comid, toID = tocomid, lengthkm)
#   
#   paths <- nhdplusTools::get_path_members(outlets, fl)
#   
#   paths$path
#   tmp1 <- fl[paths$path[[1]],]
#   tmp2 <- fl[paths$path[[2]],]
#   tmp3 <- fl[paths$path[[3]],]
#   
#   
#   paths
#   mapview::mapview(fline) +   mapview::mapview(outs, color = "red") + 
#     mapview::mapview(tmp1, color = "green") +
#     mapview::mapview(tmp2, color = "green") + 
#     mapview::mapview(tmp3, color = "green")
#   550028237
#   550028236
#   
#   # remove self intersecting cross sections if rm_self_intersect == TRUE, otherwise keep them
#   if(rm_self_intersect){
#     ll2 <-
#       ll[lengths(st_intersects(ll)) == 1, ] %>% 
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
