#' Generate a Perpendicular Linestring of a Given Width
#' @param edge LINESRTING
#' @param width Length of Perpendicular LINESTRING
#' @return GEOS object
#' @export

cut_transect = function(edge, width){
  
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
#' @param bf_widths Bankfull Widths (length of cross sections for each net element)
#' @param num Number of transects per Net element
#' @return sf object
#' @export

cut_cross_sections = function(net, id = NULL, bf_widths = 100, num = 10,
                              smooth = TRUE, densify = FALSE){
  

  if(smooth){ 
    message("Smoothing")
    net = smoothr::smooth(net, "spline") 
  }
  
  if(densify){ net = smoothr::densify(net) }
  
  ll = list()
  
  if(length(bf_widths) != nrow(net)){
    bf_widths = rep(bf_widths[1], nrow(net))
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
      edges = edges[as.integer(seq.int(1, length(edges), length.out = min(num[j], length(edges))))]
    }
    
    ll[[j]] = get_transects(edges, line, bf_widths[j])
  }
  

  ids_length = lengths(ll)
  ll = st_as_sf(Reduce(c,ll))
  
  if(nrow(ll) == 0){
    return(NULL)
  }
  
  message("Formating")
  
  if(!is.null(id)){
    ll$hy_id = rep(net[[id]], times = ids_length)
  } else {
    ll$hy_id = rep(1:nrow(net), times = ids_length)
  }
  
  ll$bf_width = rep(bf_widths, times = ids_length)
  
  ll[lengths(st_intersects(ll)) == 1, ] %>% 
    group_by(hy_id) %>% 
    mutate(cs_id = 1:n()) %>% 
    ungroup() %>% 
    mutate(lengthm = as.numeric(st_length(.)))
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
  
  . <-  L <-  L1 <-  L2  <-  R  <-  R1 <-  R2  <- Z  <-  Z2 <-  anchor <-  b1  <- b2  <- bf_width  <- count_left <- 
    count_right  <-  cs_id <-  hy_id <-  in_channel_pts  <- lengthm <-  low_pt  <- max_bottom  <- mean_dist <-  mid_bottom  <- min_bottom  <- pt_id <- relative_distance <-  third <-  NULL
  
  filter(cs_pts) %>% 
    group_by(hy_id, cs_id) %>% 
    mutate(third = ceiling(n() / 3),
           mean_dist = mean(diff(relative_distance)),
           in_channel_pts = ceiling(bf_width[1] / mean_dist),
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
    select(hy_id, cs_id, pt_id, Z, relative_distance, bf_width, class)
  
}

