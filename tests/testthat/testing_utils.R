# File containing a bunch of utility functions used for testing 
# - checking correct column outputs
# - checking SF characteristics (geometry type, CRS, etc)
#  

# library(hydrofabric3D)
# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
find_connected_components_has_min_output_cols <- function(df, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  to_id <- hydrofabric3D:::as_to_id(crosswalk_id)
  
  expected_cols <- c("component_id", 
                     crosswalk_id, 
                     to_id,
                     "fromnode",
                     "tonode"
  )
  
  return(
    all(expected_cols %in% names(df)) && length(expected_cols) == length(names(df))
  )
}

# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
get_node_topology_has_min_output_cols <- function(node_df, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  to_id <- hydrofabric3D:::as_to_id(crosswalk_id)
  
  expected_cols <- c(crosswalk_id, 
                     to_id,
                     "fromnode",
                     "tonode"
                     )
  
  return(
    all(expected_cols %in% names(node_df)) && length(expected_cols) == length(names(node_df))
  )
}


prep_flowlines_for_transect_cuts <- function(flines, id_col, min_bf_width) {

  flines <- 
    flines %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", min_bf_width) %>%  
    dplyr::rename(!!sym(id_col) := crosswalk_id) %>% 
    nhdplusTools::rename_geometry("geometry") %>% 
    dplyr::select(
      dplyr::any_of(id_col), 
      tot_drainage_areasqkm,
      bf_width,
      geometry
    ) 
  
  return(flines)
  
}

# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
relief_detailed_has_min_output_cols <- function(relief, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, 
                     "cs_id" ,
                     "cs_lengthm",
                     "has_relief",
                     "max_relief", 
                     "pct_of_length_for_relief"
                     )
  
  return(
    all(expected_cols %in% names(relief)) && length(expected_cols) == length(names(relief))
  )
}


# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
relief_has_min_output_cols <- function(relief, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, 
                     "cs_id" ,
                     "has_relief"
  )
  
  return(
    all(expected_cols %in% names(relief)) && length(expected_cols) == length(names(relief))
  )
}


has_same_unique_tmp_ids <- function(x, y, crosswalk_id = "hydrofabric_id") {
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  start_ids <- hydrofabric3D:::get_unique_tmp_ids(df = x, x = crosswalk_id)
  end_ids   <- hydrofabric3D:::get_unique_tmp_ids(df = y, x = crosswalk_id)
  
  # all IDs are in x AND y and same number of ids
  same_unique_ids <- all(start_ids %in% end_ids) && all(end_ids %in% start_ids) && length(start_ids) == length(end_ids)
  
  return(same_unique_ids)
}

# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
bank_attrs_has_min_output_cols <- function(bank_attrs, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, 
                     "cs_id" ,
                     "bottom",
                     "left_bank",
                     "right_bank",
                     "valid_banks"
  )
  
  return(
    all(expected_cols %in% names(bank_attrs)) && length(expected_cols) == length(names(bank_attrs))
  )
}

# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
classified_cs_pts_has_min_output_cols <- function(classified_pts, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, 
                     "cs_id","pt_id", 
                     "cs_lengthm", "relative_distance", "Z",  
                     "points_per_cs",
                     "class", "point_type", 
                     "bottom",
                     "left_bank",
                     "right_bank",
                     "valid_banks",
                     "has_relief",
                     "geometry"
                     )
  
  return(
    all(expected_cols %in% names(classified_pts)) && length(expected_cols) == length(names(classified_pts))
  )
}

# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
cs_pts_has_min_output_cols <- function(cs_pts, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, 
                     "cs_id","pt_id", "cs_lengthm", 
                     "relative_distance", "Z", "points_per_cs", "geometry"
  )
  
  return(
    all(expected_cols %in% names(cs_pts)) && length(expected_cols) == length(names(cs_pts))
  )
}

check_cs_pts_has_exact_cols <- function(cs_pts, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
                     )
  
  return(
    all(expected_cols %in% names(cs_pts)) && length(expected_cols) == length(names(cs_pts))
  )
}


# TODO: this is a better named version of check_cs_pts_and_transect_cols() (DUPLICATE)
cs_pts_has_correct_cols_from_transects <- function(cs_pts, transects, crosswalk_id = "hydrofabric_id") {
  # crosswalk_id = "hy_id"
  # crosswalk_id <- ID_COL
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
  )
  
  return(
    all(unique(c(expected_cols, names(transects))) %in% names(cs_pts))
  )
}

check_transect_output_cols <- function(transects, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, "cs_id","cs_lengthm", "cs_measure", "ds_distance", 
                     # "lengthm", 
                     "sinuosity", "geometry")
  
  return(
    all(expected_cols %in% names(transects)) && length(expected_cols) == length(names(transects))
  )
}

# minimum possible columns for a transect
check_min_transect_output_cols <- function(transects, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, "cs_id", "geometry")
  
  return(
    all(expected_cols %in% names(transects))
  )
}

# minimum possible columns for a dataframe / sf dataframe
check_required_cols <- function(x, 
                                expected_cols = NULL
                                ) {
  
  if(is.null(expected_cols)) {
    return(FALSE)
  }
  
  return(
    all(expected_cols %in% names(x))
  )
}


# minimum possible columns for a transect
check_min_extended_transects_output_cols <- function(x, 
                                                     crosswalk_id = "hydrofabric_id", 
                                                     cs_id = "cs_id"
                                                     ) {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  if(is.null(cs_id)) {
    cs_id = "cs_id"
  }
  
  expected_cols <- c(crosswalk_id, 
                     cs_id, 
                     "cs_lengthm", 
                     "extension_distance", 
                     "left_distance", "right_distance", 
                     "left_is_extended", "right_is_extended"
                     )
  
  return(
    all(expected_cols %in% names(x))
  )
}

check_cs_pts_and_transect_cols <- function(cs_pts, transects, crosswalk_id = "hydrofabric_id") {
  # crosswalk_id = "hy_id"
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
                     )
  
  return(
    all(unique(c(expected_cols, names(transects))) %in% names(cs_pts))
  )
}

check_cs_pts_has_required_cols <- function(transects, crosswalk_id = "hydrofabric_id") {
  
  if(is.null(crosswalk_id)) {
    crosswalk_id = "hydrofabric_id"
  }
  
  expected_cols <- c(crosswalk_id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
  )
  
  return(
    all(expected_cols %in% names(transects))
  )
}

has_same_crs <- function(sf1, sf2) {
  
  return (
    sf::st_crs(sf1) == sf::st_crs(sf2)
  )
  
}
make_flowlines_and_transects_test_data <- function() {
  
  u_shape_coords <- matrix(c(
    0, 0,   # bottom-left
    0, 2,   # top-left
    2, 2,   # top-right
    2, 0    # bottom-right
  ), ncol = 2, byrow = TRUE)
  
  v_shape_coords <- matrix(c(
    4, 4,   # bottom-left
    5, 5,   # top-left
    6, 4   # top-right
  ), ncol = 2, byrow = TRUE)
  
  separated_trans_coords <- matrix(c(
    5, 10, 
    7, 10  #
    
  ), ncol = 2, byrow = TRUE)
  
  main_shapes <- sf::st_sfc(
    c(
    sf::st_linestring(u_shape_coords),
    sf::st_linestring(v_shape_coords),
    sf::st_linestring(separated_trans_coords)
    ),
    crs = 5070
    )  %>% 
    sf::st_cast("LINESTRING")
  
  
  trans_coords1 <- matrix(c(
    -1, 1, # transect 1
    1, 1  # transect 1
    
  ), ncol = 2, byrow = TRUE)
  trans1 <- sf::st_sfc(sf::st_linestring(trans_coords1), crs = 5070)  
 
  trans_coords2 <- matrix(c(
    0.5, 3, # transect 2
    0.5, 0.5  # transect 2
    
  ), ncol = 2, byrow = TRUE)
  trans2 <- sf::st_sfc(sf::st_linestring(trans_coords2), crs = 5070)  
 
  trans_coords3 <- matrix(c(
    1.5, 3, # transect 2
    1.5, 0.5  # transect 2
    
  ), ncol = 2, byrow = TRUE)
  trans3 <- sf::st_sfc(sf::st_linestring(trans_coords3), crs = 5070)  
  
  trans_coords4 <- matrix(c(
    1, 0.25, # transect 2
    3, 0.25  # transect 2
    
  ), ncol = 2, byrow = TRUE)
  trans4 <- sf::st_sfc(sf::st_linestring(trans_coords4), crs = 5070)  
  
  
  trans_coords5 <- matrix(c(
    -1, 1.25, # transect 2
    3, 1.25  # transect 2
    
  ), ncol = 2, byrow = TRUE)
  
  trans5 <- sf::st_sfc(sf::st_linestring(trans_coords5), crs = 5070)  
  
  # plot(main_shapes, add = F)
  # plot(trans1, add = T)
  # plot(trans2, add = T)
  # plot(trans3, add = T)
  # plot(trans4, add = T)
  # plot(trans5, add = T)
  
  combined <- sf::st_as_sf(c(trans1, trans2, trans3, trans4, trans5, main_shapes), crs = 5070)
  # plot(combined$x[8])
  
  combined$line_type <- c("transect", "transect", "transect", "transect", "transect", "flowline", "flowline2", "separated_transect")
  
  # ggplot2::ggplot() +
  #   ggplot2::geom_sf(data = combined, ggplot2::aes(color = line_type))
  
  return(combined)
}

make_flowlines_and_transects_test_data_all_valid <- function() {
  
  u_shape_coords <- matrix(c(
    0, 0,   # bottom-left
    0, 2,   # top-left
    2, 2,   # top-right
    2, 0    # bottom-right
  ), ncol = 2, byrow = TRUE)
  
  v_shape_coords <- matrix(c(
    4, 4,   # bottom-left
    5, 5,   # top-left
    6, 4   # top-right
  ), ncol = 2, byrow = TRUE)
  
  separated_trans_coords <- matrix(c(
    5, 10, 
    7, 10  #
    
  ), ncol = 2, byrow = TRUE)
  
  main_shapes <- sf::st_sfc(
    c(
      sf::st_linestring(u_shape_coords),
      sf::st_linestring(v_shape_coords),
      sf::st_linestring(separated_trans_coords)
    ),
    crs = 5070
  )  %>% 
    sf::st_cast("LINESTRING")
  
  
  trans_coords1 <- matrix(c(
    -1, 1, # transect 1
    1, 1  # transect 1
    
  ), ncol = 2, byrow = TRUE)
  trans1 <- sf::st_sfc(sf::st_linestring(trans_coords1), crs = 5070)  
  
  # trans_coords2 <- matrix(c(
  #   0.5, 3, # transect 2
  #   0.5, 0.5  # transect 2
  #   
  # ), ncol = 2, byrow = TRUE)
  # trans2 <- sf::st_sfc(sf::st_linestring(trans_coords2), crs = 5070)  
  
  trans_coords3 <- matrix(c(
    1.5, 3, # transect 2
    1.5, 0.5  # transect 2
    
  ), ncol = 2, byrow = TRUE)
  trans3 <- sf::st_sfc(sf::st_linestring(trans_coords3), crs = 5070)  
  
  trans_coords4 <- matrix(c(
    1, 0.25, # transect 2
    3, 0.25  # transect 2
    
  ), ncol = 2, byrow = TRUE)
  trans4 <- sf::st_sfc(sf::st_linestring(trans_coords4), crs = 5070)  
  
  
  # trans_coords5 <- matrix(c(
  #   -1, 1.25, # transect 2
  #   3, 1.25  # transect 2
  #   
  # ), ncol = 2, byrow = TRUE)
  # 
  # trans5 <- sf::st_sfc(sf::st_linestring(trans_coords5), crs = 5070)  
  
  # plot(main_shapes, add = F)
  # plot(trans1, add = T)
  # plot(trans2, add = T)
  # plot(trans3, add = T)
  # plot(trans4, add = T)
  # plot(trans5, add = T)
  
  combined <- sf::st_as_sf(c(trans1, trans3, trans4, main_shapes), crs = 5070)
  # plot(combined$x[8])
  
  combined$line_type <- c("transect", "transect", "transect", "flowline", "flowline2", "separated_transect")
  
  # ggplot2::ggplot() +
  #   ggplot2::geom_sf(data = combined, ggplot2::aes(color = line_type))
  
  return(combined)
}

# Create a V shaped linestring given a lat/lon 
create_v_line <- function(lat, lon, crs = 4326) {
  # lat <- 34.41249
  # lon <- -119.74095
  
  # middle of the V shape
  apex <- c(lon, lat)
  
  left_point  <- c(apex[1] - 0.001, apex[2] + 0.001)  # move left and up
  right_point <- c(apex[1] + 0.001, apex[2] + 0.001)  # move right and up
  
  coords      <- rbind(left_point, apex, right_point)

  v_shape_line   <- sf::st_sf(geometry = sf::st_sfc(
                                            sf::st_linestring(coords), crs = crs
                                            )
                      )
  
  return(v_shape_line)
}


make_single_straight_line <- function(epsg_code = 5070) {
  
  line_coords <- matrix(c(
    -100, 100,
    100, 100  
    
  ), ncol = 2, byrow = TRUE)
  
  line_geom <- sf::st_sfc(sf::st_linestring(line_coords), crs = epsg_code)  
  
  line <- sf::st_as_sf(c(line_geom), crs = epsg_code)
  
  # plot(line$x)
  # line %>% 
  #   sf::st_cast("POINT")
  
  # sf::st_segmentize(line, 100) %>% 
  #   sf::st_cast("POINT")
  

  
  # ggplot2::ggplot() +
  #   ggplot2::geom_sf(data = combined, ggplot2::aes(color = line_type))
  
  return(line)
}
# -------------------------------------------------------------------------------
# ---- Functions for generating testing cross sections data ----
# -------------------------------------------------------------------------------

# generate cross section Z values for testing
make_cs_line <- function(y_start, y_end, n = 10) {
  
  # evenly spaced x vals
  x_vals <- seq(0, 1, length.out = n)
  
  # calculate slope (change in y / change in x)
  slope <- (y_end - y_start) / 1  # (x_end - x_start) = 1 since x goes from 0 to 1
  
  # determine corresponding y values (y = mx + b)
  intercept <- y_start  
  y_vals    <- slope * x_vals + intercept
  
  #  calc relative distance (percent along the line (e.g., 0.1, 0.2, ..., 1.0)
  relative_distance <- seq(1 / n, 1, length.out = n)
  
  points_df <- data.frame(
    pt_id             = 1:n,
    relative_distance = relative_distance,
    X                 = x_vals,
    Z                 = y_vals
  )
  
  return(points_df)
}

get_flat_line <- function(y = 0, n = 10) {
  return(
    make_cs_line(y_start = y, y_end = y, n = n)
  )
}

get_neg_diagonal <- function(start = 10, end = 6) {
  
  if(start < end) {
    stop("'start' is less than 'end', 'start' value must be greater than 'end' value to generate a negative slope line")
  }
  
  # start = 10
  # end = 8
  
  n <- start - end
  
  # make_cs_line(y_start = start, y_end = end, n = n) %>% .$Z %>% plot()
  return(
    make_cs_line(y_start = start, y_end = end, n = n)
  )
  
}

get_pos_diagonal <- function(start = 6, end = 10) {
  
  if(end < start) {
    stop("'end' is less than 'start', 'end' value must be greater than 'start' value to generate a positive slope line")
  }
  
  # start = 6
  # end = 10
  
  n <- end - start
  
  # make_cs_line(y_start = start, y_end = end, n = n)
  # make_cs_line(y_start = start, y_end = end, n = n) %>% .$Z %>% plot()
  
  return(
    make_cs_line(y_start = start, y_end = end, n = n)
  )
  
}

merge_lines <- function(...) {
  
  args <- list(...)
  
  lines               <- dplyr::bind_rows(args)
  n                   <- nrow(lines)
  relative_distance   <- seq(1 / n, 1, length.out = n)
  
  lines <- 
    lines %>% 
    dplyr::mutate(
      pt_id             = 1:dplyr::n()
    )
  
  lines$relative_distance <- relative_distance
  
  return(lines)
}

make_cs <- function(...) {
  
  # x = c(10, 4, 6)  
  # x2 <- c(4, 10, 6)
  # args <- list(x, x2)
  
  args <- list(...)
  
  cs_parts <- list()
  
  for (i in seq_along(args)) {
    
    # message(i)
    cs_structure <- args[[i]]
    
    if (length(cs_structure) != 3) {
      next
    }
    
    start_y <- cs_structure[1]
    end_y   <- cs_structure[2]
    n       <- cs_structure[3]
    
    # make_cs_line(start_y, end_y, n)$Z %>% 
    cs_parts[[i]] <- make_cs_line(start_y, end_y, n)
    
    
  }
  
  cs <- merge_lines(cs_parts)
  
  return(cs)
  
}

make_cs_curve <- function(left_y_range, right_y_range, bottom_length) {
  # start_y1 = 10
  # end_y1 = 4
  # start_y2 = 4
  # end_y2 <- 8
  # bottom_length = 1
  # left_y_range <- c(10, 4)
  
  start_y1 <- left_y_range[1]
  end_y1 <- left_y_range[2]
  
  start_y2 <- right_y_range[1]
  end_y2   <- right_y_range[2]
  
  bottom_y <- min(end_y1, start_y2)
  
  left    <- get_neg_diagonal(start_y1, end_y1)
  bottom  <- get_flat_line(bottom_y, bottom_length)
  right   <- get_pos_diagonal(start_y2, end_y2)
  
  cs      <- merge_lines(left, bottom, right)
  
  # gen_cs %>%
  #   .$Z %>% plot()
  
  return(cs)
  
}

# generate set of points for linestrings
make_pts <- function(start_x, start_y, num_points, step_x = 10, step_y = 5) {
  points <- matrix(ncol = 2, nrow = num_points)
  points[1, ] <- c(start_x, start_y)  
  
  for (i in 2:num_points) {
    points[i, ] <- points[i-1, ] + c(step_x, step_y)  
  }
  
  return(points)
}

get_test_lines <- function(num_linestrings, id_col_name = NULL, connected = TRUE, step_x = 10, step_y = 5) {
  
  if(is.null(id_col_name)) {
    id_col_name <- "ex_id"
  }
  
  linestrings <- list()
  
  if (connected) {
    start_x <- 0
    start_y <- 0
    
    for (i in 1:num_linestrings) {
      points <- make_pts(start_x, start_y, 2, step_x, step_y)  # 2 points per linestring
      linestrings[[i]] <- sf::st_linestring(points)
      start_x <- points[2, 1]  #2nd pt is  start of the next line
      start_y <- points[2, 2]
    }
    
  } else {
    for (i in 1:num_linestrings) {
      start_x <- i * 20  # shift the start position of each line by 20  to disconnect
      start_y <- i * 10
      points <- make_pts(start_x, start_y, 2, step_x, step_y)
      linestrings[[i]] <- sf::st_linestring(points)
    }
  }
  
  # sf_lines <- sf::st_sfc(linestrings)
  sf_df <- sf::st_sf(geometry = sf::st_sfc(linestrings))
  
  sf_df <- 
    # sf_df %>% 
    # dplyr::mutate(
    #   ex_id = 1:dplyr::n()
    # )
    sf_df %>% 
    dplyr::mutate(
      !!id_col_name := 1:dplyr::n()
    )
  
  return(sf_df)
}


# left_chan   <- get_neg_diagonal(6, 2)
# bottom      <- get_flat_line(2, 4)
# right_chan  <- get_pos_diagonal(2, 6)
# 
# gen_cs <- merge_lines(left_chan, bottom, right_chan)
# 
# gen_cs %>%
#   .$Z %>% plot()

# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------

# get_cs_point_types2 <- function(
#     depths,
#     num_of_pts,
#     cs_length,
#     relative_distance,
#     point_types
# ) {
#   
#   # ----------------------------------------------------------------------
#   # num_of_pts          <- cs_pts$points_per_cs[1]
#   # pt_ids              <- cs_pts$pt_id
#   # relative_distance   <- cs_pts$relative_distance
#   # depths              <- cs_pts$Z
#   # cs_length           <- cs_pts$cs_lengthm[1]
#   # 
#   # point_types = classify_banks_and_bottoms(
#   #   num_of_pts = num_of_pts,
#   #   pt_ids     = pt_ids,
#   #   depths     = depths
#   # )
#   # 
#   # # smooth_Z = smooth_depths(Z, window = 3),
#   # # Z2      = use_smoothed_depths(Z, smooth_Z, point_type)
#   # # Z_start = Z,
#   # 
#   # depths           = use_smoothed_depths(start_depths = depths, 
#   #                                   smoothed_depths = smooth_depths(depths, window = 3), 
#   #                                   point_types = point_types
#   # )
#   # 
#   # # plot(Z)
#   
#   # ----------------------------------------------------------------------
#   # ----------------------------------------------------------------------
#   
#   # get the number of points in each third of the cross section  
#   third          <- ceiling(num_of_pts / 3)
#   
#   # Find the anchor points that tell us where the left / right banks start 
#   # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
#   anchors <- find_anchor_pts(
#     depths            = depths,
#     num_of_pts        = num_of_pts,
#     cs_length         = cs_length,
#     relative_distance = relative_distance,
#     point_types       = point_types
#   )
#   
#   left_anchor    <- anchors[1]
#   bottom_anchor  <- anchors[2]
#   right_anchor   <- anchors[3]
#   
#   left_to_middle  <- 1:left_anchor
#   middle_to_right <- right_anchor:length(depths)
#   
#   left_side    <- depths[left_to_middle]
#   right_side   <- depths[middle_to_right]
#   
#   # middle_third <- (left_anchor + 1):(right_anchor - 1)
#   middle_third <- (left_anchor):(right_anchor)
#   
#   point_types[middle_third]  <- ifelse(
#     point_types[middle_third] != "bottom", 
#     "channel", 
#     point_types[middle_third]
#   )
#   
#   point_types[1:(left_anchor-1)]                      <- "left_bank"
#   point_types[(right_anchor+1):length(point_types)]   <- "right_bank"
#   # point_types[1:left_anchor]                      <- "left_bank"
#   # point_types[right_anchor:length(point_types)]   <- "right_bank"
#   
#   return(point_types)
#   
# }
# 
# 
# get_cs_point_types <- function(
#     depths,
#     num_of_pts,
#     cs_length,
#     relative_distance,
#     point_types
# ) {
#   
#   # ----------------------------------------------------------------------
#   # num_of_pts          <- cs_pts$points_per_cs[1]
#   # pt_ids              <- cs_pts$pt_id
#   # relative_distance   <- cs_pts$relative_distance
#   # depths              <- cs_pts$Z
#   # cs_length           <- cs_pts$cs_lengthm[1]
#   # 
#   # point_types = classify_banks_and_bottoms(
#   #   num_of_pts = num_of_pts,
#   #   pt_ids     = pt_ids,
#   #   depths     = depths
#   # )
#   # 
#   # # smooth_Z = smooth_depths(Z, window = 3),
#   # # Z2      = use_smoothed_depths(Z, smooth_Z, point_type)
#   # # Z_start = Z,
#   # 
#   # depths           = use_smoothed_depths(start_depths = depths, 
#   #                                   smoothed_depths = smooth_depths(depths, window = 3), 
#   #                                   point_types = point_types
#   # )
#   # 
#   # # plot(Z)
#   
#   # ----------------------------------------------------------------------
#   # ----------------------------------------------------------------------
#   
#   # get the number of points in each third of the cross section  
#   third          <- ceiling(num_of_pts / 3)
#   
#   # Find the anchor points that tell us where the left / right banks start 
#   # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
#   anchors <- find_anchor_pts(
#     depths            = depths,
#     num_of_pts        = num_of_pts,
#     cs_length         = cs_length,
#     relative_distance = relative_distance,
#     point_types       = point_types
#   )
#   
#   left_anchor    <- anchors[1]
#   bottom_anchor  <- anchors[2]
#   right_anchor   <- anchors[3]
#   
#   left_to_middle  <- 1:left_anchor
#   middle_to_right <- right_anchor:length(depths)
#   
#   # plot(depths)
#   # plot(depths[left_to_middle])
#   
#   # plot_df <-
#   #   data.frame(Z = depths) %>%
#   #   dplyr::mutate(
#   #     n = 1:dplyr::n(),
#   #     side = dplyr::case_when(
#   #       n %in% left_to_middle ~ "left_side",
#   #       n %in% middle_to_right ~ "right_side",
#   #       TRUE ~ "bottom"
#   #     ))
#   # plot_df %>%
#   #   ggplot2::ggplot() +
#   #     ggplot2::geom_point(
#   #       ggplot2::aes(
#   #         x = n,
#   #         y = Z,
#   #         color = side), size = 4)
#   
#   left_side    <- depths[left_to_middle]
#   right_side   <- depths[middle_to_right]
#   
#   # middle_third <- (left_anchor + 1):(right_anchor - 1)
#   # 
#   # point_types[middle_third]  <- ifelse(
#   #                                   point_types[middle_third] != "bottom", 
#   #                                   "channel", 
#   #                                   point_types[middle_third]
#   #                                   )
#   # 
#   # point_types[1:left_anchor]                      <- "left_bank"
#   # point_types[right_anchor:length(point_types)]   <- "right_bank"
#   
#   
#   # middle_to_right
#   # TODO: this could throw an error
#   # max(depths[0])
#   
#   # get the maximum depth (highest elevation) point on the LEFT side and all points equal to that max elevation
#   left_max       <- max(left_side)
#   left_max_pts   <- which(left_side == left_max)
#   
#   # rev_max_left_idx <- which.max(rev(left_side))
#   # (length(left_side) - rev_max_left_idx) + 1
#   # which.max(left_side)
#   # which.max(rev(left_side))
#   
#   # get the maximum depth (highest elevation) point on the RIGHT side and all points equal to that max elevation
#   right_max       <- max(right_side)
#   right_max_pts   <- (which(right_side == right_max) + right_anchor) - 1
#   # right_max_pts   <- which(right_side == right_max)
#   
#   # (right_max_pts + right_anchor) - 1
#   # (num_of_pts - right_max_pts ) + 1
#   
#   start_of_left_bank  <- left_max_pts[length(left_max_pts)]
#   start_of_right_bank <- right_max_pts[1]
#   # start_of_right_bank <- right_max_pts[length(right_max_pts)]
#   
#   # assign points to the LEFT of the max LEFT SIDE elevation to "left_bank", then
#   # set the remaining points between that high elevation and the "bottom" to "channel" points
#   point_types[1:(start_of_left_bank-1)]                    <- "left_bank"
#   
#   # if (start_of_left_bank == left_anchor) {
#   #   point_types[(start_of_left_bank + 1)]              <- "channel"
#   # } else {
#   #   point_types[(start_of_left_bank + 1):left_anchor]  <- "channel"
#   # }
#   #
#   # if (start_of_left_bank != left_anchor) {
#   #   point_types[(start_of_left_bank + 1):left_anchor]  <- "channel"
#   # } else {
#   #   point_types[(start_of_left_bank + 1)]              <- "channel"
#   # }
#   
#   # assign points to the RIGHT of the max RIGHT SIDE elevation to "right_bank", then
#   # set the remaining points between the right side of the "bottom" and the right side high elevation point to "channel" points
#   point_types[(start_of_right_bank + 1):length(point_types)]   <- "right_bank"
#   # point_types[right_anchor:(start_of_right_bank - 1)]  <- "channel"
#   
#   # left_of_right_side <- (start_of_right_bank - 1)
#   #
#   # if (start_of_right_bank == right_anchor) {
#   #   point_types[left_of_right_side]               <- "channel"
#   # } else {
#   #   point_types[right_anchor:(start_of_right_bank - 1)]  <- "channel"
#   # }
#   
#   # point_types != "bottom"
#   
#   # middle_section <- point_types[(left_anchor + 1):(right_anchor - 1)]
#   # point_types[(left_anchor + 1):(right_anchor - 1)] != "bottom"
#   
#   middle_third <- (left_anchor):(right_anchor)
#   # middle_third <- (left_anchor + 1):(right_anchor - 1)
#   
#   point_types[middle_third]  <- ifelse(
#     point_types[middle_third] != "bottom",
#     "channel",
#     point_types[middle_third]
#   )
#   
#   # point_types[left_anchor]
#   
#   # # TODO: this sets any remaining bank points to "channel" points as needed
#   point_types[which(point_types == "bank")]              <- "channel"
#   
#   # right_anchor:(start_of_right_bank - 1)
#   return(point_types)
#   
# }

# classify_cs_pts <- function(
#     depths,
#     pt_ids,
#     num_of_pts,
#     relative_distances,
#     cs_length
# ) {
#   
#   point_types <- classify_banks_and_bottoms(
#     num_of_pts = num_of_pts,
#     pt_ids     = pt_ids,
#     depths     = depths
#   )
#   # point_types
#   # depths %>% 
#   #   smooth_depths(depths, window = 3) %>% 
#   #   use_smoothed_depths(depths, ., point_types)
#   # smooth_Z <- use_smoothed_depths(
#   #                 depths, 
#   #                 smooth_depths(depths, window = 3), 
#   #                 point_types
#   #                 # )
#   # smoothed_depths <- smooth_depths(
#   #   depths = depths, 
#   #   window = 3
#   #   )
#   
#   # TODO: Good simple plot showing smoothing changes from DEM ----> smoothed DEM pts
#   # plot(depths)
#   # plot(smooth_depths(depths, window = 3))
#   # plot(use_smoothed_depths(
#   #       depths, 
#   #       smooth_depths(depths, window = 3),  # get a smoothed version of the DEM points
#   #       point_types
#   #     ))
#   
#   # # get a smoothed version of the DEM points
#   # depths <- use_smoothed_depths(depths, smoothed_depths, point_types)
#   
#   depths <- use_smoothed_depths(
#     depths, 
#     smooth_depths(depths, window = 3),  # get a smoothed version of the DEM points
#     point_types
#   )
#   
#   # plot(smooth_depth)
#   # banks_and_anchor <- find_banks_and_anchor_pts(depths = depths,num_of_pts  = num_of_pts,
#   # cs_length = cs_length, relative_distance = relative_distances,
#   # point_types = point_types)
#   
#   
#   # Find the anchor points that tell us where the left / right banks start 
#   # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
#   anchors <- find_anchor_pts(
#     depths            = depths,
#     num_of_pts        = num_of_pts,
#     cs_length         = cs_length,
#     relative_distance = relative_distances,
#     point_types       = point_types
#   )
#   
#   left_anchor    <- anchors[1]
#   bottom_anchor  <- anchors[2]
#   right_anchor   <- anchors[3]
#   
#   third          <- ceiling(num_of_pts / 3)
#   
#   # left_bank    <- banks_and_anchor[1]
#   # anchor       <- banks_and_anchor[2]
#   # right_bank   <- banks_and_anchor[3]
#   
#   left_to_middle  <- 1:left_anchor
#   middle_to_right <- right_anchor:length(depths)
#   
#   # plot(depths)
#   # plot(depths[left_to_middle])
#   
#   # plot_df <- 
#   #   data.frame(
#   #   Z = depths
#   # ) %>% 
#   #   dplyr::mutate(
#   #     n = 1:dplyr::n(),
#   #     side = dplyr::case_when(
#   #       n %in% left_to_middle ~ "left_side",
#   #       n %in% middle_to_right ~ "right_side",
#   #       TRUE ~ "bottom"
#   #     )
#   #   )
#   # 
#   # plot_df %>% 
#   #   ggplot2::ggplot() +
#   #     ggplot2::geom_point(
#   #       ggplot2::aes(
#   #         x = n,
#   #         y = Z,
#   #         color = side
#   #       ),
#   #       size = 4
#   #     )
#   # 
#   
#   left_side    <- depths[left_to_middle]
#   right_side   <- depths[middle_to_right]
#   
#   # middle_to_right
#   # TODO: this could throw an error 
#   # max(depths[0])
#   
#   # get the maximum depth (highest elevation) point on the LEFT side and all points equal to that max elevation
#   left_max       <- max(left_side)
#   left_max_pts   <- which(left_side == left_max)
#   
#   # rev_max_left_idx <- which.max(rev(left_side))
#   # (length(left_side) - rev_max_left_idx) + 1
#   # which.max(left_side)
#   # which.max(rev(left_side))
#   
#   # get the maximum depth (highest elevation) point on the RIGHT side and all points equal to that max elevation
#   right_max       <- max(right_side)
#   right_max_pts   <- (which(right_side == right_max) + right_anchor) - 1
#   # right_max_pts   <- which(right_side == right_max)
#   # (right_max_pts + right_anchor) - 1
#   # (num_of_pts - right_max_pts ) + 1
#   
#   start_of_left_bank  <- left_max_pts[length(left_max_pts)]
#   start_of_right_bank <- right_max_pts[1]
#   # start_of_right_bank <- right_max_pts[length(right_max_pts)]
#   
#   # assign points to the LEFT of the max LEFT SIDE elevation to "left_bank", then
#   # set the remaining points between that high elevation and the "bottom" to "channel" points
#   point_types[1:start_of_left_bank]                    <- "left_bank"
#   point_types[(start_of_left_bank + 1):left_anchor]    <- "channel"
#   
#   # assign points to the RIGHT of the max RIGHT SIDE elevation to "right_bank", then
#   # set the remaining points between the right side of the "bottom" and the right side high elevation point to "channel" points
#   point_types[start_of_right_bank:length(point_types)] <- "right_bank"
#   point_types[right_anchor:(start_of_right_bank - 1)]  <- "channel"
#   
#   # right_anchor:(start_of_right_bank - 1)
#   return(point_types)
#   
# }

# classify_banks_and_bottoms <- function(
#     num_of_pts, 
#     pt_ids, 
#     depths
# ) {
#   
#   # calc the 
#   # - number of points in a third of the cross section (third)
#   third              <- ceiling(num_of_pts / 3)
#   
#   mid_third_left_idx   <- third
#   mid_third_right_idx  <- ((2 * third) - 1)
#   
#   mid_third_idxs    <- mid_third_left_idx:mid_third_right_idx
#   mid_third_low_pt  <- min(depths[mid_third_idxs])
#   
#   # logic for determining if its a bottom point (at lowest depth AND in middle third)
#   is_at_bottom_Z      <- depths <= mid_third_low_pt
#   is_at_middle_third  <- dplyr::between(pt_ids, mid_third_left_idx, mid_third_right_idx)
#   
#   point_type <- ifelse(is_at_bottom_Z & is_at_middle_third, "bottom", "bank")
#   
#   return(point_type)
# }

# # Use the bottom points from the original depth values otherwise use the (rolling mean) smoothed depth values
# use_smoothed_depths <- function(
#     start_depths, 
#     smoothed_depths, 
#     point_types
# ) {
#   return(
#     ifelse(point_types == "bottom", start_depths, smoothed_depths)
#   )
# }

# find_in_channel_pts <- function(
#     relative_distances, 
#     cs_length
# ) {
#   
#   # calc the:
#   # - mean distance between points (mean_dist)
#   # - how much in channel points we have (in_channel_pts)
#   
#   dist_between_pts   <- mean(diff(relative_distances))
#   in_channel_pts     <- ceiling(cs_length / dist_between_pts)
#   
#   right_anchor <- ceiling(in_channel_pts / 2)
#   left_anchor  <- in_channel_pts - right_anchor
#   
#   return(c(left_anchor, right_anchor))
# }

# # given depths and relative distances and points classified into banks vs bottoms
# # determine the indices for 
# # - start of the left side bank/channel 
# # - start of theright/side bank channel
# # - the index of the point at the middle of the bottom 
# # Returns a numeric vector of length 3 with the index of the left anchor, middle point, and right anchor (i.e. c(2, 5, 9) -> c(left_anchor, middle_bottom, right_anchor))
# find_anchor_pts <- function(depths, 
#                             num_of_pts,
#                             cs_length, 
#                             relative_distance, 
#                             point_types
# ) {
#   
#   
#   # ------------------------------
#   # depths            = depths
#   # num_of_pts        = num_of_pts
#   # relative_distance = relative_distances
#   # cs_length         = cs_length
#   # point_types       = point_types
#   
#   # ------------------------------
#   third          = ceiling(num_of_pts / 3)
#   
#   dist_between_pts   <- mean(diff(relative_distance))
#   in_channel_pts     <- ceiling(cs_length / dist_between_pts)
#   
#   b1  <- ceiling(in_channel_pts / 2) # b1
#   b2  <- in_channel_pts - b1 # b2
#   
#   # bank_idxs      <- find_in_channel_pts(relative_distances, cs_length)
#   # 
#   # left_bank      <- bank_idxs[1] 
#   # right_bank     <- bank_idxs[2]
#   
#   bottom_idxs <- which(point_types == "bottom")
#   # point_type_is_bottom = which(point_types == "bottom")
#   
#   min_bottom  <- bottom_idxs[1]
#   mid_bottom  <- bottom_idxs[ceiling(length(bottom_idxs) / 2)]
#   max_bottom  <- bottom_idxs[length(bottom_idxs)]
#   # min_bottom     = which(point_types == "bottom")[1]
#   # mid_bottom     = which(point_types == "bottom")[ceiling(length(which(point_types == "bottom"))/2)]
#   # max_bottom     = which(point_types == "bottom")[length(which(point_types == "bottom"))]
#   
#   L1             = pmax(1, mid_bottom - b1)
#   L2             = pmax(1, mid_bottom - b2)
#   
#   R1             = pmin(mid_bottom + b2, num_of_pts)
#   R2             = pmin(mid_bottom + b1, num_of_pts)
#   
#   anchor         = ifelse(depths[R2] < depths[L1], 2, 1)
#   
#   LEFT           = pmax(third, ifelse(anchor == 1, L1, L2))
#   RIGHT          = pmin(2*third, ifelse(anchor == 1, R1, R2))
#   
#   count_left     = min_bottom - LEFT
#   count_right    = RIGHT - max_bottom
#   
#   LEFT           = ifelse(count_left == 0, LEFT - count_right, LEFT)
#   RIGHT          = ifelse(count_right == 0, RIGHT + count_left, RIGHT)
#   
#   return(c(LEFT, mid_bottom, RIGHT))
#   
# }

# smooth_depths <- function(
#     depths,
#     num_of_pts = NULL,
#     window     = 3
# ) {
#   
#   # depths <- cs_pts$Z
#   # num_of_pts          <- 10
#   # window = 3
#   
#   if(is.null(num_of_pts)) {
#     num_of_pts <- length(depths)    
#   } 
#   
#   # calculate a rolling mean of the depths between the starting and ending depth
#   smoothed_depths <- c(
#     depths[1],
#     zoo::rollmean(depths, window),
#     depths[num_of_pts]
#   )
#   
#   return(smoothed_depths)
# }
# 
# classify_z_pts <- function(
#     depths, 
#     num_of_pts,
#     cs_length,
#     relative_distance,
#     pt_ids, 
#     point_types
#     # L,
#     # R
# ) {
#   
#   # Find the anchor points that tell us where the left / right banks start 
#   # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
#   anchors <- find_anchor_pts(
#     depths            = depths,
#     num_of_pts        = num_of_pts,
#     cs_length         = cs_length,
#     relative_distance = relative_distance,
#     point_types       = point_types
#   )
#   
#   # left_anchor    <- anchors[1]
#   # bottom_anchor  <- anchors[2]
#   # right_anchor   <- anchors[3]
#   
#   L    <- anchors[1]
#   # bottom_anchor  <- anchors[2]
#   R   <- anchors[3]
#   
#   # depths      = depths
#   # pt_ids      = pt_ids
#   # point_types = point_types
#   # L           = left_bank
#   # R           = right_bank
#   
#   slope          <- diff(c(0, depths))  
#   bottom_depth   <- min(depths)      
#   points_per_id  <- length(depths)
#   
#   # bank_or_bottom <- point_types
#   
#   classification <- sapply(1:points_per_id, function(i) {
#     message(i)
#     if (i == 1) {
#       
#       message("LB\n")
#       return("left_bank")   # handle first point
#       
#     } else if (i == points_per_id) {
#       
#       message("RB\n")
#       return("right_bank")  # and the last point
#     } else if (depths[i] == bottom_depth & point_types[i] == "bottom") {
#       message("BOTTOM\n")
#       return("bottom")      # identify the bottom point(s) (min depths value)
#     }
#     # RIGHT SIDE OF BOTTOM
#     else if (pt_ids[i] >= R) {
#       
#       if (slope[i + 1] <= 0) {
#         
#         message("Right bank\n")
#         return("right_bank")     #  slope changing from rising to falling (approaching bottom)
#         
#       } else {
#         
#         message("Right channel\n")
#         return("right_channel")  # rising slope but not near the bottom
#         
#       }
#       
#     }
#     # LEFT SIDE OF THE BOTTOM
#     else if (pt_ids[i] <= L) {
#       
#       # second derivate is 
#       if (slope[i - 1] <= 0) {
#         
#         message("Left channel\n")
#         return("left_channel")   # slope changing from falling to rising (leaving bottom)
#         
#       } else {
#         
#         message("Left bank\n")
#         return("left_bank")  #  slope  falling but not near the bottom
#       }
#     } else{
#       message("NOT ON LEFT OR RIGHT SIDE (i.e. bottom...?)")
#       return("bottom")
#     } 
#     message("NO MATCH\n")
#     
#   })
#   classification[classification %in% c("left_channel", "right_channel")] <- "channel"
#   
#   return(classification)
# }


#############################################################################################################
# classify_banks_and_bottoms()
# use_smoothed_depths()
# smooth_depths()
# find_anchor_pts()
# classify_derivatives()
# clean_point_types()
# set_bank_anchors()
#############################################################################################################

# classify_banks_and_bottoms <- function(
#     num_of_pts, 
#     pt_ids, 
#     depths
# ) {
#   
#   # calc the 
#   # - number of points in a third of the cross section (third)
#   third              <- ceiling(num_of_pts / 3)
#   
#   mid_third_left_idx   <- third
#   mid_third_right_idx  <- ((2 * third) - 1)
#   
#   mid_third_idxs    <- mid_third_left_idx:mid_third_right_idx
#   mid_third_low_pt  <- min(depths[mid_third_idxs])
#   
#   # logic for determining if its a bottom point (at lowest depth AND in middle third)
#   is_at_bottom_Z      <- depths <= mid_third_low_pt
#   is_at_middle_third  <- dplyr::between(pt_ids, mid_third_left_idx, mid_third_right_idx)
#   
#   point_type <- ifelse(is_at_bottom_Z & is_at_middle_third, "bottom", "bank")
#   
#   return(point_type)
# }
# 
# smooth_depths <- function(
#     depths,
#     num_of_pts = NULL,
#     window     = 3
# ) {
#   
#   # depths <- cs_pts$Z
#   # num_of_pts          <- 10
#   # window = 3
#   
#   if(is.null(num_of_pts)) {
#     num_of_pts <- length(depths)    
#   } 
#   
#   # calculate a rolling mean of the depths between the starting and ending depth
#   smoothed_depths <- c(
#     depths[1],
#     zoo::rollmean(depths, window),
#     depths[num_of_pts]
#   )
#   
#   return(smoothed_depths)
# }
# 
# # Use the bottom points from the original depth values otherwise use the (rolling mean) smoothed depth values
# use_smoothed_depths <- function(
#     start_depths, 
#     smoothed_depths, 
#     point_types
# ) {
#   return(
#     ifelse(point_types == "bottom", start_depths, smoothed_depths)
#   )
# }
# 
# # given depths and relative distances and points classified into banks vs bottoms
# # determine the indices for 
# # - start of the left side bank/channel 
# # - start of theright/side bank channel
# # - the index of the point at the middle of the bottom 
# # Returns a numeric vector of length 3 with the index of the left anchor, middle point, and right anchor (i.e. c(2, 5, 9) -> c(left_anchor, middle_bottom, right_anchor))
# find_anchor_pts <- function(depths, 
#                             num_of_pts,
#                             cs_length, 
#                             relative_distance, 
#                             point_types
# ) {
#   
#   
#   # ------------------------------
#   # depths            = depths
#   # num_of_pts        = num_of_pts
#   # relative_distance = relative_distances
#   # cs_length         = cs_length
#   # point_types       = point_types
#   
#   # ------------------------------
#   third          = ceiling(num_of_pts / 3)
#   
#   dist_between_pts   <- mean(diff(relative_distance))
#   in_channel_pts     <- ceiling(cs_length / dist_between_pts)
#   
#   b1  <- ceiling(in_channel_pts / 2) # b1
#   b2  <- in_channel_pts - b1 # b2
#   
#   # bank_idxs      <- find_in_channel_pts(relative_distances, cs_length)
#   # 
#   # left_bank      <- bank_idxs[1] 
#   # right_bank     <- bank_idxs[2]
#   
#   bottom_idxs <- which(point_types == "bottom")
#   # point_type_is_bottom = which(point_types == "bottom")
#   
#   min_bottom  <- bottom_idxs[1]
#   mid_bottom  <- bottom_idxs[ceiling(length(bottom_idxs) / 2)]
#   max_bottom  <- bottom_idxs[length(bottom_idxs)]
#   # min_bottom     = which(point_types == "bottom")[1]
#   # mid_bottom     = which(point_types == "bottom")[ceiling(length(which(point_types == "bottom"))/2)]
#   # max_bottom     = which(point_types == "bottom")[length(which(point_types == "bottom"))]
#   
#   L1             = pmax(1, mid_bottom - b1)
#   L2             = pmax(1, mid_bottom - b2)
#   
#   R1             = pmin(mid_bottom + b2, num_of_pts)
#   R2             = pmin(mid_bottom + b1, num_of_pts)
#   
#   anchor         = ifelse(depths[R2] < depths[L1], 2, 1)
#   
#   LEFT           = pmax(third, ifelse(anchor == 1, L1, L2))
#   RIGHT          = pmin(2 * third, ifelse(anchor == 1, R1, R2))
#   
#   count_left     = min_bottom - LEFT
#   count_right    = RIGHT - max_bottom
#   
#   LEFT           = ifelse(count_left == 0, LEFT - count_right, LEFT)
#   RIGHT          = ifelse(count_right == 0, RIGHT + count_left, RIGHT)
#   
#   return(c(LEFT, mid_bottom, RIGHT))
#   
# }
# 
# # Given depths of a cross section, calculate the first and second derivative of the points 
# # and then classify the points based on 1st / 2nd derivative values at each point 
# # labelling points as:
# # - flat
# # - concave_up_increasing
# # - concave_up_decreasing
# # - concave_down_increasing
# # - concave_down_decreasing
# # - "linear"
# # 
# # -->>> 
# #     with the intention that points with concavity / linear should be "channel" points
# classify_derivatives <- function(depths) {
#   
#   classify_derivs <- function(slope, second_deriv) {
#     if (slope == 0) {
#       return("flat")
#     } else if (second_deriv > 0 && slope > 0) {
#       return("concave_up_increasing")
#     } else if (second_deriv > 0 && slope < 0) {
#       return("concave_up_decreasing")
#     } else if (second_deriv < 0 && slope > 0) {
#       return("concave_down_increasing")
#     } else if (second_deriv < 0 && slope < 0) {
#       return("concave_down_decreasing")
#     } else {
#       return("linear")  
#     }
#   }
#   
#   # padding the starting depth values w/ a duplicate first and last values
#   depths_padded <- c(depths[1], depths, depths[length(depths)])
#   
#   # calculate first deriv (slope) on padded depth values
#   slopes <- diff(depths_padded)
#   
#   # calculate second deriv (differences of the first differences)
#   second_derivative <- diff(slopes)
#   
#   # classify points based on padded 1st and 2nd derivatives 
#   classifications <- mapply(classify_derivs, slopes[1:(length(second_derivative))], second_derivative)
#   
#   return(classifications)
# } 
# 
# # given a vector of point types representing a cross section of depths, set rules on what points can exist in proximity to other points 
# # - set any banks surrounded by channel and bottoms to "channel" 
# # - set any bank points surrounded by "channel" points to "channel"
# # - set any banks surrounded by bottom points to "bottom"
# clean_point_types <- function(point_types) {
#   
#   # point_types <-
#   #   smoothed %>%
#   # #   dplyr::filter(hy_id == "wb-1003265", cs_id == 2) %>%
#   # point_types <- c(point_types[1:6], c("bottom", "bottom", "channel", "channel"), point_types[7:length(point_types)] )
#   # point_types2 <- point_types
#   
#   # ----------------------------------------------------------------------------------------------------------------------------------------
#   # ---- Go through points and make sure point types follow certain rules regarding the neighboring group of point types:
#   #
#   # 1. A point is a BANK AND its between a "channel" and "bottom" point 
#   #    ----> set point_type to "channel"
#   #
#   # 2. A point is a BANK AND its between 2 "bottom" points 
#   #    ----> set point_type to "bottom"
#   # ----------------------------------------------------------------------------------------------------------------------------------------
#   
#   L  <- 1
#   M1 <- 1
#   M2 <- 1
#   R  <- 1
#   
#   RIGHT_BOUND <- length(point_types)
#   
#   # i = 1
#   
#   while (L <= R & 
#          L < RIGHT_BOUND & 
#          R < RIGHT_BOUND & 
#          M1 < RIGHT_BOUND & 
#          M2 < RIGHT_BOUND
#   ) {
#     
#     # message("-----------------------------","ITERATION ", i, "-----------------------------")
#     
#     # message("> L: ", L)
#     # message("> M1: ", M1)
#     # message("> M2: ", M2)
#     # message("> R: ", R)
#     
#     # get the point type of the start of the left group
#     left_type = point_types[L]
#     # left_group = point_types[L]            
#     
#     # message("Moving LEFT pointer")
#     while(
#       # point_types[L] == left_type
#       point_types[L] == left_type & !is.na(point_types[L])
#     ) {
#       L = L + 1
#       # message(" > ", L-1, " to ", L)
#       # message(" -> ", point_types[L - 1], " --> ", point_types[L])
#     }
#     
#     # get the rightmost left side group 
#     L = L - 1
#     
#     left_group = point_types[L]
#     
#     # message("Left group: ", left_group)
#     
#     # make a M1 and M2 points which will be the start and end indices of the middle group
#     M1 <- L + 1
#     M2 <- M1
#     
#     # current middle group 
#     mid_type <- point_types[M1]
#     # mid_group = point_types[M]  
#     
#     # message("Moving MIDDLE pointer...")
#     # move middle right pointer until another group is found
#     while(
#       # point_types[M2] == mid_type
#       point_types[M2] == mid_type & !is.na(point_types[M2])
#     ) {
#       M2 <- M2 + 1
#       # message(" > ", M2 - 1, " to ", M2)
#       # message(" -> ", point_types[M2 - 1], " --> ", point_types[M2])
#     }
#     
#     # get the group to left of the right hand group
#     M2 <- M2 - 1
#     
#     mid_group <- point_types[M2]
#     
#     # message("Moving RIGHT pointer...")
#     # move the right pointer to the right of the middle group
#     R <- M2 + 1
#     
#     right_group <- point_types[R]
#     
#     at_final_groups <- is.na(left_group) | is.na(mid_group) | is.na(right_group)
#     # at_last_group <- is.na(right_group)
#     
#     if (at_final_groups) {
#       message("Reached last groups, stopping early!")
#       break
#     }
#     
#     # ---------------------------------------------------------   
#     # ----- conditions for changing point types -----
#     # ---------------------------------------------------------   
#     # - point is a BANK AND its between a "channel" and "bottom" point 
#     #    ----> set point_type to "channel"
#     is_bank_mid_group                   <-  mid_group %in% c("left_bank", "right_bank")
#     
#     is_bank_between_bottom_and_channel  <-  is_bank_mid_group & 
#       left_group == "bottom" & 
#       right_group == "channel"
#     
#     is_bank_between_channel_and_bottom  <-  is_bank_mid_group & 
#       left_group == "channel" & 
#       right_group == "bottom"
#     
#     if(is_bank_between_channel_and_bottom | is_bank_between_bottom_and_channel) {
#       message("\n~~~~~~~~~~~~~","\n-----> FOUND 'bank' point between channel and bottom point: \n (", left_group, " > ", mid_group, " < ", right_group, "\n~~~~~~~~~~~~~")
#       point_types[M1:M2] <- "channel"
#     } 
#     
#     # - point is a BANK AND its between 2 "channel" points 
#     #    ----> set point_type to "channel"
#     is_bank_between_channel_and_channel   <- is_bank_mid_group & 
#       left_group == "channel" & 
#       right_group == "channel" 
#     
#     if(is_bank_between_channel_and_channel) {
#       message("\n~~~~~~~~~~~", "\n-----> FOUND 'bank' point between 2 channel points: \n (", left_group, " > ", mid_group, " < ", right_group, "\n~~~~~~~~~~~")
#       point_types[M1:M2] <- "channel"
#     }
#     
#     # - point is a BANK AND its between 2 "bottom" points 
#     #    ----> set point_type to "bottom"
#     is_bank_between_bottom_and_bottom   <- is_bank_mid_group & 
#       left_group == "bottom" & 
#       right_group == "bottom" 
#     
#     if(is_bank_between_bottom_and_bottom) {
#       message("\n~~~~~~~~~~~", "\n-----> FOUND 'bank' point between 2 bottom points: \n (", left_group, " > ", mid_group, " < ", right_group, "\n~~~~~~~~~~~")
#       point_types[M1:M2] <- "bottom"
#     } 
#     
#     
#     
#     message("=======================================", 
#             "\nFound groups: \n", "\n- Left: (",  L, ") ", left_group, "\n- Mid: (", M1, " - ",  M2, ") ", mid_group, "\n- Right: (", R, ") ",  right_group,
#             "\n=======================================")
#     
#     # move the Left pointer to the start of the next group 
#     L <- L + 1
#     
#   }
#   
#   # ----------------------------------------------------------------------------------------------------------------------------------------
#   # ---- Go make sure that the "bottom" points have ATLEAST a single "channel" point to the left and right of the "bottom" ----
#   # ----------------------------------------------------------------------------------------------------------------------------------------
#   
#   # set pointers 
#   L  <- 1
#   M1 <- 1
#   M2 <- 1
#   R  <- 1
#   
#   # set a safe default value for the left and right of the bottom assuming 
#   # the edge of the bottom is 1 position left and right of the middle third of the points
#   third   <- length(point_types) %/% 3
#   # third   <- ceiling(length(point_types) / 3)
#   
#   left_of_bottom   <- third - 1
#   right_of_bottom  <- (third * 2) + 1
#   
#   max_bottom_width <- 0
#   
#   while (L <= R & 
#          L < RIGHT_BOUND & 
#          R < RIGHT_BOUND & 
#          M1 < RIGHT_BOUND & 
#          M2 < RIGHT_BOUND
#   ) {
#     
#     # get the point type of the start of the left group
#     left_type = point_types[L]
#     while(
#       point_types[L] == left_type & !is.na(point_types[L])
#     ) {
#       L = L + 1
#     }
#     
#     # get the rightmost left side group 
#     L = L - 1
#     left_group = point_types[L]
#     
#     # make a M1 and M2 points which will be the start and end indices of the middle group
#     M1 <- L + 1
#     M2 <- M1
#     
#     # current middle group 
#     mid_type <- point_types[M1]
#     
#     # move middle right pointer until another group is found
#     while(
#       point_types[M2] == mid_type & !is.na(point_types[M2])
#     ) {
#       M2 <- M2 + 1
#     }
#     
#     # get the group to left of the right hand group
#     M2 <- M2 - 1
#     
#     mid_group <- point_types[M2]
#     
#     # move the right pointer to the right of the middle group
#     R <- M2 + 1
#     
#     right_group <- point_types[R]
#     
#     at_final_groups <- is.na(left_group) | is.na(mid_group) | is.na(right_group)
#     
#     if (at_final_groups) {
#       message("Reached last groups, stopping early!")
#       break
#     }
#     
#     # ---------------------------------------------------------   
#     # ----- make sure points to the left and right of the bottom are set to channel -----
#     # ---------------------------------------------------------   
#     
#     is_bottom    <- mid_group == "bottom" 
#     
#     # width of the current group of bottom points
#     bottom_width <- (M2 - M1) + 1
#     
#     # is the current bottom bigger than any previously seen bottoms?
#     at_current_biggest_bottom <- bottom_width >= max_bottom_width
#     
#     # if we're at a bottom group and its the current biggest bottom, 
#     # store the point to the left and to the right of the current bottom group
#     if(is_bottom & at_current_biggest_bottom) {
#       
#       max_bottom_width <- max(bottom_width, max_bottom_width)
#       
#       left_of_bottom  <- M1 - 1
#       right_of_bottom <- M2 + 1
#       
#     } 
#     # move the Left pointer to the start of the next group 
#     L <- L + 1
#   }
#   
#   # NOTE: make sure that to the left and right of the bottom, are "channel" points
#   point_types[left_of_bottom]  <- "channel"
#   point_types[right_of_bottom] <- "channel"
#   
#   return(point_types)
#   
# }
# 
# # given a vector of depths and vector of corresponding point types representing a cross section of depths, 
# # make sure there is a left_bank at the left most highest position of the left side
# # make sure there is a "right_bank" at the right most highest position of the right side
# set_bank_anchors <- function(
#     depths,
#     point_types,
#     L,
#     R
# ) {
#   
#   CS_START  <- 1
#   CS_END    <- length(depths)
#   
#   left_bank_count   <- sum(point_types[1:L] == "left_bank")
#   right_bank_count  <- sum(point_types[R:CS_END] == "right_bank")
#   
#   has_both_banks    <- left_bank_count > 0 & right_bank_count > 0
#   
#   if (has_both_banks) {
#     # message("Cross section already has 'left_bank' and 'right_bank' points, returning input point_types")
#     return(point_types)
#   }
#   
#   left_max_index <- which.max(depths[1:L])
#   
#   ## TODO: method 1 for getting highest right most point index
#   ## TODO: reverse the right side, get the first max point with which.max() then subtract this offset from total number of points and add 1
#   right_offset     <- which.max(depths[CS_END:R])
#   right_max_index  <- (CS_END - right_offset) + 1
#   
#   ## TODO: method 2 for getting highest right most point index
#   # right_offset     <- max(which(depths[R:CS_END] == max(depths[R:CS_END]))) - 1
#   # right_max_index  <- R + right_offset
#   
#   point_types[left_max_index]  <- "left_bank"
#   point_types[right_max_index] <- "right_bank"
#   
#   return(point_types)
#   
# }
