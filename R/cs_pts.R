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
    "prev_Y_is_missing", "right_start", "right_start_max", "start_or_end", "start_pt_id"
  )
)

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

#' Add a points per cross section column to an sf dataframe of linestrings given a DEM and min points value
#' 
#' This function calculates and adds a column called 'points_per_cs' to an sf dataframe
#' representing cross-sections (linestrings) based on a provided DEM and a minimum points
#' value per cross section.
#'
#' @param cs An sf dataframe representing cross-sections (linestrings). With a required cs_lengthm column (length of cross section in meters)
#' @param points_per_cs numeric, number of points per cross section. Default is NULL
#' @param min_pts_per_cs An optional minimum points value per cross section. If not provided, 
#' @param dem A SpatRaster object representing the Digital Elevation Model (DEM) or a character string referencing a remote resource.
#' the function calculates it based on the length of cross-sections and the resolution of the DEM.
#' @importFrom terra linearUnits rast res
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


#' Classify Cross Section Points 
#' @param cs_pts CS points, output of hydrofabric3D::cross_section_pts()
#' @param pct_of_length_for_relief numeric, percent of cross section length (cs_lengthm) to use as the 
#' threshold depth for classifying whether a cross section has "relief". If a cross section has at least X% of its length in depth, 
#' then it is classified as "having relief" (i.e. has_relief = TRUE). Value must be non negative number (greater than or equal to 0). 
#' Default is 0.01 (1% of the cross sections length).
#' @return sf object
#' @importFrom dplyr filter group_by mutate ungroup select between n left_join
#' @importFrom zoo rollmean
#' @export
classify_points <- function(
    cs_pts, 
    pct_of_length_for_relief = 0.01
){
  
  . <-  L <-  L1 <-  L2  <-  R  <-  R1 <-  R2  <- Z  <-  Z2 <-  anchor <-  b1  <- b2  <- cs_lengthm  <- count_left <- 
    count_right  <-  cs_id <-  hy_id <-  in_channel_pts  <- lengthm <-  low_pt  <- max_bottom  <- mean_dist <-  mid_bottom  <- min_bottom  <- pt_id <- relative_distance <-  third <- NULL
  
  # type checking
  if (!is.numeric(pct_of_length_for_relief)) {
    stop("Invalid argument type, 'pct_of_length_for_relief' must be of type 'numeric', given type was '",   
         class(pct_of_length_for_relief), "'")
  }
  
  # Make sure pct_of_length_for_relief is valid percentage value (greater than 0)
  if (pct_of_length_for_relief < 0 ) {
    stop("Invalid value 'pct_of_length_for_relief' of ", pct_of_length_for_relief, ", 'pct_of_length_for_relief' must be greater than or equal to 0")
  } 
  
  # # remove any columns that already exist
  cs_pts <- dplyr::select(cs_pts, 
                          !dplyr::any_of(c("class", "point_type", "bottom", "left_bank", "right_bank", "valid_banks", "has_relief"))
                          )
  
  # required cols that will be selected from the classified_pts object and in this order
  req_cols       <- c("hy_id", "cs_id", "pt_id", "Z", "relative_distance", "cs_lengthm", "class", "point_type")
  
  # any starting columns in the original data 
  starting_cols  <- names(cs_pts)
  
  # name and order of columns to select with
  cols_to_select <- c(req_cols, starting_cols[!starting_cols %in% req_cols])
  
  # create classifications for points
  classified_pts <-
    dplyr::filter(cs_pts) %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      third = ceiling(n() / 3),
      mean_dist = mean(diff(relative_distance)),
      in_channel_pts = ceiling(cs_lengthm[1] / mean_dist),
      b1 = ceiling(in_channel_pts / 2),
      b2 = in_channel_pts - b1,
      low_pt  = min(Z[third[1]:(2*third[1] - 1)]),
      class = ifelse(Z <= low_pt & dplyr::between(pt_id, third[1], (2*third[1] - 1)), 
                     "bottom", 
                     "bank"),
      Z2 = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()]),
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
      class = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
      class = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
      class = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(point_type = class) %>% 
    dplyr::select(dplyr::any_of(cols_to_select))
  # dplyr::select(dplyr::all_of(cols_to_select))      # Stricter, requires ALL of the columns to be present or it will throw an error
  # dplyr::select(hy_id, cs_id, pt_id, Z, 
  # relative_distance, cs_lengthm, class, point_type) # Old strict ordering, removed this to keep other columns in the input data and not lose any data for the user.
  # classified_pts[cols_to_select]                       # Another method for selecting columns....
  
  # get bank validity attributes for each hy_id/cs_id
  # - Uses the count of point types per cross section and checks Z to make sure that a "bottom" point is
  #   in each cross section and each "bottom" point has a valid left and right bank)
  bank_validity_df <- get_bank_attributes(classified_pts)
  
  # # Or add bank attributes 
  # banked_pts <- add_bank_attributes(output_pts)
  
  # get relief data, determine if a cross section has relief within X% percentage of the cross sections length
  relief_df <- get_relief(
    classified_pts, 
    pct_of_length_for_relief = pct_of_length_for_relief, 
    detailed                 = FALSE
  )
  
  # join the bank validity attributes with the relief values
  validity_checks <- dplyr::left_join(
    bank_validity_df, 
    relief_df, 
    by = c("hy_id", "cs_id")
  )
  
  # join the new validity check values to the classified points
  classified_pts <- 
    classified_pts %>% 
    dplyr::left_join(
      validity_checks,
      by = c("hy_id", "cs_id")
    ) 
  
  # move the geometry column to the last column (if one exists)
  classified_pts <- move_geometry_to_last(classified_pts)
  
  return(classified_pts)
  
}

# classify_points3 <- function(
#     cs_pts, 
#     pct_of_length_for_relief = 0.01
# ){
#   
#   . <-  L <-  L1 <-  L2  <-  R  <-  R1 <-  R2  <- Z  <-  Z2 <-  anchor <-  b1  <- b2  <- cs_lengthm  <- count_left <- 
#     count_right  <-  cs_id <-  hy_id <-  in_channel_pts  <- lengthm <-  low_pt  <- max_bottom  <- mean_dist <-  mid_bottom  <- min_bottom  <- pt_id <- relative_distance <-  third <- NULL
#   
#   # cs_pts = cs_pts_adjusted
#   # pct_of_length_for_relief = 0.01
#   test_cs <- 
#     cs_pts_adjusted %>% 
#     dplyr::filter(is_adjusted) %>% 
#     hydrofabric3D::add_tmp_id() %>% 
#     dplyr::filter(tmp_id == test_id) 
#     # dplyr::select(-bottom, -left_bank, -right_bank, -has_relief, -valid_banks, -point_type, -class) %>%
#     # hydrofabric3D::classify_points() %>%
#     # classify_points3() %>%
#     # dplyr::mutate(
#     #   updated = "UPDATED - Reclassified"
#     # )
#   
#   # type checking
#   if (!is.numeric(pct_of_length_for_relief)) {
#     stop("Invalid argument type, 'pct_of_length_for_relief' must be of type 'numeric', given type was '",   
#          class(pct_of_length_for_relief), "'")
#   }
#   
#   # Make sure pct_of_length_for_relief is valid percentage value (greater than 0)
#   if (pct_of_length_for_relief < 0 ) {
#     stop("Invalid value 'pct_of_length_for_relief' of ", pct_of_length_for_relief, ", 'pct_of_length_for_relief' must be greater than or equal to 0")
#   } 
#   
#   # # remove any columns that already exist
#   cs_pts <- dplyr::select(cs_pts,
#                           -class, -point_type, -bottom, -left_bank, -right_bank, -valid_banks, -has_relief)
#   test_cs <- dplyr::select(test_cs,
#                           -class, -point_type, -bottom, -left_bank,
#                           -right_bank, -valid_banks, -has_relief)
#   
#   # required cols that will be selected from the classified_pts object and in this order
#   req_cols       <- c("hy_id", "cs_id", "pt_id", "Z", "relative_distance", "cs_lengthm", "class", "point_type")
#   
#   # any starting columns in the original data 
#   starting_cols  <- names(cs_pts)
#   
#   starting_cols  <- names(test_cs)
#   
#   # name and order of columns to select with
#   cols_to_select <- c(req_cols, starting_cols[!starting_cols %in% req_cols])
#   
#   # create classifications for points
#   classified_pts <-
#     # dplyr::filter(cs_pts) %>% 
#     dplyr::filter(test_cs) %>% 
#     dplyr::group_by(hy_id, cs_id) %>% 
#     dplyr::mutate(
#       third = ceiling(n() / 3),
#       mean_dist = mean(diff(relative_distance)),
#       in_channel_pts = ceiling(cs_lengthm[1] / mean_dist),
#       b1 = ceiling(in_channel_pts / 2),
#       b2 = in_channel_pts - b1,
#       low_pt  = min(Z[third[1]:(2*third[1] - 1)]),
#       class = ifelse(Z <= low_pt & dplyr::between(pt_id, third[1], (2*third[1] - 1)), 
#                      "bottom", 
#                      "bank"),
#       Z2 = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()]),
#       Z = ifelse(class == "bottom", Z, Z2),
#       min_bottom = which(class == "bottom")[1],
#       mid_bottom = which(class == "bottom")[ceiling(length(which(class == "bottom"))/2)],
#       max_bottom = which(class == "bottom")[length(which(class == "bottom"))],
#       L1 = pmax(1, mid_bottom - b1),
#       L2 = pmax(1, mid_bottom - b2),
#       R1 = pmin(mid_bottom + b2, n()),
#       R2 = pmin(mid_bottom + b1, n()),
#       anchor = ifelse(Z[R2] < Z[L1], 2, 1),
#       L = pmax(third, ifelse(anchor == 1, L1, L2)),
#       R = pmin(2*third[1], ifelse(anchor == 1, R1, R2)),
#       count_left = min_bottom - L,
#       count_right = R - max_bottom,
#       L = ifelse(count_left == 0, L - count_right, L),
#       R = ifelse(count_right == 0, R + count_left, R),
#       class = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
#       class = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
#       class = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)) %>%
#     dplyr::ungroup() %>% 
#     dplyr::mutate(point_type = class) %>% 
#     dplyr::select(dplyr::any_of(cols_to_select))
#   # dplyr::select(dplyr::all_of(cols_to_select))      # Stricter, requires ALL of the columns to be present or it will throw an error
#   # dplyr::select(hy_id, cs_id, pt_id, Z, 
#   # relative_distance, cs_lengthm, class, point_type) # Old strict ordering, removed this to keep other columns in the input data and not lose any data for the user.
#   # classified_pts[cols_to_select]                       # Another method for selecting columns....
#   
#   # get bank validity attributes for each hy_id/cs_id
#   # - Uses the count of point types per cross section and checks Z to make sure that a "bottom" point is
#   #   in each cross section and each "bottom" point has a valid left and right bank)
#   bank_validity_df <- get_bank_attributes(classified_pts)
#   
#   # # Or add bank attributes 
#   # banked_pts <- add_bank_attributes(output_pts)
#   
#   # get relief data, determine if a cross section has relief within X% percentage of the cross sections length
#   relief_df <- get_relief(
#     classified_pts, 
#     pct_of_length_for_relief = pct_of_length_for_relief, 
#     detailed                 = FALSE
#   )
#   
#   # join the bank validity attributes with the relief values
#   validity_checks <- dplyr::left_join(
#     bank_validity_df, 
#     relief_df, 
#     by = c("hy_id", "cs_id")
#   )
#   
#   # join the new validity check values to the classified points
#   classified_pts <- 
#     classified_pts %>% 
#     dplyr::left_join(
#       validity_checks,
#       by = c("hy_id", "cs_id")
#     ) 
#   
#   # move the geometry column to the last column (if one exists)
#   classified_pts <- move_geometry_to_last(classified_pts)
#   
#   return(classified_pts)
#   
# }

#' Classify Cross Section Points v1 (Deprecated version)
#' @param cs_pts CS points
#' @return sf object
#' @importFrom dplyr filter group_by mutate ungroup select between n
#' @importFrom zoo rollmean
classify_points2 <- function(cs_pts){
  
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
      class = ifelse(Z <= low_pt & dplyr::between(pt_id, third[1], (2*third[1] - 1)), 
                     "bottom", 
                     "bank"),
      Z2 = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()]),
      Z = ifelse(class == "bottom", Z, Z2),
      min_bottom = which(class == "bottom")[1],
      mid_bottom = which(class == "bottom")[ceiling(length(which(class == "bottom"))/2)],
      max_bottom = which(class == "bottom")[length(which(class == "bottom"))],
      L1 = pmax(1, mid_bottom - b1),
      L2 = pmax(1, mid_bottom - b2),
      R1 = pmin(mid_bottom + b2, dplyr::n()),
      R2 = pmin(mid_bottom + b1, dplyr::n()),
      anchor = ifelse(Z[R2] < Z[L1], 2, 1),
      L = pmax(third, ifelse(anchor == 1, L1, L2)),
      R = pmin(2*third[1], ifelse(anchor == 1, R1, R2)),
      count_left = min_bottom - L,
      count_right = R - max_bottom,
      L = ifelse(count_left == 0, L - count_right, L),
      R = ifelse(count_right == 0, R + count_left, R),
      class = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
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