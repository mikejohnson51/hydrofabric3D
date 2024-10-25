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

#' Get Points across transects with elevation values
#' @param cs character, Hydrographic LINESTRING Network file path
#' @param crosswalk_id character, ID column
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
    crosswalk_id   = NULL, 
    points_per_cs  = NULL,
    min_pts_per_cs = 10,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
){
  
  ### ### ## ## ### ## ### ##
  
  # cs             = transects2
  # crosswalk_id             = NULL
  # points_per_cs  = NULL
  # min_pts_per_cs = 10
  # dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
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
  
  # TODO: also check that the input 'cs' is an sf dataframe with LINESTRINGS / MULT
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # TODO: this might mess things up, assigning a new hydrofabric_id to 
    # TODO: the cross section input and then not giving that back to the user, 
    # TODO: how would they map the output of this function back to their input transects ????
    # cs  <- add_hydrofabric_id(cs) 
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "cs_lengthm")
  
  # validate input graph
  is_valid <- validate_df(cs, REQUIRED_COLS, "cs")

  # add points per cross sections 
  cs <- add_points_per_cs(
    cs             = cs,
    points_per_cs  = points_per_cs,
    min_pts_per_cs = min_pts_per_cs,
    dem            = dem
  )
  
  # Extract DEM "Z" values for each point along cross section linestrings
  cs_pts <- extract_dem_values(
                          cs           = cs, 
                          crosswalk_id = crosswalk_id, 
                          dem          = dem
                          )
  
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
  
  REQUIRED_COLS <- c("cs_lengthm")
  
  # validate input graph
  is_valid <- validate_df(cs, REQUIRED_COLS, "cs")
  
  # if (!all(REQUIRED_COLS %in% names(cs))) {
  #   missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(cs))]
  #   stop("'cs' is missing one or more of the required columns:\n > ", 
  #        paste0(missing_cols, collapse = "\n > "))
  # }
  
  # get the points per cross section based on the prescribed "points_per_cs" or the provided DEM (if points_per_cs is NULL) 
  cs$points_per_cs <- get_points_per_cs(cs_length = cs$cs_lengthm, 
                                        points_per_cs = points_per_cs, 
                                        min_pts_per_cs = min_pts_per_cs, 
                                        dem = dem
                                        )
  
  return(cs)
}

#' Calculate the points per cross section based off length 
#' 
#' @param cs_length numeric vector, lengths of each cross section (meters)
#' @param points_per_cs numeric, number of points per cross section. Default is NULL
#' @param min_pts_per_cs An optional minimum points value per cross section. If not provided, 
#' @param dem A SpatRaster object representing the Digital Elevation Model (DEM) or a character string referencing a remote resource.
#' the function calculates it based on the length of cross-sections and the resolution of the DEM.
#' @importFrom terra linearUnits rast res
#' @return numeric vector, indicating the number of points per cross section for each cs_length
get_points_per_cs <- function(cs_length,
                              points_per_cs  = NULL,
                              min_pts_per_cs = 10,
                              dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
) {
  
  has_points_per_cs <- !is.null(points_per_cs)
  has_dem           <- !is.null(dem)
  
  # If NULL value is given to points_per_cs argument, calculate points_per_cs values
  # - IF DEM has a longitude/latitude CRS (terra::linearUnits == 0):
  # -- then divide the cross section length by 111139 and divide that resulting value by the minimum resolution value from the DEM (then round the result up)
  # - ELSE:
  # -- just divide the cross section length by the minimum resolution value from the DEM (then round the result up)
  # -- If no DEM is given, then just do the pmax of the min and points_per_cs value
  if (!has_points_per_cs && has_dem) {
    
    points_per_cs <- dem_based_points_per_cs(cs_length = cs_length, dem = dem)
  }
  
  # if points_per_cs is still NULL, set a points per cs of 1 for every value 
  points_per_cs <- if (is.null(points_per_cs)) { rep(1, length(cs_length)) } else { points_per_cs }
  
  # Take the max between the given minimum points per cross section and the derived points per cross section
  points_per_cs   <- pmax(min_pts_per_cs, points_per_cs)
  # points_per_cs   <- pmax(min_pts_per_cs, points_per_cs)
  
  return(points_per_cs)
}

#' Calculate the points per cross section based off length relative to a DEM
#' Given the length of cross sections and a DEM, approximate the appropriate number of points for each cross section length
#' @param cs_length numeric vector, lengths of each cross section (meters)
#' @param dem A SpatRaster object representing the Digital Elevation Model (DEM) or a character string referencing a remote resource.
#' the function calculates it based on the length of cross-sections and the resolution of the DEM.
#' @importFrom terra linearUnits rast res
#' @return numeric vector of length cs_length, with the number of points per cs_length
dem_based_points_per_cs <- function(
    cs_length,
    dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
) {
  
  # cs_length = c(100, 500)
  # dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
  
  # If NULL value is given to points_per_cs argument, calculate points_per_cs values
  # - IF DEM has a longitude/latitude CRS (terra::linearUnits == 0):
  # -- then divide the cross section length by 111139 and divide that resulting value by the minimum resolution value from the DEM (then round the result up)
  # - ELSE:
  # -- just divide the cross section length by the minimum resolution value from the DEM (then round the result up)
    if (terra::linearUnits(terra::rast(dem)) == 0) {
      points_per_cs = ceiling(
        (cs_length / 111139) / min(terra::res(terra::rast(dem)))
      )
    } else {
      points_per_cs = ceiling(
        (cs_length) / min(terra::res(terra::rast(dem)))
      )
    }
  
  return(points_per_cs)
}




#' Convert SF linestring transect lines into SF points with 
#'
#' @param transects sf linestring
#' @param points_per_cs numeric vector of length 'transects', indicating the number of points to get per transect
#'
#' @return sf point dataframe
#' @importFrom sf st_set_geometry st_line_sample st_cast 
transects_to_cs_pts <- function(transects, points_per_cs) {
  
  cs <- sf::st_set_geometry(
            transects, 
            sf::st_line_sample(transects, points_per_cs)
          ) %>% 
            sf::st_cast("POINT") 
  
  return(cs)
}

#' Given a set of linestrings, extract DEM values at points along the linestring
#'
#' @param cs cross section sf object
#' @param crosswalk_id character, column name of unique flowline / transect ID
#' @param dem SpatRaster DEM or character pointing to remote DEM resource
#' @importFrom dplyr mutate group_by n ungroup select everything across any_of
#' @importFrom sf st_set_geometry st_line_sample st_cast
#' @importFrom terra extract project vect crs rast
#' @return sf dataframe with Z values extracted from DEM
extract_dem_values <- function(cs, crosswalk_id = NULL, dem = NULL) {
  
  # cs           = trans 
  # crosswalk_id = ID_COL
  # dem          = NULL
  
  has_dem <- !is.null(dem)
  
  # TODO: not sure if this is the best way to do this, we just want it so if 
  # TODO: you dont specify an ID (or dont have an ID), then we autogenerate one
  # default NULL crosswalk_id to the default 'hydrofabric_id' 
  if(is.null(crosswalk_id)) {
    # net <- add_hydrofabric_id(net) 
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "points_per_cs", "cs_lengthm")
  
  # validate input graph
  is_valid <- validate_df(cs, REQUIRED_COLS, "cs")
  
  suppressWarnings({
    cs_pts <- 
      # sf::st_set_geometry(cs, sf::st_line_sample(cs, cs$points_per_cs)) %>% 
      # sf::st_cast("POINT") %>%
      cs %>% 
      transects_to_cs_pts(cs$points_per_cs) 
      
      # if a DEM was given, then extract the Z values, otherwise set Z to NA
      if (has_dem) {
        cs_pts <- 
          cs_pts %>% 
          dplyr::mutate(Z = extract_pt_val(terra::rast(dem), .))
      } else {
        cs_pts$Z <- NA
      }
    
    cs_pts <- 
      cs_pts %>% 
      # dplyr::mutate(Z = extract_pt_val(terra::rast(dem), .)) %>% 
      dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
      # dplyr::group_by(hy_id, cs_id) %>% 
      dplyr::mutate(
        pt_id              = 1:dplyr::n(),
        relative_distance  = seq(from = 0, to = cs_lengthm[1], length.out = dplyr::n())
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(
        dplyr::any_of(crosswalk_id),
        cs_id, 
        pt_id, 
        Z, 
        cs_lengthm, 
        relative_distance, 
        dplyr::everything()
      )
    # dplyr::select(dplyr::any_of(cols_to_select))
  })
  
  return(cs_pts)
  
}

#' Extract raster values at points
#'
#' @param rast terra SpatRaster
#' @param pts sf or terra points
#'
#' @return numeric vector of values from SpatRaster
#' @importFrom terra extract project vect crs
extract_pt_val <- function(rast, pts) {
  return(
    terra::extract(
      rast,
      terra::project(terra::vect(pts), terra::crs(rast))
    )[, 2]
  ) 
}

#' Classify Cross Section Points (version 2)
#' Version 2 of cross section point classifier function, uses 1st and 2nd derivative of the depths to better classify channel points
#' @param cs_pts CS points, output of hydrofabric3D::cross_section_pts()
#' @param crosswalk_id character, ID column in cs_pts
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
    crosswalk_id = NULL,
    pct_of_length_for_relief = 0.01
){
  
  # -----------------------------------------------------------------------
  # cs_pts <- cs
  # crosswalk_id = "hy_id"
  # pct_of_length_for_relief = 0.01
  
  # cs_pts <- data.frame(
  #   hy_id      = c("A", "A",  "A"),
  #   cs_id      = c(1, 1, 1),
  #   pt_id             = c(1, 2, 3),
  #   cs_lengthm        = c(100, 100, 100),
  #   relative_distance = c(0.333333, 0.666667, 1.0000000),
  #   Z = c(4, 1, 4)
  # )
  # cs_pts <- data.frame(
  #   hy_id      = c("A", "A",  "A"),
  #   cs_id      = c(1, 1, 1),
  #   pt_id             = c(1, 2, 3),
  #   cs_lengthm        = c(100, 100, 100),
  #   relative_distance = c(0.333333, 0.666667, 1.0000000),
  #   Z = c(4, 1, 4)
  # )
  # crosswalk_id = "hy_id"
  # pct_of_length_for_relief = 0.01
  
  # message("message from dev testing ~.__.~")
  # -----------------------------------------------------------------------
  
  . <-  L <-  L1 <-  L2  <-  R  <-  R1 <-  R2  <- Z  <-  Z2 <-  anchor <-  b1  <- b2  <- cs_lengthm  <- count_left <- 
    count_right  <-  cs_id <-  hy_id <-  in_channel_pts  <- lengthm <-  low_pt  <- max_bottom  <- mean_dist <-  mid_bottom  <- min_bottom  <- pt_id <- relative_distance <-  third <- NULL
  # TODO: maybe relief_to_length_ratio is more intuitive than pct_of_length_for_relief ????
  # relief_to_length_ratio = 0.01
  
  # cs_pts2 <- cs_pts
  # crosswalk_id = "hy_id"
  # pct_of_length_for_relief = 0.01
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # cs  <- add_hydrofabric_id(cs) 
    crosswalk_id  <- 'hydrofabric_id'
  }

  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "cs_lengthm", "relative_distance")

  # validate input cs pts
  is_valid <- validate_df(cs_pts, REQUIRED_COLS, "cs_pts") 

  # if (!all(REQUIRED_COLS %in% names(cs_pts))) {
  #   missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(cs_pts))]
  #   stop("'cs_pts' is missing one or more of the required columns:\n > ", 
  #        paste0(missing_cols, collapse = "\n > "))
  # }
  
  # type checking
  if (!is.numeric(pct_of_length_for_relief)) {
    stop("Invalid argument type, 'pct_of_length_for_relief' must be of type 'numeric', given type was '",   
         class(pct_of_length_for_relief), "'")
  }
  
  # Make sure pct_of_length_for_relief is valid percentage value (greater than 0)
  if (pct_of_length_for_relief < 0 ) {
    stop("Invalid value 'pct_of_length_for_relief' of ", 
         pct_of_length_for_relief, ", 'pct_of_length_for_relief' must be greater than or equal to 0")
  } 
  
  # # remove any columns that already exist
  cs_pts <- dplyr::select(cs_pts, 
                          !dplyr::any_of(c("class", "point_type", "bottom", "left_bank", 
                                           "right_bank", "valid_banks", "has_relief"))
  )
  
  # required cols that will be selected from the classified_pts object and in this order
  output_cols       <- c(crosswalk_id, "cs_id", "pt_id", "Z", "relative_distance", 
                      "cs_lengthm", "class", "point_type")
  
  # any starting columns in the original data 
  starting_cols  <- names(cs_pts)
  
  # name and order of columns to select with
  cols_to_select <- c(output_cols, starting_cols[!starting_cols %in% output_cols])
  
  # check if we're missing the required points_per_cs column, if so, 
  # we generate one based on the number of points in each cross section
  is_missing_points_per_cs <- !"point_per_cs" %in% names(cs_pts)
  
  if (is_missing_points_per_cs) {
    cs_pts <- 
      cs_pts %>% 
      dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
      dplyr::mutate(points_per_cs = dplyr::n()) %>% 
      dplyr::ungroup()
  } 
  
  # create classifications for points
  classified_pts <-
    dplyr::filter(cs_pts) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::group_by(hy_id, cs_id) %>%
    dplyr::mutate(
      class       = classify_banks_and_bottoms(
                        num_of_pts = points_per_cs[1],
                        pt_ids     = pt_id,
                        depths     = Z
                      ),
      Z           = use_smoothed_depths(
                        start_depths    = Z, 
                        smoothed_depths = smooth_depths(Z, window = 3), 
                        point_types     = class
                      ),
      anchors     = list(
                      find_anchor_pts(
                        depths             = Z,
                        num_of_pts         = points_per_cs[1],
                        cs_length          = cs_lengthm[1],
                        relative_distance  = relative_distance,
                        point_types        = class
                      )
                    ),
      L              = anchors[[1]][1],
      R              = anchors[[1]][3],
      class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
      class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
      class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class),
      
      # get classification of concavity based on 1st and 2nd derivatives of depth points
      deriv_type   = classify_derivatives(Z),
      deriv_type   = dplyr::case_when(
        (grepl("concave", deriv_type) | deriv_type == "linear") & class != "bottom" ~ "channel",
        TRUE                         ~ class
      ),
      # side = dplyr::case_when(
      #   pt_id >= R[1] ~ "right_side",
      #   pt_id <= L[1] ~ "left_side",
      #   TRUE          ~ "middle"
      # ),
      # # max_left      = which.max(Z[1:L[1]]),
      # # max_right     = which.max(Z[R[1]:length(Z)]) + R[1],
      # max_left      = pmax(which.max(Z[1:L[1]]), 1),
      # max_right     = pmin(which.max(Z[R[1]:length(Z)]) + R[1], points_per_cs[1]),
      
      deriv_type   = clean_point_types(deriv_type),
      # deriv_type   = set_missing_bottom(
      #                                 depths      = Z, 
      #                                 point_types = deriv_type
      #                                 ),
      deriv_type   = set_channel_anchors(deriv_type),
      deriv_type   = set_bank_anchors2(
                                    depths = Z,
                                    point_types = deriv_type,
                                    L = L[1],
                                    R = R[1]
                                    ),
      deriv_type   = set_missing_bottom(
        depths      = Z, 
        point_types = deriv_type
      ),
      deriv_type   = set_left_bank(
        point_types = deriv_type
      ),
      deriv_type   = set_right_bank(
        point_types = deriv_type
      ),
      deriv_type   = set_channel_surrounded_by_bottom(
        depths      = Z,
        point_types = deriv_type
      ),
      class        = deriv_type,
      point_type   = deriv_type
      # class      = clean_point_types(class),
      # point_type = class
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(dplyr::any_of(cols_to_select))
  # dplyr::select(dplyr::all_of(cols_to_select))      # Stricter, requires ALL of the columns to be present or it will throw an error
  # classified_pts$point_type
  
  
  # classified_pts[cols_to_select]                       # Another method for selecting columns....
  
  # get bank validity attributes for each hy_id/cs_id
  # - Uses the count of point types per cross section and checks Z to make sure that a "bottom" point is
  #   in each cross section and each "bottom" point has a valid left and right bank)
  bank_validity_df <- get_bank_attributes(classified_pts, crosswalk_id)
  
  # classified_pts %>%
  # dplyr::filter(hy_id %in% c('wb-1003260')) %>%
  # hydrofabric3D::plot_cs_pts(color = "point_type")
  
  # # Or add bank attributes 
  # banked_pts <- add_bank_attributes(output_pts)
  
  # get relief data, determine if a cross section has relief within X% percentage of the cross sections length
  relief_df <- get_relief(
    classified_pts, 
    crosswalk_id             = crosswalk_id,
    pct_of_length_for_relief = pct_of_length_for_relief, 
    detailed                 = FALSE
  )
  
  # join the bank validity attributes with the relief values
  validity_checks <- dplyr::left_join(
    bank_validity_df, 
    relief_df, 
    by = c(crosswalk_id, "cs_id")  
    # by = c("hy_id", "cs_id")
  )
  
  # join the new validity check values to the classified points
  classified_pts <- 
    classified_pts %>% 
    dplyr::left_join(
      validity_checks,
      by = c(crosswalk_id, "cs_id")  
      # by = c("hy_id", "cs_id")
    ) 
  
  # move the geometry column to the last column (if one exists)
  classified_pts <- move_geometry_to_last(classified_pts)
  
  return(classified_pts)
  
}

#############################################################################################################
# classify_banks_and_bottoms()
# use_smoothed_depths()
# smooth_depths()
# find_anchor_pts()
# classify_derivatives()
# clean_point_types()
# set_bank_anchors()
#############################################################################################################
# dplyr::mutate(
#   class = classify_banks_and_bottoms(
#     num_of_pts = points_per_cs[1],
#     pt_ids     = pt_id,
#     depths     = Z
#   ),
#   Z           = use_smoothed_depths(start_depths = Z, 
#                                     smoothed_depths = smooth_depths(Z, window = 3), 
#                                     point_types = class
#   ),
#   anchors = list(find_anchor_pts(
#     depths             = Z,
#     num_of_pts         = points_per_cs[1],
#     cs_length          = cs_lengthm[1],
#     relative_distance  = relative_distance,
#     point_types        = class
#   )
#   ),
#   L  = anchors[[1]][1],
#   R  = anchors[[1]][2],
#   class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
#   class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
#   class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class),
#   
#   deriv_type  = classify_derivatives(Z),
#   deriv_type = dplyr::case_when(
#     (grepl("concave", deriv_type) | deriv_type == "linear") & class != "bottom" ~ "channel",
#     TRUE                         ~ class
#   ),
#   
#   # side = dplyr::case_when(
#   #   pt_id >= R[1] ~ "right_side",
#   #   pt_id <= L[1] ~ "left_side",
#   #   TRUE          ~ "middle"
#   # ),
#   # # max_left      = which.max(Z[1:L[1]]),
#   # # max_right     = which.max(Z[R[1]:length(Z)]) + R[1],
#   # max_left      = pmax(which.max(Z[1:L[1]]), 1),
#   # max_right     = pmin(which.max(Z[R[1]:length(Z)]) + R[1], points_per_cs[1]),
#   # deriv_type    = dplyr::case_when(
#   #   # pt_id <= max_left ~ "left_bank",
#   #   # pt_id >= max_right ~ "right_bank",
#   #   pt_id == max_left ~ "left_bank",
#   #   pt_id == max_right ~ "right_bank",
#   #   TRUE ~ deriv_type
#   #   ),
#   
#   point_type = class,
#   # clean_pt   = clean_point_types(point_type),
#   # clean_pt   = set_bank_anchors(depths = Z, 
#   #                               point_types = clean_pt,
#   #                               L = L[1],
#   #                               R = R[1]
#   #                               ),
#   clean_der   = clean_point_types(deriv_type),
#   clean_der2  = set_bank_anchors(depths = Z,
#                                  point_types = clean_der,
#                                  L = L[1],
#                                  R = R[1]
#   )
# )

#' Classify banks and bottoms
#'
#' @param num_of_pts integer
#' @param pt_ids numeric vector
#' @param depths numeric vector
#' @importFrom dplyr between
#' @return character vector
#' @export
classify_banks_and_bottoms <- function(
    num_of_pts, 
    pt_ids, 
    depths
) {
  
  # calc the 
  # - number of points in a third of the cross section (third)
  third              <- ceiling(num_of_pts / 3)
  
  mid_third_left_idx   <- third
  mid_third_right_idx  <- ((2 * third) - 1)
  
  mid_third_idxs    <- mid_third_left_idx:mid_third_right_idx
  mid_third_low_pt  <- min(depths[mid_third_idxs])
  
  # logic for determining if its a bottom point (at lowest depth AND in middle third)
  is_at_bottom_Z      <- depths <= mid_third_low_pt
  is_at_middle_third  <- dplyr::between(pt_ids, mid_third_left_idx, mid_third_right_idx)
  
  point_type <- ifelse(is_at_bottom_Z & is_at_middle_third, "bottom", "bank")
  
  return(point_type)
}

#' Smooth out depth values
#'
#' @param depths 
#' @param num_of_pts 
#' @param window 
#'
#' @return numeric vector
#' @export
smooth_depths <- function(
    depths,
    num_of_pts = NULL,
    window     = 3
) {
  
  # depths <- cs_pts$Z
  # num_of_pts          <- 10
  # window = 3
  
  if(is.null(num_of_pts)) {
    num_of_pts <- length(depths)    
  } 
  
  # calculate a rolling mean of the depths between the starting and ending depth
  smoothed_depths <- c(
    depths[1],
    zoo::rollmean(depths, window),
    depths[num_of_pts]
  )
  
  return(smoothed_depths)
}

#' @title Use the bottom points from the original depth values otherwise use the (rolling mean) smoothed depth values
#'
#' @param start_depths 
#' @param smoothed_depths 
#' @param point_types 
#'
#' @return numeric vector
#' @export
use_smoothed_depths <- function(
    start_depths, 
    smoothed_depths, 
    point_types
) {
  return(
    ifelse(point_types == "bottom", start_depths, smoothed_depths)
  )
}

#' @title Find the anchor points on the left and right of the bottoms (where bottom points end)
#' given depths and relative distances and points classified into banks vs bottoms
#' determine the indices for 
#' - start of the left side bank/channel 
#' - start of the right/side bank channel
#' - the index of the point at the middle of the bottom 
#' @param depths 
#' @param num_of_pts 
#' @param cs_length 
#' @param relative_distance 
#' @param point_types 
#'
#' @return numeric vector of length 3 with the index of the left anchor, middle point, and right anchor (i.e. c(2, 5, 9) -> c(left_anchor, middle_bottom, right_anchor))
#' @export
find_anchor_pts <- function(depths, 
                            num_of_pts,
                            cs_length, 
                            relative_distance, 
                            point_types
) {
  
  
  # ------------------------------
  # depths            = depths
  # num_of_pts        = num_of_pts
  # relative_distance = relative_distances
  # cs_length         = cs_length
  # point_types       = point_types
  
  # ------------------------------
  third          = ceiling(num_of_pts / 3)
  
  dist_between_pts   <- mean(diff(relative_distance))
  in_channel_pts     <- ceiling(cs_length / dist_between_pts)
  
  b1  <- ceiling(in_channel_pts / 2) # b1
  b2  <- in_channel_pts - b1 # b2
  
  # bank_idxs      <- find_in_channel_pts(relative_distances, cs_length)
  # 
  # left_bank      <- bank_idxs[1] 
  # right_bank     <- bank_idxs[2]
  
  bottom_idxs <- which(point_types == "bottom")
  # point_type_is_bottom = which(point_types == "bottom")
  
  min_bottom  <- bottom_idxs[1]
  mid_bottom  <- bottom_idxs[ceiling(length(bottom_idxs) / 2)]
  max_bottom  <- bottom_idxs[length(bottom_idxs)]
  # min_bottom     = which(point_types == "bottom")[1]
  # mid_bottom     = which(point_types == "bottom")[ceiling(length(which(point_types == "bottom"))/2)]
  # max_bottom     = which(point_types == "bottom")[length(which(point_types == "bottom"))]
  
  L1             = pmax(1, mid_bottom - b1)
  L2             = pmax(1, mid_bottom - b2)
  
  R1             = pmin(mid_bottom + b2, num_of_pts)
  R2             = pmin(mid_bottom + b1, num_of_pts)
  
  anchor         = ifelse(depths[R2] < depths[L1], 2, 1)
  
  LEFT           = pmax(third, ifelse(anchor == 1, L1, L2))
  RIGHT          = pmin(2 * third, ifelse(anchor == 1, R1, R2))
  
  count_left     = min_bottom - LEFT
  count_right    = RIGHT - max_bottom
  
  LEFT           = ifelse(count_left == 0, LEFT - count_right, LEFT)
  RIGHT          = ifelse(count_right == 0, RIGHT + count_left, RIGHT)
  
  return(c(LEFT, mid_bottom, RIGHT))
  
}

#' @title Classify a numeric vector based on 1st and 2nd derivative values
#' @description Given depths of a cross section, calculate the first and second derivative of the points
#' and then classify the points based on 1st / 2nd derivative values at each point. 
#' With the intention that points with concavity / linear should be "channel" points
#' Points are labeled as:
#' - flat
#' - concave_up_increasing
#' - concave_up_decreasing
#' - concave_down_increasing
#' - concave_down_decreasing
#' - "linear"
#' @param depths numeric vector
#'
#' @return character vector
#' @export
classify_derivatives <- function(depths) {
  
  classify_derivs <- function(slope, second_deriv) {
    if (slope == 0) {
      return("flat")
    } else if (second_deriv > 0 && slope > 0) {
      return("concave_up_increasing")
    } else if (second_deriv > 0 && slope < 0) {
      return("concave_up_decreasing")
    } else if (second_deriv < 0 && slope > 0) {
      return("concave_down_increasing")
    } else if (second_deriv < 0 && slope < 0) {
      return("concave_down_decreasing")
    } else {
      return("linear")  
    }
  }
  
  # padding the starting depth values w/ a duplicate first and last values
  depths_padded <- c(depths[1], depths, depths[length(depths)])
  
  # calculate first deriv (slope) on padded depth values
  slopes <- diff(depths_padded)
  
  # calculate second deriv (differences of the first differences)
  second_derivative <- diff(slopes)
  
  # classify points based on padded 1st and 2nd derivatives 
  classifications <- mapply(classify_derivs, slopes[1:(length(second_derivative))], second_derivative)
  
  return(classifications)
} 

#' Cleanup some common invalid point type classifications in cross section points
#' given a vector of point types representing a cross section of depths, set rules on what points can exist in proximity to other points
#' - set any banks surrounded by channel and bottoms to "channel"
#' - set any bank points surrounded by "channel" points to "channel"
#' - set any banks surrounded by bottom points to "bottom"
#' @param point_types character vector 
#'
#' @return character vector
#' @export
clean_point_types <- function(point_types) {
  
  # point_types <- classified_pts$deriv_type
  
  # point_types <-
  #   smoothed %>%
  # #   dplyr::filter(hy_id == "wb-1003265", cs_id == 2) %>%
  # point_types <- c(point_types[1:6], c("bottom", "bottom", "channel", "channel"), point_types[7:length(point_types)] )
  # point_types2 <- point_types
  
  # ----------------------------------------------------------------------------------------------------------------------------------------
  # ---- Go through points and make sure point types follow certain rules regarding the neighboring group of point types:
  #
  # 1. A point is a BANK AND its between a "channel" and "bottom" point 
  #    ----> set point_type to "channel"
  #
  # 2. A point is a BANK AND its between 2 "bottom" points 
  #    ----> set point_type to "bottom"
  # ----------------------------------------------------------------------------------------------------------------------------------------
  
  L  <- 1
  M1 <- 1
  M2 <- 1
  R  <- 1
  
  RIGHT_BOUND <- length(point_types)
  
  # i = 1
  
  while (L <= R & 
         L < RIGHT_BOUND & 
         R < RIGHT_BOUND & 
         M1 < RIGHT_BOUND & 
         M2 < RIGHT_BOUND
  ) {
    
    # message("-----------------------------","ITERATION ", i, "-----------------------------")
    
    # message("> L: ", L)
    # message("> M1: ", M1)
    # message("> M2: ", M2)
    # message("> R: ", R)
    
    # get the point type of the start of the left group
    left_type = point_types[L]
    # left_group = point_types[L]            
    
    # message("Moving LEFT pointer")
    while(
      # point_types[L] == left_type
      point_types[L] == left_type & !is.na(point_types[L])
    ) {
      L = L + 1
      # message(" > ", L-1, " to ", L)
      # message(" -> ", point_types[L - 1], " --> ", point_types[L])
    }
    
    # get the rightmost left side group 
    L = L - 1
    
    left_group = point_types[L]
    
    # message("Left group: ", left_group)
    
    # make a M1 and M2 points which will be the start and end indices of the middle group
    M1 <- L + 1
    M2 <- M1
    
    # current middle group 
    mid_type <- point_types[M1]
    # mid_group = point_types[M]  
    
    # message("Moving MIDDLE pointer...")
    # move middle right pointer until another group is found
    while(
      # point_types[M2] == mid_type
      point_types[M2] == mid_type & !is.na(point_types[M2])
    ) {
      M2 <- M2 + 1
      # message(" > ", M2 - 1, " to ", M2)
      # message(" -> ", point_types[M2 - 1], " --> ", point_types[M2])
    }
    
    # get the group to left of the right hand group
    M2 <- M2 - 1
    
    mid_group <- point_types[M2]
    
    # message("Moving RIGHT pointer...")
    # move the right pointer to the right of the middle group
    R <- M2 + 1
    
    right_group <- point_types[R]
    
    at_final_groups <- is.na(left_group) | is.na(mid_group) | is.na(right_group)
    # at_last_group <- is.na(right_group)
    
    if (at_final_groups) {
      # message("Reached last groups, stopping early!")
      break
    }
    
    # ---------------------------------------------------------   
    # ----- conditions for changing point types -----
    # ---------------------------------------------------------   
    # - point is a BANK AND its between a "channel" and "bottom" point 
    #    ----> set point_type to "channel"
    is_bank_mid_group                   <-  mid_group %in% c("left_bank", "right_bank")
    
    is_bank_between_bottom_and_channel  <-  is_bank_mid_group & 
                                            left_group == "bottom" & 
                                            right_group == "channel"
    
    is_bank_between_channel_and_bottom  <-  is_bank_mid_group & 
                                            left_group == "channel" & 
                                            right_group == "bottom"
    
    if(is_bank_between_channel_and_bottom | is_bank_between_bottom_and_channel) {
      # message("\n~~~~~~~~~~~~~","\n-----> FOUND 'bank' point between channel and bottom point: \n (", left_group, " > ", mid_group, " < ", right_group, "\n~~~~~~~~~~~~~")
      point_types[M1:M2] <- "channel"
    } 
    
    # - point is a BANK AND its between 2 "channel" points 
    #    ----> set point_type to "channel"
    is_bank_between_channel_and_channel   <- is_bank_mid_group & 
                                             left_group == "channel" & 
                                             right_group == "channel" 
    
    if(is_bank_between_channel_and_channel) {
      # message("\n~~~~~~~~~~~", "\n-----> FOUND 'bank' point between 2 channel points: \n (", left_group, " > ", mid_group, " < ", right_group, "\n~~~~~~~~~~~")
      point_types[M1:M2] <- "channel"
    }
    
    # - point is a BANK AND its between 2 "bottom" points 
    #    ----> set point_type to "bottom"
    is_bank_between_bottom_and_bottom   <- is_bank_mid_group & 
                                           left_group == "bottom" & 
                                           right_group == "bottom" 
    
    if(is_bank_between_bottom_and_bottom) {
      # message("\n~~~~~~~~~~~", "\n-----> FOUND 'bank' point between 2 bottom points: \n (", left_group, " > ", mid_group, " < ", right_group, "\n~~~~~~~~~~~")
      point_types[M1:M2] <- "bottom"
    } 
    
    # message("=======================================", 
    #         "\nFound groups: \n", "\n- Left: (",  L, ") ", 
    #         left_group, "\n- Mid: (", M1, " - ",  M2, ") ", mid_group, "\n- Right: (", R, ") ",  right_group,
    #         "\n=======================================")
    
    # move the LEFT pointer to the start of the next group 
    L <- L + 1
    
  }
  
  # # ----------------------------------------------------------------------------------------------------------------------------------------
  # # ---- Go make sure that the "bottom" points have ATLEAST a single "channel" point to the left and right of the "bottom" ----
  # # ----------------------------------------------------------------------------------------------------------------------------------------
  # 
  # # set pointers 
  # L  <- 1
  # M1 <- 1
  # M2 <- 1
  # R  <- 1
  # 
  # # set a safe default value for the left and right of the bottom assuming 
  # # the edge of the bottom is 1 position left and right of the middle third of the points
  # third   <- length(point_types) %/% 3
  # # third   <- ceiling(length(point_types) / 3)
  # 
  # left_of_bottom   <- third - 1
  # right_of_bottom  <- (third * 2) + 1
  # 
  # max_bottom_width <- 0
  # 
  # while (L <= R & 
  #        L < RIGHT_BOUND & 
  #        R < RIGHT_BOUND & 
  #        M1 < RIGHT_BOUND & 
  #        M2 < RIGHT_BOUND
  # ) {
  #   
  #   # get the point type of the start of the left group
  #   left_type = point_types[L]
  #   while(
  #     point_types[L] == left_type & !is.na(point_types[L])
  #   ) {
  #     L = L + 1
  #   }
  #   
  #   # get the rightmost left side group 
  #   L = L - 1
  #   left_group = point_types[L]
  #   
  #   # make a M1 and M2 points which will be the start and end indices of the middle group
  #   M1 <- L + 1
  #   M2 <- M1
  #   
  #   # current middle group 
  #   mid_type <- point_types[M1]
  #   
  #   # move middle right pointer until another group is found
  #   while(
  #     point_types[M2] == mid_type & !is.na(point_types[M2])
  #   ) {
  #     M2 <- M2 + 1
  #   }
  #   
  #   # get the group to left of the right hand group
  #   M2 <- M2 - 1
  #   
  #   mid_group <- point_types[M2]
  #   
  #   # move the right pointer to the right of the middle group
  #   R <- M2 + 1
  #   
  #   right_group <- point_types[R]
  #   
  #   at_final_groups <- is.na(left_group) | is.na(mid_group) | is.na(right_group)
  #   
  #   if (at_final_groups) {
  #     # message("Reached last groups, stopping early!")
  #     break
  #   }
  #   
  #   # ---------------------------------------------------------   
  #   # ----- make sure points to the left and right of the bottom are set to channel -----
  #   # ---------------------------------------------------------   
  #   
  #   is_bottom    <- mid_group == "bottom" 
  #   
  #   # width of the current group of bottom points
  #   bottom_width <- (M2 - M1) + 1
  #   
  #   # is the current bottom bigger than any previously seen bottoms?
  #   at_current_biggest_bottom <- bottom_width >= max_bottom_width
  #   
  #   # if we're at a bottom group and its the current biggest bottom, 
  #   # store the point to the left and to the right of the current bottom group
  #   if(is_bottom & at_current_biggest_bottom) {
  #     
  #     max_bottom_width <- max(bottom_width, max_bottom_width)
  #     
  #     left_of_bottom  <- M1 - 1
  #     right_of_bottom <- M2 + 1
  #     
  #   } 
  #   # move the Left pointer to the start of the next group 
  #   L <- L + 1
  # }
  # 
  # # make sure points left and right of bottom stay in bounds of vector
  # left_of_bottom   <- max(left_of_bottom, 1)
  # right_of_bottom  <- min(right_of_bottom, RIGHT_BOUND)
  # 
  # # NOTE: make sure that to the left and right of the bottom, are "channel" points
  # point_types[left_of_bottom]  <- "channel"
  # point_types[right_of_bottom] <- "channel"
  
  return(point_types)
  
}

#' Set a "channel" point on the left and right ends of the "bottom" in a set of cross section point types 
#' Make sure that the "bottom" has ATLEAST 1 channel point on both the left and right sides of the bottom
#' @param point_types character vector 
#'
#' @return character vector
#' @export
set_channel_anchors <- function(point_types) {

  # point_types <- cs_pts2$deriv_type
  # point_types[15] <- "bottom"
  
  # point_types <- classified_pts$deriv_type
  # point_types <- c("left_bank", "left_bank", "channel", "bottom", "bottom", "channel", "channel", "right_bank", "right_bank")
  # 
  # point_types <- c("bottom", "bottom", "channel", "channel", "right_bank", "right_bank")
  # point_types <- c("left_bank",  "bottom", "bottom", "right_bank")
  
  # point_types
  # classified_pts$class
  # plot(classified_pts$Z)
  
  # point_types <-
  #   smoothed %>%
  # #   dplyr::filter(hy_id == "wb-1003265", cs_id == 2) %>%
  # point_types <- c(point_types[1:6], c("bottom", "bottom", "channel", "channel"), point_types[7:length(point_types)] )
  # point_types2 <- point_types
  # ----------------------------------------------------------------------------------------------------------------------------------------
  # ---- Go make sure that the "bottom" points have ATLEAST a single "channel" point to the left and right of the "bottom" ----
  # ----------------------------------------------------------------------------------------------------------------------------------------
  
  # set pointers 
  L  <- 1
  M1 <- 1
  M2 <- 1
  R  <- 1

  RIGHT_BOUND <- length(point_types)
  
  # set a safe default value for the left and right of the bottom assuming 
  # the edge of the bottom is 1 position left and right of the middle third of the points
  third   <- length(point_types) %/% 3
  # third   <- ceiling(length(point_types) / 3)
  
  # make sure points left and right of bottom stay in bounds of vector
  left_of_bottom   <- max(third - 1, 1)
  right_of_bottom  <- min((third * 2) + 1, RIGHT_BOUND)
  # left_of_bottom   <- third - 1
  # right_of_bottom  <- (third * 2) + 1
  
  use_only_right_of_bottom <- FALSE
 
  max_bottom_width <- 0
  
  while (L <= R & 
         L < RIGHT_BOUND & 
         R < RIGHT_BOUND & 
         M1 < RIGHT_BOUND & 
         M2 < RIGHT_BOUND
  ) {
    
    # get the point type of the start of the left group
    left_type = point_types[L]
    
    while(
      point_types[L] == left_type & !is.na(point_types[L])
    ) {
      L = L + 1
    }
    
    # get the rightmost left side group 
    L = L - 1
    left_group = point_types[L]
    
    # make a M1 and M2 points which will be the start and end indices of the middle group
    M1 <- L + 1
    M2 <- M1
    
    # current middle group 
    mid_type <- point_types[M1]
    
    # move middle right pointer until another group is found
    while(
      point_types[M2] == mid_type & !is.na(point_types[M2])
    ) {
      M2 <- M2 + 1
    }
    
    # get the group to left of the right hand group
    M2 <- M2 - 1
    
    mid_group <- point_types[M2]
    
    # move the right pointer to the right of the middle group
    R <- M2 + 1
    # R <- min(M2 + 1, RIGHT_BOUND)
    
    right_group <- point_types[R]
    
    at_final_groups <- is.na(left_group) | is.na(mid_group) | is.na(right_group)
    # at_final_groups <- is.na(left_group) | is.na(mid_group) | is.na(right_group) | R >= RIGHT_BOUND
    
    if (at_final_groups) {
      # TODO: if the left group is the bottom AND right_group is NA (out of bounds) 
      # TODO: then we only want to set the point to the RIGHT of the bottom to a channel
      use_only_right_of_bottom  <- left_group == "bottom"  & is.na(right_group) 
      # use_only_right_of_bottom  <- left_group == "bottom"  & ( is.na(right_group) | R >= RIGHT_BOUND )
      # use_only_left_of_bottom   <- right_group == "bottom" & is.na(left_group) 
      
      # message("Reached last groups, stopping early!")
      break
    }
    
    # ---------------------------------------------------------   
    # ----- make sure points to the left and right of the bottom are set to channel -----
    # ---------------------------------------------------------   
    
    is_bottom    <- mid_group == "bottom" 
    
    # width of the current group of bottom points
    bottom_width <- (M2 - M1) + 1
    
    # is the current bottom bigger than any previously seen bottoms?
    at_current_biggest_bottom <- bottom_width >= max_bottom_width
    
    # if we're at a bottom group and its the current biggest bottom, 
    # store the point to the left and to the right of the current bottom group
    if (is_bottom & at_current_biggest_bottom) {
      
      max_bottom_width <- max(bottom_width, max_bottom_width)
      
      left_of_bottom  <- M1 - 1
      right_of_bottom <- M2 + 1
      
    } 
    # move the Left pointer to the start of the next group 
    L <- L + 1
  }
  
  # make sure points left and right of bottom stay in bounds of vector
  left_of_bottom   <-  max(left_of_bottom, 1)
  right_of_bottom  <-  min(right_of_bottom, RIGHT_BOUND)
  
  if (use_only_right_of_bottom) {
    point_types[right_of_bottom] <- ifelse(point_types[right_of_bottom] == "bottom", "bottom", "channel")
    # point_types[right_of_bottom] <- "channel"
    
    return(point_types)
    
  }
  
  # NOTE: make sure that to the left and right of the bottom, are "channel" points
  point_types[left_of_bottom]  <- ifelse(point_types[left_of_bottom] == "bottom", "bottom", "channel")
  point_types[right_of_bottom] <- ifelse(point_types[right_of_bottom] == "bottom", "bottom", "channel")
  
  return(point_types)
  
}


#' @title Given a vector of depths and vector of corresponding point types representing a cross section of depths, 
#' @description Conditions for setting left and right bank classes:
#' - make sure there is a left_bank at the left most highest position of the left side
#' - make sure there is a "right_bank" at the right most highest position of the right side
#' @param depths numeric, depth values
#' @param point_types character, poitn type
#' @param L numeric, left pointer
#' @param R numeric, right pointer
#'
#' @return character vector
#' @export
set_bank_anchors <- function(
    depths,
    point_types,
    L,
    R
) {
  
  CS_START  <- 1
  CS_END    <- length(depths)
  
  left_bank_count   <- sum(point_types[1:L] == "left_bank")
  right_bank_count  <- sum(point_types[R:CS_END] == "right_bank")
  
  has_both_banks    <- left_bank_count > 0 & right_bank_count > 0
  
  if (has_both_banks) {
    # message("Cross section already has 'left_bank' and 'right_bank' points, returning input point_types")
    return(point_types)
  }
  
  left_max_index <- which.max(depths[1:L])
  
  ## TODO: method 1 for getting highest right most point index
  ## TODO: reverse the right side, get the first max point with which.max() then subtract this offset from total number of points and add 1
  right_offset     <- which.max(depths[CS_END:R])
  right_max_index  <- (CS_END - right_offset) + 1
  
  ## TODO: method 2 for getting highest right most point index
  # right_offset     <- max(which(depths[R:CS_END] == max(depths[R:CS_END]))) - 1
  # right_max_index  <- R + right_offset
  
  point_types[left_max_index]  <- "left_bank"
  point_types[right_max_index] <- "right_bank"
  
  return(point_types)
  
}

#' @title Given a vector of depths and vector of corresponding point types representing a cross section of depths (version 2)
#' @description Conditions for setting left and right bank classes:
#' - make sure there is a left_bank at the left most highest position of the left side
#' - make sure there is a "right_bank" at the right most highest position of the right side
#' @param depths numeric, depth values
#' @param point_types character, poitn type
#' @param L numeric, left pointer
#' @param R numeric, right pointer
#'
#' @return character vector
#' @export
set_bank_anchors2 <- function(
    depths,
    point_types,
    L,
    R
) {
  
  CS_START  <- 1
  CS_END    <- length(depths)
  
  left_bank_count   <- sum(point_types[1:L] == "left_bank")
  right_bank_count  <- sum(point_types[R:CS_END] == "right_bank")
  
  has_both_banks    <- left_bank_count > 0 & right_bank_count > 0
  
  if (has_both_banks) {
    # message("Cross section already has 'left_bank' and 'right_bank' points, returning input point_types")
    return(point_types)
  }
  
  # move the left and right delienters ONE more position away 
  # from the bottom points to insure required "channel" points are NOT overwritten
  L <- max(L - 1, 1)
  R <- min(R + 1, CS_END)
  
  left_max_index <- which.max(depths[1:L])
  
  ## TODO: method 1 for getting highest right most point index
  ## TODO: reverse the right side, get the first max point with which.max() then subtract this offset from total number of points and add 1
  right_offset     <- which.max(depths[CS_END:R])
  right_max_index  <- (CS_END - right_offset) + 1
  
  ## TODO: method 2 for getting highest right most point index
  # right_offset     <- max(which(depths[R:CS_END] == max(depths[R:CS_END]))) - 1
  # right_max_index  <- R + right_offset
  
  point_types[left_max_index]  <- "left_bank"
  point_types[right_max_index] <- "right_bank"
  
  return(point_types)
  
}

#' Check if a set of cross_section point types are in the pattern of left_bank, channel, then right_bank points
#'
#' @param point_types character vector 
#'
#' @return logical, TRUE if points are left_bank --> channel --> right_bank order
#' @export
is_bank_channel_bank_pattern <- function(point_types) {
  
  # point_types <- c("left_bank", "left_bank", "channel", "channel", "channel", "right_bank", "right_bank")
  # point_types <- c("left_bank", "left_bank", "channel", "channel", "channel")
  # point_types <- c("left_bank", "left_bank", "right_bank", "channel", "channel", "channel", "right_bank", "right_bank")
  # point_types <- c("left_bank", "left_bank", "channel", "bottom", "bottom", "channel", "channel", "right_bank", "right_bank")
  
  is_only_valid_point_types    <- all(point_types %in% c("left_bank", "channel", "right_bank"))
  has_both_banks_and_channel   <- all(c("left_bank", "channel", "right_bank") %in% point_types)
  # !(is_only_valid_point_types & has_both_banks_and_channel)
  
  if (!(is_only_valid_point_types & has_both_banks_and_channel)) {
    return(FALSE)
  }
  
  # get indices of where each group starts 
  left_banks    <- which(point_types == "left_bank")
  channels      <- which(point_types == "channel")
  right_banks   <- which(point_types == "right_bank")
  
  # all(c(left_banks, channels, right_banks) == seq_along(point_types))
  
  # Check if left_bank, channel, and right_bank are consecutive and in correct order
  return(
    all(c(left_banks, channels, right_banks) == seq_along(point_types))
  )
}

#' Set an artificial bottom at the lowest channel point if cross section points are in a left_bank -> channel -> right_bank pattern
#' Only applies to cross section points where there is NO bottom and 
#' the point types are in this order: left_bank -> channel -> right_bank
#' @param depths numeric vector
#' @param point_types character vector
#'
#' @return character vector
#' @export
set_missing_bottom <- function(depths, point_types) {
  
  # depths <- c(10, 8, 5, 4, 5, 7, 12)
  # point_types <- c("left_bank", "left_bank", "channel", "channel", "channel", "right_bank", "right_bank")
  # depths <- c(10, 8, 5, 4, 7, 12)
  # point_types <- c("left_bank", "left_bank", "channel", "channel", "right_bank", "right_bank")
  # depths <- c(10, 8, 4, 7, 12)
  # point_types <- c("left_bank", "left_bank", "channel", "right_bank", "right_bank")
  # plot(depths)
  
  # depths
  # depths <- classified_pts$Z
  # point_types <- classified_pts$deriv_type
  # point_types
  
  is_bank_channel_bank <- is_bank_channel_bank_pattern(point_types)
  
  # return early if the cross section points are NOT the pattern we're interested in
  if (!is_bank_channel_bank) {
    return(point_types)
  }
  
  # get minimum depths at each point type
  min_left_bank  <- min(depths[point_types == "left_bank"])
  min_right_bank <- min(depths[point_types == "right_bank"])
  min_channel    <- min(depths[point_types == "channel"])
  
  is_channel_below_banks <- min_channel <= min_left_bank && min_channel <= min_right_bank
  
  if(is_channel_below_banks) {
    set_as_bottom_pt <- depths <= min_channel & point_types == "channel"
    
    point_types[set_as_bottom_pt] <- "bottom"
  }
  
  return(point_types)
  
}

#' Set any points to the left of the right most left bank point to "left_bank" IF there is a channel then a bottom to the right of the left_bank point
#'
#' @param point_types character vector 
#'
#' @return character
#' @export
set_left_bank <- function(point_types) {
  
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "channel", "channel", "bottom", "channel", "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "channel", "channel", "channel", "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "bottom", "channel", "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "bottom", "channel", "right_bank")
  # point_types <- c("channel", "channel", "channel", "bottom", "channel", "right_bank")
  # point_types <- c("left_bank", "left_bank", "bottom", "bottom", "bottom", "right_bank")
  # point_types <- c("left_bank", "left_bank", "channel", "channel", "channel")
  # point_types <- c("left_bank", "left_bank", "right_bank", "channel", "channel", "channel", "right_bank", "right_bank")
  # point_types <- c("left_bank", "left_bank", "channel", "bottom", "bottom", "channel", "channel", "right_bank", "right_bank")
  
  # get indices of where each group starts 
  left_banks    <- which(point_types == "left_bank")
  bottoms       <- which(point_types == "bottom")
  channels      <- which(point_types == "channel")
  # right_banks   <- which(point_types == "right_bank")
  
  # make sure there is a bottom point
  has_bottom           <- length(bottoms) > 0
  has_left_bank        <- length(left_banks) > 0
  
  right_most_left_bank <- max(left_banks)
  
  has_channel_to_the_right_of_left_bank  <- any(channels > right_most_left_bank) && has_left_bank
  has_bottom_after_channel           <- any(channels[channels > right_most_left_bank] < min(bottoms)) && has_bottom
  
  # if there is a valid left_bank -> channel -> bottom order, then set points to the left of the last "left_bank" point all to "left_bank"
  if (has_channel_to_the_right_of_left_bank && has_bottom_after_channel) {
    point_types[1:right_most_left_bank] <- "left_bank"
  } 
  
  # point_types[(right_most_left_bank+1):total_length]
  # all(c(left_banks, channels, right_banks) == seq_along(point_types))
  
  # Check if left_bank, channel, and right_bank are consecutive and in correct order
  return(
    point_types
  )
}

#' Set any points to the right of the left most right bank point to "right_bank" IF there is a channel then a bottom to the left of the right_bank point
#'
#' @param point_types character vector 
#'
#' @return character
#' @export
set_right_bank <- function(point_types) {
  
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "channel", "channel", "bottom", "channel", "right_bank", "channel",  "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "channel", "channel", "channel", "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "bottom", "channel", "right_bank", "channel", "channel", "right_bank", "channel", "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "bottom", "bottom", "right_bank", "channel", "channel", "right_bank", "channel", "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "bottom", "channel", "right_bank")
  # point_types <- c("channel", "channel", "channel", "bottom", "channel", "right_bank")
  # point_types <- c("left_bank", "left_bank", "bottom", "bottom", "bottom", "right_bank")
  # point_types <- c("left_bank", "left_bank", "channel", "channel", "channel")
  # point_types <- c("left_bank", "left_bank", "right_bank", "channel", "channel", "channel", "right_bank", "right_bank")
  # point_types <- c("left_bank", "left_bank", "channel", "bottom", "bottom", "channel", "channel", "right_bank", "right_bank")
  
  # get indices of where each group starts 
  # left_banks    <- which(point_types == "left_bank")
  right_banks   <- which(point_types == "right_bank")
  bottoms       <- which(point_types == "bottom")
  channels      <- which(point_types == "channel")
  
  # make sure there is a bottom point
  has_bottom           <- length(bottoms) > 0
  has_right_bank       <- length(right_banks) > 0
  
  left_most_right_bank <- min(right_banks)
  
  has_channel_to_the_left_of_right_bank  <- any(channels < left_most_right_bank) && has_right_bank
  has_bottom_after_channel               <- any(channels[channels < left_most_right_bank] > max(bottoms)) && has_bottom
  
  # if there is a valid bottom <- channel <- right_bank order, then set points to the right of the first right_bank point all to "right_bank"
  if (has_channel_to_the_left_of_right_bank && has_bottom_after_channel) {
    point_types[left_most_right_bank:length(point_types)] <- "right_bank"
  } 
  
  # point_types
  
  # point_types[(right_most_left_bank+1):total_length]
  # all(c(left_banks, channels, right_banks) == seq_along(point_types))
  
  # Check if left_bank, channel, and right_bank are consecutive and in correct order
  return(
    point_types
  )
}

#' Set any channel points that are entirely surrounded by bottom points to bottom points 
#'
#' @param depths numeric vector
#' @param point_types character vector 
#'
#' @return character
#' @export
set_channel_surrounded_by_bottom <- function(
    depths,
    point_types
    ) {
  
  # point_types <- c('left_bank', 'left_bank', 'left_bank', 'left_bank', 
  #                  'left_bank', 'left_bank', 'left_bank', 'left_bank',
  #                  'channel', 'channel', 'bottom', 'bottom', 'channel', 'bottom', 'bottom', 'bottom',
  #                  'channel', 'channel', 'channel', 'channel', 
  #                  'channel', 'channel', 'right_bank', 'right_bank', 
  #                  'right_bank', 'right_bank', 'right_bank', 'right_bank', 'right_bank')
  # depths <- c(250.928558349609, 251.240821555808, 251.615333462939, 252.091121909059, 252.708690878786, 253.497185601128, 254.31950510284, 
  #             254.667724986135, 253.720274748625, 250.969419502917, 246.858016967773, 246.858016967773, 246.858017156153, 246.858016967773, 
  #             246.858016967773, 246.858016967773, 247.939518681279, 249.154418756933, 250.436868173105, 251.538464016385, 252.277690369406, 
  #             252.62558887623, 252.708550723983, 252.6675974528, 252.628967473536, 252.636721010561, 252.666483749578, 
  #             252.655649255823, 252.593170166016)
  
  # point_types <- c('left_bank', 'left_bank', 'left_bank', 'left_bank', 
  #                  'left_bank', 'left_bank', 'left_bank', 'left_bank',
  #                  'channel', 'channel', 'bottom', 'bottom', 'channel', 'channel', 'channel', 
  #                  'bottom', 'bottom', 'bottom',
  #                  'channel', 'channel', 'channel', 'channel', 
  #                  'channel', 'channel', 'right_bank', 'right_bank', 
  #                  'right_bank', 'right_bank', 'right_bank', 'right_bank', 'right_bank')
  # 
  # depths <- c(250.928558349609, 251.240821555808, 251.615333462939, 252.091121909059, 252.708690878786, 253.497185601128, 254.31950510284, 
  #             254.667724986135, 253.720274748625, 250.969419502917, 246.858016967773, 246.858016967773, 
  #             
  #             246.858017156153,246.858017156153,246.858017156153,
  #             
  #             246.858016967773, 
  #             246.858016967773, 246.858016967773, 247.939518681279, 249.154418756933, 250.436868173105, 251.538464016385, 252.277690369406, 
  #             252.62558887623, 252.708550723983, 252.6675974528, 252.628967473536, 252.636721010561, 252.666483749578, 
  #             252.655649255823, 252.593170166016)
  
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "channel", "channel", "bottom", "channel", "right_bank", "channel",  "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "channel", "channel", "channel", "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "bottom", "channel", "right_bank", "channel", "channel", "right_bank", "channel", "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "left_bank", "bottom", "bottom", "right_bank", "channel", "channel", "right_bank", "channel", "right_bank")
  # point_types <- c("left_bank", "channel", "channel", "bottom", "channel", "right_bank")
  # point_types <- c("channel", "channel", "channel", "bottom", "channel", "right_bank")
  # point_types <- c("left_bank", "left_bank", "bottom", "bottom", "bottom", "right_bank")
  # point_types <- c("left_bank", "left_bank", "channel", "channel", "channel")
  # point_types <- c("left_bank", "left_bank", "right_bank", "channel", "channel", "channel", "right_bank", "right_bank")
  # point_types <- c("left_bank", "left_bank", "channel", "bottom", "bottom", "channel", "channel", "right_bank", "right_bank")
  # paste0(paste0("'", cpts$point_type, "'"), collapse= ", ")
  # paste0(cpts$Z, collapse= ", ")
  
  # depths <- c(250.928558349609, 251.240821555808, 251.615333462939, 252.091121909059, 252.708690878786, 253.497185601128, 254.31950510284, 254.667724986135, 253.720274748625, 250.969419502917, 246.858016967773, 246.858016967773, 246.858017156153, 246.858016967773, 246.858016967773, 246.858016967773, 247.939518681279, 249.154418756933, 250.436868173105, 251.538464016385, 252.277690369406, 252.62558887623, 252.708550723983, 252.6675974528, 252.628967473536, 252.636721010561, 252.666483749578, 252.655649255823, 252.593170166016)

  # get indices of where each group starts 
  bottoms       <- which(point_types == "bottom")
  channels      <- which(point_types == "channel")
  
  # channels[((channels + 1) %in% bottoms) & ((channels - 1) %in% bottoms)]
  # make sure there is a bottom point and channel point
  has_bottom        <- length(bottoms) > 0
  has_channel       <- length(channels) > 0
  
  # if we are missing either bottom OR channel points, just return the input point types vector
  if (!has_bottom | !has_channel) {
    return(point_types)
  }
  
  min_bottom_depth <- min(depths[bottoms])
  
  min_bottom <- min(bottoms)
  max_bottom <- max(bottoms)
  
  # get any channel pts that are between sets of bottom points
  channel_pts_between_bottoms                <- channels[(channels > min_bottom) & (channels < max_bottom)]
  
  # any(channels[(10 > min_bottom) & (100000000 < max_bottom)])
  
  # as long as some points were identified as channel points between bottom points....
  if( any(channel_pts_between_bottoms )) {
    
      # check that the depth at those channel points between the bottom points is at the bottom (approximately)
      channel_pts_between_bottoms_and_at_bottom  <- ceiling(depths[channel_pts_between_bottoms])  <= ceiling(min_bottom_depth)
      
      # if there are channel points that are both surrounded by bottoms AND at the right bottom depth, then just set those points to bottom points
      if (any(channel_pts_between_bottoms_and_at_bottom)) {
        point_types[channel_pts_between_bottoms[channel_pts_between_bottoms_and_at_bottom]] <- "bottom"
      }
  
  }
 
  # Check if left_bank, channel, and right_bank are consecutive and in correct order
  return(
    point_types
  )
}

# -----------------------------------------------------------------------------------------

# classify_pts_tmp = function(
#     cs_pts, 
#     crosswalk_id = NULL,
#     pct_of_length_for_relief = 0.01
#     ){
  
#   . <-  L <-  L1 <-  L2  <-  R  <-  R1 <-  R2  <- Z  <-  Z2 <-  anchor <-  b1  <- b2  <- cs_lengthm  <- count_left <- 
#     count_right  <-  cs_id <-  hy_id <-  in_channel_pts  <- lengthm <-  low_pt  <- max_bottom  <- mean_dist <-  mid_bottom  <- min_bottom  <- pt_id <- relative_distance <-  third <- NULL

#     classified_pts = 
#       cs_pts %>%
#       dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
#       # dplyr::group_by(hy_id, cs_id) %>%
#       dplyr::mutate(
#           third          = calculate_third(dplyr::n()),
#           mean_dist      = calculate_mean_dist(relative_distance),
#           in_channel_pts = calculate_in_channel_pts(cs_lengthm[1], mean_dist),
#           b1             = calculate_b1(in_channel_pts),
#           b2             = calculate_b2(in_channel_pts, b1),
#           low_pt         = find_low_pt(Z, third[1]),
#           class          = classify_initial(Z, low_pt, pt_id, third[1]),
#           # Z2             = smooth_depth(Z, class)
#           Z2             = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()]),
#           Z              = ifelse(class == "bottom", Z, Z2),
#           min_bottom     = find_min_bottom(class),
#           mid_bottom     = find_mid_bottom(class),
#           max_bottom     = find_max_bottom(class),
#           L1             = calculate_L1(mid_bottom, b1),
#           L2             = calculate_L2(mid_bottom, b2),
#           R1             = calculate_R1(mid_bottom, b2, dplyr::n()),
#           R2             = calculate_R2(mid_bottom, b1, dplyr::n()),
#           anchor         = determine_anchor(Z, L1, R2),
#           L              = adjust_L(third, anchor, L1, L2),
#           R              = adjust_R(third, anchor, R1, R2),
#           count_left     = calculate_count_left(min_bottom, L),
#           count_right    = calculate_count_right(R, max_bottom),
#           # boundaries     = adjust_boundaries(L, count_left, count_right, R)
#           L              = ifelse(count_left == 0, L - count_right, L),
#           R              = ifelse(count_right == 0, R + count_left, R),
#           class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
#           class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
#           class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)
#           # L              = boundaries$L,
#           # R              = boundaries$R,
#           # class          = final_classify(pt_id, L[1], R[1], class)
#       ) 
# }


# calculate_third <- function(n) {
#   ceiling(n / 3)
# }

# calculate_mean_dist <- function(relative_distance) {
#   mean(diff(relative_distance))
# }

# calculate_in_channel_pts <- function(cs_lengthm, mean_dist) {
#   ceiling(cs_lengthm / mean_dist)
# }

# calculate_b1 <- function(in_channel_pts) {
#   ceiling(in_channel_pts / 2)
# }

# calculate_b2 <- function(in_channel_pts, b1) {
#   in_channel_pts - b1
# }

# find_low_pt <- function(Z, third) {
#   min(Z[third:(2*third - 1)])
# }

# classify_initial <- function(Z, low_pt, pt_id, third) {
#   ifelse(Z <= low_pt & dplyr::between(pt_id, third, (2*third - 1)), 
#          "bottom", 
#          "bank")
# }

# smooth_depth <- function(Z, class) {
#   if (class == "bottom") {
#     Z
#   } else {
#     c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()])
#   }
# }

# find_min_bottom <- function(class) {
#   which(class == "bottom")[1]
# }

# find_mid_bottom <- function(class) {
#   which(class == "bottom")[ceiling(length(which(class == "bottom"))/2)]
# }

# find_max_bottom <- function(class) {
#   which(class == "bottom")[length(which(class == "bottom"))]
# }

# calculate_L1 <- function(mid_bottom, b1) {
#   pmax(1, mid_bottom - b1)
# }

# calculate_L2 <- function(mid_bottom, b2) {
#   pmax(1, mid_bottom - b2)
# }

# calculate_R1 <- function(mid_bottom, b2, n) {
#   pmin(mid_bottom + b2, n)
# }

# calculate_R2 <- function(mid_bottom, b1, n) {
#   pmin(mid_bottom + b1, n)
# }

# determine_anchor <- function(Z, L1, R2) {
#   ifelse(Z[R2] < Z[L1], 2, 1)
# }

# adjust_L <- function(third, anchor, L1, L2) {
#   pmax(third, ifelse(anchor == 1, L1, L2))
# }

# adjust_R <- function(third, anchor, R1, R2) {
#   pmin(2*third, ifelse(anchor == 1, R1, R2))
# }

# calculate_count_left <- function(min_bottom, L) {
#   min_bottom - L
# }

# calculate_count_right <- function(R, max_bottom) {
#   R - max_bottom
# }

# # adjjust the boundaries if there are no points to the left or right
# adjust_boundaries <- function(L, count_left, count_right, R) {
#   L <- ifelse(count_left == 0, L - count_right, L)
#   R <- ifelse(count_right == 0, R + count_left, R)
#   return(list(L = L, R = R))
# }

# # final classification of points as "channel", "left_bank", or "right_bank"
# final_classify <- function(pt_id, L, R, class) {
#   class <- ifelse(dplyr::between(pt_id, L, R) & class != 'bottom', "channel", class)
#   class <- ifelse(class == 'bank' & pt_id <= L, "left_bank", class)
#   class <- ifelse(class == 'bank' & pt_id >= R, "right_bank", class)
#   return(class)
# }

# -----------------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------------
# ---- OLD FUNCTION VERSIONS ----
# TODO: DELETE
# -----------------------------------------------------------------------------------------

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
cross_section_pts2 = function(
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
add_points_per_cs2 <- function(cs,
                               points_per_cs  = NULL,
                               min_pts_per_cs = 10,
                               dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
) {
  
  # TODO: also check that the input 'cs' is an sf dataframe with LINESTRINGS / MULT
  REQUIRED_COLS <- c("cs_lengthm")
  
  if (!all(REQUIRED_COLS %in% names(cs))) {
    
    missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(cs))]
    
    stop("'cs' is missing one or more of the required columns:\n > ", 
         paste0(missing_cols, collapse = "\n > "))
  }
  
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
extract_dem_values2 <- function(cs, dem) {
  
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

#' Given a set of linestrings, extract DEM values at points along the linestring
#'
#' @param cs cross section sf object
#' @param crosswalk_id character, column name of unique flowline / transect ID
#' @param dem SpatRaster DEM or character pointing to remote DEM resource
#' @importFrom dplyr mutate group_by n ungroup select everything across any_of
#' @importFrom sf st_set_geometry st_line_sample st_cast
#' @importFrom terra extract project vect crs rast
#' @return sf dataframe with Z values extracted from DEM
extract_dem_values3 <- function(cs, crosswalk_id = NULL, dem = NULL) {
  
  extract_pt_val <- function(rast, pts) {
    terra::extract(
      rast,
      terra::project(terra::vect(pts), terra::crs(rast))
    )[, 2]
  }
  
  # TODO: not sure if this is the best way to do this, we just want it so if 
  # TODO: you dont specify an ID (or dont have an ID), then we autogenerate one
  # default NULL crosswalk_id to the default 'hydrofabric_id' 
  if(is.null(crosswalk_id)) {
    # net <- add_hydrofabric_id(net) 
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  # TODO: also check that the input 'cs' is an sf dataframe with LINESTRINGS / MULT
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "points_per_cs", "cs_lengthm")
  
  # if (!all(names(cs) %in% REQUIRED_COLS)) {
  #   stop("'cs' is missing one or more of the required columns:\n > ", 
  #        paste0(REQUIRED_COLS, collapse = "\n > "))
  # }
  # 
  if (!all(REQUIRED_COLS %in% names(cs))) {
    
    missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(cs))]
    
    stop("'cs' is missing one or more of the required columns:\n > ", 
         paste0(missing_cols, collapse = "\n > "))
  }
  
  suppressWarnings({
    cs_pts <- 
      sf::st_set_geometry(
        cs, 
        sf::st_line_sample(cs, cs$points_per_cs)
        ) %>% 
      sf::st_cast("POINT") %>%
      dplyr::mutate(Z = extract_pt_val(terra::rast(dem), .)) %>% 
      dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
      # dplyr::group_by(hy_id, cs_id) %>% 
      dplyr::mutate(
        pt_id             = 1:dplyr::n(),
        relative_distance = seq(from = 0, to = cs_lengthm[1], length.out = dplyr::n())
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(
        dplyr::any_of(crosswalk_id),
        cs_id, 
        pt_id, 
        Z, 
        cs_lengthm, 
        relative_distance, 
        dplyr::everything()
        )
    # dplyr::select(dplyr::any_of(cols_to_select))
  })
  
  return(cs_pts)
  
}


#' Classify Cross Section Points (v1 deprecated)
#' @param cs_pts CS points, output of hydrofabric3D::cross_section_pts()
#' @param crosswalk_id character, ID column in cs_pts
#' @param pct_of_length_for_relief numeric, percent of cross section length (cs_lengthm) to use as the 
#' threshold depth for classifying whether a cross section has "relief". If a cross section has at least X% of its length in depth, 
#' then it is classified as "having relief" (i.e. has_relief = TRUE). Value must be non negative number (greater than or equal to 0). 
#' Default is 0.01 (1% of the cross sections length).
#' @return sf object
#' @importFrom dplyr filter group_by mutate ungroup select between n left_join
#' @importFrom zoo rollmean
classify_points2 <- function(
    cs_pts, 
    crosswalk_id = NULL,
    pct_of_length_for_relief = 0.01
){
  
  . <-  L <-  L1 <-  L2  <-  R  <-  R1 <-  R2  <- Z  <-  Z2 <-  anchor <-  b1  <- b2  <- cs_lengthm  <- count_left <- 
    count_right  <-  cs_id <-  hy_id <-  in_channel_pts  <- lengthm <-  low_pt  <- max_bottom  <- mean_dist <-  mid_bottom  <- min_bottom  <- pt_id <- relative_distance <-  third <- NULL
  # TODO: maybe relief_to_length_ratio is more intuitive than pct_of_length_for_relief ????
  # relief_to_length_ratio = 0.01
  
  # cs_pts2 <- cs_pts
  # crosswalk_id = "hy_id"
  # pct_of_length_for_relief = 0.01
  
  # make a unique ID if one is not given (NULL 'crosswalk_id')
  if(is.null(crosswalk_id)) {
    # cs  <- add_hydrofabric_id(cs) 
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "cs_lengthm", "relative_distance")
  # REQUIRED_COLS <- c(crosswalk_id, "cs_id")
  
  if (!all(REQUIRED_COLS %in% names(cs_pts))) {
    
    missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(cs_pts))]
    
    stop("'cs_pts' is missing one or more of the required columns:\n > ", 
         paste0(missing_cols, collapse = "\n > "))
  }
  
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
  req_cols       <- c(crosswalk_id, "cs_id", "pt_id", "Z", "relative_distance", 
                      "cs_lengthm", "class", "point_type")
  
  # any starting columns in the original data 
  starting_cols  <- names(cs_pts)
  
  # name and order of columns to select with
  cols_to_select <- c(req_cols, starting_cols[!starting_cols %in% req_cols])
  
  # create classifications for points
  classified_pts <-
    dplyr::filter(cs_pts) %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::group_by(hy_id, cs_id) %>%
    dplyr::mutate(
      third          = ceiling(dplyr::n() / 3),
      mean_dist      = mean(diff(relative_distance)),
      in_channel_pts = ceiling(cs_lengthm[1] / mean_dist),
      b1             = ceiling(in_channel_pts / 2),
      b2             = in_channel_pts - b1,
      low_pt         = min(Z[third[1]:(2*third[1] - 1)]),
      class          = ifelse(Z <= low_pt & dplyr::between(pt_id, third[1], (2*third[1] - 1)), 
                              "bottom", 
                              "bank"
      ),
      Z2             = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()]),
      Z              = ifelse(class == "bottom", Z, Z2),
      min_bottom     = which(class == "bottom")[1],
      mid_bottom     = which(class == "bottom")[ceiling(length(which(class == "bottom"))/2)],
      max_bottom     = which(class == "bottom")[length(which(class == "bottom"))],
      L1             = pmax(1, mid_bottom - b1),
      L2             = pmax(1, mid_bottom - b2),
      R1             = pmin(mid_bottom + b2, n()),
      R2             = pmin(mid_bottom + b1, n()),
      anchor         = ifelse(Z[R2] < Z[L1], 2, 1),
      L              = pmax(third, ifelse(anchor == 1, L1, L2)),
      R              = pmin(2*third[1], ifelse(anchor == 1, R1, R2)),
      count_left     = min_bottom - L,
      count_right    = R - max_bottom,
      L              = ifelse(count_left == 0, L - count_right, L),
      R              = ifelse(count_right == 0, R + count_left, R),
      class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
      class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
      class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(point_type = class) %>% 
    dplyr::select(dplyr::any_of(cols_to_select))
  # dplyr::select(dplyr::all_of(cols_to_select))      # Stricter, requires ALL of the columns to be present or it will throw an error
  
  # classified_pts[cols_to_select]                       # Another method for selecting columns....
  
  # get bank validity attributes for each hy_id/cs_id
  # - Uses the count of point types per cross section and checks Z to make sure that a "bottom" point is
  #   in each cross section and each "bottom" point has a valid left and right bank)
  bank_validity_df <- get_bank_attributes(classified_pts, crosswalk_id)
  
  # classified_pts %>%
  # dplyr::filter(hy_id %in% c('wb-1003260')) %>%
  # hydrofabric3D::plot_cs_pts(color = "point_type")
  
  # # Or add bank attributes 
  # banked_pts <- add_bank_attributes(output_pts)
  
  # get relief data, determine if a cross section has relief within X% percentage of the cross sections length
  relief_df <- get_relief(
    classified_pts, 
    crosswalk_id             = crosswalk_id,
    pct_of_length_for_relief = pct_of_length_for_relief, 
    detailed                 = FALSE
  )
  
  # join the bank validity attributes with the relief values
  validity_checks <- dplyr::left_join(
    bank_validity_df, 
    relief_df, 
    by = c(crosswalk_id, "cs_id")  
    # by = c("hy_id", "cs_id")
  )
  
  # join the new validity check values to the classified points
  classified_pts <- 
    classified_pts %>% 
    dplyr::left_join(
      validity_checks,
      by = c(crosswalk_id, "cs_id")  
      # by = c("hy_id", "cs_id")
    ) 
  
  # move the geometry column to the last column (if one exists)
  classified_pts <- move_geometry_to_last(classified_pts)
  
  return(classified_pts)
  
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
classify_points3 <- function(
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
  req_cols       <- c("hy_id", "cs_id", "pt_id", "Z", "relative_distance", "cs_lengthm", 
                      "class", "point_type")
  
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

#' Classify Cross Section Points v1 (Deprecated version)
#' @param cs_pts CS points
#' @return sf object
#' @importFrom dplyr filter group_by mutate ungroup select between n
#' @importFrom zoo rollmean
classify_points4 <- function(cs_pts){
  
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

# #' Get Points across transects with elevation values
# #' @param cs character, Hydrographic LINESTRING Network file path
# #' @param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
# #' @param min_pts_per_cs Minimun number of points per cross section required.
# #' @param dem the DEM to extract data from
# #' @param scale numeric, If a transect line DEM extraction results in all equal Z values,
# #'  by what percent of the transect lines length (meters) should the transect line be
# #'   extended in both directions to try to capture representative Z values ? Default is 0.5 (50% of the transect length)
# #' @return sf object
# #' @importFrom dplyr mutate group_by ungroup n select everything relocate last_col bind_rows filter
# #' @importFrom terra linearUnits res rast extract project vect crs 
# #' @importFrom sf st_line_sample st_set_geometry st_cast
# #' @export
# cross_section_pts_v3 = function(
#     cs             = NULL,
#     points_per_cs  = NULL,
#     min_pts_per_cs = 10,
#     dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
#     scale          = 0.5
# ){
  
#   # check if a cross section is given, and return NULL if missing
#   if (is.null(cs)) {
#     return(NULL)
#   }
  
#   # check if a file path or not
#   if(is.character(cs)) {
#     # Read in file
#     cs <- sf::read_sf(cs)
#   }
  
#   # add points per cross sections 
#   cs <- add_points_per_cs(
#     cs             = cs,
#     points_per_cs  = points_per_cs,
#     min_pts_per_cs = min_pts_per_cs,
#     dem            = dem
#   )
  
  
#   # Extract DEM "Z" values for each point along cross section linestrings
#   cs_pts <- extract_dem_values(cs = cs, dem = dem)
  
#   # check for any flat cross sections (All Z values are equal within a given cross section)
#   # flat_cs <- check_z_values(pts = cs_pts, threshold = 0)
#   flat_cs <- check_z_values(pts = cs_pts, threshold = 0.5)
  
#   # if there are no flatlines, return the cs_pts object
#   if (nrow(flat_cs) == 0) {
    
#     cs_pts <- 
#       cs_pts %>% 
#       dplyr::mutate(
#         is_extended = FALSE
#       ) %>% 
#       dplyr::relocate(geom, .after = dplyr::last_col())
    
#     return(cs_pts)
    
#   }
  
#   # subset transects (cs) to the flat cross sections in flat_cs
#   to_extend <- 
#     cs %>% 
#     dplyr::mutate(
#       tmp_id = paste0(hy_id, "_", cs_id)
#     ) %>% 
#     dplyr::filter(tmp_id %in% unique(
#       dplyr::mutate(flat_cs,
#                     tmp_id = paste0(hy_id, "_", cs_id))$tmp_id
#     )
#     ) %>% 
#     dplyr::select(-tmp_id)
  
#   # dplyr::mutate(extend_by = scale * cs_lengthm)
#   # extend linestring geometries by a percent of linestring length
#   extended <- extend_by_percent(x = to_extend, pct = scale, length_col = "cs_lengthm")
  
#   # mapview::mapview(cs, color = "dodgerblue") +  
#   # mapview::mapview(extended, color = "red") +  
#   #   mapview::mapview(to_extend, color = "green")
#   # 
#   # add cross section points to extended cross sections
#   extended <- add_points_per_cs(
#     cs             = extended,
#     points_per_cs  = points_per_cs,
#     min_pts_per_cs = min_pts_per_cs,
#     dem            = dem
#   )
  
#   # extended <- add_points_per_cs(cs = extended, dem = dem,  points_per_cs = NULL, min_pts_per_cs = 10)
  
#   # extract DEM values for newly extended cross sections
#   extended_pts <- extract_dem_values(cs = extended, dem = dem)
  
#   # take the below points, and put them back into "cs_pts" object
#   # then go back to the input "transects" ("cs") object and update the transect geometries based on the extensions done above^^
#   # then resave the input transects dataset back to its original location....
#   extended_pts <- 
#     extended_pts %>% 
#     # sf::st_drop_geometry() %>% 
#     # dplyr::select(hy_id, cs_id, Z) %>%
#     dplyr::group_by(hy_id, cs_id) %>% 
#     dplyr::mutate(
#       is_same_Z = max(Z) - min(Z) <= 0
#       # is_same_Z = dplyr::n_distinct(Z) == 1,
#     ) %>% 
#     dplyr::ungroup() %>%    
#     dplyr::mutate(
#       tmp_id = paste0(hy_id, "_", cs_id)
#     )
  
#   # separate newly extended cross sections with new Z values into groups (those that show "good" DEM values after extension are kept) 
#   to_keep <- dplyr::filter(extended_pts, !is_same_Z)
#   to_drop <- dplyr::filter(extended_pts, is_same_Z)
  
#   # filter out cross section points that have "same Z" values (remove flat Z values)
#   final_pts <-
#     cs_pts %>%  
#     dplyr::mutate(
#       tmp_id = paste0(hy_id, "_", cs_id)
#     ) %>% 
#     dplyr::filter(
#       !tmp_id %in% unique(to_drop$tmp_id)
#       # !tmp_id %in% unique(paste0(to_drop$hy_id, "_", to_drop$cs_id))
#     ) 
  
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
#     dplyr::select(-tmp_id) %>% 
#     dplyr::relocate(geom, .after = dplyr::last_col())
  
#   return(final_pts)
  
#   # tmp %>% 
#   #   ggplot2::ggplot() +
#   #   ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z,color = is_same_Z)) +
#   #   ggplot2::facet_wrap(~cs_id)
  
# }

# #' Get Points across transects with elevation values
# #' @param cs Hydrographic LINESTRING Network
# #' @param points_per_cs  the desired number of points per CS. If NULL, then approximently 1 per grid cell resultion of DEM is selected.
# #' @param min_pts_per_cs Minimun number of points per cross section required.
# #' @param dem the DEM to extract data from
# #' @return sf object
# #' @importFrom dplyr mutate group_by ungroup n select everything
# #' @importFrom terra linearUnits res rast extract project vect crs 
# #' @importFrom sf st_line_sample st_set_geometry st_cast
# #' @export
# cross_section_pts_original = function(cs,
#                                 points_per_cs = NULL,
#                                 min_pts_per_cs = 10,
#                                 dem = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"){
  
#   # check if a cross section is given, and return NULL if missing
#   if (is.null(cs)) {
#     return(NULL)
#   }
  
#   # IF NULL value is given to points_per_cs argument, calculate points_per_cs values
#   # - IF DEM has a longitude/latitude CRS (terra::linearUnits == 0):
#   # -- then divide the cross section length by 111139 and divide that resulting value by the minimum resolution value from the DEM (then round the result up)
#   # - ELSE: 
#   # -- just divide the cross section length by the minimum resolution value from the DEM (then round the result up)
#   if (is.null(points_per_cs)) {
#     if (terra::linearUnits(terra::rast(dem)) == 0) {
#       points_per_cs = ceiling(
#         (cs$lengthm / 111139) / min(terra::res(terra::rast(dem)))
#       )
#     } else {
#       points_per_cs = ceiling(
#         (cs$lengthm) / min(terra::res(terra::rast(dem)))
#       )
#     }
#   }
  
#   # take the max between the given minimum points per cross section and the derived points per cross section
#   cs$points_per_cs = pmax(min_pts_per_cs, points_per_cs)
  
#   # function to extract Z/elevation values at a point from DEM
#   extract_pt_val = function(rast, pts){ 
#     terra::extract(rast, 
#                    terra::project(terra::vect(pts), 
#                                   terra::crs(rast))
#     )[, 2] 
#   }
  
#   suppressWarnings({
    
#     return(
#       sf::st_set_geometry(cs, sf::st_line_sample(cs, cs$points_per_cs)) %>% 
#         sf::st_cast("POINT") %>%
#         dplyr::mutate(Z = extract_pt_val(terra::rast(dem), .)) %>% 
#         dplyr::group_by(hy_id, cs_id) %>% 
#         dplyr::mutate(
#           pt_id             = 1:dplyr::n(),
#           relative_distance = seq(from = 0, to = lengthm[1], length.out = dplyr::n())
#         ) %>% 
#         dplyr::ungroup() %>% 
#         dplyr::select(hy_id, cs_id, pt_id, Z, lengthm, relative_distance, dplyr::everything())
#     )
    
#   })
  
# }

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