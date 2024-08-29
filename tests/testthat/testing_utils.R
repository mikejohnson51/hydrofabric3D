# File containing a bunch of utility functions used for testing 
# - checking correct column outputs
# - checking SF characteristics (geometry type, CRS, etc)
#  

# library(hydrofabric3D)


prep_flowlines_for_transect_cuts <- function(flines, id_col, min_bf_width) {

  flines <- 
    flines %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", min_bf_width) %>%  
    dplyr::rename(!!sym(id_col) := id) %>% 
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
relief_detailed_has_min_output_cols <- function(relief, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
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
relief_has_min_output_cols <- function(relief, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id" ,
                     "has_relief"
  )
  
  return(
    all(expected_cols %in% names(relief)) && length(expected_cols) == length(names(relief))
  )
}


has_same_unique_tmp_ids <- function(x, y, id = "hydrofabric_id") {
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  start_ids <- hydrofabric3D:::get_unique_tmp_ids(df = x, x = get(id))
  end_ids   <- hydrofabric3D:::get_unique_tmp_ids(df = y, x = get(id))
  
  # all IDs are in x AND y and same number of ids
  same_unique_ids <- all(start_ids %in% end_ids) && all(end_ids %in% start_ids) && length(start_ids) == length(end_ids)
  
  return(same_unique_ids)
}

# TODO: this is a better named version of check_cs_pts_has_exact_cols() (DUPLICATE)
bank_attrs_has_min_output_cols <- function(bank_attrs, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
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
classified_cs_pts_has_min_output_cols <- function(classified_pts, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "class", "point_type", 
                     "points_per_cs", 
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
cs_pts_has_min_output_cols <- function(cs_pts, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
  )
  
  return(
    all(expected_cols %in% names(cs_pts)) && length(expected_cols) == length(names(cs_pts))
  )
}

check_cs_pts_has_exact_cols <- function(cs_pts, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
                     )
  
  return(
    all(expected_cols %in% names(cs_pts)) && length(expected_cols) == length(names(cs_pts))
  )
}


# TODO: this is a better named version of check_cs_pts_and_transect_cols() (DUPLICATE)
cs_pts_has_correct_cols_from_transects <- function(cs_pts, transects, id = "hydrofabric_id") {
  # id = "hy_id"
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
  )
  
  return(
    all(unique(c(expected_cols, names(transects))) %in% names(cs_pts))
  )
}

check_cs_pts_and_transect_cols <- function(cs_pts, transects, id = "hydrofabric_id") {
  # id = "hy_id"
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
                     "cs_id","pt_id", "Z", "cs_lengthm", 
                     "relative_distance", "points_per_cs", "geometry"
                     )
  
  return(
    all(unique(c(expected_cols, names(transects))) %in% names(cs_pts))
  )
}

check_cs_pts_has_required_cols <- function(transects, id = "hydrofabric_id") {
  
  if(is.null(id)) {
    id = "hydrofabric_id"
  }
  
  expected_cols <- c(id, 
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