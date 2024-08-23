# File containing a bunch of utility functions used for testing 
# - checking correct column outputs
# - checking SF characteristics (geometry type, CRS, etc)
#  

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