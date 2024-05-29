# Given 2 geos_geometry point geometries, create a line between the 2 points
# start: geos_geoemtry, point
# end: geos_geoemtry, point
# Returns geos_geometry linestring

#' Given 2 geos_geometry point geometries, create a line between the 2 points
#'
#' @param start geos_geoemtry, point
#' @param end geos_geoemtry, point
#' @param line_crs crs
#' @importFrom geos geos_y geos_x geos_make_linestring
#' @return geos_geometry linestring
make_line_from_start_and_end_pts <- function(start, end, line_crs) {
  Y_start <- geos::geos_y(start)
  X_start <- geos::geos_x(start)
  Y_end   <- geos::geos_y(end)
  X_end   <- geos::geos_x(end)
  
  # make the new transect line from the start and points 
  geos_ls <- geos::geos_make_linestring(x = c(X_start, X_end),
                                        y = c(Y_start, Y_end), 
                                        crs = line_crs)
  
  return(geos_ls)                                              
  
  
}

#' Check if an updated transect line is valid relative to the other transects and flowlines in the network
#' The 'transect_to_check' should be 'used' (i.e. function returns TRUE) if 
#' the 'transect_to_check' does NOT interesect any other transects ('transect_lines') AND it only intersects a single flowline ONCE.
#' If the 'transect_to_check' intersects ANY other transects OR intersects a flowline more
#' than once (OR more than one flowline in the network) then the function returns FALSE.
#' @param transect_to_check geos_geometry, linestring
#' @param transect_lines geos_geometry, linestring
#' @param flowlines geos_geometry, linestring
#'
#' @return TRUE if the extension should be used, FALSE if it shouldn't be used
#' @importFrom geos geos_intersection geos_type geos_intersects
is_valid_transect_line <- function(transect_to_check, transect_lines, flowlines) {
  
  # ###   ##   ##   ##   ##   ##   ##   ##   ##   ##  
  # extension_line <- left_extended_trans
  # transect_lines <- transect_geoms
  # flowlines <- flines_geos
  # ###   ##   ##   ##   ##   ##   ##   ##   ##   ##  
  
  # Define conditions to decide which version of the transect to use
  
  # 1. Use transect with extension in BOTH directions
  # 2. Use transect with LEFT extension only
  # 3. Use transect with RIGHT extension only
  
  # Check that the extended transect lines only intersect a single flowline in the network only ONCE
  intersects_with_flowlines <- geos::geos_intersection(
    transect_to_check,
    flowlines
  )
  intersects_flowline_only_once <- sum(geos::geos_type(intersects_with_flowlines) == "point") == 1 && 
    sum(geos::geos_type(intersects_with_flowlines) == "multipoint") == 0 
  
  # check that the extended transect line does NOT intersect other transect lines (other than SELF)
  intersects_other_transects <- sum(geos::geos_intersects(transect_to_check, transect_lines)) > 1
  
  # TRUE == Only one flowline is intersected a single time AND no other transect lines are intersected
  use_transect <- intersects_flowline_only_once  && !intersects_other_transects
  
  return(use_transect)
}