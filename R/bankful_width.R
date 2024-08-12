
#' Calculates the powerlaw derived Bankful width given total drainage area (square kilometers)
#' @param total_drainage_area_sqkm 
#' @return numeric, bankful width estimate
#' @export
calc_powerlaw_bankful_width <- function(total_drainage_area_sqkm) {
  
  # Check if 'total_drainage_area_sqkm' is numeric or a numeric vector
  if (!is.numeric(total_drainage_area_sqkm)) {
    stop("'total_drainage_area_sqkm' must be a numeric")
  }
  
  return(exp(0.700    + 0.365* log(total_drainage_area_sqkm)))
  
}
