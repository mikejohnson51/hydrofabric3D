
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

#' Add powerlaw_bankful_width column
#' @param total_drainage_area_sqkm_col 
#' @return character, column with the total downstrream drainage area in square kilometers (numeric column)
#' @export
add_powerlaw_bankful_width <- function(df, total_drainage_area_sqkm_col, min_bf_width) {
  
  # df <- flowlines
  # total_drainage_area_sqkm_col = "tot_drainage_areasqkm"
  # MIN_BF_WIDTH <- 50
  
  # Check if 'total_drainage_area_sqkm' is numeric or a numeric vector
  if (!is.character(total_drainage_area_sqkm_col)) {
    stop("'total_drainage_area_sqkm_col' must be a character")
  }
  
  if (!total_drainage_area_sqkm_col %in% names(df)) {
    stop("'total_drainage_area_sqkm_col' ", total_drainage_area_sqkm_col, " is not a column in input 'df'")
  }
  if(!is.numeric(df[[total_drainage_area_sqkm_col]])) {
     stop("'total_drainage_area_sqkm_col' ", total_drainage_area_sqkm_col, " must be a numeric column in input 'df'")
  }
  
  # df[1, ][[total_drainage_area_sqkm_col]] <- NA
  # df[[total_drainage_area_sqkm_col]]
  
  # fill any NA values with the given default Bankful width value
  df[is.na(df[[total_drainage_area_sqkm_col]]), ][[total_drainage_area_sqkm_col]] <- min_bf_width
  
  bf_widths <- pmax(
                min_bf_width, 
                calc_powerlaw_bankful_width(df[[total_drainage_area_sqkm_col]]) * 11
                )
  
  df['bf_width'] <- bf_widths
   
  return(df)
  
}



# flowlines %>% 
#   dplyr::mutate(
#     bf_width        = hydrofabric3D::calc_powerlaw_bankful_width(tot_drainage_areasqkm),
#     # bf_width        = hydrofabric3D::calc_powerlaw_bankful_width(tot_drainage_areasqkm),
#     # bf_width        = pmax(50, bf_width * 2)
#     bf_width        = pmax(50, bf_width * 11)
#   ) %>% 
#   dplyr::select(
#     hy_id = id, 
#     # tot_drainage_areasqkm, 
#     bf_width,
#     # input_bf_width,
#     geometry = geom
#   ) 