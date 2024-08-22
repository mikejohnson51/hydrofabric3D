#' @title Extend an sf linestring dataframe by a percent of the lines length
#'
#' @param x linestring sf dataframe
#' @param pct numeric, percent of line to extend linestring by in both directions
#' @param length_col character, name of the column in "x" that has the length of the linestring (meters)
#' @importFrom dplyr group_by mutate ungroup rename
#' @importFrom sf st_length st_geometry st_drop_geometry st_as_sf st_crs
#' @importFrom nhdplusTools rename_geometry
#' @return sf dataframe with extended linestring geometries
extend_by_percent <- function(
    x, 
    pct        = 0.5, 
    length_col = NULL
) {
  
  # rename the geometry to "geom"
  x <- nhdplusTools::rename_geometry(x, "geom")
  
  # length_col is NULL then set it to "cs_lengthm"
  if(is.null(length_col)) {
    length_col = "cs_lengthm"
  }
  
  #  if the length_col string is not a column in the x,
  # then create a column based on the length of the linestring using "length_col" as name of column 
  if (!length_col %in% names(x)) {
    
    # add a "length_col" column of the length of each linestring in meters
    x[length_col] <- as.numeric(sf::st_length(sf::st_geometry(x)))
    # x <- dplyr::mutate(x,  length_col = as.numeric(sf::st_length(.)))
  }
  
  # extend linestrings by pct * length of line
  extended_df <-
    x %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      extended_geom = geos_extend_line(
        geom, 
        distance = (
          ((pct)*(!!dplyr::sym(length_col))) / 2
        ),
        # distance = (pct)*(!!dplyr::sym(length_col)),
        dir      = "both"
      ) 
    ) %>% 
    dplyr::ungroup()
  
  # drop original geometry column
  extended_df <-  sf::st_drop_geometry(extended_df)
  
  # set the extended geometry as the new geometry
  extended_df$extended_geom <- sf::st_geometry(sf::st_as_sf(extended_df$extended_geom))
  
  # make extended_df an sf object
  extended_df <- sf::st_as_sf(
    extended_df, 
    crs = sf::st_crs(x)
  )
  
  # rename "extended_geom" col to "geom"
  extended_df <- dplyr::rename(extended_df, "geom" = "extended_geom")
  
  # recalculate length of linestring and update length_col value
  extended_df[[length_col]] <- as.numeric(sf::st_length(extended_df$geom))
  
  return(extended_df)
  
}

#' @title Extend an sf linestring dataframe by a specified lengths vector
#'
#' @param x linestring sf dataframe
#' @param length_vector numeric, vector of length 'x' representing the number of meters to extend 'x' from both directions (i.e. 10 means the linestring will be extended 10m from both ends of the line)
#' @param length_col character, name of the column in "x" that has the length of the linestring (meters)
#' @importFrom dplyr group_by mutate ungroup rename
#' @importFrom sf st_length st_geometry st_drop_geometry st_as_sf st_crs
#' @importFrom nhdplusTools rename_geometry
#' @return sf dataframe with extended linestring geometries
extend_by_length <- function(
    x, 
    length_vector,
    length_col = NULL
) {
  
  # rename the geometry to "geom"
  x <- nhdplusTools::rename_geometry(x, "geom")
  
  # length_col is NULL then set it to "cs_lengthm"
  if(is.null(length_col)) {
    length_col = "cs_lengthm"
  }
  
  #  if the length_col string is not a column in the x,
  # then create a column based on the length of the linestring using "length_col" as name of column 
  if (!length_col %in% names(x)) {
    
    # add a "length_col" column of the length of each linestring in meters
    x[length_col] <- as.numeric(sf::st_length(sf::st_geometry(x)))
    # x <- dplyr::mutate(x,  length_col = as.numeric(sf::st_length(.)))
  }
  
  # TODO: this needs a check to make sure a column with this name does NOT already exist
  # add length vector col to extended lines out by in next step
  x$length_vector_col <- length_vector
  
  # extend linestrings by pct * length of line
  extended_df <-
    x %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      extended_geom = geos_extend_line(
        geom, 
        distance = length_vector_col,
        # distance = (pct)*(!!dplyr::sym(length_col)),
        dir      = "both"
      ) 
    ) %>% 
    dplyr::ungroup()
  
  # drop original geometry column
  extended_df <-  sf::st_drop_geometry(extended_df)
  
  # set the extended geometry as the new geometry
  extended_df$extended_geom <- sf::st_geometry(sf::st_as_sf(extended_df$extended_geom))
  
  # make extended_df an sf object
  extended_df <- sf::st_as_sf(
    extended_df, 
    crs = sf::st_crs(x)
  )
  
  # rename "extended_geom" col to "geom"
  extended_df <- dplyr::rename(extended_df, "geom" = "extended_geom")
  
  # recalculate length of linestring and update length_col value
  extended_df[[length_col]] <- as.numeric(sf::st_length(extended_df$geom))
  
  # drop the added length_vector_col
  extended_df <- dplyr::select(
    extended_df, 
    -length_vector_col
  )
  
  return(extended_df)
  
}