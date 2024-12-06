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
    "new_cs_lengthm"
  )
)

#' @title Function to add a new "tmp_id" column to a dataframe from 2 other columns
#' @description
#' Internal convenience function for creating a tmp_id column from 2 other columns in a dataframe. 
#' Default is to use hy_id and cs_id columns to create a tmp_id = <hy_id>_<cs_id>.
#' @param df dataframe with x and y as columns
#' @param x character, column name in df to make up the first part of the added tmp_id column (tmp_id = x_y). Default is "hy_id." 
#' @param y character, column name in df to make up the second part of the added tmp_id column (tmp_id = x_y). Default is "cs_id."
#' 
#' @return The input dataframe with the "tmp_id" column added.
#' 
#' @export
add_tmp_id <- function(df, x = "hy_id", y = "cs_id") {
  # # Create the "tmp_id" column by concatenating values from "x" and "y"
  
  # first try to add the tmp_id as if 'x' and 'y' are characters
  # if that fails, then use 'x' and 'y' as tidyselectors in dplyr::mutate()
    tmp_ids = paste0(df[[x]], "_", df[[y]])
    df$tmp_id = tmp_ids

  return(df)
}

#' @title Get a list of unique tmp_ids in a dataframe 
#' @description
#' Dataframe can have "tmp_id" column already or the columns can be specified with 'x' and 'y' arguments
#' 
#' @param df dataframe with x and y as columns, with an optional "tmp_id" column, otherwise a tmp_id will be created from x_y
#' @param x The name of the column in df to make up the first part of the added tmp_id column (tmp_id = x_y). Default is hy_id.
#' @param y The name of the column in df to make up the second part of the added tmp_id column (tmp_id = x_y). Default is cs_id.
#' 
#' @return character vector of unique "tmp_id" values in the given dataframe 
#' 
#' @export
get_unique_tmp_ids <- function(df, x = "hy_id", y = "cs_id") {
  
  # if no tmp_id exists, add one
  if (!"tmp_id" %in% names(df)) {

    tmp_ids = paste0(df[[x]], "_", df[[y]])
    df$tmp_id = tmp_ids

  }
  
  # get the unique tmp_ids
  unique_tmp_ids <- unique(df$tmp_id)
  
  return(unique_tmp_ids)
  
}

#' Add a unique 'hydrofabric_id` to each row of a dataframe
#' Internal conveniance function for when a dataframe / flowlines network does NOT have a specified ID column
#' @param df sf dataframe, tibble, or dataframe
#' @importFrom dplyr mutate n
#' @return dataframe, sf dataframe, or tibble
#' @export 
add_hydrofabric_id <- function(df) {
  df <- 
    df %>% 
    dplyr::mutate(
      hydrofabric_id = 1:dplyr::n()
    )
  return(df) 
}

#' Add a length column to a sf geometry dataframe
#'
#' @param x sf dataframe
#' @param length_col character, name to use for length column. Default is NULL which will use "geom_length" as the length column name
#' @param add_unit_to_col logical, whether to try and extract units from the geometry and append these to the column name. Default is FALSE 
#'
#' @return sf dataframe 
#' @importFrom sf st_length st_geometry 
#' @export
add_length_col <- function(x, 
                           length_col = NULL, 
                           add_unit_to_col = FALSE
) {
  
  # length_col is NULL then set it to "cs_lengthm"
  if(is.null(length_col)) {
    length_col = "geom_length"
  }
  
  length_vect <- sf::st_length(sf::st_geometry(x))
  
  # attempt to add a unit string to the column name
  if (add_unit_to_col) {
    numerator_unit    <- units(length_vect)$numerator
    numerator_unit    <- if (length(numerator_unit) > 0) { numerator_unit } else { NULL } 
    
    denominator_unit  <- units(length_vect)$denominator
    denominator_unit  <- if (length(denominator_unit) > 0) { denominator_unit } else { NULL }
    
    unit_str   <- paste0(c(numerator_unit, denominator_unit), collapse = "_")
    length_col <- paste0(length_col, unit_str)
  }
  
  if (length_col %in% names(x)) {
    warning("Length column '", length_col, "' is already a column in 'x', and will be overwritten")
  }
  
  # create a column based on the length of the linestring using "length_col" as name of column 
  x[length_col] <- as.numeric(length_vect)
  
  return(x)
  
}

#' Adds a logical 'is_outlet' flag to a set of transects identifying the most downstream transect
#'
#' @param x sf dataframe linestrings
#' @param crosswalk_id character
#' @importFrom dplyr group_by across any_of mutate ungroup row_number
#'
#' @return sf dataframe of transects with added is_outlet logical column
#' @export
add_is_outlet_flag <- function(x, crosswalk_id = NULL) {
  
  is_valid_df <- validate_df(x, c(crosswalk_id, "cs_measure"), "x") 
  
  x <-
    x %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
    dplyr::mutate(
      is_outlet = which.max(cs_measure) == dplyr::row_number()
      # is_outlet = cs_id[which.max(cs_measure)] == cs_id
    ) %>% 
    dplyr::ungroup()
  
  return(x)
  
}

#' Extract the VPU string from part of a string
#'
#' @param s character to extract vpuid out of
#' @param prefix character, default is an empty string
#'
#' @return character vpuid
#' @noRd
#' @keywords internal
extract_vpu <- function(s, prefix = "") {
  regex_pattern <- paste0(prefix, "(\\d+[A-Za-z]?).*")
  return(
    gsub(regex_pattern, "\\1", s)
  )
}

#' @title Move Geometry Column to the last column position
#' @description 
#' Internal utility function for taking a dataframe or an sf dataframe, checks for the existence of a geometry type column, and 
#' if it exists, moves it to the last column. If no geometry column exists, it returns the input dataframe as is.
#' @param df A dataframe or an sf dataframe.
#' @return Returns the input dataframe with the geometry column moved to the last position if it exists. Otherwise, returns the input dataframe as is.
#' @importFrom dplyr relocate all_of last_col
#' @noRd
#' @keywords internal
#' @examples
#' \dontrun{
#' # Create a dataframe
#' df <- data.frame(x = c(1, 2, 3), y = c(4, 5, 6))
#' # Add a geometry column (sf dataframe)
#' df_sf <- sf::st_sf(df, geometry = sf::st_sfc(sf::st_point(c(1, 2, 3))))
#' # move column 
#' df_sf <- dplyr::relocate(df_sf, x, geometry, y)
#' df_sf # geometry column should be move to the middle column
#' # Move geometry column to last position
#' df_sf_moved <- move_geometry_to_last(df_sf)
#' df_sf_moved # geometry column should be move the end column
#' }
move_geometry_to_last <- function(df) {
  # Check if any of the columns in the dataframe are geometry types
  check_for_geom <- sapply(df, function(col) {
    any(class(col) %in% c("sfc", "sfc_GEOMETRY", 
                         "sfc_POINT",  "sfc_MULTIPOINT", 
                          "sfc_LINESTRING", "sfc_MULTILINESTRING", 
                          "sfc_POLYGON",  "sfc_MULTIPOLYGON"
                          ))
  })
  
  # If there is a geometry type column, move it to the last position
  if (any(check_for_geom)) {
    geometry_colname <- names(df)[check_for_geom]
    
    # move geometry column to the end
    df <- 
      df %>% 
      dplyr::relocate(dplyr::all_of(geometry_colname), .after = dplyr::last_col())
  }
  
  return(df)
}

# TODO: probably delete this function given you can just use dplyr::select()....

#' Remove specified columns from a dataframe if they exist.
#'
#' @param df A dataframe.
#' @param columns_to_remove character vector specifying the names of columns to be removed.
#' @return dataframe with specified columns removed if they exist.
remove_cols_from_df <- function(df, columns_to_remove) {
  
  existing_columns <- intersect(columns_to_remove, colnames(df))
  
  # If columns exist, remove them
  if (length(existing_columns) > 0) {
    df <- df[, !colnames(df) %in% existing_columns, drop = FALSE]
  }
  
  return(df)
}

#' Reorder columns in a dataframe
#' Internal helper
#' @param df dataframe or sf dataframe
#' @param start_order character vector of columns to put first in order 
#'
#' @noRd
#' @keywords internal
#' @return dataframe with reordered columns
reorder_cols <- function(df, start_order) {
  
  col_names <- names(df)
  
  # all columns in first_cols exist in the dataframe
  missing_cols <- setdiff(start_order, col_names)
  if (length(missing_cols) > 0) {
    stop("The following columns are missing from the dataframe: ", paste(missing_cols, collapse = ", "))
  }
  
  # the new column order
  col_order <- c(start_order, col_names[!col_names %in% start_order])
  
  # reorder cols
  return(
    df[, col_order]
  )
}


#' Validate that a dataframe is valid type (inherits from a dataframe) and has the given required columns
#'
#' @param x dataframe, sf, or tibble
#' @param cols character vector of columns that are required in 'x'
#' @param obj_name character string, optional name of object to display in error messages. If not obj_name is given, obj_name defaults to 'x'
#'
#' @noRd
#' @keywords internal
#' @return logical, TRUE value if all conditions are met and valid, otherwise an error is raised
validate_df <- function(x, cols, obj_name = NULL) {
  
  if(is.null(obj_name) | !inherits(obj_name, "character")) {
    obj_name <- "x"
  }
  
  if (!inherits(x, "data.frame")) {
    stop("Invalid '", obj_name, "' type of '",  class(x), "' \n '", 
         obj_name ,"' must inherit from 'data.frame', 'tibble', or 'sf'")
  }
  
  if (!all(cols %in% names(x))) {
    
    missing_cols <- cols[which(!cols %in% names(x))]
    
    stop("'", obj_name, "' is missing one or more of the required columns:\n > ", 
         paste0(missing_cols, collapse = "\n > "))
  }
  
  return(TRUE)
  
}

#' Validate all arguments given are of the types specified in type_map
#' Function throws an error if any argument is not specified in the type map or its type does not match the possible types in type_map
#' Internal function for easily type checking all the inputs for a given function
#' @param ... list of args
#' @param type_map list, argument names as the keys and a vector of values representing the valid types
#'
#' @return logical, TRUE if no error, otherwise raises an error 
#' @noRd
#' @keywords internal
validate_arg_types <- function(..., type_map) {
  args <- list(...)
  
  is_empty_args      <- length(args) == 0 
  is_empty_type_map  <- length(type_map) == 0
  
  if (is_empty_args && !is_empty_type_map) {
    stop(paste0("No arguments provided but 'type_map' has ", length(type_map), " provided argument types"))
  }
  
  for (arg_name in names(args)) {
    # message(arg_name)
    
    if (!arg_name %in% names(type_map)) {
      stop(paste0("Unexpected argument: ", arg_name))
    }
    
    expected_types <- type_map[[arg_name]]
    val <- args[[arg_name]]
    
    is_expected_type <- any(unlist(lapply(expected_types, function(i) inherits(args[[arg_name]], i))))
    
    if (!is_expected_type) {
      stop(paste0("Argument '", arg_name, "' must be of type(s) '", 
                  paste0(expected_types, collapse=', '),  
                  "' but got '", class(args[[arg_name]]), "'"))
    }
  }
  
  return(TRUE)
}


#' Validate Inputs for cut_cross_sections Function
#'
#' This function validates the inputs for the cut_cross_sections function to ensure they meet the required criteria.
#'
#' @param net An sf object representing the hydrographic network.
#' @param crosswalk_id A unique identifier column in the network data.
#' @param cs_widths Bankfull widths (length of cross sections) for each network element.
#' @param num Number of transects per network element.
#' @param smooth Logical, whether to smooth linestring geometries or not. 
#' @param densify Numeric, the factor by which to densify the linestrings.
#' @param rm_self_intersect Logical, whether to remove self-intersecting transect linestrings.
#' @param fix_braids Logical, whether to fix braided transect lines or not.
#' @param braid_threshold Numeric, the total length of all flowlines in a braid below which fix_braid_transects should operate.
#' @param braid_method Character, the method to determine the geometries to cut. Options are "comid", "component", or "neighbor". Default is "comid".
#' @param precision Numeric, the number of meters to approximate final cross-section linestring length.
#' @param add Logical, indicating whether to add original 'net' data to the outputted transect lines.
#' @param verbose Logical, whether messages should be shown or not.
#' @return NULL if inputs are valid; otherwise, an error is thrown.
#' @noRd
#' @keywords internal
validate_cut_cross_section_inputs <- function(net, 
                                              crosswalk_id,
                                              cs_widths,
                                              num, 
                                              smooth,
                                              densify, 
                                              rm_self_intersect, 
                                              fix_braids, 
                                              braid_threshold , 
                                              braid_method, 
                                              precision, 
                                              add,
                                              verbose
) {
  
  # Check if 'net' is an sf object
  if (!inherits(net, "sf")) {
    stop("'net' must be an sf object.")
  }
  
  # Check if 'crosswalk_id' is NOT a character AND its NOT NULL
  if (!is.character(crosswalk_id) && !is.null(crosswalk_id)) {
    # if (is.null(crosswalk_id) || !is.character(crosswalk_id)) {
    stop("'crosswalk_id' must be a character vector or NULL")
  }
  
  # check if NOT NULL crosswalk_id is a column in 'net' 
  if (!crosswalk_id %in% names(net) && !is.null(crosswalk_id)) {
    stop("'crosswalk_id' column ", crosswalk_id, " is not a valid column in 'net'. 'crosswalk_id' must be a character vector or NULL")
  }
  
  # Check if 'cs_widths' is numeric or a numeric vector
  if (!is.numeric(cs_widths)) {
    stop("'cs_widths' must be a numeric")
  }
  
  # Check if 'num' is numeric or a numeric vector
  if (!is.numeric(num)) {
    stop("'num' must be numeric")
  }
  
  # Check if 'densify' is numeric or NULL
  if (!is.numeric(densify) && !is.null(densify)) {
    stop("'densify' must be numeric or NULL.")
  }
  
  # Check if 'smooth' is a logical value
  if (!is.logical(smooth)) {
    stop("'smooth' must be a logical value.")
  }
  
  # Check if 'rm_self_intersect' is a logical value
  if (!is.logical(rm_self_intersect)) {
    stop("'rm_self_intersect' must be a logical value.")
  }
  
  # Check if 'fix_braids' is a logical value
  if (!is.logical(fix_braids)) {
    stop("'fix_braids' must be a logical value.")
  }
  
  # Check if 'braid_threshold' is numeric or NULL
  if (!is.null(braid_threshold) && !is.numeric(braid_threshold)) {
    stop("'braid_threshold' must be numeric or NULL.")
  }
  
  # Check if 'braid_method' is one of the valid options
  valid_methods <- c("crosswalk_id", "component", "neighbor")
  if (!braid_method %in% valid_methods) {
    stop("'braid_method' must be one of 'crosswalk_id', 'component', or 'neighbor'.")
  }
  
  # Check if 'precision' is numeric and greater than 0
  if (!is.numeric(precision) || precision <= 0) {
    stop("'precision' must be a numeric value greater than 0.")
  }
  
  # Check if 'add' is a logical value
  if (!is.logical(add)) {
    stop("'add' must be a logical value.")
  }
  
  # Check if 'verbose' is a logical value
  if (!is.logical(verbose)) {
    stop("'verbose' must be a logical value.")
  }
  
  return(NULL)
}

#' Output a message if verbose is TRUE, otherwise don't
#' internal helper function for outputting messages if verbose is TRUE
#' @param ... list of strings to output in message
#' @param verbose logical, whether to output message or not
#'
#' @return NULL
#' @noRd
#' @keywords internal
message_if_verbose <- function(..., verbose = TRUE) {
  if(verbose) {
    args = paste(list(...), collapse = "")
    message(args)
  }
}

#' Select standard cross section point columns 
#' Internal helper function for selecting cross section point columns aligning with standard data model for cross section points
#' @param cs_pts dataframe, tibble, or sf dataframe 
#' @param crosswalk_id character, unique ID column
#' @importFrom dplyr select any_of 
#' @importFrom hydroloom rename_geometry 
#' @return dataframe, tibble, or sf dataframe with only relavent cross section point columns
#' @export
select_cs_pts <- function(cs_pts, crosswalk_id = NULL) {
  
  if(is.null(crosswalk_id)) {
    stop("Please provide a valid 'crosswalk_id' which uniquely identifies each cross section in 'cs_pts'")
  }
  
  cs_pts <- hydroloom::rename_geometry(cs_pts, "geometry")
  
  cs_pts <- 
    cs_pts %>% 
    dplyr::select(
      dplyr::any_of(c(
        crosswalk_id,
        "cs_id",
        "pt_id",
        "cs_lengthm",
        "relative_distance",
        "X", 
        "Y",
        "Z",
        "slope",
        "class", 
        "point_type",
        "valid_banks",
        "has_relief",
        "Z_source",
        "geometry"
      )
      )
    )
  
  return(cs_pts)
}

#' Select standard transect columns 
#' Internal helper function for selecting transect columns aligning with standard data model for transect
#' @param transects dataframe, tibble, or sf dataframe 
#' @param crosswalk_id character, unique ID column
#' @importFrom dplyr select any_of 
#' @importFrom hydroloom rename_geometry 
#' @return dataframe, tibble, or sf dataframe with only relavent transects columns
#' @export
select_transects <- function(transects, crosswalk_id = NULL) {
  
  if(is.null(crosswalk_id)) {
    # crosswalk_id  <- 'hydrofabric_id'
    stop("Please provide a valid 'crosswalk_id' which uniquely identifies the flowline associated with each transect in 'transects'")
  }
  
  transects <- hydroloom::rename_geometry(transects, "geometry")
  
  transects <- 
    transects %>% 
    dplyr::select(
      dplyr::any_of(c(
        crosswalk_id,
        "cs_id",
        "cs_lengthm",
        "cs_measure",
        "sinuosity",
        "cs_source",
        "geometry"
      )
      )
    )
  
  return(transects)
}

#' Check if data is an SF linestring / multilinestring
#'
#' @param data dataframe, tibble, sf dataframe, geometry
#'
#' @return logical, TRUE if data is an sf linestring / multilinestring
#' @noRd
#' @keywords internal
is_sf_linestring <- function(data) {
  
  # check if the object inherits from sf class
  is_sf <- inherits(data, "sf")
  
  # Check if the object is a linestring or multilinestring
  is_linestring <- (
    inherits(data, "LINESTRING") || 
      inherits(data, "MULTILINESTRING") || 
      inherits(data, "sf_LINESTRING") || 
      inherits(data, "sf_MULTILINESTRING") || 
      inherits(data, "sfc_LINESTRING") ||
      inherits(data, "sfc_MULTILINESTRING")
  )
  
  return(is_sf && is_linestring)
}

#' Convert an sf dataframe with a point geometry column to non spatial with XY columns
#'
#' @param pts sf dataframe of points
#' 
#' @importFrom sf st_coordinates st_drop_geometry
#' @importFrom dplyr mutate 
#' @return data.frame or tibble with added X and Y columns
#' @export 
pts_to_XY <- function(pts) {
  
  pts <- 
    pts %>% 
    dplyr::mutate(
      X = sf::st_coordinates(.)[,1],
      Y = sf::st_coordinates(.)[,2]
    ) 
  
  pts <- sf::st_drop_geometry(pts)
  
  return(pts)
  
}

#' Dissove polygons based on intersections with other polygons
#'
#' @param x sf dataframe
#' @importFrom sf st_union st_cast st_intersects 
#' @importFrom dplyr group_by summarize 
#' @return dissolved sf dataframe
#' @noRd
#' @keywords internal
cluster_dissolve <- function(x) {
  
  cluster <- unlist(
    sf::st_intersects(
      x, 
      x %>% 
        sf::st_union() %>% 
        sf::st_cast("POLYGON")
    )
  )
  
  clustered <- 
    cbind(x, cluster) %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarize()
  
  return(clustered)
  
}


#' Use sf::st_union to resolve overlaps in polygons based on intersections with other polygons
#'
#' @param x sf dataframe
#' @importFrom sf st_union st_as_sf  
#' @importFrom rmapshaper ms_explode
#' @return sf dataframe with overlaps removed
#' @export
dissolve_overlaps <- function(x) {
  
  x <- 
    x %>% 
    sf::st_union() %>%
    rmapshaper::ms_explode() %>%
    sf::st_as_sf() 
  
  return(x)
  
}

#' Use sf::st_difference to resolve overlaps in polygons based on intersections with other polygons
#'
#' @param x sf dataframe
#' @importFrom sf st_difference st_as_sf  
#' @importFrom rmapshaper ms_explode
#' @return sf dataframe with overlaps removed
#' @export
diff_overlaps <- function(x) {
  
  x <- 
    x %>% 
    # sf::st_union() %>%
    sf::st_difference() %>%
    rmapshaper::ms_explode() %>%
    sf::st_as_sf() 
  
  return(x)
  
}

#' Make a progress bar and return an "make_progress()" function to update the progress bar.
#' Credit to the exactextractr team: https://github.com/isciences/exactextractr/blob/5fd17dcf02717332b125345aea586304f668cf12/R/exact_extract_helpers.R#L361
#' @param progress logical, whether to make a progress bar or not (FALSE)
#' @param n numeric, total number of iterations
#' @importFrom utils txtProgressBar
#' @return make_progress function, when called will increment the progress bar text
#' @noRd
#' @keywords internal
#'
#' @examples
#' progress=TRUE
#' x = 1:500000
#' make_progress <- make_progress_bar(progress, length(x))
#' for (i in 1:length(x)) {
#'     make_progress()
#' }
make_progress_bar <- function(progress, n) {
  if (progress && n > 1) {
    pb <- utils::txtProgressBar(min = 0, max = n, initial=0, style=3)
    make_progress <- function() {
      i <- 1 + utils::getTxtProgressBar(pb)
      utils::setTxtProgressBar(pb, i)
      if (i == n) {
        close(pb)
      }
    }
  } else {
    make_progress <- function() {}
  }
  
  return(make_progress)
}

