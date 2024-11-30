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
    "new_cs_lengthm", "polygon_index",
    "crosswalk_id", "extend_invalid_transects2",
    "anchors", "deriv_type", "edge", "extension_distance", 
    "left_is_extended", "right_is_extended", "to_node", "verbose", 
    "toindid", "indid", "toid", "is", "internal_is_braided2"
  )
)

#' Add a 1:number of rows 'initial_order' column
#' Internal helper function for readability
#'
#' @param x dataframe, sf dataframe or tibble
#' @importFrom dplyr mutate n
#' @return dataframe, sf dataframe or tibble with an added 'initial_order' column 
#' @noRd
#' @keywords internal
add_initial_order <- function(x) {
  
  x <- 
    x %>% 
    dplyr::mutate(
      initial_order = 1:dplyr::n()
    )
  
  return(x)
  
}

#' Rearrange transects / cross sections in order from upstream to downstream
#'
#' @param x dataframe, sf dataframe or tibble
#' @param crosswalk_id character, unique ID column
#' @param order_by character, either "cs_id" or "cs_measure" 
#' @importFrom dplyr mutate group_by across any_of arrange n ungroup 
#' @return dataframe, sf dataframe or tibble with an added 'cs_id' column 
#' @export
cs_arrange <- function(x, 
                      crosswalk_id = NULL,
                      order_by = c("cs_id", "cs_measure")
                      ) {
  
  # evaluate order_by choices
  order_by <- match.arg(order_by)
  
  is_x_valid        <- validate_df(x,
                                   c(crosswalk_id, order_by),
                                   "x")
  
  x <-
    x %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id)))) %>% 
    dplyr::arrange(.data[[order_by]], .by_group = TRUE) %>% 
    dplyr::ungroup() 
  
  return(x)
  
}

#' Add a 1:number of cross sections 'cs_id' for each crosswalk_id by cs_measure  
#'
#' @param x dataframe, sf dataframe or tibble
#' @param crosswalk_id character, unique ID column
#' @importFrom dplyr mutate group_by across any_of arrange n ungroup 
#' @return dataframe, sf dataframe or tibble with an added 'cs_id' column 
#' @export
add_cs_id_sequence <- function(x, crosswalk_id = NULL) {
  is_x_valid        <- validate_df(x, 
                                   c(crosswalk_id, "cs_measure"), 
                                   "x")
  
  x <- 
    x %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id)))) %>% 
    dplyr::arrange(cs_measure, .by_group = TRUE) %>% 
    dplyr::mutate(
      cs_id = 1:dplyr::n()
    ) %>% 
    dplyr::ungroup() 
  
  return(x)
  
}

#' Get the initial ordering of crosswalk IDs in a dataframe of transects
#'
#' @param x dataframe or sf dataframe
#' @param crosswalk_id character
#'
#' @importFrom dplyr mutate any_of select group_by slice_min ungroup n
#' @importFrom sf st_drop_geometry 
#' @return dataframe, sf dataframe or tibble of crosswalk_id with initial_order column
#' @noRd
#' @keywords internal
get_transect_initial_order <- function(x, crosswalk_id = NULL) {
  
  is_x_valid        <- validate_df(x, 
                                   c(crosswalk_id, "cs_id"), 
                                   "x")
  
  x[[crosswalk_id]] <- factor(x[[crosswalk_id]], levels = unique(x[[crosswalk_id]]))
  
  x_order <- 
    x %>% 
    sf::st_drop_geometry() %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id)))) %>% 
    dplyr::slice_min(cs_id, n = 1, with_ties = FALSE) %>%
    # dplyr::filter(cs_id == min(cs_id)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      initial_order = 1:dplyr::n()
    ) %>% 
    # hydrofabric3D:::add_initial_order() %>% 
    dplyr::select(dplyr::any_of(crosswalk_id), initial_order)
  
  # t_order[[crosswalk_id]] <- as.character(t_order[[crosswalk_id]])
  
  return(x_order)
  
}

#' @title Fix IDs in a dataframe
#'
#' @description 
#' This function renumbers cross section IDs in a dataframe to ensure each crosswalk_id has cross sections
#' numbered from 1 to the total number of cross sections on the crosswalk_id.
#'
#' @param df A dataframe containing crosswalk_id and cs_id columns.
#' @param crosswalk_id crosswalk_id character, name of primary ID column
#' @return The input dataframe with renumbered cs_id values.
#' @importFrom dplyr select group_by slice ungroup mutate n left_join rename relocate
#' @importFrom sf st_drop_geometry
#' @export
renumber_cs_ids <- function(df, crosswalk_id = NULL) {
  # df <- data.frame(
  #   id =   c(rep("A", 10), 
  #            rep("B", 10)),
  #   cs_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  #             1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  #             ),
  #   cs_measure =   c(seq(0, 100, length.out = 10),   
  #                    seq(0, 100, length.out = 10))
  #   )
  
  is_valid_df <- validate_df(df, 
                             c(crosswalk_id, "cs_id", "cs_measure"), 
                             "df")
  
  # set to the default unique crosswalk ID if NULL is given 
  if(is.null(crosswalk_id)) {
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  if (!crosswalk_id %in% names(df)) {
    stop("'crosswalk_id' ", crosswalk_id, " is not a column in input dataframe.")
  } 
  
  if (!"cs_id" %in% names(df)) {
    stop("'cs_id' is not a column in input dataframe. Input dataframe must have a 'cs_id' column to uniquely identfy each cross section within each 'crosswalk_id'")
  } 
  
  if (length(unique(df[[crosswalk_id]])) == 0) {
    stop("'crosswalk_id' ", crosswalk_id, " contains only empty values")
  }
  
  if (length(unique(df$cs_id)) == 0) {
    stop("'cs_id' contains only empty values")
  }
  
  if (any(is.na(df[[crosswalk_id]]))) {
    stop("'crosswalk_id' ", crosswalk_id, " column contains NA values")
  }
  
  if (any(is.na(df$cs_id))) {
    stop("'cs_id' column contains NA values")
  }
  
  # make a dataframe that has a new_cs_id column that has 
  # the cs_id renumbered to fill in any missing IDs,
  # so each hy_id has cs_ids that go from 1 - number of cross sections on hy_id
  # The dataframe below will be used to join the "new_cs_id" with 
  # the original "cs_ids" in the final_pts output data
  renumbered_ids <- 
    df %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      # hy_id, 
      dplyr::any_of(crosswalk_id),
      cs_id, 
      cs_measure
      # cs_id, pt_id, cs_measure
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    # dplyr::group_by(hy_id, cs_id) %>%
    # dplyr::arrange(cs_measure, .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id)))) %>% 
    # dplyr::group_by(hy_id) %>%
    dplyr::arrange(cs_measure, .by_group = TRUE) %>%
    dplyr::mutate(
      new_cs_id = 1:dplyr::n()
      # tmp_id = paste0(hy_id, "_", cs_id)
    ) %>%
    add_tmp_id(x = crosswalk_id, y = "cs_id") %>% 
    dplyr::ungroup() %>%
    dplyr::select(new_cs_id, tmp_id)
  # 
  # renumbered_ids %>%
  #   dplyr::filter(new_cs_id != cs_id)
  
  # Join the new cs_ids back with the final output data to replace the old cs_ids
  df <- dplyr::left_join(
    add_tmp_id(df, x = crosswalk_id, y = "cs_id"),
    # dplyr::mutate(df,tmp_id = paste0(hy_id, "_", cs_id)),
    renumbered_ids,
    by = "tmp_id"
  ) %>%
    dplyr::select(-cs_id, -tmp_id) %>%
    dplyr::rename("cs_id" = "new_cs_id") %>%
    dplyr::relocate(dplyr::any_of(crosswalk_id), cs_id)
  # dplyr::relocate(hy_id, cs_id)
  
  # df %>% 
  #   # dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) 
  #   dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id)))) %>% 
  #   dplyr::arrange(cs_measure, .by_group = TRUE)
  
  
  return(df)
}
