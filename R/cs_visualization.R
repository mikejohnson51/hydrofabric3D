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
    "make_geoms_to_cut_plot"
  )
)

#' Plots an X-Y scatter plot of cross section points 
#' @param cs_pts data.frame of cross section points with columns hy_id, cs_id and columns for X and Y axises (i.e. "pt_id", "Z")
#' @param x character name of column in cs_pts to use for X axis
#' @param y character name of column in cs_pts to use for Y axis
#' @param color character name of column in cs_pts to color points on plot
#' @param grid logical, if TRUE then use facet_grid, otherwise use facet_wrap. Default is FALSE (uses facet_wrap)
#' 
#' @return ggplot2 object
#' @importFrom ggplot2 ggplot geom_point aes facet_grid facet_wrap
#' @importFrom dplyr sym
#' @export 
plot_cs_pts <- function(cs_pts, 
                        x     = "pt_id", 
                        y     = "Z",
                        color = NULL,
                        grid  = FALSE
) {
  
  ######   ######   ######   ######
  # x = "pt_id"
  # y = "Z"
  # color = "cs_source"
  # color = 2
  # color = NULL
  # grid = FALSE
  # cs_pts = cs_pts
  ######   ######   ######   ######
  
  cs_plot <- 
    # cs_pts %>%
    cs_pts %>%
    ggplot2::ggplot() +
    # ggplot2::geom_point(ggplot2::aes(x = pt_id, y = Z)) 
    ggplot2::geom_point(
      ggplot2::aes(
        x = !!dplyr::sym(x), 
        y = !!dplyr::sym(y),
        color = !!ifelse(is.character(color), dplyr::sym(color), TRUE)
      )
    )
  # tidyselect::all_of("pt_id")
  
  # if grid == TRUE, then use facet_grid, otherwise use facet wrap
  if (grid) {
    
    cs_plot <- 
      cs_plot +
      ggplot2::facet_grid(hy_id~cs_id, scales = "free_y")
    
  } else {
    
    cs_plot <- 
      cs_plot +
      ggplot2::facet_wrap(hy_id~cs_id,  scales = "free_y")
  }
  
  return(cs_plot)
  
}