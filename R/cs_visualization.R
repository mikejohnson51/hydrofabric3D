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