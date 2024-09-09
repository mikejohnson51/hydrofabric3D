library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

# source("testing_utils.R")
source("tests/testthat/testing_utils.R")
devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::classify_pts() ----
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# ---- hydrofabric3D::classify_pts() ----
# -------------------------------------------------------------------

# TODO:
testthat::test_that("check 'valid_banks' attribute of CLASSIFIED CS points from DEFAULT transects output", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 20
  
  PCT_OF_LENGTH_FOR_RELIEF <- 0.01
  
  flowlines <-
    flowlines %>% 
    # dplyr::slice(1) %>%
    # dplyr::slice(1:3) %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::rename(!!sym(ID_COL) := id) %>% 
    dplyr::select(
      dplyr::any_of(ID_COL), 
      tot_drainage_areasqkm,
      bf_width,
      geom
    ) 
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  ) %>% 
    dplyr::select( 
      dplyr::any_of(ID_COL),
      cs_id,
      cs_lengthm
    )  
  # dplyr::filter(hy_id == "wb-1003267", cs_id == 3)
  # dplyr::filter(hy_id == "wb-1003267", cs_id == 2)
  # dplyr::filter(hy_id == "wb-1003259", cs_id == 1)
  # dplyr::filter(hy_id == "wb-1003266", cs_id %in% c(1, 2))
  # dplyr::filter(hy_id == "wb-1003263", cs_id %in% c(1, 2))
  # dplyr::filter(hy_id == "wb-1003266", cs_id == 1)
  # dplyr::filter(hy_id == "wb-1003267", cs_id == 3)
  # dplyr::filter(hy_id == "wb-1003266", cs_id == 2)
  # dplyr::filter(hy_id == "wb-1003261", cs_id == 2)
  # dplyr::filter(hy_id == "wb-1003260", cs_id == 2)
  # dplyr::filter(hy_id == "wb-1003258", cs_id == 1)
  
  cs_pts = hydrofabric3D::cross_section_pts(
    cs              = transects,
    crosswalk_id    = ID_COL,
    points_per_cs   = POINTS_PER_CS,
    min_pts_per_cs  = MIN_PTS_PER_CS,
    dem             = DEM_PATH
  )
  
  cs_pts$Z
  
  tmp <- 
    cs_pts %>% 
    # dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
    dplyr::group_by(hy_id, cs_id) %>%
    dplyr::mutate(
      third          = ceiling(dplyr::n() / 3),
      mean_dist      = mean(diff(relative_distance)),
      in_channel_pts = ceiling(cs_lengthm[1] / mean_dist),
      b1             = ceiling(in_channel_pts / 2),
      b2             = in_channel_pts - b1,
      low_pt         = min(Z[third[1]:(2*third[1] - 1)]),
      class          = ifelse(Z <= low_pt & dplyr::between(pt_id, third[1], (2*third[1] - 1)), 
                              "bottom", 
                              "bank"
      ),
      Z2             = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()]),
      Z              = ifelse(class == "bottom", Z, Z2),
      min_bottom     = which(class == "bottom")[1],
      mid_bottom     = which(class == "bottom")[ceiling(length(which(class == "bottom"))/2)],
      max_bottom     = which(class == "bottom")[length(which(class == "bottom"))],
      L1             = pmax(1, mid_bottom - b1),
      L2             = pmax(1, mid_bottom - b2),
      R1             = pmin(mid_bottom + b2, n()),
      R2             = pmin(mid_bottom + b1, n()),
      anchor         = ifelse(Z[R2] < Z[L1], 2, 1),
      L              = pmax(third, ifelse(anchor == 1, L1, L2)),
      R              = pmin(2*third[1], ifelse(anchor == 1, R1, R2)),
      count_left     = min_bottom - L,
      count_right    = R - max_bottom,
      L              = ifelse(count_left == 0, L - count_right, L),
      R              = ifelse(count_right == 0, R + count_left, R),
      class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
      class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
      class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class),
      
      point_type     = class,
      deriv_type     = get_deriv_classes(Z),
      deriv = dplyr::case_when(
        (grepl("concave", deriv_type) | deriv_type == "linear") & point_type != "bottom" ~ "channel",
        TRUE                         ~ point_type 
      ),       
      side = dplyr::case_when(
        pt_id >= R[1] ~ "right_side",
        pt_id <= L[1] ~ "left_side",
        TRUE ~ "other"
      ),
      deriv2 = dplyr::case_when(
        pt_id == which(Z == max(Z) & side == "left_side")[1] ~ "left_bank",
        pt_id == rev(which(Z == max(Z) & side == "right_side"))[1] ~ "right_bank",
        TRUE ~ deriv
      )
      # deriv = dplyr::case_when(
      #   pt_id == rev(which(Z == max(Z) & side == "right_side"))[1] ~ "right_bank",
      #   TRUE ~ deriv
      # )
    ) %>% 
    dplyr::relocate(hy_id, cs_id, pt_id, Z, side, point_type, deriv, deriv_type)

  # tmp %>% 
  #   dplyr::mutate(
  #     side = dplyr::case_when(
  #      pt_id >= R[1] ~ "right_side",
  #      pt_id <= L[1] ~ "left_side",
  #      TRUE ~ "other"
  #     )
  #   ) %>% 
  #      dplyr::relocate(hy_id, cs_id, pt_id, Z, side, point_type, deriv, deriv_type) %>% 
  
  tmp %>% 
    # dplyr::filter(side == "left_side") %>%
    dplyr::mutate(
      deriv2 = dplyr::case_when(
        pt_id == which(Z == max(Z) & side == "left_side")[1] ~ "left_bank",
        TRUE ~ deriv
      ),
      deriv2 = dplyr::case_when(
        pt_id == rev(which(Z == max(Z) & side == "right_side"))[1] ~ "right_bank",
        TRUE ~ deriv2
      )
      # max_left_pt = which(Z == max(Z) & side == "left_side")[1]
      # max_right_pt = which(Z == max(Z) & side == "right_side")[1]
      # deriv = 
    ) %>% 
    dplyr::relocate(Z, deriv2, deriv)
  
  tmp %>% 
    # dplyr::mutate(
    #   deriv = dplyr::case_when(
    #     grepl("concave", deriv_type) | deriv_type == "linear" ~ "concave",
    #     TRUE                         ~ "not_concave"
    #   )
    # ) %>% 
    hydrofabric3D::plot_cs_pts(color = "deriv", size = 5)
  
  cs_pts %>% 
    hydrofabric3D::classify_points(crosswalk_id="hy_id") %>% 
    hydrofabric3D::plot_cs_pts(color = "point_type", size = 5)
  # anchors <- find_anchor_pts(
  #   depths            = depths,
  #   num_of_pts        = num_of_pts,
  #   cs_length         = cs_length,
  #   relative_distance = relative_distance,
  #   point_types       = point_types
  # )
  
  smoothed <- 
    cs_pts %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      point_type = classify_banks_and_bottoms(
        num_of_pts = points_per_cs[1],
        pt_ids     = pt_id,
        depths     = Z
      ),
      # smooth_Z = smooth_depths(Z, window = 3),
      # Z2      = use_smoothed_depths(Z, smooth_Z, point_type)
      # Z_start = Z,
      Z           = use_smoothed_depths(start_depths = Z, 
                                        smoothed_depths = smooth_depths(Z, window = 3), 
                                        point_types = point_type
      ),
      anchors = list(find_anchor_pts(
        depths             = Z,
        num_of_pts         = points_per_cs[1],
        cs_length          = cs_lengthm[1],
        relative_distance  = relative_distance,
        point_types        = point_type
        )
      ),
      
      left_anchor  = anchors[[1]][1],
      right_anchor = anchors[[1]][2],
      deriv_type  = get_deriv_classes(Z),
      deriv_type2 = ifelse(point_type == "bottom", "bottom", deriv_type),
      deriv_type2 = ifelse(deriv_type2 %in% c("concave_down_increasing", 
                                              "concave_down_decreasing",
                                              "concave_up_increasing", 
                                              "concave_up_decreasing",
                                              "linear"
                                              ),
                           "channel", 
                           deriv_type2 
                           ),
      min_bottom = min(which(point_type == "bottom")),
      max_bottom = max(which(point_type == "bottom")),
      deriv_type2          = ifelse(
                                    dplyr::between(pt_id, left_anchor[1], right_anchor[1]) & point_type != 'bottom', 
                                    "channel", 
                                    deriv_type2
                                    ),
      # deriv_type2          = ifelse(point_type == 'bank' & pt_id <= left_anchor[1], "left_bank", deriv_type2),
      # deriv_type2          = ifelse(point_type == 'bank' & pt_id >= right_anchor[1], "right_bank", deriv_type2)
      
      deriv_type2 = ifelse(
                      # pt_id <= left_anchor[1] & deriv_type2 == "flat",
                      # pt_id <= left_anchor[1],
                      pt_id <= left_anchor[1] & point_type == "bank",
                      # pt_id < min_bottom & point_type == "bank",
                      "left_bank",
                      deriv_type2
                    ),
      deriv_type2 = ifelse(
                        # pt_id >= right_anchor[1] & deriv_type2 == "flat",
                        # pt_id >= right_anchor[1],
                        pt_id >= right_anchor[1] & point_type == "bank",
                        # pt_id > max_bottom & deriv_type2 == "flat",
                        "right_bank",
                        deriv_type2
                      ),
      deriv_type2 = ifelse(
        pt_id == (min(which(deriv_type2 == "bottom")) - 1),
        "channel",
        deriv_type2
        ),
      deriv_type2 = ifelse(
        pt_id == (1 + max(which(deriv_type2 == "bottom"))),
        "channel",
        deriv_type2
      )
      # min_bottom = min(which(deriv_type2 == "bottom")),
      # max_bottom = max(which(deriv_type2 == "bottom"))
      
      # deriv_type2 = ifelse(
      #     max_bottom == max(pt_id) & deriv_type2[max(pt_id)] != "channel",
      #     "channel",
      #     deriv_type2
      # )
      # deriv_type2 = ifelse(
      #   Z == min(Z),
      #   "bottom",
      #   deriv_type2
      # )
      )
  
  # smoothed$anchors[1]
  # smoothed$left_anchor[1]
  # smoothed$right_anchor[1]
  
  # min_bottom = min(which(smoothed$point_type == "bottom"))
  # max_bottom = max(which(smoothed$point_type == "bottom"))
  
  smoothed %>% 
    hydrofabric3D::plot_cs_pts(color = "deriv_type2", size = 5) 
  
  smoothed$point_type
  # plot(cs_pts$Z)
  plot(smoothed$Z)
  depths <- smoothed$Z
  
  get_deriv_classes <- function(depths) {
    
    # message("Making classify_derivs_funct")
    # Function to classify points based on both first and second derivatives
    classify_derivs <- function(slope, second_deriv) {
      if (slope == 0) {
        return("flat")
      } else if (second_deriv > 0 && slope > 0) {
        return("concave_up_increasing")
      } else if (second_deriv > 0 && slope < 0) {
        return("concave_up_decreasing")
      } else if (second_deriv < 0 && slope > 0) {
        return("concave_down_increasing")
      } else if (second_deriv < 0 && slope < 0) {
        return("concave_down_decreasing")
      } else {
        return("linear")  # In case of second derivative zero but non-flat slope
      }
    }
    
    # Padding the original depth values with the first and last values
    depths_padded <- c(depths[1], depths, depths[length(depths)])
    
    # Calculate first derivative (slope) on padded depth values
    slopes <- diff(depths_padded)
    
    # Calculate second derivative (differences of the first differences)
    second_derivative <- diff(slopes)
    
    # Classify points based on padded first and second derivatives
    classifications <- mapply(classify_derivs, slopes[1:(length(second_derivative))], second_derivative)
    
    return(classifications)
  } 
  # Add NA for the last point (due to diff reducing length by 1 twice)
  classifications <- c(classifications, NA)
  # Calculate first derivative (slope)
  slopes <- diff(depths)
  
  # Padding: duplicate the first and last slope
  slopes_padded <- c(slopes[1], slopes, slopes[length(slopes)])
  
  # Calculate second derivative (differences of the first differences)
  second_derivative <- diff(slopes_padded)
  
  # Padding: duplicate the first and last second derivative
  second_deriv_padded <- c(second_derivative[1], second_derivative, second_derivative[length(second_derivative)])
  
  # Classify points based on padded first and second derivatives
  classifications <- mapply(classify_derivs, slopes_padded, second_deriv_padded)
  
  # depths <- c( 125.4605, 125.4605, 125.4605, 125.4604, 125.4601, 125.4601, 125.4601, 125.4605, 125.4607, 125.4607)
  plot(depths)
  # Calculate slope (differences between consecutive points)
  slopes <- diff(depths)
  
  # Classify the slopes
  slope_class <- ifelse(slopes > 0, "pos", ifelse(slopes < 0, "neg", "flat"))
  
  # To include the last point (since `diff` returns n-1 elements):
  slope_class <- c(slope_class, "flat")  # or assign "flat"/NA to the last point
  # slope_class <- c(slope_class, NA)  # or assign "flat"/NA to the last point
  
  plot(slopes)
  plot(c(slopes, 0))
  # plot(depths~c(slopes, 0))
  
  # Calculate second derivative (differences of the first differences)
  second_derivative <- diff(slopes)
  
  # Depth values
  depths <- c(125.4605, 125.4605, 125.4605, 125.4604, 125.4601, 125.4601, 125.4601, 125.4605, 125.4607, 125.4607)
  
  # Calculate first derivative (slope)
  slopes <- diff(depths)
  
  # Padding: duplicate the first and last slope
  slopes_padded <- c(slopes[1], slopes, slopes[length(slopes)])
  
  # Calculate second derivative (differences of the first differences)
  second_derivative <- diff(slopes_padded)
  
  # Padding: duplicate the first and last second derivative
  second_deriv_padded <- c(second_derivative[1], second_derivative, second_derivative[length(second_derivative)])
  
  # Classify points based on padded first and second derivatives
  classifications <- mapply(classify_points, slopes_padded, second_deriv_padded)
  
  # Function to classify points based on both first and second derivatives
  classify_points <- function(slope, second_deriv) {
    if (slope == 0) {
      return("flat")
    } else if (second_deriv > 0 && slope > 0) {
      return("concave up, increasing slope")
    } else if (second_deriv > 0 && slope < 0) {
      return("concave up, decreasing slope")
    } else if (second_deriv < 0 && slope > 0) {
      return("concave down, increasing slope")
    } else if (second_deriv < 0 && slope < 0) {
      return("concave down, decreasing slope")
    } else {
      return("linear")  # In case of second derivative zero but non-flat slope
    }
  }
  
  # Apply the classification logic
  classifications <- mapply(classify_points, slopes[-length(slopes)], second_derivative)
  mapply(classify_points, slopes, second_derivative)
  
  # Classify the second derivative values
  second_deriv_class <- ifelse(second_derivative > 0, "convex", 
                               ifelse(second_derivative < 0, "concave", "linear"))
  
  # To include the last two points (since `diff` returns n-1 elements each time):
  second_deriv_class <- c(second_deriv_class, NA, NA)  # Adding NAs or labels for the last two points
  
  
  
  first_deriv <- diff(depths)
  diff(first_deriv)
  
  pts <- c(depths[1], depths, depths[length(depths)])
  
  diff(pts)
  plot(pts)
  
  smoothed$Z
  
  
  cs_pts %>% 
    hydrofabric3D::classify_points(crosswalk_id="hy_id") %>% 
    hydrofabric3D::plot_cs_pts(color = "point_type", size = 6)
  
  # -------------------------------------------------------------------
  # ----- Inputs -----
  # -------------------------------------------------------------------
  
  plots1 <- 
    cs_pts %>% 
    hydrofabric3D::classify_points(crosswalk_id="hy_id") %>% 
    hydrofabric3D::plot_cs_pts(color = "point_type", size = 6)
  plots1 <- 
    plots1 + 
    ggplot2::labs(title = "Method #1") + 
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold"))
  plots1
  # num_of_pts          <- cs_pts$points_per_cs[1]
  # pt_ids              <- cs_pts$pt_id
  # relative_distances  <- cs_pts$relative_distance
  # depths              <- cs_pts$Z
  # cs_length           <- cs_pts$cs_lengthm[1]
  
  # -------------------------------------------------------------------
  # -------------------------------------------------------------------
  # cs_pts %>% 
  # classify_points(crosswalk_id = "hy_id") %>% 
  # dplyr::select(pt_id, Z, point_type) 
  
  classified2 <-
    # plots2 <-
    cs_pts %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    dplyr::mutate(
      point_type = classify_banks_and_bottoms(
        num_of_pts = points_per_cs[1],
        pt_ids     = pt_id,
        depths     = Z
      ),
      # smooth_Z = smooth_depths(Z, window = 3),
      # Z2      = use_smoothed_depths(Z, smooth_Z, point_type)
      # Z_start = Z,
      Z           = use_smoothed_depths(start_depths = Z, 
                                        smoothed_depths = smooth_depths(Z, window = 3), 
                                        point_types = point_type
      ),
      
      point_type2 = classify_z_pts(
        depths = Z,
        num_of_pts        = points_per_cs[1],
        cs_length         = cs_lengthm[1],
        relative_distance = relative_distance,
        pt_ids = pt_id,
        point_types = point_type
      ),
      point_type3 = classify_pts_from_slope(depths = Z)
      # point_type3 = get_cs_point_types(
      #                   depths            = Z,
      #                   num_of_pts        = points_per_cs[1],
      #                   cs_length         = cs_lengthm[1],
      #                   relative_distance = relative_distance,
      #                   point_types       = point_type
      #                 )
    ) 
  classified2$point_type3
  plots2 <- 
    classified2 %>% 
    hydrofabric3D::plot_cs_pts(color = "point_type3", size = 6)
  # dplyr::relocate(Z, point_type, point_type2)
  # dplyr::select(hy_id, cs_id, pt_id, Z, point_type)
  # ggplot2::ggplot() + 
  # ggplot2::geom_point(
  #   ggplot2::aes(
  #     x = pt_id,
  #     y = Z,
  #     color = point_type
  #   ), size = 5
  # )
  
  plots2 <- 
    plots2 + 
    ggplot2::labs(title = "Method #2") + 
    ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold"))
  
  plots2
})