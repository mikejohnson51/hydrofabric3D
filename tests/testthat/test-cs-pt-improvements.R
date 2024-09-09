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
    
  library(patchwork)
  plots1 / plots2
  plots1 + plots2
  cs_pts %>% 
    classify_points(crosswalk_id = "hy_id") %>% 
    dplyr::select(hy_id, cs_id, pt_id, Z, point_type) %>% 
    ggplot2::ggplot() + 
    ggplot2::geom_point(
      ggplot2::aes(
        x = pt_id,
        y = Z,
        color = point_type
      ), size = 5
    )
  # hydrofabric3D::plot_cs_pts(color = "point_type")
    # .$Z %>%
    # .$smooth_Z %>%
    # .$Z2 %>%
    # plot()
  get_cs_point_types2 <- function(
    depths,
    num_of_pts,
    cs_length,
    relative_distance,
    point_types
  ) {
    
    # ----------------------------------------------------------------------
    # num_of_pts          <- cs_pts$points_per_cs[1]
    # pt_ids              <- cs_pts$pt_id
    # relative_distance   <- cs_pts$relative_distance
    # depths              <- cs_pts$Z
    # cs_length           <- cs_pts$cs_lengthm[1]
    # 
    # point_types = classify_banks_and_bottoms(
    #   num_of_pts = num_of_pts,
    #   pt_ids     = pt_ids,
    #   depths     = depths
    # )
    # 
    # # smooth_Z = smooth_depths(Z, window = 3),
    # # Z2      = use_smoothed_depths(Z, smooth_Z, point_type)
    # # Z_start = Z,
    # 
    # depths           = use_smoothed_depths(start_depths = depths, 
    #                                   smoothed_depths = smooth_depths(depths, window = 3), 
    #                                   point_types = point_types
    # )
    # 
    # # plot(Z)
    
    # ----------------------------------------------------------------------
    # ----------------------------------------------------------------------
    
    # get the number of points in each third of the cross section  
    third          <- ceiling(num_of_pts / 3)
    
    # Find the anchor points that tell us where the left / right banks start 
    # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
    anchors <- find_anchor_pts(
      depths            = depths,
      num_of_pts        = num_of_pts,
      cs_length         = cs_length,
      relative_distance = relative_distance,
      point_types       = point_types
    )
    
    left_anchor    <- anchors[1]
    bottom_anchor  <- anchors[2]
    right_anchor   <- anchors[3]
    
    left_to_middle  <- 1:left_anchor
    middle_to_right <- right_anchor:length(depths)
    
    left_side    <- depths[left_to_middle]
    right_side   <- depths[middle_to_right]
    
    # middle_third <- (left_anchor + 1):(right_anchor - 1)
    middle_third <- (left_anchor):(right_anchor)
    
    point_types[middle_third]  <- ifelse(
      point_types[middle_third] != "bottom", 
      "channel", 
      point_types[middle_third]
    )
    
    point_types[1:(left_anchor-1)]                      <- "left_bank"
    point_types[(right_anchor+1):length(point_types)]   <- "right_bank"
    # point_types[1:left_anchor]                      <- "left_bank"
    # point_types[right_anchor:length(point_types)]   <- "right_bank"
    
    return(point_types)
    
  }
  
  
  get_cs_point_types <- function(
    depths,
    num_of_pts,
    cs_length,
    relative_distance,
    point_types
  ) {
    
    # ----------------------------------------------------------------------
    # num_of_pts          <- cs_pts$points_per_cs[1]
    # pt_ids              <- cs_pts$pt_id
    # relative_distance   <- cs_pts$relative_distance
    # depths              <- cs_pts$Z
    # cs_length           <- cs_pts$cs_lengthm[1]
    # 
    # point_types = classify_banks_and_bottoms(
    #   num_of_pts = num_of_pts,
    #   pt_ids     = pt_ids,
    #   depths     = depths
    # )
    # 
    # # smooth_Z = smooth_depths(Z, window = 3),
    # # Z2      = use_smoothed_depths(Z, smooth_Z, point_type)
    # # Z_start = Z,
    # 
    # depths           = use_smoothed_depths(start_depths = depths, 
    #                                   smoothed_depths = smooth_depths(depths, window = 3), 
    #                                   point_types = point_types
    # )
    # 
    # # plot(Z)
    
    # ----------------------------------------------------------------------
    # ----------------------------------------------------------------------
    
      # get the number of points in each third of the cross section  
      third          <- ceiling(num_of_pts / 3)
      
      # Find the anchor points that tell us where the left / right banks start 
      # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
      anchors <- find_anchor_pts(
        depths            = depths,
        num_of_pts        = num_of_pts,
        cs_length         = cs_length,
        relative_distance = relative_distance,
        point_types       = point_types
      )
      
      left_anchor    <- anchors[1]
      bottom_anchor  <- anchors[2]
      right_anchor   <- anchors[3]
      
      left_to_middle  <- 1:left_anchor
      middle_to_right <- right_anchor:length(depths)
      
      # plot(depths)
      # plot(depths[left_to_middle])
      
      # plot_df <-
      #   data.frame(Z = depths) %>%
      #   dplyr::mutate(
      #     n = 1:dplyr::n(),
      #     side = dplyr::case_when(
      #       n %in% left_to_middle ~ "left_side",
      #       n %in% middle_to_right ~ "right_side",
      #       TRUE ~ "bottom"
      #     ))
      # plot_df %>%
      #   ggplot2::ggplot() +
      #     ggplot2::geom_point(
      #       ggplot2::aes(
      #         x = n,
      #         y = Z,
      #         color = side), size = 4)
      
      left_side    <- depths[left_to_middle]
      right_side   <- depths[middle_to_right]
      
      # middle_third <- (left_anchor + 1):(right_anchor - 1)
      # 
      # point_types[middle_third]  <- ifelse(
      #                                   point_types[middle_third] != "bottom", 
      #                                   "channel", 
      #                                   point_types[middle_third]
      #                                   )
      # 
      # point_types[1:left_anchor]                      <- "left_bank"
      # point_types[right_anchor:length(point_types)]   <- "right_bank"
      
      
      # middle_to_right
      # TODO: this could throw an error
      # max(depths[0])

      # get the maximum depth (highest elevation) point on the LEFT side and all points equal to that max elevation
      left_max       <- max(left_side)
      left_max_pts   <- which(left_side == left_max)

      # rev_max_left_idx <- which.max(rev(left_side))
      # (length(left_side) - rev_max_left_idx) + 1
      # which.max(left_side)
      # which.max(rev(left_side))

      # get the maximum depth (highest elevation) point on the RIGHT side and all points equal to that max elevation
      right_max       <- max(right_side)
      right_max_pts   <- (which(right_side == right_max) + right_anchor) - 1
      # right_max_pts   <- which(right_side == right_max)

      # (right_max_pts + right_anchor) - 1
      # (num_of_pts - right_max_pts ) + 1

      start_of_left_bank  <- left_max_pts[length(left_max_pts)]
      start_of_right_bank <- right_max_pts[1]
      # start_of_right_bank <- right_max_pts[length(right_max_pts)]

      # assign points to the LEFT of the max LEFT SIDE elevation to "left_bank", then
      # set the remaining points between that high elevation and the "bottom" to "channel" points
      point_types[1:(start_of_left_bank-1)]                    <- "left_bank"

      # if (start_of_left_bank == left_anchor) {
      #   point_types[(start_of_left_bank + 1)]              <- "channel"
      # } else {
      #   point_types[(start_of_left_bank + 1):left_anchor]  <- "channel"
      # }
      #
      # if (start_of_left_bank != left_anchor) {
      #   point_types[(start_of_left_bank + 1):left_anchor]  <- "channel"
      # } else {
      #   point_types[(start_of_left_bank + 1)]              <- "channel"
      # }

      # assign points to the RIGHT of the max RIGHT SIDE elevation to "right_bank", then
      # set the remaining points between the right side of the "bottom" and the right side high elevation point to "channel" points
      point_types[(start_of_right_bank + 1):length(point_types)]   <- "right_bank"
      # point_types[right_anchor:(start_of_right_bank - 1)]  <- "channel"

      # left_of_right_side <- (start_of_right_bank - 1)
      #
      # if (start_of_right_bank == right_anchor) {
      #   point_types[left_of_right_side]               <- "channel"
      # } else {
      #   point_types[right_anchor:(start_of_right_bank - 1)]  <- "channel"
      # }

      # point_types != "bottom"

      # middle_section <- point_types[(left_anchor + 1):(right_anchor - 1)]
      # point_types[(left_anchor + 1):(right_anchor - 1)] != "bottom"

      middle_third <- (left_anchor):(right_anchor)
      # middle_third <- (left_anchor + 1):(right_anchor - 1)
      
      point_types[middle_third]  <- ifelse(
                                        point_types[middle_third] != "bottom",
                                        "channel",
                                        point_types[middle_third]
                                        )

      # point_types[left_anchor]

      # # TODO: this sets any remaining bank points to "channel" points as needed
      point_types[which(point_types == "bank")]              <- "channel"
      
      # right_anchor:(start_of_right_bank - 1)
      return(point_types)
      
  }
  
  classify_cs_pts <- function(
    depths,
    pt_ids,
    num_of_pts,
    relative_distances,
    cs_length
  ) {
    
    point_types <- classify_banks_and_bottoms(
                    num_of_pts = num_of_pts,
                    pt_ids     = pt_ids,
                    depths     = depths
                  )
    # point_types
    # depths %>% 
    #   smooth_depths(depths, window = 3) %>% 
    #   use_smoothed_depths(depths, ., point_types)
    # smooth_Z <- use_smoothed_depths(
    #                 depths, 
    #                 smooth_depths(depths, window = 3), 
    #                 point_types
    #                 # )
    # smoothed_depths <- smooth_depths(
    #   depths = depths, 
    #   window = 3
    #   )
    
    # TODO: Good simple plot showing smoothing changes from DEM ----> smoothed DEM pts
    # plot(depths)
    # plot(smooth_depths(depths, window = 3))
    # plot(use_smoothed_depths(
    #       depths, 
    #       smooth_depths(depths, window = 3),  # get a smoothed version of the DEM points
    #       point_types
    #     ))
    
    # # get a smoothed version of the DEM points
    # depths <- use_smoothed_depths(depths, smoothed_depths, point_types)
    
    depths <- use_smoothed_depths(
                    depths, 
                    smooth_depths(depths, window = 3),  # get a smoothed version of the DEM points
                    point_types
                    )
    
    # plot(smooth_depth)
    # banks_and_anchor <- find_banks_and_anchor_pts(depths = depths,num_of_pts  = num_of_pts,
    # cs_length = cs_length, relative_distance = relative_distances,
    # point_types = point_types)
    
    
    # Find the anchor points that tell us where the left / right banks start 
    # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
    anchors <- find_anchor_pts(
                     depths            = depths,
                     num_of_pts        = num_of_pts,
                     cs_length         = cs_length,
                     relative_distance = relative_distances,
                     point_types       = point_types
                   )
     
    left_anchor    <- anchors[1]
    bottom_anchor  <- anchors[2]
    right_anchor   <- anchors[3]
     
    third          <- ceiling(num_of_pts / 3)
    
    # left_bank    <- banks_and_anchor[1]
    # anchor       <- banks_and_anchor[2]
    # right_bank   <- banks_and_anchor[3]
    
    left_to_middle  <- 1:left_anchor
    middle_to_right <- right_anchor:length(depths)
    
    # plot(depths)
    # plot(depths[left_to_middle])
    
    # plot_df <- 
    #   data.frame(
    #   Z = depths
    # ) %>% 
    #   dplyr::mutate(
    #     n = 1:dplyr::n(),
    #     side = dplyr::case_when(
    #       n %in% left_to_middle ~ "left_side",
    #       n %in% middle_to_right ~ "right_side",
    #       TRUE ~ "bottom"
    #     )
    #   )
    # 
    # plot_df %>% 
    #   ggplot2::ggplot() +
    #     ggplot2::geom_point(
    #       ggplot2::aes(
    #         x = n,
    #         y = Z,
    #         color = side
    #       ),
    #       size = 4
    #     )
    # 
    
    left_side    <- depths[left_to_middle]
    right_side   <- depths[middle_to_right]
    
    # middle_to_right
    # TODO: this could throw an error 
    # max(depths[0])
    
    # get the maximum depth (highest elevation) point on the LEFT side and all points equal to that max elevation
    left_max       <- max(left_side)
    left_max_pts   <- which(left_side == left_max)
    
    # rev_max_left_idx <- which.max(rev(left_side))
    # (length(left_side) - rev_max_left_idx) + 1
    # which.max(left_side)
    # which.max(rev(left_side))
    
    # get the maximum depth (highest elevation) point on the RIGHT side and all points equal to that max elevation
    right_max       <- max(right_side)
    right_max_pts   <- (which(right_side == right_max) + right_anchor) - 1
    # right_max_pts   <- which(right_side == right_max)
    # (right_max_pts + right_anchor) - 1
    # (num_of_pts - right_max_pts ) + 1
    
    start_of_left_bank  <- left_max_pts[length(left_max_pts)]
    start_of_right_bank <- right_max_pts[1]
    # start_of_right_bank <- right_max_pts[length(right_max_pts)]
    
    # assign points to the LEFT of the max LEFT SIDE elevation to "left_bank", then
    # set the remaining points between that high elevation and the "bottom" to "channel" points
    point_types[1:start_of_left_bank]                    <- "left_bank"
    point_types[(start_of_left_bank + 1):left_anchor]    <- "channel"
    
    # assign points to the RIGHT of the max RIGHT SIDE elevation to "right_bank", then
    # set the remaining points between the right side of the "bottom" and the right side high elevation point to "channel" points
    point_types[start_of_right_bank:length(point_types)] <- "right_bank"
    point_types[right_anchor:(start_of_right_bank - 1)]  <- "channel"
    
    # right_anchor:(start_of_right_bank - 1)
    return(point_types)
    
  }
  
  # depths
  # 
  # left_max_Z
  # 
  # classed_pts <- 
  #   cs_pts %>%
  #     sf::st_drop_geometry() %>% 
  #     hydrofabric3D::classify_points(crosswalk_id = "hy_id") %>% 
  #     dplyr::mutate(
  #       new_classes = FALSE
  #     )
  # 
  # classed_pts2 <- classed_pts 
  # classed_pts2$Z          <- depths
  # classed_pts2$point_type <- classify_z_pts(
  #                                 depths      = depths,
  #                                 pt_ids      = pt_ids,
  #                                 point_types = point_types,
  #                                 L           = left_bank,
  #                                 R           = right_bank
  #                               )
  # classed_pts2 <- 
  #   classed_pts2 %>% 
  #   dplyr::mutate(
  #     new_classes = TRUE
  #   )
  # 
  # dplyr::bind_rows(
  #   classed_pts,
  #   classed_pts2
  # ) %>% 
  # # classed_pts %>% 
  #   ggplot2::ggplot() +
  #   ggplot2::geom_point(
  #     ggplot2::aes(
  #       x = pt_id,
  #       y = Z,
  #       color = point_type
  #     ), 
  #     size = 5
  #   ) +
  #   ggplot2::facet_wrap(~new_classes)
  
    # hydrofabric3D::plot_cs_pts(color = "point_type2")
  
  # bank_idxs <- find_in_channel_pts(relative_distances, cs_length) 
  # 
  # LEFT = L
  # RIGHT = R
  # 
  # class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class)
  # class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class)
  # class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)
  
  # # compute the first derivative (slope)
  # slope <- diff(depths) / diff(relative_distances)
  # 
  # # compute the second derivative (curvature)
  # second_derivative <- diff(slope) / diff(relative_distances[-1])
  # 
  # second_derivative %>% round(5)
 
  classify_banks_and_bottoms <- function(
    num_of_pts, 
    pt_ids, 
    depths
  ) {
    
    # calc the 
    # - number of points in a third of the cross section (third)
    third              <- ceiling(num_of_pts / 3)
    
    mid_third_left_idx   <- third
    mid_third_right_idx  <- ((2 * third) - 1)
    
    mid_third_idxs    <- mid_third_left_idx:mid_third_right_idx
    mid_third_low_pt  <- min(depths[mid_third_idxs])
    
    # logic for determining if its a bottom point (at lowest depth AND in middle third)
    is_at_bottom_Z      <- depths <= mid_third_low_pt
    is_at_middle_third  <- dplyr::between(pt_ids, mid_third_left_idx, mid_third_right_idx)
    
    point_type <- ifelse(is_at_bottom_Z & is_at_middle_third, "bottom", "bank")
    
    return(point_type)
  }
  
  # Use the bottom points from the original depth values otherwise use the (rolling mean) smoothed depth values
  use_smoothed_depths <- function(
    start_depths, 
    smoothed_depths, 
    point_types
  ) {
    return(
      ifelse(point_types == "bottom", start_depths, smoothed_depths)
    )
  }
  
  find_in_channel_pts <- function(
    relative_distances, 
    cs_length
  ) {
    
    # calc the:
    # - mean distance between points (mean_dist)
    # - how much in channel points we have (in_channel_pts)
    
    dist_between_pts   <- mean(diff(relative_distances))
    in_channel_pts     <- ceiling(cs_length / dist_between_pts)
    
    right_anchor <- ceiling(in_channel_pts / 2)
    left_anchor  <- in_channel_pts - right_anchor
    
    return(c(left_anchor, right_anchor))
  }
  
  # given depths and relative distances and points classified into banks vs bottoms
  # determine the indices for 
  # - start of the left side bank/channel 
  # - start of theright/side bank channel
  # - the index of the point at the middle of the bottom 
  # Returns a numeric vector of length 3 with the index of the left anchor, middle point, and right anchor (i.e. c(2, 5, 9) -> c(left_anchor, middle_bottom, right_anchor))
  find_anchor_pts <- function(depths, 
                                        num_of_pts,
                                        cs_length, 
                                        relative_distance, 
                                        point_types
                                        ) {
    
    
    # ------------------------------
    # depths            = depths
    # num_of_pts        = num_of_pts
    # relative_distance = relative_distances
    # cs_length         = cs_length
    # point_types       = point_types
    
    # ------------------------------
    third          = ceiling(num_of_pts / 3)
    
    dist_between_pts   <- mean(diff(relative_distance))
    in_channel_pts     <- ceiling(cs_length / dist_between_pts)
    
    b1  <- ceiling(in_channel_pts / 2) # b1
    b2  <- in_channel_pts - b1 # b2
    
    # bank_idxs      <- find_in_channel_pts(relative_distances, cs_length)
    # 
    # left_bank      <- bank_idxs[1] 
    # right_bank     <- bank_idxs[2]
    
    bottom_idxs <- which(point_types == "bottom")
    # point_type_is_bottom = which(point_types == "bottom")
    
    min_bottom  <- bottom_idxs[1]
    mid_bottom  <- bottom_idxs[ceiling(length(bottom_idxs) / 2)]
    max_bottom  <- bottom_idxs[length(bottom_idxs)]
    # min_bottom     = which(point_types == "bottom")[1]
    # mid_bottom     = which(point_types == "bottom")[ceiling(length(which(point_types == "bottom"))/2)]
    # max_bottom     = which(point_types == "bottom")[length(which(point_types == "bottom"))]
    
    L1             = pmax(1, mid_bottom - b1)
    L2             = pmax(1, mid_bottom - b2)
    
    R1             = pmin(mid_bottom + b2, num_of_pts)
    R2             = pmin(mid_bottom + b1, num_of_pts)

    anchor         = ifelse(depths[R2] < depths[L1], 2, 1)
    
    LEFT           = pmax(third, ifelse(anchor == 1, L1, L2))
    RIGHT          = pmin(2*third, ifelse(anchor == 1, R1, R2))
    
    count_left     = min_bottom - LEFT
    count_right    = RIGHT - max_bottom
    
    LEFT           = ifelse(count_left == 0, LEFT - count_right, LEFT)
    RIGHT          = ifelse(count_right == 0, RIGHT + count_left, RIGHT)
    
    return(c(LEFT, mid_bottom, RIGHT))
    
  }
  
  smooth_depths <- function(
    depths,
    num_of_pts = NULL,
    window     = 3
  ) {
    
    # depths <- cs_pts$Z
    # num_of_pts          <- 10
    # window = 3
    
    if(is.null(num_of_pts)) {
      num_of_pts <- length(depths)    
    } 
    
    # calculate a rolling mean of the depths between the starting and ending depth
    smoothed_depths <- c(
      depths[1],
      zoo::rollmean(depths, window),
      depths[num_of_pts]
    )
    
    return(smoothed_depths)
  }
  
  classify_z_pts <- function(
                        depths, 
                        num_of_pts,
                        cs_length,
                        relative_distance,
                        pt_ids, 
                        point_types
                        # L,
                        # R
                        ) {
    
    # Find the anchor points that tell us where the left / right banks start 
    # NOTE: See find_anchor_pts() (i.e. it's approximately first points to the left/right of the banks but NOT EXACTLY that simple) 
    anchors <- find_anchor_pts(
      depths            = depths,
      num_of_pts        = num_of_pts,
      cs_length         = cs_length,
      relative_distance = relative_distance,
      point_types       = point_types
    )
    
    # left_anchor    <- anchors[1]
    # bottom_anchor  <- anchors[2]
    # right_anchor   <- anchors[3]
    
    L    <- anchors[1]
    # bottom_anchor  <- anchors[2]
    R   <- anchors[3]
    # depths      = depths
    # pt_ids      = pt_ids
    # point_types = point_types
    # L           = left_bank
    # R           = right_bank
    
    slope          <- diff(c(0, depths))  
    bottom_depth   <- min(depths)      
    points_per_id  <- length(depths)
    
    # bank_or_bottom <- point_types
    # i = 3 
   # i =4 
    classification <- sapply(1:points_per_id, function(i) {
      message(i)
      if (i == 1) {
        
        message("LB\n")
        return("left_bank")   # handle first point
        
      } else if (i == points_per_id) {
        
        message("RB\n")
        return("right_bank")  # and the last point
        
      } else if (depths[i] == bottom_depth & point_types[i] == "bottom") {
        
        message("BOTTOM\n")
        return("bottom")      # identify the bottom point(s) (min depths value)
        
        
      }
      # RIGHT SIDE OF BOTTOM
      else if (pt_ids[i] >= R) {
        
        if (slope[i + 1] <= 0) {
          
          message("Right bank\n")
          return("right_bank")     #  slope changing from rising to falling (approaching bottom)
          
        } else {
          
          message("Right channel\n")
          return("right_channel")  # rising slope but not near the bottom
          
        }
        
      }
      # LEFT SIDE OF THE BOTTOM
      else if (pt_ids[i] <= L) {
        
        # second derivate is 
        if (slope[i - 1] <= 0) {
          
          message("Left channel\n")
          return("left_channel")   # slope changing from falling to rising (leaving bottom)
          
        } else {
          
          message("Left bank\n")
          return("left_bank")  #  slope  falling but not near the bottom
          
        }
        
      } else{
        message("NOT ON LEFT OR RIGHT SIDE (i.e. bottom...?)")
        return("bottom")
      } 
      # else if(slope[i] == 0) {
        # message("SLOPE is 0 --> CHAN\n")
        # return("channel")
      # }
      
      message("NO MATCH\n")
      
    })
    
    # classification <- sapply(1:points_per_id, function(i) {
    #   message(i)
    #   if (i == 1) {
    #     message("LB\n")
    #     return("left_bank")   # handle first point
    #   } else if (i == points_per_id) {
    #     message("RB\n")
    #     return("right_bank")  # and the last point
    #   } else if (depths[i] == bottom_depth) {
    #     message("BOTTOM\n")
    #     return("bottom")      # identify the bottom point(s) (min depths value)
    #   } else if (slope[i] > 0) {
    #     if (slope[i + 1] <= 0) {
    #       message("CHAN\n")
    #       return("channel")     #  slope changing from rising to falling (approaching bottom)
    #     } else {
    #       message("RB\n")
    #       return("right_bank")  # rising slope but not near the bottom
    #     }
    #   } else if (slope[i] < 0) {
    #     if (slope[i - 1] >= 0 || slope[i + 1] == 0) {
    #       message("CHAN\n")
    #       return("channel")   # slope changing from falling to rising (leaving bottom)
    #     } else {
    #       message("LB\n")
    #       return("left_bank")  #  slope  falling but not near the bottom
    #     }
    #   } else if(slope[i] == 0) {
    #     message("SLOPE is 0 --> CHAN\n")
    #     return("channel")
    #   }
    #   message("NO MATCH\n")
    # })
    
    classification[classification %in% c("left_channel", "right_channel")] <- "channel"
    
    return(classification)
  }
  
  
  # # df <- data.frame(
  # #   Z = depths,
  # #   Z2 = smoothed_depths,
  # #   Z3 = depths2,
  # #   point_type = point_types
  # # )
  # plot(depths)
  # # plot(depths2)
  # plot(smoothed_depths)
  # 
  # plot(use_smoothed_depths(depths, smoothed_depths, point_types))
  # # Use the bottom points from the original depth values otherwise use the (rolling mean) smoothed depth values
  # use_smoothed_depths <- function(
  #   start_depths, 
  #   smoothed_depths, 
  #   point_types
  #   ) {
  #   return(
  #     ifelse(point_types == "bottom", start_depths, smoothed_depths)
  #     )
  # }
  # 
  # # determine bottom point types
  # third           <- ceiling(num_of_pts / 3)
  # mean_dist       <- mean(diff(relative_distances))
  # in_channel_pts  <- ceiling(cs_length / mean_dist)
  # 
  # bank1 <- ceiling(in_channel_pts / 2)
  # bank2 <- in_channel_pts - bank1
  # 
  # # third_left_idx   <- third
  # # third_right_idx  <- ((2 * third) - 1)
  # 
  # mid_third_left_idx   <- third
  # mid_third_right_idx  <- ((2 * third) - 1)
  # 
  # mid_third_idxs    <- mid_third_left_idx:mid_third_right_idx
  # # mid_third_idxs    <- third:mid_third_right_idx 
  # # mid_third_idxs    <- third:((2 * third) - 1)
  # 
  # mid_third_low_pt  <- min(depths[mid_third_idxs])
  # # mid_third_low_pt  <- min(depths[third:((2 * third) - 1)])
  # 
  # is_at_bottom_Z      <- depths <= mid_third_low_pt
  # is_at_middle_third  <- dplyr::between(pt_ids, mid_third_left_idx, mid_third_right_idx)
  # point_type          <- ifelse(is_at_bottom_Z & is_at_middle_third, 
  #                              "bottom", 
  #                              "bank"
  #                              )
  # # Use the bottom points from the original depth values otherwise use the (rolling mean) smoothed depth values
  # use_smoothed_depths <- function(
  #   start_depths, 
  #   smoothed_depths, 
  #   point_types
  # ) {
  #   return(
  #     ifelse(point_types == "bottom", start_depths, smoothed_depths)
  #   )
  # }
  # 
  # classify_banks_and_bottoms <- function(
  #                       num_of_pts, 
  #                       pt_ids, 
  #                       depths
  #                     ) {
  #   
  #   # calc the 
  #   # - number of points in a third of the cross section (third)
  #   third              <- ceiling(num_of_pts / 3)
  #   
  #   mid_third_left_idx   <- third
  #   mid_third_right_idx  <- ((2 * third) - 1)
  #   
  #   mid_third_idxs    <- mid_third_left_idx:mid_third_right_idx
  #   mid_third_low_pt  <- min(depths[mid_third_idxs])
  #   
  #   # logic for determining if its a bottom point (at lowest depth AND in middle third)
  #   is_at_bottom_Z      <- depths <= mid_third_low_pt
  #   is_at_middle_third  <- dplyr::between(pt_ids, mid_third_left_idx, mid_third_right_idx)
  #   
  #   point_type <- ifelse(is_at_bottom_Z & is_at_middle_third, "bottom", "bank")
  #   
  #   return(point_type)
  # }
  # 
  # find_in_channel_pts <- function(
  #             relative_distances, 
  #             cs_length
  #           ) {
  #   
  #   # calc the:
  #   # - mean distance between points (mean_dist)
  #   # - how much in channel points we have (in_channel_pts)
  #   
  #   dist_between_pts   <- mean(diff(relative_distances))
  #   in_channel_pts     <- ceiling(cs_length / dist_between_pts)
  # 
  #   right_bank <- ceiling(in_channel_pts / 2)
  #   left_bank  <- in_channel_pts - bank1
  #   
  #   return(c(left_bank, right_bank))
  # }
  # 
  # find_banks_and_anchor_pts <- function(depths, 
  #                                       num_of_pts,
  #                                       relative_distance, 
  #                                       cs_length, 
  #                                       point_types, 
  #                                       pts_in_a_third
  # ) {
  #   
  #   bank_idxs <- find_in_channel_pts(relative_distances, cs_length) 
  #   
  #   left_bank  <- bank_idxs[1] 
  #   right_bank <- bank_idxs[2]
  #   
  #   min_bottom     = which(point_types == "bottom")[1]
  #   mid_bottom     = which(point_types == "bottom")[ceiling(length(which(point_types == "bottom"))/2)]
  #   max_bottom     = which(point_types == "bottom")[length(which(point_types == "bottom"))]
  #   
  #   L1             = pmax(1, mid_bottom - left_bank)
  #   L2             = pmax(1, mid_bottom - right_bank)
  #   R1             = pmin(mid_bottom + right_bank, num_of_pts)
  #   R2             = pmin(mid_bottom + left_bank, num_of_pts)
  #   
  #   anchor         = ifelse(depths[R2] < depths[L1], 2, 1)
  #   L              = pmax(pts_in_a_third, ifelse(anchor == 1, L1, L2))
  #   R              = pmin(2*pts_in_a_third[1], ifelse(anchor == 1, R1, R2))
  #   count_left     = min_bottom - L
  #   count_right    = R - max_bottom
  #   L              = ifelse(count_left == 0, L - count_right, L)
  #   R              = ifelse(count_right == 0, R + count_left, R)
  #   
  #   return(c(L, anchor, R))
  #   
  # }
  # 
  # smooth_depths <- function(
  #   depths,
  #   num_of_pts = NULL,
  #   window     = 3
  # ) {
  #   
  #   # depths <- cs_pts$Z
  #   # num_of_pts          <- 10
  #   # window = 3
  #   
  #   if(is.null(num_of_pts)) {
  #     num_of_pts <- length(depths)    
  #   } 
  #   
  #   # calculate a rolling mean of the depths between the starting and ending depth
  #   smoothed_depths <- c(
  #     depths[1],
  #     zoo::rollmean(depths, window),
  #     depths[num_of_pts]
  #   )
  #   
  #   return(smoothed_depths)
  # }
  
  9*11 
  ceiling(9/2)
  
  cs_pts %>% 
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
      max_bottom     = which(class == "bottom")[length(which(class == "bottom"))]
      # L1             = pmax(1, mid_bottom - b1),
      # L2             = pmax(1, mid_bottom - b2),
      # R1             = pmin(mid_bottom + b2, n()),
      # R2             = pmin(mid_bottom + b1, n()),
      # anchor         = ifelse(Z[R2] < Z[L1], 2, 1),
      # L              = pmax(third, ifelse(anchor == 1, L1, L2)),
      # R              = pmin(2*third[1], ifelse(anchor == 1, R1, R2)),
      # count_left     = min_bottom - L,
      # count_right    = R - max_bottom,
      # L              = ifelse(count_left == 0, L - count_right, L),
      # R              = ifelse(count_right == 0, R + count_left, R),
      # class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
      # class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
      # class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)
    ) %>% 
    dplyr::relocate(third, mean_dist, in_channel_pts, b1, b2, low_pt, class, Z, Z2, min_bottom, relative_distance, pt_id) 
  
  classified <- classify_points(
    cs_pts = cs_pts,
    crosswalk_id = ID_COL,
    pct_of_length_for_relief = PCT_OF_LENGTH_FOR_RELIEF
  ) 
  
  classified %>% 
    hydrofabric3D::plot_cs_pts(color = "point_type")
  
  true_valid_banks <- 
    classified %>% 
    dplyr::filter(valid_banks) %>% 
    # dplyr::slice(1:10) %>% 
    dplyr::group_by(hy_id, cs_id)  %>% 
    dplyr::mutate(
      double_check_valid_banks = dplyr::case_when(
        ((left_bank > bottom) & (!is.na(left_bank))) & ((right_bank > bottom) & (!is.na(right_bank))) ~ TRUE,
        # left_bank > bottom & right_bank > bottom ~ TRUE,
        TRUE                                     ~ FALSE
      ),
      correct_valid_banks = valid_banks == double_check_valid_banks
    ) %>% 
    dplyr::relocate(correct_valid_banks, double_check_valid_banks, valid_banks) 
  
  all_valid_banks_are_correct <- all(true_valid_banks$correct_valid_banks)
  testthat::expect_true(all_valid_banks_are_correct)
  
})
