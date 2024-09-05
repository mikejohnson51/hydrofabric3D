library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

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
  MIN_PTS_PER_CS    <- 10
  
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
              ) %>% 
    dplyr::filter(hy_id == "wb-1003267", cs_id == 3)
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
  cs_pts %>% hydrofabric3D::plot_cs_pts()
  
  num_of_pts          <- cs_pts$points_per_cs[1]
  pt_ids              <- cs_pts$pt_id
  relative_distances  <- cs_pts$relative_distance 
  depths              <- cs_pts$Z
  cs_length           <- cs_pts$cs_lengthm[1]
  
  # -------------------------------------------------------------------
  # -------------------------------------------------------------------
  
  point_types <- classify_banks_and_bottoms(
                  num_of_pts,
                  pt_ids,
                  depths
                )
  
  smoothed_depths <- smooth_depths(
    depths = depths, 
    window = 3
    )
  
  # plot(depths)
  # plot(smoothed_depths) 
 
  # get a smoothed version of the DEM points
  depths <- use_smoothed_depths(depths, smoothed_depths, point_types)
  
  banks_and_anchor <- find_banks_and_anchor_pts(
                        depths            = depths,
                        num_of_pts        = num_of_pts,
                        relative_distance = relative_distances,
                        cs_length         = cs_length,
                        point_types       = point_types,
                        pts_in_a_third    = third
                      )
  
  left_bank    <- banks_and_anchor[1]
  anchor       <- banks_and_anchor[2]
  right_bank   <- banks_and_anchor[3]
  
  left_to_middle  <- 1:left_bank
  middle_to_right <- right_bank:length(depths)
  
  left_side    <- depths[left_to_middle]
  
  left_max_depth <- max(left_side)
  left_max_idxs <- which(left_side == left_max_depth)
  left_bank_anchor <- left_max_idxs[length(left_max_idxs)]
  
  point_types[1:left_bank_anchor] <- "left_bank"
  point_types[(left_bank_anchor + 1):left_bank] <- "channel"
  
  
  which(left)
  # left_max <- depths
  
  
  depths
  
  left_max_Z
  
  classed_pts <- 
    cs_pts %>%
      sf::st_drop_geometry() %>% 
      hydrofabric3D::classify_points(crosswalk_id = "hy_id") %>% 
      dplyr::mutate(
        new_classes = FALSE
      )
  
  classed_pts2 <- classed_pts 
  classed_pts2$Z          <- depths
  classed_pts2$point_type <- classify_z_pts(
                                  depths      = depths,
                                  pt_ids      = pt_ids,
                                  point_types = point_types,
                                  L           = left_bank,
                                  R           = right_bank
                                )
  classed_pts2 <- 
    classed_pts2 %>% 
    dplyr::mutate(
      new_classes = TRUE
    )
  
  dplyr::bind_rows(
    classed_pts,
    classed_pts2
  ) %>% 
  # classed_pts %>% 
    ggplot2::ggplot() +
    ggplot2::geom_point(
      ggplot2::aes(
        x = pt_id,
        y = Z,
        color = point_type
      ), 
      size = 5
    ) +
    ggplot2::facet_wrap(~new_classes)
  
    # hydrofabric3D::plot_cs_pts(color = "point_type2")
  
  bank_idxs <- find_in_channel_pts(relative_distances, cs_length) 
  
  LEFT = L
  RIGHT = R

  class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class)
  class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class)
  class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)
  
  
  # compute the first derivative (slope)
  slope <- diff(depths) / diff(relative_distances)
  
  # compute the second derivative (curvature)
  second_derivative <- diff(slope) / diff(relative_distances[-1])
  
  second_derivative %>% round(5)
  
  classify_z_pts <- function(
                        depths, 
                        pt_ids, 
                        point_types,
                        L,
                        R
                        ) {
    
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
    #   
    #   message("NO MATCH\n")
    #   
    # })
    
    classification[classification %in% c("left_channel", "right_channel")] <- "channel"
    
    return(classification)
    
  }
  
  
  # df <- data.frame(
  #   Z = depths,
  #   Z2 = smoothed_depths,
  #   Z3 = depths2,
  #   point_type = point_types
  # )
  plot(depths)
  # plot(depths2)
  plot(smoothed_depths)
  
  plot(use_smoothed_depths(depths, smoothed_depths, point_types))
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
  
  # determine bottom point types
  third           <- ceiling(num_of_pts / 3)
  mean_dist       <- mean(diff(relative_distances))
  in_channel_pts  <- ceiling(cs_length / mean_dist)
  
  bank1 <- ceiling(in_channel_pts / 2)
  bank2 <- in_channel_pts - bank1
  
  # third_left_idx   <- third
  # third_right_idx  <- ((2 * third) - 1)
  
  mid_third_left_idx   <- third
  mid_third_right_idx  <- ((2 * third) - 1)
  
  mid_third_idxs    <- mid_third_left_idx:mid_third_right_idx
  # mid_third_idxs    <- third:mid_third_right_idx 
  # mid_third_idxs    <- third:((2 * third) - 1)
  
  mid_third_low_pt  <- min(depths[mid_third_idxs])
  # mid_third_low_pt  <- min(depths[third:((2 * third) - 1)])
  
  is_at_bottom_Z      <- depths <= mid_third_low_pt
  is_at_middle_third  <- dplyr::between(pt_ids, mid_third_left_idx, mid_third_right_idx)
  point_type          <- ifelse(is_at_bottom_Z & is_at_middle_third, 
                               "bottom", 
                               "bank"
                               )
  
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
  
  find_in_channel_pts <- function(
              relative_distances, 
              cs_length
            ) {
    
    # calc the:
    # - mean distance between points (mean_dist)
    # - how much in channel points we have (in_channel_pts)
    
    dist_between_pts   <- mean(diff(relative_distances))
    in_channel_pts     <- ceiling(cs_length / dist_between_pts)

    right_bank <- ceiling(in_channel_pts / 2)
    left_bank  <- in_channel_pts - bank1
    
    return(c(left_bank, right_bank))
  }
  
  find_banks_and_anchor_pts <- function(depths, 
                                        num_of_pts,
                                        relative_distance, 
                                        cs_length, 
                                        point_types, 
                                        pts_in_a_third
  ) {
    
    bank_idxs <- find_in_channel_pts(relative_distances, cs_length) 
    
    left_bank  <- bank_idxs[1] 
    right_bank <- bank_idxs[2]
    
    min_bottom     = which(point_types == "bottom")[1]
    mid_bottom     = which(point_types == "bottom")[ceiling(length(which(point_types == "bottom"))/2)]
    max_bottom     = which(point_types == "bottom")[length(which(point_types == "bottom"))]
    
    L1             = pmax(1, mid_bottom - left_bank)
    L2             = pmax(1, mid_bottom - right_bank)
    R1             = pmin(mid_bottom + right_bank, num_of_pts)
    R2             = pmin(mid_bottom + left_bank, num_of_pts)
    
    anchor         = ifelse(depths[R2] < depths[L1], 2, 1)
    L              = pmax(pts_in_a_third, ifelse(anchor == 1, L1, L2))
    R              = pmin(2*pts_in_a_third[1], ifelse(anchor == 1, R1, R2))
    count_left     = min_bottom - L
    count_right    = R - max_bottom
    L              = ifelse(count_left == 0, L - count_right, L)
    R              = ifelse(count_right == 0, R + count_left, R)
    
    return(c(L, anchor, R))
    
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
