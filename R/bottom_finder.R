# finds the beginngin and end of plateous (flat set of cross section points)
find_plateaus <- function(x) {
  n <- length(x)
  
  plateau_starts <- c()
  plateau_ends   <- c()
  
  i <- 1
  
  while (i <= n) {
    # find beginning of plateau
    start <- i
    while (i < n && x[i] == x[i + 1]) {
      i <- i + 1
    }
    # found a plateau (length > 1)
    if (i > start) {
      
      plateau_starts <- c(plateau_starts, start)
      plateau_ends   <- c(plateau_ends, i)
      
    }
    
    i <- i + 1
    
  }
  
  return(
    list(starts = plateau_starts, ends = plateau_ends)
         )
}

# find local minima indices using plateau midpoints
find_local_minima <- function(x) {
  # x = c(1, 1, 1, 1)
  n <- length(x)
  if (n < 2) {
    return(integer(0))
  }
  
  indices  <- c()
  plateaus <- find_plateaus(x)
  
  # first point or plateau
  if (length(plateaus$starts) > 0 && plateaus$starts[1] == 1) {
    if (plateaus$ends[1] < n && x[plateaus$ends[1]] < x[plateaus$ends[1] + 1]) {
      
      # use middle index of first plateau
      indices <- c(indices, floor((plateaus$starts[1] + plateaus$ends[1]) / 2))
    }
  } else if (x[1] < x[2]) {
    indices <- c(indices, 1)
  }
  
  # interior points and plateaus
  i <- 2
  while (i < n) {
    
    #  current position is at start of a flat plateau 
    plateau_idx <- which(plateaus$starts == i)
    
    if (length(plateau_idx) > 0) {
      
      plateau_end <- plateaus$ends[plateau_idx]
      
      # is plataeu a  minimum?
      if (
        (i > 1 && x[i] < x[i-1]) && 
        (plateau_end < n && x[plateau_end] < x[plateau_end + 1])
      ) {
        
        # use middle of the plataeu
        indices <- c(indices, floor((i + plateau_end) / 2))
        
      }
      
      i <- plateau_end + 1
      
    } else {
      
      # case when its just a single point minimum (simple case actually)
      if (x[i] < x[i-1] && x[i] < x[i+1]) {
        
        indices <- c(indices, i)
        
      }
      i <- i + 1
    }
  }
  
  # check if last point or plateau
  if (length(plateaus$ends) > 0 && plateaus$ends[length(plateaus$ends)] == n) {
    
    last_plateau_start <- plateaus$starts[length(plateaus$starts)]
    
    if (last_plateau_start > 1 && x[last_plateau_start] < x[last_plateau_start - 1]) {
      # if (x[last_plateau_start] < x[last_plateau_start - 1]) {
      
      # use middle of last plateau
      indices <- c(indices, floor((last_plateau_start + n) / 2))
    }
  } else if (x[n] < x[n-1]) {
    indices <- c(indices, n)
  }
  
  return(indices)
}

#  finds local maxima indices including plateaus
find_local_maxima <- function(x) {
  n <- length(x)
  
  if (n < 2) {
    return(integer(0))
  }
  
  indices  <- c()
  plateaus <- find_plateaus(x)
  
  #  first point or plateau
  if (length(plateaus$starts) > 0 && plateaus$starts[1] == 1) {
    
    if (plateaus$ends[1] < n && x[plateaus$ends[1]] > x[plateaus$ends[1] + 1]) {
      
      indices <- c(indices, c(plateaus$starts[1], plateaus$ends[1]))
      
    }
  } else if (x[1] > x[2]) {
    
    indices <- c(indices, 1)
    
  }
  
  #  interior points and plateaus
  i <- 2
  while (i < n) {
    # is current index a plateau start ? 
    plateau_idx <- which(plateaus$starts == i)
    
    if (length(plateau_idx) > 0) {
      
      plateau_end <- plateaus$ends[plateau_idx]
      
      # plateau is plateu a maximum?
      if ((i > 1 && x[i] > x[i-1]) &&
          (plateau_end < n && x[plateau_end] > x[plateau_end + 1])
          ) {
        
        indices <- c(indices, c(i, plateau_end))
        
      }
      
      i <- plateau_end + 1
      
    } else {
      # Single point maximum
      
      if (x[i] > x[i-1] && x[i] > x[i+1]) {
        indices <- c(indices, i)
        
      }
      
      i <- i + 1
      
    }
  }
  
  # check if last point or plateau
  if (length(plateaus$ends) > 0 && plateaus$ends[length(plateaus$ends)] == n) {
    
    last_plateau_start <- plateaus$starts[length(plateaus$starts)]
    
    if (last_plateau_start > 1 && x[last_plateau_start] > x[last_plateau_start - 1]) {
      # if (x[last_plateau_start] > x[last_plateau_start - 1]) { 
      indices <- c(indices, c(last_plateau_start, n))
      
    }
  } else if (x[n] > x[n-1]) {
    
    indices <- c(indices, n)
    
  }
  
  return(indices)
}

# finds local minimas and then the neighboring local maximas, returns a list of the index values for those poitnts
find_bottom_candidates <- function(x, index_only = TRUE) {
  
  # x = Z_VALS
  # index_only    <- T
  # x = c(1, 1, 3,  1)
  # x = c(1, 2, 1, 2, 3, 4, 5, 5, 3, 2, 4, 6, 8) 
  # plot(x) 
  
  # get pts of local mins and maxs
  minima_idx <- find_local_minima(x)
  maxima_idx <- find_local_maxima(x)
  
  # minima_idx
  
  #  result list
  result <- list()
  
  # process each minimum pt
  for (current_min_idx in minima_idx) {
    
    # current_min_idx <- minima_idx[1]
    
    # Find maxima to the left
    left_maxima  <- maxima_idx[maxima_idx < current_min_idx]
    left_max_idx <- if (length(left_maxima) > 0) max(left_maxima) else NA
    
    # Find maxima to the right
    right_maxima  <- maxima_idx[maxima_idx > current_min_idx]
    right_max_idx <- if (length(right_maxima) > 0) min(right_maxima) else NA
    index_only    <- TRUE
    
    minimum <- if(index_only) { current_min_idx } else {
      list(
        index = current_min_idx,
        value = x[current_min_idx]
      )
    }
    
    left_max <- if (!is.na(left_max_idx)) {
      if(index_only) {
        left_max_idx
      } else {
        list(
          index = left_max_idx,
          value = x[left_max_idx]
        )
      }
    } else { 
      NULL
    }
    
    right_max <- if (!is.na(right_max_idx)) {
      if(index_only) {
        right_max_idx
      } else {
        list(
          index = right_max_idx,
          value = x[right_max_idx]
        )
      }
    } else { 
      NULL
    }
    
    # get the width and depth of this bucket
    width     <- (right_max_idx - left_max_idx) + 1
    depth     <- min(x[left_max_idx], x[right_max_idx]) - x[current_min_idx]
    
    # Create entry in result list
    result[[length(result) + 1]] <- list(
      minimum       = minimum,
      left_max      = left_max,
      right_max     = right_max,
      width         = width,
      depth         = depth
    )
  }
  
  return(result)
}

# x = c(1, 1, 1, 1, 3, 4, 6, 7, 8, 7, 6, 6, 6, 5, 4, 3, 1, 3, 4, 5, 5, 6, 6, 5, 5, 2, 2, 0, 2, 2, 4, 6, 7, 8, 8, 
#       9, 4, 3, 3, 2, 1, 3, 4, 5, 6, 7, 8, 9, 10, 10, 10, 12, 13, 14, 15, 14, 16, 16
# )
# x = c(1, 2, 1, 1, 3, 4)
# x = c(1, 1, 1, 1)

# plot(x)

# bucket_indexes <- 
#   x %>% 
#   find_bottom_candidates() %>% 
#   rm_edge_buckets() %>% 
#   anchor_picker()

# anchor_pts <- list(
#   L = bucket_indexes$left_max,
#   M = bucket_indexes$minimum,
#   R = bucket_indexes$right_maxixum
# )

# TODO: work in progress, getting this implemented into classify_points(), if this method fails, fall back to original method
# TODO: that relies on the middle third of cross sections containing the bottom points / thalweg

# from the output of the find_minima and neighbors, pick the widest and deepest bucket
anchor_picker <- function(bucket_indexes) {
  
  if(length(bucket_indexes) > 0) {
    sort_order <- order( 
      -sapply(bucket_indexes, `[[`, "width"),
      -sapply(bucket_indexes, `[[`, "depth")
    )
    
    anchor <- bucket_indexes[sort_order[1]]
    
    return(anchor)
  } 
  
  return(list())
}

# min_indexes <- find_bottom_candidates(Z_VALS)
# min_indexes <- rm_edge_buckets(min_indexes)

# removes buckets on the edges of the set of points
rm_edge_buckets <- function(bucket_indexes) {
  if (length(bucket_indexes) > 0) {
    bucket_indexes <- bucket_indexes[sapply(bucket_indexes, function(i) { !is.null(i$left_max) & !is.null(i$right_max) })]
  }
  
  return(bucket_indexes)
  
}