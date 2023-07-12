# library(pbapply)
library(terra)
library(sf)
library(dplyr)
# library(terrainSliceR)
library(mapview)
# library(smoothr)
library(nhdplusTools)
library(wk)
library(geos)
library(vctrs)
library(AOI)
library(ggplot2)
library(tidyr)

source("R/transects.R")
source("R/braids.R")

# ***********************
# ---- TEST NETWORKS ----
# ***********************

# net <- nhdplusTools::navigate_network(start = 1862004, mode = "UT",  distance_km = 100)
# net <- nhdplusTools::navigate_network(start = 1861888, mode = "UT", distance_km = 50)
# net <- nhdplusTools::navigate_network(start = 3558432, mode = "UT", distance_km = 100)
# net <- nhdplusTools::navigate_network(start = 8898064, mode = "UT", distance_km = 100)
# net <-
#   terrainSliceR::linestring %>% 
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# net$geometry %>% plot()

net <- nhdplusTools::navigate_network(
  start       = 101,
  mode        = "UT",
  distance_km = 100
  ) %>%
  dplyr::select( comid, divergence, totdasqkm, fromnode, tonode)

net2 <-
  # net2 %>% 
  net %>% 
  dplyr::select( comid, divergence, totdasqkm, fromnode, tonode) %>% 
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))

# rm(net2)
# plot(net2$geometry)
# plot(net$geometry)

transects = cut_cross_sections2(
  net       = net2,
  id        = "comid",
  cs_widths = pmax(50, net2$bf_width * 7),
  num       = 4,
  add       = TRUE
  ) %>% 
  sf::st_transform(5070)

# pmax(50, net$bf_width * 7)
# transects2 = cut_cross_sections(
#   net       = net,
#   id        = "comid", 
#   cs_widths = pmax(50, net$bf_width * 7),
#   num       = 10
# )

# transects_nocs = cut_cross_sections_curr(
#   net       = net,
#   id        = "comid",
#   # cs_widths = pmax(50, net$bf_width * 7),
#   num       = 10
# )
# 325*10

plot(net2$geometry)
plot(transects$geometry, col = "red", add = T)
mapview::mapview(net2) + 
  mapview::mapview(transects, color = "red") 
  # mapview::mapview(transects2, color = "green") +
  # mapview::mapview(transects_nocs, color = "green")

# # # Get the comids for braided flowlines
# braids <- find_braids(
#   network = net2,
#   return_as = "dataframe",
#   add = TRUE
# )

braids <- find_braids(
  network = net2,
  return_as = "dataframe",
  nested = FALSE,
  add = TRUE
)

braids <- sf::st_transform(braids, 5070)
net2 <- sf::st_transform(net2, 5070)

# # multibraids
# multis <- dplyr::filter(braids, is_multibraid)
# mb

# all braids
all_braids <-   dplyr::filter(braids, braid_id != "no_braid")

# not braided sections
no_braid <- dplyr::filter(braids, braid_id == "no_braid")

# multibraids
multis <- dplyr::filter(braids, is_multibraid)

# SINGLE BRAIDS
singles <- dplyr::filter(braids, !is_multibraid, braid_id != "no_braid")

# DEAL WITH SINGLE BRAIDS

# DETERMINE WHICH FLOWLINE IN EACH BRAID IS MAINSTEM OR if no mainstem, use the divergence == 1 flowlines

main_singles <- 
  singles %>% 
  dplyr::group_by(braid_id) %>% 
  dplyr::filter(divergence == 0) %>% 
  dplyr::ungroup()

# determine the remaining braids that did NOT have a divergence flag of 0, use divergence == 1 as mainstem
div_singles <-
  singles %>% 
    dplyr::filter(!braid_id %in% main_singles$braid_id) %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::filter(
      divergence == 1
    ) %>% 
    dplyr::ungroup()
    # dplyr::mutate(
    #   new_main = ifelse(divergence == 1, 0, 2)
    # )

# check if any left over braids
other_singles <- dplyr::filter(singles, !braid_id %in% c(main_singles$braid_id, div_singles$braid_id))

if(nrow(other_singles) != 0) {
  message("Still need to determine ", nrow(other_singles), " single braided mainstems for braid_ids")
} else {
  message("All single braided mainstem braid_ids accounted for")
}


# Get the transects that are on the divergence == 0 single braided mainstem sections ^^^ (main_singles)
# Mainstem single braid cross sections (ms_xs)
ms_xs <- 
  transects %>% 
  dplyr::filter(hy_id %in% main_singles$comid) %>% 
  dplyr::left_join(
    sf::st_drop_geometry(
      dplyr::select(
        main_singles, comid, braid_id, is_multibraid
      )
    ),
    by = c("hy_id" = "comid")
  )

# Get the transects that are on the divergence == 1 single braided mainstem sections ^^^ (div_singles)
# Pseudo mainstem sections (using divergence == 1 as mainstem) single braid cross sections (ms_xs)
div_xs <- 
  transects %>% 
  dplyr::filter(hy_id %in% div_singles$comid) %>% 
  dplyr::left_join(
    sf::st_drop_geometry(
      dplyr::select(
        div_singles, comid, braid_id, is_multibraid
      )
    ),
    by = c("hy_id" = "comid")
  )

# stash = c()

for (i in 1:nrow(ms_xs)) {
  message("i: ", i, "/", nrow(ms_xs))
  
  # if (i == 11) {
  #   
  #   stop()
  #   
  # }
  # i
  # i = 4
  line <- ms_xs[i, ]$geometry
  bid <-  ms_xs[i, ]$braid_id
  com <-  ms_xs[i, ]$hy_id
  
  boi <- 
    singles %>% 
    dplyr::filter(braid_id == bid)
  
  # st_ends_heading(line)
  
  # new_line <- st_extend_line(line, 1000)
  # notline <- 
  #   boi %>% 
  #   dplyr::filter(boi, !comid %in% com)
  # 
  
  # max distance from transect of interest and rest of braid flowlines
  max_dist <- as.numeric(max(sf::st_distance(  
                dplyr::filter(boi, !comid %in% com), 
                line)
                ))
  
  # ms_xs[i, ]$bf_width
  # distances <- seq(0, max_dist, 100)
  
  # max(c(max_dist, 500))
  
  # 
  distances <- seq(0, max(c(max_dist, 2000)), ms_xs[i, ]$bf_width)
  
  # find the distance it takes from the HEAD of the transect string to reach another part of the braid
  head <- binary_search_distance(
                    x         = distances, 
                    ls        = line,
                    other     = dplyr::filter(boi, !comid %in% com),
                    direction = "head"
                    )
  # find the distance it takes from the TAIL of the transect string to reach another part of the braid
  tail <- binary_search_distance(
                            x         = distances, 
                            ls        = line,
                            other     = dplyr::filter(boi, !comid %in% com),
                            direction = "tail"
                          )
  # length(distances)
  # if(head > length(distances)) {
  #   head = head - 1
  # }
  # if(tail > length(distances)) {
  #   tail = tail - 1
  # }
  
    # head_line <- st_extend_line(line, distances[head-1], 'head')
    # tail_line <- st_extend_line(line, distances[tail-1], 'tail')
    # mapview::mapview(boi) + line + head_line + tail_line
    
  if(which.min(c(head, tail)) == 1) { 
    
    dir = "head"
    cross_idx <- head
    
    } else {
      
    dir = "tail"
    cross_idx <- tail
    
    }
    
    if(cross_idx > length(distances)) {
      
      cross_idx = cross_idx - 1
    }
  # 
  # primary transect line, still needs other side to be fixed/cleaned up 
  crosser <- st_extend_line(line, distances[cross_idx], dir)
  
  # point that crosses over other flowline
  cross_pt <- sf::st_intersection(crosser, dplyr::filter(boi, !comid %in% com))

  # linestring start and ends, 
  end   <- lwgeom::st_endpoint(crosser)
  start <- lwgeom::st_startpoint(crosser)
  
  # distance from crossing point and end/start points. 
  # We end up taking the minimum distance from the crossing point to the ends of the new "crosser" line we made above.
  end_dist   <- as.numeric(sf::st_distance(cross_pt, end))
  start_dist <- as.numeric(sf::st_distance(cross_pt, start))
  

  # END_PT------- CROSSER_PT ----------------------------START_PT
  # |-----------------|
  # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
  # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
  ext_dist <- (ms_xs[i, ]$cs_widths/2) - min(end_dist, start_dist)
  
  # generate final extended line
  res <- st_extend_line(line, distances[cross_idx] + ext_dist, dir)
  # ms_xs[-i,]
  
  if (lengths(sf::st_intersects(res, ms_xs[-i,])) == 0) {
    ms_xs[i, ]$geometry <- res
  }
  
  # ms_xs[i, ]$geometry <- res
  
  message('==============')
  # mapview::mapview(ms_xs) + boi
  # 
  # mapview::mapview(line, color = "red") +
  #   mapview::mapview(crosser, color = "dodgerblue") +
  #   mapview::mapview(res, color = "green") +
  #   # mapview::mapview(cross_pt, col.regions = "red") +
  #   start + end  + boi + ms_xs + transects + singles
  # 
  # mapview::mapview(ms_xs, color = "red") +
  #   mapview::mapview(singles, color = "dodgerblue") +
  #   mapview::mapview(transects, color = "green")
  # library(lwgeom)
  # pt <- sf::st_snap(cross_pt, crosser, tolerance = 10)
  # line_pts <- 
  #   crosser %>% 
  #   sf::st_line_sample(density = 0.07) %>% 
  #   sf::st_cast("POINT") 
  # mapview::mapview(line, color = "red") +
  #   mapview::mapview(crosser, color = "dodgerblue") +
  #   mapview::mapview(res, color = "green") +
  #   mapview::mapview(cross_pt, col.regions = "red") +
  #   start + end  + boi + ms_xs
  
  # # mapview::mapview(singles, color = "dodgerblue") + 
  #   mapview::mapview(main_singles, color = "dodgerblue") +
  #   mapview::mapview(boi, color = "red") +
  #   mapview::mapview(ms_xs, color = "green") + 
  #   mapview::mapview(line, color = "yellow")  +
  #     new_line
}

#   dplyr::filter(boi, !comid %in% com)
binary_search_distance <- function(x, ls, others, direction = "head") {
  
  L = 1
  R = length(x)
  
  # while(L <= R & !flag) {
  while(L <= R) {
    
    M = (L + R) %/% 2
    
    # message("L: ", L)
    # message("M: ", M)
    # message("R: ", R)
    # message("x[L]: ", x[L])
    # message("x[M]: ", x[M])
    # message("x[R]: ", x[R])
    
    new_line <- st_extend_line(ls, x[M], end = direction)

    # check if any of the other braid linestrings get intersected by the extended line:
    # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
    # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
    if(any(lengths(sf::st_intersects(others, new_line)) > 0)) {
      # message("DECREM RIGHT.--> need smaller value")
      # message("R = R - 1 = : ", M - 1)
      # decrement right pointer to middle - 1
      R = M - 1
    } else {
      # message("DECREM RIGHT.--> need smaller value")
      # message("L = M + 1 = : ", M + 1)
      # increment left pointer to middle + 1
      L = M + 1
    }
    # message("=======================")
    # mapview::mapview(new_line) + boi + line + singles + others <- 
  }
  # l_line = st_extend_line(ls, x[L])
  # return(l_line)
  return(L)
}
# function for finding direction each line end point is pointing
st_ends_heading <- function(line) {
  M <- sf::st_coordinates(line)
  i <- c(2, nrow(M) - 1)
  j <- c(1, -1)
  
  headings <- mapply(i, j, FUN = function(i, j) {
    Ax <- M[i-j,1]
    Ay <- M[i-j,2]
    Bx <- M[i,1]
    By <- M[i,2]
    unname(atan2(Ay-By, Ax-Bx))
  })
  
  return(headings)
}

# function which extends the line (one or both ends) by a given distance (in unit distance):
st_extend_line <- function(line, distance, end = "both") {
  
  if (!(end %in% c("both", "head", "tail")) | length(end) != 1){
    stop("'end' must be 'both', 'head' or 'tail'")
  }
  
  M <- sf::st_coordinates(line)[,1:2]
  keep <- !(end == c("tail", "head"))
  
  ends <- c(1, nrow(M))[keep]
  headings <- st_ends_heading(line)[keep]
  distances <- if (length(distance) == 1) rep(distance, 2) else rev(distance[1:2])
  
  M[ends,] <- M[ends,] + distances[keep] * c(cos(headings), sin(headings))
  newline <- sf::st_linestring(M)
  
  # If input is sfc_LINESTRING and not sfg_LINESTRING
  if (is.list(line)) newline <- sf::st_sfc(newline, crs = sf::st_crs(line))
  
  return(newline)
}
ms_xs


mapview::mapview(singles, color = "dodgerblue") + 
  mapview::mapview(main_singles, color = "red") +
  mapview::mapview(ms_xs, color = "yellow") +
  mapview::mapview(div_singles, color = "green") +
  mapview::mapview(div_xs, color = "cyan") + 
  mapview::mapview(transects, color = "hotpink") +
  net2


main_flines <- 
  b %>% 
  dplyr::group_by(braid_id) %>% 
  # dplyr::mutate(
  #   mainstem = ifelse(divergence == 0, "mainstem", "div")
  # ) %>% 
  # dplyr::filter(mainstem == "mainstem")
  dplyr::filter(divergence == 0) %>% 
  dplyr::ungroup()

mapview::mapview(main_flines) + 
  mapview::mapview(b, color = "red") +
  mapview::mapview(mb, color = "green") +
  mapview::mapview(mb, color = "green") +
  transects
# cross sections on braided river segments
braid_xs <- 
  transects %>% 
  dplyr::filter(hy_id %in% dplyr::filter(braids, braid_id != "no_braid")$comid) %>% 
  dplyr::left_join(
    sf::st_drop_geometry(
      dplyr::select(
      dplyr::filter(braids, braid_id != "no_braid"), comid, braid_id, is_multibraid
      )
    ),
    by = c("hy_id" = "comid")
  )
mapview::mapview(singles) + 
  mapview::mapview(main_singles, color = "red") +
  mapview::mapview(div_singles, color = "green") +
  mapview::mapview(braid_xs, color = "cyan")
mapview::mapview(no_braid) + 
  mapview::mapview(b, color = "red") +
  mapview::mapview(mb, color = "green") +
  mapview::mapview(braid_xs, color = "green")


braid_ids <- unique(Reduce(c, strsplit(unique(braid_xs$braid_id), ", ")))

bxs <- lapply(1:length(braid_ids), function(k) {
  
  k = 6
  bid <- braid_ids[k]
  bid
  blines <- 
    braids %>% 
    dplyr::filter(braid_id == bid)
  tmp <- braid_xs %>% 
    dplyr::filter(braid_id == bid)
  
  ucomids <- unique(tmp$hy_id)
  
  mapview::mapview(braids, color = "dodgerblue")  +
    mapview::mapview(blines, color = "red") + 
    tmp 
  for (i in ucomids) {
    i = 1
    
    ucomids[i]
  
   toi <- dplyr::filter(tmp, hy_id %in% ucomids[i])
   others <- dplyr::filter(tmp, !hy_id %in% ucomids[i])
   
   match_lines <- others[sf::st_nearest_feature(toi, others) , ]
   mapview::mapview(braids, color = "dodgerblue")  +
     mapview::mapview(blines, color = "red") + 
     tmp + 
     toi +
     others + 
     mapview::mapview(match_lines, color = "red") 
   mapview::mapview()   
  }
  tmp
  
  mapview::mapview(braids, color = "dodgerblue")  + mapview::mapview(blines, color = "red") + tmp 
  
  })

# make_braids_gif(
#   network    = find_braids(network = net2,return_as = "dataframe",nested = F,add = T),
#   save_path  = "D:/gif/braid_gif6.gif",
#   height     = 8,
#   width      = 12,
#   gif_width  = 1900,
#   gif_height = 1600,
#   delay      = 0.80,
#   verbose    = T
# )




