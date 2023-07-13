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

net2 <- nhdplusTools::navigate_network(
  start       = 101,
  mode        = "UT",
  distance_km = 100
  ) %>%
  dplyr::select( comid, divergence, totdasqkm, fromnode, tonode)

net3 <-
  # net2 %>% 
  net2 %>% 
  dplyr::select( comid, divergence, totdasqkm, fromnode, tonode) %>% 
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))

# rm(net2)
# plot(net2$geometry)
# plot(net$geometry)

transects = cut_cross_sections2(
  net       = net3,
  id        = "comid",
  cs_widths = pmax(50, net3$bf_width * 7),
  num       = 4,
  add       = TRUE
  ) 
# %>% 
  # sf::st_transform(5070)

braids <- find_braids(
  network   = net3,
  return_as = "dataframe",
  nested    = FALSE,
  add       = TRUE
) %>% 
  dplyr::filter(braid_id != "no_braid")

sing <- dplyr::filter(braids, !is_multibraid)
multi <- dplyr::filter(braids, is_multibraid)
  # dplyr::
div_multi <-  multi %>% 
  dplyr::filter(divergence == 0)
one_multi <-  multi %>% 
  dplyr::filter(divergence == 1)

nest_braids <- find_braids(
  network   = net3,
  return_as = "dataframe",
  nested    = T,
  add       = TRUE
) %>% 
  dplyr::filter(braid_id != "no_braid")

nest_multi <- dplyr::filter(nest_braids, is_multibraid)
# FIX the single braids...

transects2 <- fix_braid_transects(net3, transects)
mapview::mapview(net3, color = "cyan") +
  mapview::mapview(transects, color = "green") +
  mapview::mapview(transects2, color = "red") +
  mapview::mapview(sing, color = "dodgerblue") +
mapview::mapview(multi, color = "gold") +
  mapview::mapview(div_multi, color = "red") +
  mapview::mapview(one_multi, color = "green") +
  mapview::mapview(nest_multi, color = "dodgerblue")
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

# plot(net2$geometry)
# plot(transects$geometry, col = "red", add = T)
# mapview::mapview(net2) + 
#   mapview::mapview(transects, color = "red") 
  # mapview::mapview(transects2, color = "green") +
  # mapview::mapview(transects_nocs, color = "green")

# # # Get the comids for braided flowlines
# braids <- find_braids(
#   network = net2,
#   return_as = "dataframe",
#   add = TRUE
# )
fix_multibraid_transects <- function(net, transect_lines) {
  # transect_lines = transects
  # net <- net3

  # keep track of the CRS of the input to retransform return 
  start_crs <- sf::st_crs(net, parameters = T)$epsg
  
  message("Start CRS: ", start_crs)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG:5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  transect_lines <- sf::st_transform(transect_lines, 5070) 
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = TRUE,
    add       = TRUE
  )
  
  # filter braids down to just multibraids
  multis <- dplyr::filter(braids, is_multibraid)
  
  # multis$braid_id
  # multis %>% dplyr::group_by(braid_id)
  
  # add number of braids in each multibraid
  multis$nbraids <- lengths(strsplit(multis$braid_id, ", "))
  
  # filter multibraids to braid_ids with more than 1 braid_id
  # multis <- dplyr::filter(multis, nbraids != 1)
  
  
  
  
  multis %>%
    dplyr::group_by(braid_id)
  # # multis <- 
  #   multis %>%
  #   dplyr::mutate(
  #     nbraids = length(strsplit(braid_id, ", ")[1])
  #   ) %>% 
  #   dplyr::filter(nbraids != 1)
  # single braids on the ACTUAL main stem
  main_multis <-
    multis %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::filter(divergence == 0) %>% 
    dplyr::ungroup()
  
  # determine the remaining braids that did NOT have a divergence flag of 0, use divergence == 1 as mainstem
  div_multis <-
    multis %>% 
    dplyr::filter(!braid_id %in% main_multis$braid_id) %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::filter(
      divergence == 1
    ) %>% 
    dplyr::ungroup()
  
  # Get the transects that are on the divergence == 0 single braided mainstem sections ^^^ (main_singles)
  # Mainstem single braid cross sections (ms_xs)
  # main_xs <-
  multis_xs <- 
    transect_lines %>% 
    dplyr::filter(hy_id %in% main_multis$comid) %>% 
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          main_multis, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) 
  
  # # Get the transects that are on the divergence == 1 single braided mainstem sections ^^^ (div_singles)
  # # Pseudo mainstem sections (using divergence == 1 as mainstem) single braid cross sections (ms_xs)
  # div_xs <-
  #   transect_lines %>%
  #   dplyr::filter(hy_id %in% div_multis$comid) %>%
  #   dplyr::left_join(
  #     sf::st_drop_geometry(
  #       dplyr::select(
  #         div_multis, comid, braid_id, is_multibraid
  #       )
  #     ),
  #     by = c("hy_id" = "comid")
  #   )
  # dplyr::left_join(  
  #   dplyr::filter(transect_lines, hy_id %in% div_singles$comid),
  #   sf::st_drop_geometry(
  #     dplyr::select(div_singles, comid, braid_id, is_multibraid)
  #     ), by = c("hy_id" = "comid"))
  # multis_xs <- dplyr::bind_rows(main_xs, div_xs)
  
  mapview::mapview(multis_xs, color = "red") + 
    mapview::mapview(transect_lines, color = "green") + 
    mapview::mapview(multis, color = "dodgerblue")
  
  
  for (i in 1:nrow(multis_xs)) {
    
    message(i, " / ", nrows(multis_xs))
    # i = 1
  # multis
    
    # braid IDs of interest
    bids <- strsplit(multis_xs[i, ]$braid_id, ", ")[[1]]
    # bids
    
    # comid of transect line
    com <- multis_xs[i, ]$hy_id
    
    # transect line to extend out
    tline <- multis_xs[i, ]$geometry
    
    # get all linestrings that are apart of the braid_ids of interest
    in_bid <- sapply(1:length(multis$braid_id), function(x) {
      any(strsplit(multis$braid_id[x], ", ")[[1]] %in% bids)
      # any(strsplit(multis_xs$braid_id[x], ", ")[[1]] %in% bids)
    })
    
    
    # multis[in_bid, ]$comid
    
    # flowlines within overall multibraid (EXCEPT SELF)
    others <- dplyr::filter(
                  multis, 
                  # comid %in% multis[in_bid, ]$hy_id
                  comid %in% multis[in_bid, ]$comid & comid != com
                  )
    
    # max(as.numeric(sf::st_distance(tline, others)))
    
    # max distance from transect of interest and rest of braid flowlines 
    # TODO (need a better method of determing max possible extension of flowline)
    max_dist <- as.numeric(max(sf::st_distance(  
      dplyr::filter(boi, !comid %in% com), 
      line)
    ))
    sf::st_distance(tline, sf::st_centroid(others))
    sf::st_distance(sf::st_centroid(others), tline)
    # sequence from 0 to the max possible extension distance 
    dist_vect <- seq(0, max(c(max_dist, 2000)), multis_xs[i, ]$bf_width)
    max_dist <- max(
                  as.numeric(
                    sf::st_distance(sf::st_centroid(others), tline)
                    )
                  )
    
    dist_vect <- seq(0, max(c(max_dist, 2000)), multis_xs[i, ]$bf_width)
    dist_vect
    # dist_vect = seq(0, )
    
    head_ext <- extend_out(
      x             = 1,
      line          = tline, 
      distances     = dist_vect,
      geoms_to_cut  = others, 
      ids           = c(com), 
      dir           = "head"
      )
    tail_ext <- extend_out(
      x             = 1,
      line          = tline, 
      distances     = dist_vect,
      geoms_to_cut  = others, 
      ids           = c(com), 
      dir           = "tail"
    )
    
    extend_out <- function(
                           x,
                           line,
                           distances,
                           geoms_to_cut, 
                           ids, 
                           dir = "head"
                           ) {
      # x = 1
      # line          = tline
      # distances      = dist_vect
      # geoms_to_cut  = others
      # ids           = c(com)
      # dir           = "head"
      # x             = 1
      # line          = tline
      # distances     = dist_vect
      # geoms_to_cut  = others
      # ids           = c(com)
      # dir           = "head"
      # if binary search returns that no intersection happened (extended all the way out)
      # BASE CASE OF RECURSION
      if(x >= length(distances)) {
        message("---- HIT THE BASE CASE ----")
        return()
      }

      message("x: ", x)
      message("length(distances): ", length(distances))
      message("ids: ", paste0(ids, sep = ", "))
      message("RUNNING binary search...")
      
      xx <- binary_search_distance(
        distances    = distances, 
        line         = line, 
        geoms_to_cut = geoms_to_cut,
        direction    = dir
      )
      
      # extend line out to where the binary search first hit a line
      crosser <- st_extend_line(line, distances[xx], end = dir)  
      
      # intersection of crosser line and rest of multibraid except current comid(s)
      # sf::st_intersection(crosser,
      #                     dplyr::filter(geoms_to_cut, !comid %in% ids)
      #                     )
      
      # get the comid of the flow line that was interesected, 
      # new comid that should be added to "ids" variable and passed to recursive function call
      new_comid <- geoms_to_cut$comid[
                          unlist(sf::st_intersects(crosser,
                                                   dplyr::filter(geoms_to_cut, !comid %in% ids)
                                                   )
                                 )
                          ]
    # others
    # in_bid
      # mapview::mapv
      # x            = xx
      # line         = crosser
      # distances    = distances
      # geoms_to_cut =  dplyr::filter(geoms_to_cut, !comid %in% c(ids, new_comid))
      # ids          = c(ids, new_comid)
      # dir          = dir
      
      message("====== RECURSIVE STEP ======")
      
    # dplyr::filter(geoms_to_cut, !comid %in% c(ids, new_comid))
    extend_out(
      x            = xx,
      line         = crosser,
      distances    = distances,
      geoms_to_cut =  dplyr::filter(geoms_to_cut, !comid %in% c(ids, new_comid)), 
      ids          = c(ids, new_comid), 
      dir          = dir
      )
      
    # mapview::mapview(multis_xs, color = "gold") +
    #   mapview::mapview(transect_lines, color = "green") +
    #   mapview::mapview(multis, color = "dodgerblue") +
    #   mapview::mapview(others, color = "red") +
    #   mapview::mapview(notself, color = "red") + tline + crosser
    # others <- dplyr::filter(multis, comid %in% multis_xs[in_bid, ]$hy_id)
    # notself <- dplyr::filter(others, hy_id != com)
    # 
    }
  
  
  }

fix_braid_transects <- function(net, transect_lines) {
  # transect_lines = transects
  # net <- net3
  
  # keep track of the CRS of the input to retransform return 
  start_crs <- sf::st_crs(net, parameters = T)$epsg
  
  message("Start CRS: ", start_crs)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG:5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = FALSE,
    add       = TRUE
    )
  
  # braids <- sf::st_transform(braids, 5070)
  # net    <- sf::st_transform(net, 5070)
  
  # # multibraids
  # multis <- dplyr::filter(braids, is_multibraid)
  # mb
  
  # all braids
  # all_braids <-   dplyr::filter(braids, braid_id != "no_braid")
  
  # not braided sections
  # no_braid <- dplyr::filter(braids, braid_id == "no_braid")
  
  # multibraids
  # multis <- dplyr::filter(braids, is_multibraid)
  
  # SINGLE BRAIDS
  singles <- dplyr::filter(braids, !is_multibraid, braid_id != "no_braid")
  
  # DEAL WITH SINGLE BRAIDS
  
  # DETERMINE WHICH FLOWLINE IN EACH BRAID IS MAINSTEM OR if no mainstem, use the divergence == 1 flowlines
  
  # single braids on the ACTUAL main stem
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
    transect_lines %>% 
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
    transect_lines %>%
    dplyr::filter(hy_id %in% div_singles$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          div_singles, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    )
    # dplyr::left_join(  
    #   dplyr::filter(transect_lines, hy_id %in% div_singles$comid),
    #   sf::st_drop_geometry(
    #     dplyr::select(div_singles, comid, braid_id, is_multibraid)
    #     ), by = c("hy_id" = "comid"))
  
  single_xs <- dplyr::bind_rows(ms_xs, div_xs)
  
  # stash = c()
  
  message("Extending transects of single braids...")
  
  for (i in 1:nrow(single_xs)) {
    message("i: ", i, "/", nrow(single_xs))
    
    # if (i == 22) {
    #   stop()
    # }
    
    # braid transect line
    line <- single_xs[i, ]$geometry
    
    # braid ID
    bid <-  single_xs[i, ]$braid_id
    
    # transect COMID
    com <-  single_xs[i, ]$hy_id
    
    # braid of interest
    boi <-  dplyr::filter(singles, braid_id == bid)
  
    # max distance from transect of interest and rest of braid flowlines 
    # TODO (need a better method of determing max possible extension of flowline)
    max_dist <- as.numeric(max(sf::st_distance(  
                  dplyr::filter(boi, !comid %in% com), 
                  line)
                  ))
    
    # sequence from 0 to the max possible extension distance 
    distances <- seq(0, max(c(max_dist, 2000)), single_xs[i, ]$bf_width)
    
    # find the distance it takes from the HEAD of the transect string to reach another part of the braid
    head <- binary_search_distance(
                      distances      = distances, 
                      line           = line,
                      geoms_to_cut   = dplyr::filter(boi, !comid %in% com),
                      direction      = "head"
                      )
    # find the distance it takes from the TAIL of the transect string to reach another part of the braid
    tail <- binary_search_distance(
                      distances         = distances, 
                      line              = line,
                      geoms_to_cut      = dplyr::filter(boi, !comid %in% com),
                      direction.        = "tail"
                      )
    
    # length(distances)
    # if(head > length(distances)) { head = head - 1 }
    # if(tail > length(distances)) { tail = tail - 1 }
    
      # head_line <- st_extend_line(line, distances[head-1], 'head')
      # tail_line <- st_extend_line(line, distances[tail-1], 'tail')
      # mapview::mapview(boi) + line + head_line + tail_line
      
    # pick the extension direction that had the soonest interesection with the rest of the braid
    if(which.min(c(head, tail)) == 1) { 
      
      dir = "head"
      cross_idx <- head
      
      } else {
        
      dir = "tail"
      cross_idx <- tail
      
      }
      
      # the index of the intersection point is out of bounds, set it to the max distance value
      if(cross_idx > length(distances)) {
        
        cross_idx = cross_idx - 1
      }
    
    # primary transect line, still needs other side to be fixed/cleaned up 
    crosser <- st_extend_line(line, distances[cross_idx], dir)
    
    # point that crosses over other flowline
    cross_pt <- sf::st_intersection(crosser, dplyr::filter(boi, !comid %in% com))
    # sf::st_intersection(crosser, boi)
    
    # linestring start and ends, 
    end   <- lwgeom::st_endpoint(crosser)
    start <- lwgeom::st_startpoint(crosser)
    
    # distance from crossing point and end/start points. 
    # We end up taking the minimum distance from the crossing point to the ends of the new "crosser" line we made above.
    end_dist   <- as.numeric(sf::st_distance(cross_pt, end))
    start_dist <- as.numeric(sf::st_distance(cross_pt, start))
  
    # set distances to 0 if no crossing point is on top of the start/end
    end_dist   <- ifelse(length(end_dist) == 0, 0, end_dist)
    start_dist <- ifelse(length(start_dist) == 0, 0, start_dist)
    
    if (end_dist == 0 & start_dist == 0) {
      message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      message("--- CONTINUING TO NEXT TRANSECT ---")
      next
    }
  
    # END_PT------- CROSSER_PT ----------------------------START_PT
    # |-----------------|
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    ext_dist <- (single_xs[i, ]$cs_widths/2) - min(end_dist, start_dist)
    
    # generate final extended line
    res <- st_extend_line(line, distances[cross_idx] + ext_dist, dir)
    
    if (lengths(sf::st_intersects(res, single_xs[-i,])) == 0) {
      single_xs[i, ]$geometry <- res
    }
    
    message('==============')
    
    # mapview::mapview(ms_xs) + boi
    # mapview::mapview(line, color = "red") +
    #   mapview::mapview(crosser, color = "dodgerblue") +
    #   mapview::mapview(res, color = "green") +
    #   # mapview::mapview(cross_pt, col.regions = "red") +
    #   start + end  + boi + ms_xs + transects + singles
  }
  
  to_replace <- transect_lines[which(transect_lines$hy_id %in% single_xs$hy_id), ]
  
  # single_xs$hy_id %in% to_replace$hy_id
  # to_replace$hy_id %>% duplicated()
  # single_xs$hy_id %>% duplicated()
  
  # sf::st_drop_geometry(
  #   transect_lines[which(transect_lines$hy_id %in% single_xs$hy_id), ]
  #   )
  # transect_lines[which(transect_lines$hy_id %in% single_xs$hy_id), ]
  # to_replace %>% 
    # sf::st_drop_geometry(to_replace) %>% 
  
  # join and replace original single braided transect lines with the updated/extended lines created above (single_xs)
  to_replace <-  sf::st_as_sf(
                  dplyr::left_join(
                      sf::st_drop_geometry(
                        transect_lines[which(transect_lines$hy_id %in% single_xs$hy_id), ]
                      ),
                      dplyr::select(
                        single_xs, hy_id, cs_id, geometry
                      ),
                      by = c("hy_id", "cs_id")
                      )
                  )
  
  # bind the updated transect lines back with the rest of the dataset
  transect_lines <- dplyr::bind_rows(
                        transect_lines[which(!transect_lines$hy_id %in% single_xs$hy_id), ],
                        to_replace
                        )
  # transect_lines
  # transects
  
  return(transect_lines)
  
  # mapview::mapview(to_replace) + transects + single_xs + singles
  
  # mapview::mapview(to_replace, color = "red") +
  #   # mapview::mapview(crosser, color = "dodgerblue") +
  #   mapview::mapview(singles, color = "dodgerblue") +
  #   mapview::mapview(single_xs, color = "green") +
  #   transects +
  #   single_xs[3, ] + 
  #   single_xs[2, ]
  
  # single_xs
  # table(transect_lines$hy_id %in% single_xs$hy_id)
  # table(single_xs$hy_id %in% transect_lines$hy_id)
  # return(new_lines)  

}
# mapview::mapview(line, color = "red") +
# mapview::mapview(ms_xs, color = "red") +
#   # mapview::mapview(crosser, color = "dodgerblue") +
#   mapview::mapview(singles, color = "dodgerblue") +
#   mapview::mapview(multis, color = "green") +
#   transects
  # mapview::mapview(cross_pt, col.regions = "red") +
  # start + end  + boi + ms_xs + transects + singles
#   dplyr::filter(boi, !comid %in% com)
binary_search_distance <- function(distances, line, geoms_to_cut, direction = "head") {
  # x  = distances
  # ls = line
  # other     = geoms_to_cut
  # direction = dir
  L = 1
  R = length(distances)
  
  # while(L <= R & !flag) {
  while(L <= R) {
    
    M = (L + R) %/% 2
    
    message("L: ", L)
    message("M: ", M)
    message("R: ", R)
    message("x[L]: ", x[L])
    message("x[M]: ", x[M])
    message("x[R]: ", x[R])
    
    if(M == 0 | M == length(distances)) {
      message("EARLY STOPPING bc M = ", M)
      message("RETURNING L = ", L)
      return(L)
    }
    
    new_line <- st_extend_line(line, distances[M], end = direction)
    
    # st_extend_line(line,    distances[0], end = direction)
    # sf::st_intersects(others, new_line)
    # mapview::mapview(others) + new_line + ls
    # (any(lengths(sf::st_intersects(others, new_line)) > 0))
    # check if any of the other braid linestrings get intersected by the extended line:
    # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
    # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
    if(any(lengths(sf::st_intersects(geoms_to_cut, new_line)) > 0)) {
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




