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
net <-
  terrainSliceR::linestring %>% 
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
net

net <- nhdplusTools::navigate_network(
  start       = 101,
  mode        = "UT", 
  distance_km = 100
  ) %>% 
  dplyr::select( comid, divergence, totdasqkm, fromnode, tonode) 

net2 <-
  net2 %>% 
  dplyr::select( comid, divergence, totdasqkm, fromnode, tonode) %>% 
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))

# rm(net2)
plot(net2$geometry)
plot(net$geometry)

transects = cut_cross_sections_curr(
  net       = net2,
  id        = "comid",
  cs_widths = pmax(50, net$bf_width * 7),
  num       = 4,
  add       = TRUE
  )
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

# Get the comids for braided flowlines
braids <- find_braids(
  network = net2,
  return_as = "dataframe",
  add = TRUE
)

# multibraids
mb <- dplyr::filter(braids, is_multibraid)

# all braids
b <-   dplyr::filter(braids, braid_id != "no_braid")

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

mapview::mapview(braids) + 
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




