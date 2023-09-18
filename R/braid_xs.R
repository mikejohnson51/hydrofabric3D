# library(pbapply)
library(terra)
library(sf)
library(dplyr)
# library(terrainSliceR)
library(mapview)
library(smoothr)
library(nhdplusTools)
library(wk)
library(geos)
library(vctrs)
library(AOI)
library(ggplot2)
library(tidyr)

source("R/transects.R")
source("R/braids.R")
source("R/fix_transects.R")

# ***************************************
# ---- VPU 01 study data for braids  ----
# ***************************************
sf::st_layers("/Users/anguswatters/Desktop/terrain_slicer_test_data/01_reference_features.gpkg")
net2 <- sf::read_sf("/Users/anguswatters/Desktop/terrain_slicer_test_data/01_reference_features.gpkg", layer = "flowlines")
names(net2) <- tolower(names(net2))

net2 <-
  net2 %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# tmp <- net2 %>% find_braids(nested = TRUE, add = TRUE)
# rm(tmp, ln)
# ln <- braid_lengths(dplyr::rename(tmp, geometry = geom))
# table(ln$braid_length > 20000)
# ln$braid_length %>% hist(breaks = 50)
system.time({
  transects_v2 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v2",
    add       = TRUE
  )
})

system.time({
  transects_v3 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v3",
    add       = TRUE
  )
})

sf::write_sf(transects_v2, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/01_reference_features_transects_70km_threshold_v2.gpkg")
sf::write_sf(transects_v3, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/01_reference_features_transects_70km_threshold_v3.gpkg")

system.time({
  no_thresh_transects_v2 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v2",
    add       = TRUE
  )
})

system.time({
  no_thresh_transects_v3 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v3",
    add       = TRUE
  )
})

sf::write_sf(no_thresh_transects_v2, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/01_reference_features_transects_no_threshold_v2.gpkg")
sf::write_sf(no_thresh_transects_v3, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/01_reference_features_transects_no_threshold_v3.gpkg")

# ********************************************
# ---- Baton Rouge study data for braids  ----
# ********************************************

# 1 = braid2_components
# 2 = braid_components
# 3 = braid2_comids
# 4 = braid2_neighs
# 5 = braid_comids
# 6 = braid_neighs

net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 450)

# net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 450)
# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 100)
# net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)
# rm(net3)

net2$geometry %>% plot()
# "15175471" %in% net2$comid 
# net3$geometry %>% plot()

net2 <-
  net2 %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))

system.time({
  transects = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = FALSE,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    # ver = "braid2_components",
    add       = TRUE
  )
})

system.time({
  transects_v1 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid2_components",
    add       = TRUE
  )
})

system.time({
  transects_v2 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid_components",
    add       = TRUE
  )
})

system.time({
  transects_v3 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid2_comids",
    add       = TRUE
  )
})
# 1 = braid2_components
# 2 = braid_components
# 3 = braid2_comids
# 4 = braid2_neighs
# 5 = braid_comids
# 6 = braid_neighs
system.time({
  transects_v4 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid2_neighs",
    add       = TRUE
  )
})

system.time({
  transects_v5 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid_comids",
    add       = TRUE
  )
})

system.time({
  transects_v6 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid_neighs",
    add       = TRUE
  )
})

sf::write_sf(transects_v1, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_no_threshold_v1.gpkg")
sf::write_sf(transects_v2, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_no_threshold_v2.gpkg")
sf::write_sf(transects_v3, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_no_threshold_v3.gpkg")
sf::write_sf(transects_v4, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_no_threshold_v4.gpkg")
sf::write_sf(transects_v5, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_no_threshold_v5.gpkg")
sf::write_sf(transects_v6, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_no_threshold_v6.gpkg")

system.time({
  transects_v1_20km = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    braid_threshold = 20000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid2_components",
    add       = TRUE
  )
})

system.time({
  transects_v2_20km = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    braid_threshold = 20000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid_components",
    add       = TRUE
  )
})

system.time({
  transects_v3_20km = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    braid_threshold = 20000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid2_comids",
    add       = TRUE
  )
})
# 1 = braid2_components
# 2 = braid_components
# 3 = braid2_comids
# 4 = braid2_neighs
# 5 = braid_comids
# 6 = braid_neighs
system.time({
  transects_v4_20km = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    braid_threshold = 20000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid2_neighs",
    add       = TRUE
  )
})

system.time({
  transects_v5_20km = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    braid_threshold = 20000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid_comids",
    add       = TRUE
  )
})

system.time({
  transects_v6_20km = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    braid_threshold = 20000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "braid_neighs",
    add       = TRUE
  )
})

sf::write_sf(transects_v1_20km, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_20km_threshold_v1.gpkg")
sf::write_sf(transects_v2_20km, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_20km_threshold_v2.gpkg")
sf::write_sf(transects_v3_20km, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_20km_threshold_v3.gpkg")
sf::write_sf(transects_v4_20km, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_20km_threshold_v4.gpkg")
sf::write_sf(transects_v5_20km, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_20km_threshold_v5.gpkg")
sf::write_sf(transects_v6_20km, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_20km_threshold_v6.gpkg")


bb <- 
  # buff <- 
  transects_v1 %>% 
  dplyr::filter(hy_id == 167800608) %>% 
  sf::st_buffer(5000) %>% 
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>% 
  sf::st_as_sf() 
# mapview::mapview(net2, color = "dodgerblue") + 
#   mapview::mapview(transects_v1, color = "red") + 
#   mapview::mapview(transects_v2, color = "red") + 
#   mapview::mapview(transects_v3, color = "red") +
#   mapview::mapview(transects_v4, color = "red") + buff

buff %>% 
  sf::st_bbox()
# plot(buff$x)
# buff %>% 
#   sf::st_bbox() %>% 
#   sf::st_as_sf()
# bb <- sf::st_bbox(c(xmin = -91.39879, 
#                     xmax = -91.37526,
#                     ymax = 30.57633,
#                     ymin = 30.47881), 
#                   crs = sf::st_crs(4269)
#                   )
trans_files <- list.files("/Users/anguswatters/Desktop/terrain_slicer_test_data/output/", full.names = F)

seq(1, length(trans_files), 6) + 6

plot_list1 <- list()
plot_list2 <- list()
plot_list3 <- list()

# for (i in 1:length(trans_files)) {
for (i in 1:6) {
  
  # i = 1
  title = gsub(".gpkg", "", trans_files[i])
  title = gsub("baton_rouge_transects_", "", title)
  shp <- sf::read_sf(
    paste0("/Users/anguswatters/Desktop/terrain_slicer_test_data/output/", trans_files[i])
    )
  shp <- sf::st_crop(shp, bb)
  flines <- sf::st_crop(net2, bb)
  # plot(shp$geom)
  trans_plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = flines, color = "dodgerblue") +
    ggplot2::geom_sf(data = shp, color = "red", alpha = 0.7, lwd = 1) + 
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 18))
  
  plot_list1[[i]] <- trans_plot

}

plot_list1 <- Filter(function(x) !is.null(x), plot_list1)

for (i in 7:12) {
  
  # i = 3
  title = gsub(".gpkg", "", trans_files[i])
  title = gsub("baton_rouge_transects_", "", title)
  shp <- sf::read_sf(
    paste0("/Users/anguswatters/Desktop/terrain_slicer_test_data/output/", trans_files[i])
  )
  shp <- sf::st_crop(shp, bb)
  flines <- sf::st_crop(net2, bb)
  # plot(shp$geom)
  trans_plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = flines, color = "dodgerblue") +
    ggplot2::geom_sf(data = shp, color = "red", alpha = 0.7, lwd = 1) + 
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 18))
  
  plot_list2[[i]] <- trans_plot
  
}

plot_list2 <- Filter(function(x) !is.null(x), plot_list2)


for (i in 13:18) {
  
  # i = 3
  title = gsub(".gpkg", "", trans_files[i])
  title = gsub("baton_rouge_transects_", "", title)
  shp <- sf::read_sf(
    paste0("/Users/anguswatters/Desktop/terrain_slicer_test_data/output/", trans_files[i])
  )
  shp <- sf::st_crop(shp, bb)
  flines <- sf::st_crop(net2, bb)
  # plot(shp$geom)
  trans_plot <- 
    ggplot2::ggplot() +
    ggplot2::geom_sf(data = flines, color = "dodgerblue") +
    ggplot2::geom_sf(data = shp, color = "red", alpha = 0.7, lwd = 1) + 
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 18))
  
  plot_list3[[i]] <- trans_plot
  
}

plot_list3 <- Filter(function(x) !is.null(x), plot_list3)

library(gridExtra)

combined_plot1 <- do.call(grid.arrange, c(plot_list1, ncol = 2))
combined_plot2 <- do.call(grid.arrange, plot_list2)
combined_plot3 <- do.call(grid.arrange, plot_list3)
library(patchwork)
# (plot_list1[[1]] + plot_list1[[2]])/(plot_list1[[3]] + plot_list1[[4]])/(plot_list1[[5]] + plot_list1[[6]])
(plot_list1[[1]] + plot_list1[[2]] + plot_list1[[3]])/(plot_list1[[4]] + plot_list1[[5]] + plot_list1[[6]])
(plot_list2[[1]] + plot_list2[[2]] + plot_list2[[3]])/(plot_list2[[4]] + plot_list2[[5]] + plot_list2[[6]])
(plot_list3[[1]] + plot_list3[[2]] + plot_list3[[3]])/(plot_list3[[4]] + plot_list3[[5]] + plot_list3[[6]])

combined_plot <- do.call(grid.arrange, plot_list)

# Create a matrix of the bounding box coordinates
bbox_coords <- matrix(c(-91.55113, -91.32282, -91.32282, -91.55113,
                        30.43339, 30.43339, 30.62234, 30.62234), 
                      ncol = 2, byrow = TRUE)

# Create a polygon using the bounding box coordinates
bbox_polygon <- st_polygon(list(matrix(c(-91.55113, -91.32282, -91.32282, -91.55113,
                                         30.43339, 30.43339, 30.62234, 30.62234), 
                                       ncol = 2, byrow = TRUE)))
sf::st_bbox()
# Create a simple features object
bbox_sf <- st_sf(geometry = bbox_polygon)
# tmp <- net2 %>% find_braids(nested = TRUE, add = TRUE)
# rm(tmp, ln)
# ln <- braid_lengths(dplyr::rename(tmp, geometry = geom))
# table(ln$braid_length > 20000)
# ln$braid_length %>% hist(breaks = 50)
system.time({
  transects_v2 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v2",
    add       = TRUE
  )
})

system.time({
  transects_v3 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v3",
    add       = TRUE
  )
})

mapview::mapview(net2, color = "dodgerblue") +
  mapview::mapview(transects_v2, color = "green") +
mapview::mapview(transects_v3, color = "red")
# 1 = braid2_components
# 2 = braid_components
# 3 = braid2_comids
# 4 = braid2_neighs
# 5 = braid_comids
# 6 = braid_neighs
sf::write_sf(transects_v2, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_70km_threshold_v2.gpkg")
sf::write_sf(transects_v3, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_70km_threshold_v3.gpkg")

system.time({
  transects_v2_20km = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    braid_threshold = 20000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v2",
    add       = TRUE
  )
})

system.time({
  transects_v3_20km = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    braid_threshold = 20000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v3",
    add       = TRUE
  )
})

mapview::mapview(net2, color = "dodgerblue") +
  mapview::mapview(transects_v2_20km, color = "green") +
  mapview::mapview(transects_v3_20km, color = "red")

sf::write_sf(transects_v2_20km, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_20km_threshold_v2.gpkg")
sf::write_sf(transects_v3_20km, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_20km_threshold_v3.gpkg")

system.time({
  no_thresh_transects_v2 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v2",
    add       = TRUE
  )
})

system.time({
  no_thresh_transects_v3 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    # braid_threshold = 70000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v3",
    add       = TRUE
  )
})

sf::write_sf(no_thresh_transects_v2, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_no_threshold_v2.gpkg")
sf::write_sf(no_thresh_transects_v3, "/Users/anguswatters/Desktop/terrain_slicer_test_data/output/baton_rouge_transects_no_threshold_v3.gpkg")

# mapview::mapview(net2, color ="dodgerblue") + 
#   mapview::mapview(transects_v2, color ="red")
# filename = "/Users/anguswatters/Desktop/transect_figs/imgs/transect_fig_01.png",
# # # *******************************
# # # ---- Test data for braids  ----
# # # *******************************
net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 100)
# net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 450)
# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 100)
# net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)
# rm(net3)
net2$geometry %>% plot()
# "15175471" %in% net2$comid 
# net3$geometry %>% plot()
net2 <-
  net2 %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))


system.time({
  transects_v1 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    braid_threshold = 100000,
    terminal_id = "terminalpa",
    rm_self_intersect = TRUE,
    ver = "v1",
    add       = TRUE
  )
})

transects_v1
plot(transects_v1$geometry)

system.time({
  transects_v2 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    braid_threshold = 100000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v2",
    add       = TRUE
  )
})

transects_v2
plot(transects_v2$geometry)

system.time({
  transects_v3 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    # braid_threshold = NULL,
    # braid_threshold = "drop_max",
    braid_threshold = 100000,
    terminal_id = "terminalpa",
    # terminal_id = NULL,
    rm_self_intersect = TRUE,
    ver = "v3",
    add       = TRUE
  )
})

transects_v3
plot(transects_v3$geometry)

mapview::mapview(net2, color = "dodgerblue") +
  mapview::mapview(transects_v1, color = "green") +
  mapview::mapview(transects_v2, color = "red") +
  mapview::mapview(transects_v3, color = "gold") 
  # mapview::mapview(transects_v22, color = "red")

# # # *******************************
# # # *******************************
system.time({
  no_fix_transects = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 6,
    densify = 2,
    fix_braids = F,
    # terminal_id = NULL,
    braid_threshold = NULL,
    terminal_id = "terminalpa",
    add       = TRUE
  )
})
# # # *******************************
# # # *******************************

# # # *******************************
# # # ---- Test data for braids  ----
# # # *******************************
# net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 450)
net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 100)
# net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)
# rm(net3)
net2$geometry %>% plot()
# "15175471" %in% net2$comid 
# net3$geometry %>% plot()
net2 <-
  net2 %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 

system.time({
  transects_v1 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    terminal_id = "terminalpa",
    rm_self_intersect = TRUE,
    v2 = FALSE,
    add       = TRUE
  )
})
transects_v1
plot(transects_v1$geometry)

system.time({
  transects_v2 = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = NULL,
    # braid_threshold = "drop_max",
    # braid_threshold = 20000,
    terminal_id = "terminalpa",
    rm_self_intersect = TRUE,
    v2 = TRUE,
    add       = TRUE
  )
})

transects_v2
plot(transects_v2$geometry)

mapview::mapview(net2, color = "dodgerblue") +
  mapview::mapview(transects_v1, color = "green") +
  mapview::mapview(transects_v2, color = "red") 
  # mapview::mapview(transects_v22, color = "red") 
# # # *******************************
# # # ---- Test data for braids  ----
# # # *******************************

# net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 300)
# net3 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 150)
net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 60)
net2$geometry %>% plot()
# net3$geometry %>% plot()
net2 <-
  net2 %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 

system.time({
  transect_lines = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 6,
    densify = 2,
    fix_braids = F,
    # terminal_id = NULL,
    braid_threshold = NULL,
    terminal_id = "terminalpa",
    add       = TRUE
  )
})

plot(no_fix$geometry)

system.time({
  no_threshold = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 10,
    densify = 3,
    fix_braids = TRUE,
    # terminal_id = NULL,
    braid_threshold = NULL,
    terminal_id = "terminalpa",
    add       = TRUE
  )
})

plot(no_threshold$geometry)

system.time({
  threshold = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 10,
    densify = 3,
    fix_braids = T,
    # terminal_id = NULL,
    braid_threshold = 10000,
    terminal_id = "terminalpa",
    add       = TRUE
  )
})
plot(no_threshold$geometry)
plot(threshold$geometry)

tmp_thresholding <- 
  no_threshold %>% 
  dplyr::mutate(
    new_length = as.numeric(sf::st_length(geometry))
  ) %>% 
  dplyr::filter(new_length <= 2000)
dplyr::filter(
  dplyr::mutate(no_threshold,
                new_length = as.numeric(sf::st_length(geometry))
                ), 
  new_length <= 2000
  )
ggplot2::ggplot() +
  ggplot2::geom_sf(data = tmp_thresholding, color = "red", lwd = 1) +
  ggplot2::geom_sf(data = net2, color = "dodgerblue") +
  ggplot2::labs(
    title = "Threshold = 0"
    # subtitle = "Transect extension"
  )


ggplot2::ggplot() +
  ggplot2::geom_sf(data = no_threshold, color = "red", lwd = 1) +
  ggplot2::geom_sf(data = net2, color = "dodgerblue") +
  ggplot2::labs(
    title = "Threshold = 0 km"
    # subtitle = "Transect extension"
  ) + 
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold")
  )

ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(
    dplyr::mutate(no_threshold,
                  new_length = as.numeric(sf::st_length(geometry))
    ), 
    new_length <= 1000
  ), 
  color = "red", 
  lwd = 1) +
  ggplot2::geom_sf(data = net2, color = "dodgerblue") +
  ggplot2::labs(
    title = "Threshold = 1 km"
    # subtitle = "Transect extension"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold")
  )

ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(
                                  dplyr::mutate(no_threshold,
                                                new_length = as.numeric(sf::st_length(geometry))
                                  ), 
                                  new_length <= 2000
                                ), 
                   color = "red", 
                   lwd = 1) +
  ggplot2::geom_sf(data = net2, color = "dodgerblue") +
  ggplot2::labs(
    title = "Threshold = 2 km"
    # subtitle = "Transect extension"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold")
  )

ggplot2::ggplot() +
  ggplot2::geom_sf(data = dplyr::filter(
    dplyr::mutate(no_threshold,
                  new_length = as.numeric(sf::st_length(geometry))
    ), 
    new_length <= 3000
  ), 
  color = "red", 
  lwd = 1) +
  ggplot2::geom_sf(data = net2, color = "dodgerblue") +
  ggplot2::labs(
    title = "Threshold = 3km"
    # subtitle = "Transect extension"
  )

ggplot2::ggplot() +
  ggplot2::geom_sf(data = tmp_thresholding, color = "red", lwd = 1) +
  ggplot2::geom_sf(data = net2, color = "dodgerblue") +
  ggplot2::labs(
    title = "Threshold = 2km"
    # subtitle = "Transect extension"
  )

ggplot2::ggplot() +
  ggplot2::geom_sf(data = threshold, color = "red", lwd = 1) +
  ggplot2::geom_sf(data = net2, color = "dodgerblue") +
  ggplot2::labs(
    title = "Threshold = 30km"
    # subtitle = "Transect extension"
  )
# cut_cross_sections <- function(
#     net, 
#     id                = NULL,
#     cs_widths         = 100, 
#     num               = 10,
#     smooth            = TRUE,
#     densify           = 2,
#     rm_self_intersect = TRUE,
#     fix_braids        = TRUE,
#     terminal_id       = NULL,
#     braid_threshold   = NULL,
#     add               = FALSE
# ) {
# 
# system.time({
#   new_way = cut_cross_sections3(
#     net       = net2,
#     id        = "comid",
#     cs_widths = pmax(50, net2$bf_width * 7),
#     num       = 5,
#     densify = 2,
#     fix_braids = FALSE,
#     # terminal_id = NULL,
#     terminal_id = "terminalpa",
#     add       = TRUE,
#     use_original = FALSE
#   )
# })
# 
# system.time({
#   original_transect_lines = cut_cross_sections3(
#     net       = net2,
#     id        = "comid",
#     cs_widths = pmax(50, net2$bf_width * 7),
#     num       = 5,
#     densify = 2,
#     fix_braids = FALSE,
#     # terminal_id = NULL,
#     terminal_id = "terminalpa",
#     add       = TRUE,
#     use_original = T
#   )
# })

# # *******************************
# # ---- Test data for braids  ----
# # *******************************

# net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 100)
# net2 <- 
#   net2 %>%
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# braids <- net2 %>% find_braids(add = T, nested = F)
#  braids$braid_id %>% unique() %>% length()
# # net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 50)
# system.time({
#   fixed_transect_lines = cut_cross_sections3(
#     net       = net2,
#     id        = "comid",
#     cs_widths = pmax(50, net2$bf_width * 7),
#     num       = 5,
#     densify = 2,
#     fix_braids = TRUE,
#     # terminal_id = NULL,
#     terminal_id = "terminalpa",
#     add       = TRUE,
#     use_original = T
#   )
# })
# 
# system.time({
#   original_transect_lines = cut_cross_sections3(
#     net       = net2,
#     id        = "comid",
#     cs_widths = pmax(50, net2$bf_width * 7),
#     num       = 5,
#     densify = 2,
#     fix_braids = FALSE,
#     # terminal_id = NULL,
#     terminal_id = "terminalpa",
#     add       = TRUE,
#     use_original = T
#   )
# })
# 
#  coords <- matrix(
#   c(-91.39, 30.42,
#     -91.27, 30.42,
#     -91.39, 30.35,
#     -91.27, 30.35),
#   ncol = 2,
#   byrow = TRUE
# )
#  
# bb <- sf::st_as_sf(
#   sf::st_as_sfc(
#   sf::st_bbox(
#     c(
#       xmin = -91.39,
#       xmax = -91.30,
#       ymax = 30.30,
#       ymin = 30.40
#       )
#     ),
#   crs = 4326
#   )
# )
# network <- net2
# network <- sf::st_crop(network, bb)
# plot(network$geometry)
# 
# fixed_transects <- sf::st_crop(fixed_transect_lines, bb) %>% dplyr::mutate(lab = "Fixed")
# original_transects <- sf::st_crop(original_transect_lines, bb) %>% dplyr::mutate(lab = "Original")
# 
# transects <- dplyr::bind_rows(
#   fixed_transects, original_transects
# ) %>% 
#   dplyr::mutate(
#     lab = factor(lab, levels = c("Original", "Fixed"))
#   )
# # network %>% 
# #   dplyr::mutate(lab = "Network") %>% 
# 
# trans_plot1 <- 
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = network) + 
#   ggplot2::labs(
#     title = "Braided system near Baton Rouge, LA",
#     color = ""
#   ) +
#   ggplot2::theme(
#     # legend.position = "bottom"
#     # legend.position = c(0.1, 0.1),  
#     # legend.justification = c(-6, 0.5)
#     plot.title = ggplot2::element_text(size = 16),
#     legend.position = c(0.3, 0.7),
#     legend.justification = c(-6, 0.5),
#     legend.text = ggplot2::element_text(size = 12)
#     )
# trans_plot1
# ggplot2::ggsave(
#   plot = trans_plot1,
#   filename = "/Users/anguswatters/Desktop/transect_figs/imgs/transect_fig_01.png",
#   scale = 1
# )
# 
# trans_plot2 <- 
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = network) + 
#   ggplot2::geom_sf(data = dplyr::filter(transects, lab == "Original"),
#                    ggplot2::aes(color = lab), lwd = 1.5) +
#   ggplot2::scale_color_manual(
#     values = c("Original" = "red",
#                "Fixed" = "forestgreen"),
#     guide = ggplot2::guide_legend(
#       direction = "vertical",
#       title.position = "top"
#     )
#   ) +
#   ggplot2::labs(
#     title = "Braided system near Baton Rouge, LA",
#     color = ""
#   ) +
#   ggplot2::theme(
#     # legend.position = "bottom"
#     # legend.position = c(0.1, 0.1),  
#                  # legend.justification = c(-6, 0.5)
#     plot.title = ggplot2::element_text(size = 16),
#     legend.position = c(0.3, 0.7),
#     legend.justification = c(-6, 0.5),
#     legend.text = ggplot2::element_text(size = 12)
#     )
# trans_plot2
# 
# ggplot2::ggsave(
#   plot = trans_plot2,
#   filename = "/Users/anguswatters/Desktop/transect_figs/imgs/transect_fig_02.png",
#   scale = 1
# )
# 
# trans_plot3 <- 
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = network) + 
#   ggplot2::geom_sf(data = transects, ggplot2::aes(color = lab), lwd = 1.5) +
#   ggplot2::scale_color_manual(
#     values = c("Original" = "red",
#                "Fixed" = "forestgreen"),
#     guide = ggplot2::guide_legend(
#       direction = "vertical",
#       title.position = "top"
#     )
#   ) +
#   ggplot2::labs(
#     title = "Braided system near Baton Rouge, LA",
#     color = ""
#   ) +
#   ggplot2::theme(
#     # legend.position = "bottom"
#     # legend.position = c(0.1, 0.1),
#     # legend.justification = c(-6, 0.5)
#     plot.title = ggplot2::element_text(size = 16),
#     legend.position = c(0.3, 0.7),
#     legend.justification = c(-6, 0.5),
#     legend.text = ggplot2::element_text(size = 12)
#     )
# # trans_plot3
# ggplot2::ggsave(
#   plot = trans_plot3,
#   filename = "/Users/anguswatters/Desktop/transect_figs/imgs/transect_fig_03.png",
#   scale = 1
# )
# 
# trans_plot4 <- 
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = network) + 
#   ggplot2::geom_sf(
#     data = transects,
#                    ggplot2::aes(color = lab), lwd = 1.5) +
#   ggplot2::scale_color_manual(
#     values = c("Original" = "transparent",
#                "Fixed" = "forestgreen"),
#     guide = ggplot2::guide_legend(
#       direction = "vertical",
#       title.position = "top"
#     )
#   ) +
#   # ggplot2::geom_sf(data = dplyr::filter(transects, lab == "Fixed"),
#   #                  ggplot2::aes(color = lab), lwd = 1.5) +
#   # ggplot2::scale_color_manual(
#   #   values = c("Original" = "red",
#   #              "Fixed" = "forestgreen"),
#   #   guide = ggplot2::guide_legend(
#   #     direction = "vertical",
#   #     title.position = "top"
#   #   )
#   # ) +
#   ggplot2::labs(
#     title = "Braided system near Baton Rouge, LA",
#     color = ""
#   ) +
#   ggplot2::theme(
#     plot.title = ggplot2::element_text(size = 16),
#     legend.position = c(0.3, 0.7),
#     legend.justification = c(-6, 0.5),
#     legend.text = ggplot2::element_text(size = 12)
#     # legend.position = c(0.1, 0.1),  
#     # legend.justification = c(-6, 0.5)
#     )
# trans_plot4
# ggplot2::ggsave(
#   plot = trans_plot4,
#   filename = "/Users/anguswatters/Desktop/transect_figs/imgs/transect_fig_04.png",
#   scale = 1
# )
# png_files <- list.files("/Users/anguswatters/Desktop/transect_figs/imgs/", full.names = T)
# 
# gifski::gifski(
#   png_files = png_files,
#   gif_file = "/Users/anguswatters/Desktop/transect_figs/transects.gif",
#   width = 2000,
#   height = 1400,
#   delay = 2,
#   loop = TRUE,
#   progress = TRUE
# )
# 
# mapview::mapview(network, color = "dodgerblue") +
#   mapview::mapview(original_transect_lines, color = "red")  +
#   mapview::mapview(fixed_transect_lines, color = "green") 
# # # ***********************************************************************************
# # # ---- VISUALIZE BRAID GROUPINGS (within first "xs" loop in fix_braid_transects) ----
# # # ***********************************************************************************
# 
# plot_list <- list()
# 
# seq(1, nrow(xs), 200)
# sq <- seq(1, nrow(xs), 100)
# paste0("plot_", 1:length(sq))
# # rm(k)
# for(k in sq) {
#   
#   # for(i in 1:17) {
#   message("k: ", k, "/", nrow(xs))
#   # comid of transect line
#   com <- xs$hy_id[k]
#   
#   # braid IDs of interest
#   bids <- strsplit(xs[k, ]$braid_id, ", ")[[1]]
#   message("bids:\n- ", paste0(bids, sep = "\n- "))
#   
#   # bids
#   # comids_in_braid_ids(x = braids, braid_ids = bids)
#   
#   # ggplot2::ggplot() + 
#   # ggplot2::geom_sf(data = components, ggplot2::aes(color = component_id))+
#   # gghighlight::gghighlight(component_id %in% unique(components[bid_in_braids, ]$component_id))
#   # 708-712
#   # 648 - 649
#   plot_title = paste0("Braid IDs: ", paste0(bids, collapse = ", "))
#   
#   # # get neighboring braid ID for our current braid
#   neighbor_braids <- get_neighbor_braids(x = braids, ids = bids, only_unique = T)
#   # 
#   # get neighboring COMIDs for our current braid
#   neighbor_comids <- comids_in_braid_ids(x = braids, braid_ids = bids)
#   
#   # remove self comid
#   neighbor_comids <- neighbor_comids[neighbor_comids != com]
#   
#   # braid flowlines other than self that are within our given braid id or are nearby
#   others <- dplyr::filter(
#     braids,
#     braid_id %in% neighbor_braids,
#     comid != com
#   )
#   
#   others2 <- dplyr::filter(
#     braids,
#     comid %in% neighbor_comids 
#   )
#   
#   plt <- 
#     ggplot2::ggplot() +
#     ggplot2::geom_sf(data = braids, color = "dodgerblue") +
#     ggplot2::geom_sf(data = others, color = "red", lwd = 1.5) +
#     ggplot2::geom_sf(data = others2, color = "green", lwd = 1) +
#     ggplot2::geom_sf(data = xs[i, ], color = "gold", lwd =6) +
#     ggplot2::labs(title = plot_title)
#   
#   plot_list[[k]] = plt
#   message("=========================")
# }
# is.null(plot_list)
# 
# plot_list <- plot_list[sapply(plot_list, function(v) { !is.null(v)})]
# plot_list["plot_1"]
# names(plot_list)
# length(plot_list)
# plot_list <- stats::setNames(plot_list,      paste0("plot_", 1:length(sq)))
# 
# plot_list[[1]]
# plot_list[[2]]
# plot_list[[3]]
# plot_list[[4]]
# plot_list

# # *******************************
# # ---- Test data for braids  ----
# # *******************************
