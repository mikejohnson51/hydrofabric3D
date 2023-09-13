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


# 
# # # *******************************
# # # ---- Test data for braids  ----
# # # *******************************
# 
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

# # *******************************
# # ---- Test data for braids  ----
# # *******************************
