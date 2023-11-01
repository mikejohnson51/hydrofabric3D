# Generate the flowlines layer for the final cross_sections_<VPU>.gpkg for each VPU
source("runners/workflow/config.R")

library(terrainSliceR)
library(dplyr)
library(sf)

# paths to nextgen datasets
nextgen_files <- list.files(nextgen_dir, full.names = FALSE)

# Full paths to nextgen datasets
nextgen_paths <- glue::glue("{nextgen_dir}{nextgen_files}")

# string to fill in "cs_source" column in output datasets
net_source <- "terrainSliceR"

# list.files("/Users/anguswatters/Desktop/lynker-spatial/01_flowlines/")

for (i in 1:length(nextgen_paths)) {
  
  # sf::st_layers(nextgen_paths[i])
  
  logger::log_info("Processing flowlines: {nextgen_files[i]}")
  
  flines <- sf::read_sf(nextgen_paths[i], layer = "flowpaths")
  
  flines <-
    flines %>% 
    dplyr::select(
      hy_id = id, 
      lengthkm,
      tot_drainage_areasqkm,
      geometry = geom
      ) %>% 
    dplyr::mutate(
      bf_width = exp(0.700    + 0.365* log(tot_drainage_areasqkm))
      )
  
  # create transect lines
  transects <- terrainSliceR::cut_cross_sections(
                      net               = flines,                        # flowlines network
                      id                = "hy_id",                       # Unique feature ID
                      cs_widths         = pmax(50, flines$bf_width * 7), # cross section width of each "id" linestring ("hy_id")
                      num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
                      smooth            = TRUE,                          # smooth lines
                      densify           = 2,                             # densify linestring points
                      rm_self_intersect = TRUE,                          # remove self intersecting transects
                      fix_braids        = FALSE,                         # whether to fix braided flowlines or not
                      #### Arguments used for when fix_braids = TRUE
                      # terminal_id       = NULL, 
                      # braid_threshold   = NULL,
                      # version           = 2,
                      # braid_method      = "comid",
                      # precision         = 1,
                      add               = TRUE                           # whether to add back the original data
                      )
  
  # name of file and path to save flowlines gpkg too
  out_file <- gsub("nextgen", "transects", nextgen_files[i])
  out_path <- glue::glue('{transects_dir}{out_file}')
  
  logger::log_info("Saving transects:\n{out_path}")
  
  # save flowlines to out_path (lynker-spatial/02_flowlines/flowlines_<VPU num>.gpkg)
  sf::write_sf(transects, out_path)
  
}