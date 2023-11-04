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
  
  #  moddel attributes
  model_attrs <- arrow::read_parquet("/Users/anguswatters/Desktop/lynker-spatial/model_attributes/nextgen_3D_12.parquet")
  
  # read in nextgen data
  flines <- sf::read_sf(nextgen_paths[i], layer = "flowpaths")
  
  # join flowlines with model atttributes
  flines <- dplyr::left_join(
                  flines,
                  dplyr::select(
                    model_attrs,
                    id, eTW
                    ),
                  by = "id"
                  )
  # calculate bankful width 
  flines <-
    flines %>% 
    dplyr::mutate(
      bf_width = 11 * eTW
    ) %>% 
    dplyr::select(
      hy_id = id, 
      lengthkm,
      tot_drainage_areasqkm,
      bf_width,
      geometry = geom
      )  
    # dplyr::mutate(
    #   bf_width = exp(0.700    + 0.365* log(tot_drainage_areasqkm))
    #   )
  
  system.time({
  
  # create transect lines
  transects <- terrainSliceR::cut_cross_sections(
                      net               = flines,                        # flowlines network
                      id                = "hy_id",                       # Unique feature ID
                      cs_widths         = pmax(50, flines$bf_width), # cross section width of each "id" linestring ("hy_id")
                      num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
                      smooth            = TRUE,                          # smooth lines
                      densify           = 3,                             # densify linestring points
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
  })
  


  # name of file and path to save flowlines gpkg too
  out_file <- gsub("nextgen", "transects", nextgen_files[i])
  out_path <- glue::glue('{transects_dir}{out_file}')
  
  logger::log_info("Saving transects:\n{out_path}")
  
  # save flowlines to out_path (lynker-spatial/02_flowlines/flowlines_<VPU num>.gpkg)
  sf::write_sf(
        dplyr::select(
          dplyr::mutate(transects,
                        cs_source = net_source), 
          hy_id, cs_source, cs_id, cs_measure, cs_length = cs_widths, geometry), 
      out_path
      )
  # sf::write_sf(transects, out_path)
}