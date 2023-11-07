# Generate the flowlines layer for the final cross_sections_<VPU>.gpkg for each VPU
source("runners/workflow/config.R")

library(terrainSliceR)
library(dplyr)
library(sf)

# paths to nextgen datasets
nextgen_files <- list.files(nextgen_dir, full.names = FALSE)

# Full paths to nextgen datasets
nextgen_paths <- glue::glue("{nextgen_dir}{nextgen_files}")

# paths to nextgen datasets
transect_files <- list.files(transects_dir, full.names = FALSE)

# Full paths to nextgen datasets
transect_paths <- glue::glue("{transects_dir}{transect_files}")

# string to fill in "cs_source" column in output datasets
cs_source <- "terrainSliceR"

for (i in 1:length(transect_paths)) {
  # pts = cross_section_pts(cs[lengths(st_intersects(cs, out)) == 1,], 
                          # dem = '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/USGS_Seamless_DEM_1.vrt')
  # sf::st_layers(nextgen_paths[i])
  # i = 1
  logger::log_info("Processing flowlines: {nextgen_files[i]}")
  ################### 
  # read in nextgen data
  transects <- sf::read_sf(transect_paths[i])

  # read in nextgen data
  flines <- sf::read_sf(nextgen_paths[i], layer = "flowpaths")

  transects <-
    transects  %>%
    dplyr::rename(lengthm = cs_length)

  # dplyr::mutate(
  #   bf_width = exp(0.700    + 0.365* log(tot_drainage_areasqkm))
  #   )

  # get cross section point elevations
  cs_pts <- terrainSliceR::cross_section_pts(
            cs  = transects[lengths(st_intersects(transects, flines)) == 1, ],
            points_per_cs = NULL,
            min_pts_per_cs = 10,
            dem = '/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/USGS_Seamless_DEM_1.vrt'
            )

    # classify the cross section points
    cs_pts <-
      cs_pts %>%
      dplyr::rename(cs_widths = lengthm) %>%
      terrainSliceR::classify_points() %>%
      dplyr::mutate(
        X = sf::st_coordinates(.)[,1],
        Y = sf::st_coordinates(.)[,2]
      ) %>%
      dplyr::select(hy_id, cs_id, pt_id, cs_widths, relative_distance, X, Y, Z, class)

  # Drop point geometries, leaving just X, Y, Z values
  cs_pts <- sf::st_drop_geometry(cs_pts)

  # add Z_source column for source of elevation data
  cs_pts <-
    cs_pts %>%
    dplyr::mutate(
      Z_source = cs_source
      ) %>%
    dplyr::relocate(hy_id, cs_id, pt_id, cs_widths, relative_distance, X, Y, Z, Z_source, class)
  ################### 
  
  
  # name of file and path to save flowlines gpkg too
  out_file <- gsub("gpkg", "parquet",
                    gsub("nextgen", "cs_pts", nextgen_files[i])
                    )
  
  out_path <- glue::glue('{cs_pts_dir}{out_file}')
  
  logger::log_info("Saving cross section points:\n{out_path}")
  
  # save cross section points as a parquet to out_path (lynker-spatial/02_cs_pts/cs_pts_<VPU num>.parquet)
  arrow::write_parquet(cs_pts, out_path)

}
