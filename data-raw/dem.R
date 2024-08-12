pacman::p_load(sf, terra, dplyr)

# bounding box of the flowlines for a mainstem "1359733" in VPU 06 from Nextgen v20.1
bb <- 
  sf::read_sf("tests/testthat/testdata/flowlines.gpkg") %>% 
  # sf::st_buffer(1500) %>%
  sf::st_buffer(500) %>%
  sf::st_bbox(crs = sf::st_crs(5070)) %>% 
  sf::st_as_sfc() %>%
  sf::st_transform(4269)

# DEM resource VRT URL
dem_url   <- "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
dem       <- terra::rast(dem_url)

# crop and downsample the raster data for testing 
dem <- 
  dem %>% 
  terra::crop(bb) %>%  
  terra::aggregate(7)

terra::writeRaster(dem, "tests/testthat/testdata/dem_flowlines.tif", overwrite = T)

# usethis::use_data(dem)