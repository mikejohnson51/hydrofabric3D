pacman::p_load(sf, terra, dplyr)

# save output path
DEM_MISSING_DEPTH_TEST_DATA_PATH        <- "tests/testthat/testdata/dem_missing_depth.tif"

TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH  <- "tests/testthat/testdata/transects_missing_depth.gpkg"
# FLOWLINES_MISSING_DEPTH_TEST_DATA_PATH  <- "tests/testthat/testdata/flowlines_missing_depth.gpkg"

# bounding box of the flowlines / transects identified to have missing values in VPU 13 from Nextgen v20.1
# This part of the DEM has a missing pixel (i.e. NA value)
bb <- 
  sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
  # sf::st_buffer(100) %>%
  sf::st_buffer(300) %>%
  sf::st_bbox(crs = sf::st_crs(5070)) %>% 
  sf::st_as_sfc() %>%
  sf::st_transform(4269)

# DEM resource VRT URL
dem_url   <- "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
dem       <- terra::rast(dem_url)

# crop and downsample the raster data for testing 
dem <- 
  dem %>% 
  terra::crop(bb) 

# NOTE: to view
# mapview::mapview(raster::raster(dem)) + sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH)

terra::writeRaster(dem, DEM_MISSING_DEPTH_TEST_DATA_PATH, overwrite = T)

# usethis::use_data(dem)