# # where to save test data
pacman::p_load(sf, dplyr)

# where to save test data
TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- "tests/testthat/testdata/transects_missing_depth.gpkg"

# set of transect lines that are known to hit an NA value in the DEM (from flowlines_missing_depth.gpkg and NA value in dem_missing_depth.tif)
transects_missing_depth <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH)

usethis::use_data(transects_missing_depth, overwrite = TRUE)

# # -------------------------------------------------------------------------------------
# # ---- Used to generate transects_missing_depth.gpkg ----
# # -------------------------------------------------------------------------------------
# 
# library(sf)
# library(dplyr)
# library(geos)
# library(terra)
# 
# # # where to save test data
# TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- "tests/testthat/testdata/transects_missing_depth.gpkg"
# 
# flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines_missing_depth.gpkg")) 
# 
# MIN_BF_WIDTH       <- 25
# ID_COL             <- "hy_id"
# NUM_OF_TRANSECTS   <- 150
# 
# # Cross section point inputs
# flowlines <- 
#   flowlines %>% 
#   add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
#   dplyr::rename(!!sym(ID_COL) := id) %>% 
#   nhdplusTools::rename_geometry("geometry") %>% 
#   dplyr::select(
#     dplyr::any_of(ID_COL), 
#     tot_drainage_areasqkm,
#     bf_width,
#     geometry
#   ) 
# 
# # these specific cross sections will end up hitting an NA point in the DEM 
# transects <- 
#   flowlines %>% 
#   hydrofabric3D::cut_cross_sections(
#     crosswalk_id = ID_COL,
#     num = NUM_OF_TRANSECTS,
#     cs_widths  = flowlines$bf_width
#   ) %>% 
#   dplyr::filter(cs_id %in% c(64, 65, 66, 67, 68))
# 
# sf::write_sf(transects_missing_depth, TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH)
