pacman::p_load(sf, dplyr)

# where to save test data
FLOWLINES_MISSING_DEPTH_TEST_DATA_PATH <- "tests/testthat/testdata/flowlines_missing_depth.gpkg"

# flowlines_missing_depth for a hy_id of "wb-2146599" in VPU 13 from Nextgen v20.1
# NOTE: this a is a hy_id with known missing data in the primary DEM used during processing / testing
flowlines_missing_depth <- sf::read_sf(FLOWLINES_MISSING_DEPTH_TEST_DATA_PATH)

usethis::use_data(flowlines_missing_depth, overwrite = TRUE)

# -------------------------------------------------------------------------------------
# ---- Used to generate flowlines_missing_depth.gpkg ----
# TODO: does require Nextgen flowlines for VPU 13 from a local gpkg, 
# TODO: need to make this so others can generate this data
# -------------------------------------------------------------------------------------

# library(sf)
# library(dplyr)
# library(geos)
# library(terra)
# 
# # # TODO:
# NEXTGEN_FLOWLINES_VPU_13_PATH          <- "/Users/anguswatters/Desktop/test_improve_flines_11.gpkg"
# 
# # where to save test data
# FLOWLINES_MISSING_DEPTH_TEST_DATA_PATH <- "tests/testthat/testdata/flowlines_missing_depth.gpkg"
# 
# # NOTE: Need nextgen flowlines for VPU 13
# net <- sf::read_sf(NEXTGEN_FLOWLINES_VPU_13_PATH)
# 
# # flowlines with known issues (missing Z data in the DEM)
# missing_Z_ids <- c("wb-2146599")
# # missing_Z_ids <- c("wb-2131572", "wb-2146599")
# 
# all_ids <- 
#   net %>% 
#   dplyr::filter(id %in% missing_Z_ids) %>% 
#   dplyr::pull(id)
# 
# # TODO: code for getting neighbor flowline 
# # TODO: might be useful when dealing with braids tests/code
# # all_ids <- dplyr::pull(
# #   tidyr::pivot_longer(
# #     dplyr::select(
# #       sf::st_drop_geometry(
# #         dplyr::mutate(
# #           dplyr::filter(net, id %in% missing_Z_ids),
# #           to_id = gsub("nex-", "wb-", foi$toid)
# #         )
# #       ),
# #       id, to_id
# #     ),
# #     cols      = c(id, to_id),
# #     names_to  = "name",
# #     values_to = "id"
# #   ),
# #   id
# # )
# 
# foi <-
#   net %>%
#   dplyr::filter(id %in% all_ids) %>%
#   dplyr::select(id,
#                 lengthkm, mainstem, tot_drainage_areasqkm,
#                 geometry = geom
#                 )
# 
# 
# sf::write_sf(foi, FLOWLINES_MISSING_DEPTH_TEST_DATA_PATH)
