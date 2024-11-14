pacman::p_load(sf, dplyr)

# flowlines where 3 mainstems meet (1353978, 1377134, 1391180) in VPU 06 from Nextgen v20.1
junction_flowlines <- sf::read_sf("tests/testthat/testdata/junction_flowlines.gpkg")

usethis::use_data(junction_flowlines, overwrite = TRUE)

# tmp_path <- "/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg/nextgen_06.gpkg"
# 
# # transects <- sf::read_sf(transects_path) %>%
# #   hydrofabric3D::add_tmp_id("hy_id")
# 
# flowlines <- sf::read_sf(tmp_path, layer = "flowpaths") %>%
#   hydrofabric3D::add_powerlaw_bankful_width(
#     total_drainage_area_sqkm_col = "tot_drainage_areasqkm",
#     min_bf_width = 50
#   ) %>%
#   dplyr::select(
#     id,
#     lengthkm,
#     mainstem,
#     tot_drainage_areasqkm,
#     geometry = geom
#   )
# 
# bb <-
#   flowlines %>%
#   dplyr::filter(id %in% c("wb-1010908")) %>%
#   sf::st_buffer(500)  %>%
#   sf::st_bbox() %>%
#   sf::st_as_sfc() %>%
#   sf::st_as_sf()
# 
# flowlines <-
#   flowlines %>%
#   sf::st_filter(bb)
# 
# sf::write_sf(flowlines, "tests/testthat/testdata/junction_flowlines.gpkg")