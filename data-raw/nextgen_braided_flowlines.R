pacman::p_load(sf, dplyr)

# nextgen_braided_flowlines for a VPU 06 COMID 22180848
nextgen_braided_flowlines<- sf::read_sf("tests/testthat/testdata/nextgen_braided_flowlines.gpkg")

usethis::use_data(nextgen_braided_flowlines, overwrite = TRUE)

# ref_path <- "/Users/anguswatters/Desktop/lynker-spatial/00_reference_features/gpkg/06_reference_features.gpkg"
# ref_fl <- sf::read_sf(ref_path, layer = "flowlines")
# 
# nav <- nhdplusTools::navigate_network(start = 22180848, 
#                                      mode = "UT", 
#                                      distance_km = 100,
#                                      network = ref_fl)
# 
# # sf::write_sf(nav, "tests/testthat/testdata/braided_flowlines.gpkg")
# nextgen_path <- "/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg//nextgen_06.gpkg"
# sf::st_layers(nextgen_path)
# nx_net <- sf::read_sf("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg//nextgen_06.gpkg", layer = "network")
# net_attr <- sf::read_sf("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg//nextgen_06.gpkg", layer = "flowpath_attributes")
# nx_fl <- sf::read_sf("/Users/anguswatters/Desktop/lynker-spatial/v20.1/gpkg//nextgen_06.gpkg", layer = "flowpaths")



# nx_sub <- 
#   nx_fl %>% 
#   dplyr::filter(mainstem %in% unique(nav$levelpathi)) 
# 
# sf::write_sf(nx_sub, "tests/testthat/testdata/nextgen_braided_flowlines.gpkg")